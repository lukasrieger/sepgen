package biabduce

import biabduce.Cont.{Continue, Stop}
import biabduce.Expression.*
import biabduce.Mode.{CalcMissing, FailOnMissing}
import biabduce.ProverException.{FalseAtom, FalseExprs, FalsePointsTo, ImplFalse, MissingExc}
import biabduce.Spatial.{Emp, PointsTo, True, given_Conversion_Spatial_L}
import biabduce.Pure.{=:=, *}
import pure.Name

import language.experimental.saferExceptions
import scala.annotation.tailrec


type Subst = Map[Expression, Expression]
type SubstP = (Subst, Subst)

object Subst:
  def empty: Subst = Map.empty

  extension (sub: Subst)
    infix def rangeMap(f: Expression => Expression): Subst =
      sub map ((i, e) => (i, f(e)))

object SubstP:
  def empty: SubstP = (Map.empty, Map.empty)

enum Mode:
  case CalcMissing
  case FailOnMissing

enum Cont:
  case Continue
  case Stop

enum ProverException extends Exception:
  case ImplFalse(reason: String, subst: SubstP, cont: Cont)
  case FalsePointsTo(reason: String, subst: SubstP, pointsTo: Spatial.S, cont: Cont)
  case FalseExprs(reason: String, subst: SubstP, exp1: Expression, exp2: Expression, cont: Cont)
  case FalseAtom(reason: String, subst: SubstP, atom: Pure, cont: Cont)
  case FalseSigma(reason: String, subst: SubstP, sigma: Spatial.S, cont: Cont)
  case MissingExc(reason: String)


case class ProverState(var missingPi: Pure.L, var missingSigma: Spatial.L):
  def modifyPi(fn: Pure.L => Pure.L): Unit = missingPi = fn(missingPi)
  def modifySigma(fn: Spatial.L => Spatial.L): Unit = missingSigma = fn(missingSigma)

type Missing = ProverState
type Frame = Spatial

case class Splitting(sub: SubstP, frame: Frame, missingPi: Pure.L, missingSigma: Spatial.L)


def runBiabduction(
                    prop1: Prop,
                    prop2: Prop
                  ): Option[Splitting] =
  try
    val proverState = ProverState(missingPi = List.empty, missingSigma = List.empty)
    val ((sub1, sub2), frame) = _checkImplication(prop1.toQuantFree, prop2.toQuantFree)(using Mode.CalcMissing, proverState)
    Some(Splitting((sub1, sub2), frame, proverState.missingPi, proverState.missingSigma))
  catch
    case e: ProverException =>
      println(s"Failed to find footprint due to: $e")
      None


def structExpImply(
                    subs: SubstP,
                    se1: Expression,
                    se2: Expression
                  )(using mode: Mode, st: ProverState): SubstP throws ProverException =
  val (e1, e2) = (se1 subst subs._1) -> (se2 subst subs._2)

  (e1, e2) match
    case (ProgramVar(n1), ProgramVar(n2)) =>
      if (n1 == n2) then subs else throw FalseExprs("expressions not equal", subs, e1, e2, Continue)
    case (ProgramVar(n1), v2@LogicalVar(n2)) =>
      (subs._1, subs._2 + (v2 -> e1))
    case (LogicalVar(n1), ProgramVar(n2)) =>
      throw FalseExprs("exps (lhs logical, rhs prg)", subs, e1, e2, Continue)
    case (v1@LogicalVar(n1), v2@LogicalVar(n2)) =>
      val v1_ = v1.fresh(n1.index.getOrElse(0) + 1)
      (subs._1 + (v1 -> v1_), subs._2 + (v2 -> v1_))
    case (AnyTerm(t1), AnyTerm(t2)) =>
      if t1 == t2 then subs else throw FalseExprs("expressions not equal", subs, e1, e2, Continue)
    case (t@AnyTerm(t1), v@LogicalVar(n2)) => (subs._1, subs._2 + (v -> t))
    case (v@LogicalVar(v1), t@AnyTerm(t1)) => mode match
      case Mode.CalcMissing =>
        st.missingPi = (se1 =:= se2) ::: st.missingPi
        subs
      case Mode.FailOnMissing => throw FalseExprs("expressions not equal", subs, e1, e2, Continue)
    case _ => throw FalseExprs("expressions can't be proven to imply each other.", subs, e1, e2, Stop)


def filterNeLhs(subst: Subst, e0: Expression, field: Option[String], sigma: Spatial.S): Option[Spatial.S] = sigma match
  case p@PointsTo(e, f, _) if (e subst subst) == e0 && f == field => Some(p)
  case _ => None


/**
 * TODO: Simple predicate abduction.
 * If a predicate appears in hpred2 -> unroll it and recurse
 * If a PointsTo assertion appears in hpred2 attempt to either find a matching PointsTo on the lhs OR
 * Find a predicate which mentions the base pointer as an argument and unroll said predicate on the lhs and then recurse.
 */
def implySpatial(
                  subs: SubstP,
                  prop1: QuantFree,
                  hpred2: Spatial.S
                )(using mode: Mode, st: ProverState): (SubstP, QuantFree) throws ProverException =
  @tailrec
  def propFind(prop: Spatial.L, filter: Spatial.S => Option[Spatial.S]): Option[Spatial.S] =
    prop match
      case hpred :: sigma_ => filter(hpred) match
        case Some(value) => Some(value)
        case None => propFind(sigma_, filter)
      case Nil => None
       
  
  hpred2 match
    case True => (subs, prop1)
    case Spatial.Emp => (subs, prop1)
    case PointsTo(pointer, field, cell) => 
      val e2 = pointer subst subs._2
      e2 match
        case biabduce.Expression.LogicalVar(v) =>
          throw ProverException.FalseSigma("rhs |-> not implemented", subs, hpred2, Stop)
        case _ => ()

      propFind(prop1.sigma, filterNeLhs(subs._1, e2, field, _)) match
        case None => throw FalsePointsTo("lhs does not have |->", subs, hpred2, Continue)
        case Some(value) => value match
          case PointsTo(_, _, se1) => 
            try 
              val subs_ = structExpImply(subs, se1, cell)
              (subs_, prop1)
            catch
              case e: ProverException =>
                mode match
                  case Mode.CalcMissing => throw MissingExc("\"could not match |-> present on both sides\"")
                  case Mode.FailOnMissing => throw e
          case Emp => throw MissingExc("\"could not match |-> (Emp)\"")
          case True => throw MissingExc("\"could not match |-> (True)\"")

def sigmaImply(
                subs: SubstP,
                prop1: QuantFree,
                sigma2: Spatial.L
              )(using mode: Mode, st: ProverState): (SubstP, QuantFree) throws ProverException =
  sigma2 match
    case Nil => (subs, prop1)
    case spatial :: sigma2_ =>
      val (subs_, prop1_) = try implySpatial(subs, prop1, spatial)
        catch case e: ProverException =>
          mode match
            case Mode.CalcMissing =>
              st.missingSigma = spatial :: st.missingSigma
              (subs, prop1)
            case _ => throw e
      try sigmaImply(subs_, prop1_, sigma2_)
      catch case e: ProverException =>
        mode match
          case Mode.CalcMissing =>
            st.missingSigma = sigma2 ::: st.missingSigma
            (subs, prop1)
          case Mode.FailOnMissing => throw e


def _checkImplication(
                       prop1: QuantFree,
                       prop2: QuantFree
                     )(using mode: Mode, st: ProverState): (SubstP, Spatial) throws ProverException  =
  val (pi1, sigma1) = prop1.refine
  val (pi2, sigma2) = prop2.refine
  val substitutions = preCheckPureImplication(SubstP.empty, pi1, pi2)

  val ((sub1, sub2), frame) = sigmaImply(substitutions, prop1, sigma2)
  val (pi1_, sigma1_) = (pi1 subst sub1, sigma1 subst sub1)

  pi2 foreach (implyAtom((sub1, sub2), pi1_, sigma1_, _))

  (sub1, sub2) -> frame.sigma


@tailrec
def preCheckPureImplication(
                             subs: SubstP,
                             pi1: Pure.L,
                             pi2: Pure.L
                           )(using mode: Mode, st: ProverState): SubstP throws ProverException  =
  pi2 match
    case Nil  => subs
    case True :: _ => subs
    case (e2_in =:= f2_in) :: pi2_ =>
      val (e2, f2) = (e2_in subst subs._2, f2_in subst subs._2)

      if e2 == f2 then preCheckPureImplication(subs, pi1, pi2_)
      else
        (e2, f2) match
          case (v2@LogicalVar(_), f2) => preCheckPureImplication((subs._1, subs._2 + (v2 -> f2)), pi1, pi2_)
          case (e2, v2@LogicalVar(_)) => preCheckPureImplication((subs._1, subs._2 + (v2 -> e2)), pi1, pi2_)
          case (_, _) =>
            val pi1_ = pi1 subst subs._1
            implyAtom(subs, pi1_, Nil, e2_in =:= f2_in)
            preCheckPureImplication(subs, pi1, pi2_)
    case (ProgramVar(_) =!= _)  :: pi2_ if mode == CalcMissing =>
      preCheckPureImplication(subs, pi1, pi2_)
    case (LogicalVar(_) =!= _) :: pi2_ =>
      preCheckPureImplication(subs, pi1, pi2_)
    case (_ =!= _) :: _ =>
      throw ImplFalse("ineq e2=f2 in rhs with e2 not primed var", (Map.empty, Map.empty), Continue)

def implyAtom(
               subs: SubstP,
               pi1: Pure.L,
               sigma1: Spatial.L,
               a: Pure.S
             )(using mode: Mode, st: ProverState): Unit throws ProverException =
  try _implyAtom(subs, pi1, sigma1, a)
  catch
    case ex: ProverException => mode match
      case CalcMissing => st.modifyPi(a :: _)
      case FailOnMissing => throw ex


def _implyAtom(
                subs: SubstP,
                pi1: Pure.L,
                sigma1: Spatial.L,
                a: Pure.S
              )(using st: ProverState): Unit throws ProverException =
  val a_ = a substS subs._2
  val ProverState(mPi, mSigma) = st
  val pi1_ = (mPi subst subs._2) ::: pi1
  val sigma1_ = (mSigma subst subs._2) ::: sigma1

  if !(pi1_ contains a_) then
    a_ match
      case _ =:= _ => throw FalseAtom("atom in rhs missing in lhs", subs, a, Continue)
      //// TODO impl check_disequal
      case _ =!= _ => throw FalseAtom("atom in rhs missing in lhs", subs, a, Continue)
      case _ => ()



def checkEqual(prop: Prop, e1: Expression, e2: Expression): Boolean =
  val pi = prop.pi
  val n_e1 = prop.expNormalizeProp(e1)
  val n_e2 = prop.expNormalizeProp(e2)
  if n_e1 == n_e2 then true
  else
    pi contains (prop pureNormalizeProp Pure.=:=(n_e1, n_e2))


def checkDisequal(prop: Prop, e1: Expression, e2: Expression): Boolean =
  val pi = prop.pi
  val spatial = prop.sigma
  val n_e1 = prop expNormalizeProp e1
  val n_e2 = prop expNormalizeProp e2
  
  def doesPiImplyDisequal(ne: Expression, ne_ : Expression) =
    pi contains (prop pureNormalizeProp Pure.=!=(ne, ne_))



  def neqSpatialPart =
    @tailrec
    def f(sigmaIrrelevant: Spatial.L, e: Expression, rest: Spatial.L): Option[(Boolean, Spatial.L)] = rest match
      case (hpred@Spatial.PointsTo(base, field, _)) :: tail =>
        field match
          case Some(_) => Some(true, sigmaIrrelevant.reverse ::: tail)
          case None => f(hpred :: sigmaIrrelevant, e, tail)
      case Nil => None

    def fNullCheck(sigmaIrrelevant: Spatial.L, e: Expression, rest: Spatial.L): Option[(Boolean, Spatial.L)] =
      if e != Expression.Const(0) then f(sigmaIrrelevant, e, rest)
      else Some(false, sigmaIrrelevant.reverse ::: rest)

    fNullCheck(List.empty, n_e1, spatial) match
      case Some((e1Allocated, spatialLeftover)) => fNullCheck(List.empty, n_e2, spatialLeftover) match
        case Some((e2Allocated, _)) => e1Allocated || e2Allocated
        case None => false
      case None => false
    
  def neqPurePart = doesPiImplyDisequal(n_e1, n_e2)
    
  neqPurePart || neqSpatialPart


def checkInconsistency(prop: Prop): Boolean = ???



