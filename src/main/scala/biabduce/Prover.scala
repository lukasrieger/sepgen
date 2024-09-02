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


def checkFootprint(
                    prop1: QuantFree,
                    prop2: QuantFree
                  ): Option[(SubstP, Frame, ProverState)] =
  try
    val proverState = ProverState(missingPi = List.empty, missingSigma = List.empty)
    val ((sub1, sub2), frame) = _checkImplication(prop1, prop2)(using Mode.CalcMissing, proverState)
    Some((sub1, sub2), frame, proverState)
  catch
    case e: ProverException => 
      println(s"Failed to find footprint due to: $e")
      None

def movePrimedLhsFromFront(subs: SubstP, sigma2: Spatial.L): Spatial.L = ???


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


def filterNeLhs(subst: Subst, e0: Expression, field: Option[Name], sigma: Spatial.S): Option[Unit] = sigma match
  case PointsTo(e, f, _) if (e subst subst) == e0 && f == field => Some(())
  case _ => None

def implySpatial(
                  subs: SubstP,
                  prop1: QuantFree,
                  hpred2: Spatial.S
                )(using mode: Mode, st: ProverState): (SubstP, QuantFree) throws ProverException =
  hpred2 match
    case True => (subs, prop1)
    case Spatial.Emp => (subs, prop1)
    case PointsTo(pointer, field, cell) =>
      val e2 = pointer subst subs._2
      e2 match
        case biabduce.Expression.LogicalVar(v) =>
          throw ProverException.FalseSigma("rhs |-> not implemented", subs, hpred2, Stop)
        case _ => ()

      createPropIter(prop1) match
        case None => throw ImplFalse("lhs is empty", subs, Continue)
        case Some(iter1) => iter1.find(filterNeLhs(subs._1, e2, field, _)) match
          case None => throw FalsePointsTo("lhs does not have |->", subs, hpred2, Continue)
          case Some(iter1_) => (iter1_.pitCurr, iter1_.pitState) match
            case (PointsTo(_, _, se1), _) =>
              try
                val subs_ = structExpImply(subs, se1, cell)
                val prop1_ = iter1_.removeCurrToProp
                (subs_, prop1_)
              catch
                case e: ProverException => mode match
                  case Mode.CalcMissing => throw MissingExc("\"could not match |-> present on both sides\"")
                  case Mode.FailOnMissing => throw e
            case (Emp, _) => throw MissingExc("\"could not match |-> (Emp)\"")
            case (True, _) => throw MissingExc("\"could not match |-> (True)\"")


def sigmaImply(
                subs: SubstP,
                prop1: QuantFree,
                sigma2: Spatial.L
              )(using mode: Mode, st: ProverState): (SubstP, QuantFree) throws ProverException =
  movePrimedLhsFromFront(subs, sigma2) match
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
  val (_, pi1, sigma1, _) = prop1.refine
  val (_, pi2, sigma2, _) = prop2.refine
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
    case Nil | True :: _ => subs
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

