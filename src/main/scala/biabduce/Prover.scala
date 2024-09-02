package biabduce

import biabduce.Cont.{Continue, Stop}
import biabduce.Expression.*
import biabduce.Mode.{CalcMissing, FailOnMissing}
import biabduce.ProverException.{FalseAtom, ImplFalse}
import biabduce.Spatial.{PointsTo, True}
import biabduce.Spatial.given_Conversion_Spatial_L
import biabduce.Pure.{=:=, *}

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
  case FalsePointsTo(reason: String, subst: SubstP, pointsTo: PointsTo, cont: Cont)
  case FalseExprs(reason: String, subst: SubstP, exp1: Expression, exp2: Expression, cont: Cont)
  case FalseAtom(reason: String, subst: SubstP, atom: Pure, cont: Cont)
  case FalseSigma(reason: String, subst: SubstP, sigma: Spatial.S, cont: Cont)


case class ProverState(var missingPi: Pure.L, var missingSigma: Spatial.L):
  def modifyPi(fn: Pure.L => Pure.L): Unit = missingPi = fn(missingPi)
  def modifySigma(fn: Spatial.L => Spatial.L): Unit = missingSigma = fn(missingSigma)

type Missing = ProverState
type Frame = Spatial

def checkImplication(
                      prop1: QuantFree,
                      prop2: QuantFree
                    )(using mode: Mode)= ???

def checkFootprint(
                    prop1: QuantFree,
                    prop2: QuantFree
                  )(using mode: Mode): Option[(Subst, Frame, ProverState)] = ???

def movePrimedLhsFromFront(subs: SubstP, sigma2: Spatial.L): Spatial.L = ???


case class PropIter[A](
                   pitSub: Subst,
                   pitPi: Pure.L,
                   pitOld: Spatial.L,
                   pitCurr: Spatial.S,
                   pitState: Option[A],
                   pitNew: Spatial.L
                   )

def implySpatial(
                  subs: SubstP, 
                  prop1: QuantFree,
                  hpred2: Spatial.S
                )(using mode: Mode): (SubstP, QuantFree) throws ProverException =
  hpred2 match
    case True => ???
    case Emp => ???
    case PointsTo(pointer, field, cell) => 
      val e2 = pointer subst subs._2
      e2 match
        case biabduce.Expression.LogicalVar(v) => 
          throw ProverException.FalseSigma("rhs |-> nopt implemented", subs, hpred2, Stop)
      
      


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
  val (pi1, sigma1) = prop1.refine
  val (pi2, sigma2) = prop2.refine
  val substitutions = preCheckPureImplication(SubstP.empty, pi1, pi2)

  val ((sub1, sub2), frame) = sigmaImply(substitutions, prop1, sigma2)
  val (pi1_, sigma1_) = (pi1 subst sub1, sigma1 subst sub1)

  pi2 foreach (implyAtom((sub1, sub2), pi1_, sigma1_, _))

  (sub1, sub2) -> frame.right


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

