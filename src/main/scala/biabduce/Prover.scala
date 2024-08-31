package biabduce

import biabduce.Cont.Continue
import biabduce.Expression.*
import biabduce.Mode.{CalcMissing, FailOnMissing}
import biabduce.ProverException.{FalseAtom, ImplFalse}
import biabduce.Spatial.PointsTo

import language.experimental.saferExceptions
import scala.annotation.tailrec


type Subst = Map[Expression, Expression]

enum Mode:
  case CalcMissing
  case FailOnMissing

enum Cont:
  case Continue
  case Stop

enum ProverException extends Exception:
  case ImplFalse(reason: String, subst: (Subst, Subst), cont: Cont)
  case FalsePointsTo(reason: String, subst: (Subst, Subst), pointsTo: PointsTo, cont: Cont)
  case FalseExprs(reason: String, subst: (Subst, Subst), exp1: Expression, exp2: Expression, cont: Cont)
  case FalseAtom(reason: String, subst: (Subst, Subst), atom: Pure, cont: Cont)
  case FalseSigma(reason: String, subst: (Subst, Subst), sigma: Spatial.S, cont: Cont)



case class ProverState(var missingPi: Pure.L, var missingSigma: Spatial.L):
  def modifyPi(fn: Pure.L => Pure.L): Unit = missingPi = fn(missingPi)
  def modifySigma(fn: Spatial.L => Spatial.L): Unit = missingSigma = fn(missingSigma)

type Missing = ProverState
type Frame = Spatial

def checkImplication(prop1: QuantFree, prop2: QuantFree)(using mode: Mode)= ???

def checkFootprint(prop1: QuantFree, prop2: QuantFree)(using mode: Mode): Option[(Subst, Frame, ProverState)] = ???

def _checkImplication(
                       prop1: QuantFree,
                       prop2: QuantFree
                     )(using mode: Mode, st: ProverState): (Subst, Spatial.L) throws ProverException = ???


@tailrec
def preCheckPureImplication(
                             subs: (Subst, Subst),
                             pi1: Pure.L,
                             pi2: Pure.L
                           )(using mode: Mode, st: ProverState): (Subst, Subst) throws ProverException  =
  pi2 match
    case Nil => subs
    case Pure.Eq(e2_in, f2_in) :: pi2_ =>
      val (e2, f2) = (e2_in subst subs._2, f2_in subst subs._2)

      if e2 == f2 then preCheckPureImplication(subs, pi1, pi2_)
      else
        (e2, f2) match
          case (v2@LogicalVar(_), f2) => preCheckPureImplication((subs._1, subs._2 + (v2 -> f2)), pi1, pi2_)
          case (e2, v2@LogicalVar(_)) => preCheckPureImplication((subs._1, subs._2 + (v2 -> e2)), pi1, pi2_)
          case (_, _) =>
            val pi1_ = pi1 subst subs._1
            implyAtom(subs, pi1_, Nil, Pure.Eq(e2_in, f2_in))
            preCheckPureImplication(subs, pi1, pi2_)
    case Pure.InEq(ProgramVar(_), _) :: pi2_ if mode == CalcMissing =>
      preCheckPureImplication(subs, pi1, pi2_)
    case Pure.InEq(LogicalVar(_), _) :: pi2_ =>
      preCheckPureImplication(subs, pi1, pi2_)
    case Pure.InEq(_, _) :: _ =>
      throw ImplFalse("ineq e2=f2 in rhs with e2 not primed var", (Map.empty, Map.empty), Continue)

def implyAtom(
               subs: (Subst, Subst),
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
                subs: (Subst, Subst),
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
      case Pure.Eq(_, _) => throw FalseAtom("atom in rhs missing in lhs", subs, a, Continue)
      //// TODO impl check_disequal
      case Pure.InEq(_, _) => throw FalseAtom("atom in rhs missing in lhs", subs, a, Continue)
      case _ => ()

