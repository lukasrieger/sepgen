package biabduce

import biabduce.Expression.*
import biabduce.ProverException.{FalseAtom, ImplFalse}
import biabduce.Spatial.PointsTo
import cats.data.{EitherT, State, StateT}
import cats.data.StateT.liftF

import scala.annotation.tailrec


// TODO: IMPL calc_missing (pls without global refs !) via ProverState (see below)

type Subst = Map[Expression, Expression]

enum Mode:
  case CalcMissing
  case FailOnMissing

enum Cont:
  case CONT
  case STOP

enum ProverException:
  case ImplFalse(reason: String, subst: (Subst, Subst), cont: Cont)
  case FalsePointsTo(reason: String, subst: (Subst, Subst), pointsTo: PointsTo, cont: Cont)
  case FalseExprs(reason: String, subst: (Subst, Subst), exp1: Expression, exp2: Expression, cont: Cont)
  case FalseAtom(reason: String, subst: (Subst, Subst), atom: Pure, cont: Cont)
  case FalseSigma(reason: String, subst: (Subst, Subst), sigma: Spatial.S, cont: Cont)

case class Missing(pi: Pure.L, sigma: Spatial.L)

type ExceptionOr[R] = Either[ProverException, R]
type ProverState[A] = StateT[ExceptionOr, Missing, A]

type Frame = Spatial.L

type StateOf[A] = State[Missing, A]
type ProverState2[A] = EitherT[StateOf, ProverException, A]


def checkImplication(prop1: QuantFree, prop2: QuantFree)(using mode: Mode)= ???

def checkFootprint(prop1: QuantFree, prop2: QuantFree)(using mode: Mode): Option[(Subst, Frame, Missing)] = ???

def _checkImplication(prop1: QuantFree, prop2: QuantFree)(using mode: Mode): ProverState[(Subst, Spatial.L)] = ???


def preCheckPureImplication(
                             subs: (Subst, Subst),
                             pi1: Pure.L,
                             pi2: Pure.L
                           )(using mode: Mode): ProverState2[(Subst, Subst)]  =
  pi2 match
    case Nil => EitherT.pure(subs)
    case Pure.Eq(e2_in, f2_in) :: pi2_ =>
      val (e2, f2) = (e2_in subst subs._2, f2_in subst subs._2)

      if e2 == f2 then preCheckPureImplication(subs, pi1, pi2_)
      else
        (e2, f2) match
          case (v2@LogicalVar(_), f2) => preCheckPureImplication((subs._1, subs._2 + (v2 -> f2)), pi1, pi2_)
          case (e2, v2@LogicalVar(_)) => preCheckPureImplication((subs._1, subs._2 + (v2 -> e2)), pi1, pi2_)
          case (e2, v2) =>
            val pi1_ = pi1 subst subs._1
            for
              _ <- implyAtom(subs, pi1_, Nil, Pure.Eq(e2_in, f2_in))
              r <- preCheckPureImplication(subs, pi1, pi2_)
            yield r
    case Pure.InEq(ProgramVar(_), f2) :: pi2_ if mode == Mode.CalcMissing => preCheckPureImplication(subs, pi1, pi2_)
    case Pure.InEq(LogicalVar(_), f2) :: pi2_ => preCheckPureImplication(subs, pi1, pi2_)
    case Pure.InEq(e1, e2) :: pi2_ =>
      EitherT.leftT(ImplFalse("ineq e2=f2 in rhs with e2 not primed var", (Map.empty, Map.empty), Cont.CONT))

def implyAtom(
               subs: (Subst, Subst),
               pi1: Pure.L,
               sigma1: Spatial.L,
               a: Pure.S
             )(using mode: Mode): ProverState2[Unit] =
  _implyAtom(subs, pi1, sigma1, a).recoverWith: ex =>
    mode match
      case Mode.CalcMissing => EitherT.liftF(State modify (m => m.copy(pi = a :: m.pi)))
      case Mode.FailOnMissing => EitherT.leftT(ex)


def _implyAtom(subs: (Subst, Subst), pi1: Pure.L, sigma1: Spatial.L, a: Pure.S)(using mode: Mode): ProverState2[Unit] =
  val a_ = a substS subs._2
  for
    missing <- EitherT.liftF(State.get[Missing])
    pi1_ = (missing.pi subst subs._2) ::: pi1
    sigma1_ = (missing.sigma subst subs._2) ::: sigma1
    _ <- EitherT.cond(
      test = pi1_ contains a_,
      right = (),
      left = a_ match
        case Pure.Eq(_, _) => FalseAtom("atom in rhs missing in lhs", subs, a, Cont.CONT)
        //// TODO impl check_disequal
        case Pure.InEq(_, _) => FalseAtom("atom in rhs missing in lhs", subs, a, Cont.CONT)
        case _ => sys.error("Unreachable (hopefully).")
    )
  yield ()
