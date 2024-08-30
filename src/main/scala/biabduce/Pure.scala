package biabduce

import biabduce.Symbolic.Exists
import cats.Applicative
import cats.syntax.functor.*
import monocle.Traversal
import monocle.function.Plated

import scala.annotation.targetName

trait HasListRepr[T]:
  type S <: T
  type L = List[S]

enum Pure:
  case Eq(left: Expression, right: Expression)
  case InEq(left: Expression, right: Expression)
  case And(left: Pure, right: Pure)
  @targetName("pureTrue")
  case True


  def rename(re: Map[Expression, Expression]): Pure = this match
    case Pure.Eq(left, right) => Pure.Eq(left rename re, right rename re)
    case Pure.InEq(left, right) => Pure.InEq(left rename re, right rename re)
    case Pure.And(left, right) => Pure.And(left rename re, right rename re)
    case Pure.True => True


  def subst(su: Map[Expression, Expression]): Pure = this match
    case Pure.Eq(left, right) => Pure.Eq(left subst su, right subst su)
    case Pure.InEq(left, right) => Pure.InEq(left subst su, right subst su)
    case Pure.And(left, right) => Pure.And(left subst su, right subst su)
    case Pure.True => True

object Pure extends HasListRepr[Pure]:
  type S = Pure.Eq | Pure.InEq | Pure.True.type
  type L = List[S]

  extension (pureL: L)
    def rename(re: Map[Expression, Expression]): L =
      pureL map (_ renameS re)

    def subst(su: Map[Expression, Expression]): L =
      pureL map (_ substS su)

  extension (pureS: S)
    def renameS(re: Map[Expression, Expression]): S =
      pureS.rename(re).asInstanceOf[S]

    def substS(su: Map[Expression, Expression]): S =
      pureS.subst(su).asInstanceOf[S]

  given Traversal[Pure, Pure] with
    override def modifyA[F[_]](f: Pure => F[Pure])(s: Pure)(using app: Applicative[F]): F[Pure] =
      s match
        case Pure.And(left, right) => app.product(f(left), f(right)).map(Pure.And.apply)
        case _ => app.pure(s)

  given Plated[Pure] = Plated(summon)


extension (pure: Pure)
  infix def and(p: Pure): Pure = Pure.And(pure, p)
  infix def and(p: Spatial): QuantFree = QuantFree.QAnd(pure, p)

  infix def and(quant: QuantFree): QuantFree = (pure, quant) match
    case (q, pi ^ sigma) => (q and pi) and sigma

  infix def and(symbolic: Symbolic): Symbolic = (pure, symbolic) match
    case (p, Exists(vars, body)) => Exists(vars, p and body)


extension (left: Expression)
  infix def `=:=`(right: Expression) = Pure.Eq(left, right)