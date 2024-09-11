package biabduce

import biabduce.Symbolic.Exists
import cats.Applicative
import cats.syntax.functor.*
import monocle.Traversal
import monocle.function.Plated
import biabduce.Pure.*

import scala.annotation.targetName

trait HasListRepr[T]:
  type S <: T
  type L = List[S]

extension (left: Expression)
  def =:=(right: Expression): Pure.S = Pure.=:=(left, right)
  def =!=(right: Expression): Pure.S = Pure.=!=(left, right)
  

enum Pure:
  case =:=(left: Expression, right: Expression)
  case =!=(left: Expression, right: Expression)
  case &(left: Pure, right: Pure)
  @targetName("pureTrue")
  case True

  def &(right: Pure): Pure = Pure.&(this, right)
  

  infix def rename(re: Map[Expression, Expression]): Pure = this match
    case left =:= right => (left rename re) =:= (right rename re)
    case left =!= right => (left rename re) =!= (right rename re)
    case left & right => (left rename re) & (right rename re)
    case Pure.True => True


  infix def subst(su: Map[Expression, Expression]): Pure = this match
    case left =:= right => (left subst su) =:= (right subst su)
    case left =!= right => (left subst su) =!= (right subst su)
    case left & right => (left subst su) & (right subst su)
    case Pure.True => True
    
  infix def normalize(su: Map[Expression, Expression]) = this subst su

object Pure extends HasListRepr[Pure]:
  type S = Pure.=:= | Pure.=!= | Pure.True.type
  type L = List[S]

  extension (pureL: L)
    
    infix def normalize(su: Map[Expression, Expression]): L =
      pureL map (_ normalizeS su)
    
    infix def rename(re: Map[Expression, Expression]): L =
      pureL map (_ renameS re)

    infix def subst(su: Map[Expression, Expression]): L =
      pureL map (_ substS su)

  extension (pureS: S)

    infix def normalizeS(su: Map[Expression, Expression]): S = pureS substS su
    
    infix def renameS(re: Map[Expression, Expression]): S =
      pureS.rename(re).asInstanceOf[S]

    infix def substS(su: Map[Expression, Expression]): S =
      pureS.subst(su).asInstanceOf[S]

  given Traversal[Pure, Pure] with
    override def modifyA[F[_]](f: Pure => F[Pure])(s: Pure)(using app: Applicative[F]): F[Pure] =
      s match
        case Pure.&(left, right) => app.product(f(left), f(right)).map(Pure.&.apply)
        case _ => app.pure(s)

  given Plated[Pure] = Plated(summon)


extension (pure: Pure)
  infix def and(p: Pure): Pure = Pure.&(pure, p)
  infix def and(p: Spatial): QuantFree = QuantFree.QAnd(pure, p)

  infix def and(quant: QuantFree): QuantFree = (pure, quant) match
    case (q, pi ^ sigma) => (q and pi) and sigma

  infix def and(symbolic: Symbolic): Symbolic = (pure, symbolic) match
    case (p, Exists(vars, body)) => Exists(vars, p and body)

  
  
given Conversion[Pure, Pure.L] = (pure: Pure) => ???
given Conversion[Pure.L, Pure] = (pureL: Pure.L) => ???