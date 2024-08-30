package biabduce

import biabduce.Pure.L
import biabduce.Spatial.SepAnd
import cats.Applicative
import cats.syntax.functor.*
import monocle.Traversal
import pure.Name

enum Spatial:
  case True
  case Emp
  case PointsTo(pointer: Expression, field: Option[Name], cell: Expression)
  case SepAnd(left: Spatial, right: Spatial)

  def rename(re: Map[Expression, Expression]): Spatial = this match
    case Spatial.PointsTo(pointer, field, cell) =>
      Spatial.PointsTo(pointer rename re, field, cell rename re)
    case Spatial.SepAnd(left, right) =>
      Spatial.SepAnd(left rename re, right rename re)
    case _ => this

  def subst(su: (Expression, Expression)): Spatial = subst(Map(su))

  def subst(su: Map[Expression, Expression]): Spatial = this match
    case Spatial.PointsTo(pointer, field, cell) =>
      Spatial.PointsTo(pointer subst su, field, cell subst su)
    case Spatial.SepAnd(left, right) =>
      Spatial.SepAnd(left subst su, right subst su)
    case _ => this

object Spatial extends HasListRepr[Spatial]:
  type S = Spatial.Emp.type | Spatial.PointsTo | Spatial.True.type
  type L = List[S]

  extension (spatialL: L)
    def rename(re: Map[Expression, Expression]): L =
      spatialL map (_ renameS re)

    def subst(su: Map[Expression, Expression]): L =
      spatialL map (_ substS su)

  extension (spatialS: S)
    def renameS(re: Map[Expression, Expression]): S =
      spatialS.rename(re).asInstanceOf[S]

    def substS(su: Map[Expression, Expression]): S =
      spatialS.subst(su).asInstanceOf[S]
  
  given Traversal[Spatial, Spatial] with
    override def modifyA[F[_]](f: Spatial => F[Spatial])(s: Spatial)(using app: Applicative[F]): F[Spatial] =
      s match
        case Spatial.SepAnd(left, right) => app.product(f(left), f(right)).map(Spatial.SepAnd.apply)
        case _ => app.pure(s)




