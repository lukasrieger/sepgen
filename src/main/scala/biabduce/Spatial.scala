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
  case PointsTo(pointer: Expression, field: Option[String], cell: Expression)
  case SepAnd(left: Spatial, right: Spatial)
  case Pred(name: Name, params: List[Expression])
  
  
  override def toString = this match
    case Spatial.True => "true"
    case Spatial.Emp => "emp"
    case Spatial.PointsTo(pointer, field, cell) =>
      field match
        case Some(value) => s"$pointer.$value |-> $cell"
        case None => s"$pointer |-> $cell"
    case Spatial.SepAnd(left, right) => s"$left ✶ $right"
    case Spatial.Pred(name, params) =>
      val prettyArgs = params.map(_.toString).mkString("(", ", ", ")")
      s"${name}$prettyArgs"

  infix def rename(re: Map[Expression, Expression]): Spatial = this match
    case Spatial.PointsTo(pointer, field, cell) =>
      Spatial.PointsTo(pointer rename re, field, cell rename re)
    case Spatial.SepAnd(left, right) =>
      Spatial.SepAnd(left rename re, right rename re)
    case _ => this

  infix def subst(su: (Expression, Expression)): Spatial = subst(Map(su))

  infix def subst(su: Map[Expression, Expression]): Spatial = this match
    case Spatial.PointsTo(pointer, field, cell) =>
      Spatial.PointsTo(pointer subst su, field, cell subst su)
    case Spatial.SepAnd(left, right) =>
      Spatial.SepAnd(left subst su, right subst su)
    case Spatial.Pred(name, params) => 
      Spatial.Pred(name, params map (_ subst su))
    case _ => this

object Spatial extends HasListRepr[Spatial]:
  type S = Spatial.Emp.type | Spatial.PointsTo | Spatial.True.type | Spatial.Pred
  type L = List[S]

  extension (spatialL: L)
    
    infix def normalize(su: Map[Expression, Expression]): L = spatialL subst su
    
    infix def rename(re: Map[Expression, Expression]): L =
      spatialL map (_ renameS re)

    infix def subst(su: Map[Expression, Expression]): L =
      spatialL map (_ substS su)

  extension (spatialS: S)
    
    infix def renameS(re: Map[Expression, Expression]): S =
      spatialS.rename(re).asInstanceOf[S]

    infix def substS(su: Map[Expression, Expression]): S =
      spatialS.subst(su).asInstanceOf[S]
  
  given Traversal[Spatial, Spatial] with
    override def modifyA[F[_]](f: Spatial => F[Spatial])(s: Spatial)(using app: Applicative[F]): F[Spatial] =
      s match
        case Spatial.SepAnd(left, right) => app.product(f(left), f(right)).map(Spatial.SepAnd.apply)
        case _ => app.pure(s)

  given Conversion[Spatial, Spatial.L] = (spatial: Spatial) => spatialToL(spatial)
  given Conversion[Spatial.L, Spatial] = (spatialL: Spatial.L) => LtoSpatial(spatialL)

private def spatialToL(spatial: Spatial): Spatial.L = spatial match
  case Spatial.SepAnd(left, right) => spatialToL(left) ::: spatialToL(right)
  case other => List(other.asInstanceOf[Spatial.S])

private def LtoSpatial(spatialL: Spatial.L): Spatial = spatialL match
  case head :: Nil => head
  case head :: tail => tail.foldLeft(head)((acc, c) => Spatial.SepAnd(acc, c))
  case Nil => Spatial.Emp
