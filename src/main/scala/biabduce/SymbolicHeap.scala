package biabduce

import biabduce.QuantFree.QAnd
import biabduce.Spatial.SepAnd
import biabduce.Symbolic.Exists
import cats.Applicative
import cats.syntax.functor.*
import monocle.Traversal
import monocle.function.Plated
import pure.Name
import util.Alpha

import scala.annotation.targetName


enum Expression extends Expression.Term with Expression.X:
  case ProgramVar(v: Name)
  case LogicalVar(v: Name)
  case AnyTerm(t: Any)

  override def fresh(index: Int): Expression = this match
    case Expression.ProgramVar(v) => Expression.ProgramVar(v.withIndex(index))
    case Expression.LogicalVar(v) => Expression.LogicalVar(v.withIndex(index))
    case Expression.AnyTerm(t) => Expression.AnyTerm(t)

object Expression extends Alpha[Expression, Expression]:
  override type Term = util.alpha.Term[Expression, Expression]
  override type X = util.alpha.X[Expression, Expression]
  


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

object Pure:
  given Traversal[Pure, Pure] with
    override def modifyA[F[_]](f: Pure => F[Pure])(s: Pure)(using app: Applicative[F]): F[Pure] =
      s match
        case Pure.And(left, right) => app.product(f(left), f(right)).map(Pure.And.apply)
        case _ => app.pure(s)

  given Plated[Pure] = Plated(summon)


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

object Spatial:
  given Traversal[Spatial, Spatial] with
    override def modifyA[F[_]](f: Spatial => F[Spatial])(s: Spatial)(using app: Applicative[F]): F[Spatial] =
      s match
        case Spatial.SepAnd(left, right) => app.product(f(left), f(right)).map(Spatial.SepAnd.apply)
        case _ => app.pure(s)

object `**`:
  def unapply(sAnd: SepAnd): (Spatial, Spatial) =
    (sAnd.left, sAnd.right)


enum QuantFree:
  case QAnd(left: Pure, right: Spatial)

  
object `^`:
  def unapply(qAnd: QAnd): (Pure, Spatial) =
    (qAnd.left, qAnd.right)


enum Symbolic:
  case Exists(vars: Set[Expression.LogicalVar], body: QuantFree)

object `∃`:
  def unapply(ex: Exists): (Set[Expression.LogicalVar], QuantFree) =
    (ex.vars, ex.body)
  
  def apply(vars: Set[Expression.LogicalVar])(body: QuantFree) = Exists(vars, body)


extension (pure: Pure)
  infix def and(p: Pure): Pure = Pure.And(pure, p)
  infix def and(p: Spatial): QuantFree = QuantFree.QAnd(pure, p)
  
  infix def and(quant: QuantFree): QuantFree = (pure, quant) match
    case (q, pi ^ sigma) => (q and pi) and sigma
  
  infix def and(symbolic: Symbolic): Symbolic = (pure, symbolic) match
    case (p, Exists(vars, body)) => Exists(vars, p and body)


extension (spatial: Spatial)
  infix def *(sp: Spatial): Spatial = SepAnd(spatial, sp)

extension (quantFree: QuantFree)
  infix def *(q: QuantFree): QuantFree = (quantFree, q) match
    case (pi1 ^ sigma1, pi2 ^ sigma2) => (pi1 and pi2) and (sigma1 * sigma2)
    
  infix def *(s: Spatial): QuantFree = (quantFree, s) match
    case (pi1 ^ sigma1, sp) => pi1 and (sp * sigma1)

extension (symbolic: Symbolic)
  infix def *(s: Symbolic): Symbolic = (symbolic, s) match
    case (Exists(vars1, body1), Exists(vars2, body2)) => Exists(vars1 ++ vars2, body1 * body2)
    
  infix def *(spatial: Spatial): Symbolic = (symbolic, spatial) match
    case(Exists(vars, body), sp) => Exists(vars, body * sp)
    
    
object Logic:
  export biabduce.Expression.*
  export biabduce.Pure.*
  export biabduce.Spatial.*
  export biabduce.QuantFree.*
  export biabduce.Symbolic.*