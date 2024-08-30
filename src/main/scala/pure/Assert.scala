package pure

import cats.Applicative
import cats.syntax.functor.*
import cats.syntax.all.toTraverseOps
import monocle.Traversal
import monocle.function.Plated
import pure.Conversions.given_Conversion_List_ExprList


sealed trait Assert:

  def rename(re: Map[Var, Var]): Assert

  def subst(su: Map[Var, Expr]): Assert

case object Emp extends Assert:
  override def rename(re: Map[Var, Var]) = this

  override def subst(su: Map[Var, Expr]) = this

case class SepAnd(left: Assert, right: Assert) extends Assert:

  override def toString: String = s"$left ✶ $right"

  override def rename(re: Map[Var, Var]) =
    SepAnd(left rename re, right rename re)

  override def subst(su: Map[Var, Expr]) =
    SepAnd(left subst su, right subst su)

object ** :
  def unapply(sep: SepAnd): Option[(Assert, Assert)] = Some(sep.left -> sep.right)

case class SepImp(left: Assert, right: Assert) extends Assert:
  override def toString: String = s"($left --* $right)"

  override def rename(re: Map[Var, Var]) =
    SepImp(left rename re, right rename re)

  override def subst(su: Map[Var, Expr]) =
    SepImp(left subst su, right subst su)

case class CoImp(left: Assert, right: Assert) extends Assert:
  override def toString: String = s"($left ~~> $right)"

  override def rename(re: Map[Var, Var]) = CoImp(left rename re, right rename re)

  override def subst(su: Map[Var, Expr]) = CoImp(left subst su, right subst su)

case class Septract(left: Assert, right: Assert) extends Assert:
  override def toString: String = s"($left ~~@ $right)"

  override def rename(re: Map[Var, Var]) =
    Septract(left rename re, right rename re)

  override def subst(su: Map[Var, Expr]) =
    Septract(left subst su, right subst su)

case class PointsTo(pointer: Expr, field: Option[String] = None, arg: Expr) extends Assert:

  override def toString: String = field match
    case Some(value) => s"$pointer.$value |-> $arg"
    case None => s"$pointer |-> $arg"


  override def rename(re: Map[Var, Var]) =
    PointsTo(pointer rename re, field, arg rename re)

  override def subst(su: Map[Var, Expr]) =
    PointsTo(pointer subst su, field, arg subst su)

case class Struct(pointer: Expr, fields: List[(String, Expr)]) extends Assert:
  override def toString: String =
    val fieldsS = fields.map(_._2).mkString("⟨", ", ", "⟩")

    s"$pointer |-> $fieldsS"

  override def rename(re: Map[Var, Var]): Assert =
    Struct(pointer rename re, fields map ((f, exp) => (f, exp rename re)))

  override def subst(su: Map[Var, Expr]): Assert =
    Struct(pointer subst su, fields map ((f, exp) => (f, exp subst su)))

case class Imp(left: Assert, right: Assert) extends Assert:
  override def toString: String = s"($left ==> $right)"

  override def rename(re: Map[Var, Var]) = Imp(left rename re, right rename re)

  override def subst(su: Map[Var, Expr]) = Imp(left subst su, right subst su)


case class Exists(x: Seq[Var], body: Assert) extends Assert, Expr.BindT[Exists]:

  def this(x: Var, body: Assert) = this (Seq(x), body)

  override def bound: Set[Var] = x.toSet

  override def rename(a: Map[Var, Var], re: Map[Var, Var]) =
    Exists(x map (_ rename a), body rename re)

  override def subst(a: Map[Var, Var], su: Map[Var, Expr]) =
    Exists(x map (_ rename a), body subst su)

  override def toString: String =
    s"∃${x.mkString(", ")} . ($body)"

object Exists:
  def apply(x: Var, body: Assert) = new Exists(Seq(x), body)

case class ForAll(x: Var, body: Assert) extends Assert, Expr.BindT[ForAll]:
  override def bound: Set[Var] = Set(x)

  override def rename(a: Map[Var, Var], re: Map[Var, Var]) =
    ForAll(x rename a, body rename re)

  override def subst(a: Map[Var, Var], su: Map[Var, Expr]) =
    ForAll(x rename a, body subst su)

  override def toString: String = s"∀$x . ($body)"


case class Pure(expr: Expr) extends Assert:

  override def rename(re: Map[Var, Var]): Pure =
    Pure(expr rename re)

  override def subst(su: Map[Var, Expr]): Pure =
    Pure(expr subst su)

  override def toString: String = expr.toString

case class Pred(pred: Name, args: List[Expr]) extends Assert:
  override def toString: String =
    val prettyArgs = args.map(_.toString).mkString("(", ", ", ")")
    s"${pred}$prettyArgs)"

  override def rename(re: Map[Var, Var]) =
    Pred(pred, args rename re)

  override def subst(su: Map[Var, Expr]) =
    Pred(pred, args subst su)

case class And(left: Assert, right: Assert) extends Assert:
  override def toString: String = s"($left ∧ $right)"

  override def rename(re: Map[Var, Var]) =
    SepAnd(left rename re, right rename re)

  override def subst(su: Map[Var, Expr]) =
    SepAnd(left subst su, right subst su)

case class Case(test: Pure, ifTrue: Assert, ifFalse: Assert) extends Assert:
  override def toString: String = s"($test ? $ifTrue : $ifFalse)"

  override def rename(re: Map[Var, Var]) =
    Case(test rename re, ifTrue rename re, ifFalse rename re)

  override def subst(su: Map[Var, Expr]) =
    Case(test subst su, ifTrue subst su, ifFalse subst su)

case class AssertList(asserts: List[Assert]) extends Assert:
  override def toString: String =
    s"(${(asserts map (_.toString)).mkString(",")})"

  override def rename(re: Map[Var, Var]) =
    AssertList(asserts map (_ rename re))

  override def subst(su: Map[Var, Expr]) =
    AssertList(asserts map (_ subst su))


object Assert:
  given assertTraversal: Traversal[Assert, Assert] =
    new Traversal[Assert, Assert]:
      override def modifyA[F[_]](f: Assert => F[Assert])(s: Assert)(using app: Applicative[F]): F[Assert] =
        s match
          case SepAnd(l, r) => app.product(f(l), f(r)).map(SepAnd.apply)
          case SepImp(l, r) => app.product(f(l), f(r)).map(SepImp.apply)
          case CoImp(l, r) => app.product(f(l), f(r)).map(CoImp.apply)
          case Septract(l, r) => app.product(f(l), f(r)).map(Septract.apply)
          case Imp(l, r) => app.product(f(l), f(r)).map(Imp.apply)
          case Exists(x, e) => f(e).map(Exists(x, _))
          case ForAll(x, e) => f(e).map(ForAll(x, _))
          case AssertList(asserts) => asserts.traverse(f).map(AssertList.apply)
          case Case(test, ifTrue, ifFalse) => app.product(f(ifTrue), f(ifFalse)).map((a, b) => Case(test, a, b))
          case _ => app.pure(s)

  given assertPlated: Plated[Assert] = Plated(assertTraversal)