package pure

import pure.Conversions.given
import util.Alpha

import scala.language.implicitConversions

enum Op(val symbol: String):
  override def toString: String = symbol

  case Plus extends Op("+")
  case Minus extends Op("-")
  case Mul extends Op("*")
  case Div extends Op("/")
  case Lt extends Op("<")
  case Gt extends Op(">")


enum Quantifier(val name: String):
  case ForAll extends Quantifier("forall")
  case Exists extends Quantifier("exists")


class ExprList(expressions: List[Expr]) extends Expr.Terms(expressions)


class VarList(vars: List[Var]) extends Expr.Xs(vars):
  def prime: List[Var] = vars map (_.prime)

  def names: List[Name] = vars map (_.name)


sealed trait Expr extends Expr.Term derives CanEqual


object Expr extends Alpha[Expr, Var]:
  override type Term = util.alpha.Term[Expr, Var]
  override type X = util.alpha.X[Expr, Var]


case class BinOp(left: Expr, op: Op, right: Expr) extends Expr:
  override def toString: String = s"($left $op $right)"

  override def free: Set[Var] = left.free ++ right.free

  def rename(re: Map[pure.Var, pure.Var]): pure.Expr =
    BinOp(left rename re, op, right rename re)

  def subst(su: Map[pure.Var, pure.Expr]): pure.Expr =
    BinOp(left subst su, op, right subst su)

case class Not(neg: Expr) extends Expr:
  override def toString: String = s"¬($neg)"

  def free: Set[pure.Var] = neg.free

  def rename(re: Map[pure.Var, pure.Var]): pure.Expr =
    Not(neg rename re)

  def subst(su: Map[pure.Var, pure.Expr]): pure.Expr =
    Not(neg subst su)

case class Eq(left: Expr, right: Expr) extends Expr:
  override def toString: String = s"$left = $right"

  def free: Set[pure.Var] = left.free ++ right.free

  def rename(re: Map[pure.Var, pure.Var]): pure.Expr =
    Eq(left rename re, right rename re)

  def subst(su: Map[pure.Var, pure.Expr]): pure.Expr =
    Eq(left subst su, right subst su)

case class App(fun: Name, args: List[Expr]) extends Expr:
  override def free: Set[Var] = args.free

  override def rename(re: Map[Var, Var]): App =
    App(fun, args rename re)

  override def subst(su: Map[Var, Expr]): App =
    App(fun, args subst su)


case class Var(name: Name) extends Expr with Expr.X:
  override def fresh(index: Int): Var = Var(name.withIndex(index))

  def prime: Var = Var(name.withName(name.name + "'"))

  override def toString: String = name.toString

object Var:
  def any = Var(Name("_"))

case class Lit(any: Any) extends Expr:
  override def free: Set[Var] = Set()

  override def rename(re: Map[Var, Var]): Expr = this

  override def subst(su: Map[Var, Expr]): Expr = this

  override def toString: String = any.toString

case class Bind(
                 quantifier: Quantifier,
                 formals: List[Var],
                 body: Expr,
               ) extends Expr with Expr.BindT[Bind]:
  override def free: Set[Var] = body.free -- formals

  override def rename(a: Map[Var, Var], re: Map[Var, Var]): Bind =
    Bind(quantifier, formals rename a, body rename re)

  override def subst(a: Map[Var, Var], su: Map[Var, Expr]): Bind =
    Bind(quantifier, formals rename a, body subst su)

  def refresh(avoid: Iterable[Var]): Bind =
    val xs = avoid filter bound
    val re = Expr.fresh(xs)
    rename(re)

  override def bound: Set[Var] = Set(formals: _*)
