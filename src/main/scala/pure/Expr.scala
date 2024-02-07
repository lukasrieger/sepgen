package pure

import util.{Alpha, Show}
import Conversions.given






sealed trait Expr(val funs: Set[Fun], val typ: Type) extends Expr.Term:
  def inst(su: Map[Param, Type]): Expr

  def inst(ty: Map[Param, Type], su: Map[Var, Expr]): Expr

  def bottomUp(g: Expr => Expr): Expr = map(identity, g)

  def topDown(f: Expr => Expr): Expr = map(f, identity)

  def replace(f: Fun, g: Fun): Expr = bottomUp {
    case App(Inst(`f`, su), args) =>
      App(Inst(g, su), args)
    case e =>
      e
  }

  def subtermOf(that: Expr): Boolean = that match
    case _ if this == that =>
      true
    case _: Var =>
      false
    case _: Lit =>
      false
    case Bind(_, _, body, _) => subtermOf(body)
    case App(inst, args) => args exists subtermOf



  def map(f: Expr => Expr, g: Expr => Expr): Expr = f(this) match
    case lit: Lit =>
      g(lit)
    case id: Var =>
      g(id)
    case App(inst, args) =>
      g(App(inst, args map (_.map(f, g))))
    case Bind(quantifier, formals, body, typ) =>
      g(Bind(quantifier, formals, body.map(f, g), typ))


object Expr extends Alpha[Expr, Var]:
  override type Term = util.alpha.Term[Expr, Var]
  override type X = util.alpha.X[Expr, Var]

  class ExprList(expressions: List[Expr]) extends Expr.Terms(expressions):
    def types: List[Type] = expressions map (_.typ)

    def funs: Set[Fun] = Set(expressions flatMap (_.funs): _*)

    def inst(su: Map[Param, Type]): List[Expr] = expressions map (_ inst su)

    def inst(ty: Map[Param, Type], su: Map[Var, Expr]): List[Expr] = expressions map (_ inst(ty, su))

  given exprShow: Show[Expr] with
    override def show(value: Expr): String = value match
      case Var(name, index) => s"$name$$$index"

  given [Sub <: Expr](using s: Show[Expr]): Show[Sub] with
    override def show(value: Sub): String = s.show(value)


case class App(inst: Inst, args: List[Expr]) extends Expr(funs = args.funs + inst.fun, typ = inst.res):
  def free: Set[Var] = args.free

  def rename(re: Map[Var, Var]): App =
    App(inst, args rename re)

  def subst(su: Map[Var, Expr]): App =
    App(inst, args subst su)

  def inst(su: Map[Param, Type]): App =
    App(inst subst su, args inst su)

  def inst(ty: Map[Param, Type], su: Map[Var, Expr]): App =
    App(inst subst ty, args inst(ty, su))


object App extends ((Inst, List[Expr]) => App):
  def apply(inst: Inst, args: List[Expr]): App =
    new App(inst, args)

  def apply(fun: Fun, args: List[Expr]): App =
    val inst = fun.generic

    val su = Type.binds(inst.args, args.types)
    val expr = new App(inst, args)

    expr inst su

case class Var(name: Name, override val typ: Type) extends Expr(funs = Set(), typ = typ) with Expr.X:
  override def fresh(index: Int): Var = Var(name.withIndex(index), typ)

  override def inst(su: Map[Param, Type]): Var = Var(name, typ subst su)

  override def inst(ty: Map[Param, Type], su: Map[Var, Expr]): Expr = subst(su)

  def prime: Var = Var(name.withName(name.name + "^"), typ)

case class Lit(any: Any, override val typ: Type) extends Expr(funs = Set(), typ = typ):
  override def free: Set[Var] = Set()
  override def rename(re: Map[Var, Var]): Expr = this
  override def subst(su: Map[Var, Expr]): Expr = this
  override def inst(su: Map[Param, Type]): Expr = this
  override def inst(ty: Map[Param, Type], su: Map[Var, Expr]): Expr = this


case class Bind(
                 quantifier: Quantifier,
                 formals: List[Var],
                 body: Expr,
                 override val typ: Type
               ) extends Expr(funs = body.funs, typ = typ) with Expr.BindT[Bind]:
  def free: Set[Var] = body.free -- formals

  def bound: Set[Var] = Set(formals: _*)

  def rename(a: Map[Var, Var], re: Map[Var, Var]): Bind =
    Bind(quantifier, formals rename a, body rename re, typ)

  def subst(a: Map[Var, Var], su: Map[Var, Expr]): Bind =
    Bind(quantifier, formals rename a, body subst su, typ)

  def inst(su: Map[Param, Type]): Bind =
    Bind(quantifier, formals inst su, body inst su, typ subst su)

  def inst(ty: Map[Param, Type], su: Map[Var, Expr]): Expr = ???

  def refresh(avoid: Iterable[Var]): Bind =
    val xs = avoid filter bound
    val re = Expr.fresh(xs)
    rename(re)


class VarList(vars: List[Var]) extends Expr.Xs(vars):
  def inst(su: Map[Param, Type]): List[Var] = vars map (_ inst su)

  def prime: List[Var] = vars map (_.prime)
  def names: List[Name] = vars map { case Var(name, _) => name }
  def types: List[Type] = vars map (_.typ)
  def pairs: List[(Name, Type)] = vars map { case Var(name, typ) => name -> typ }
  def asFormals: List[(Var, Type)] = vars map (x => x -> x.typ)
  def asScope: List[(Name, Var)] = vars map { case x @ Var(name, typ) => name -> x }
