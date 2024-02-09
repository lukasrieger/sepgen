package pure

import util.{Alpha, Show}
import Conversions.given


enum Quantifier(val name: String):
    case ForAll extends Quantifier("forall")
    case Exists extends Quantifier("exists")


class ExprList(expressions: List[Expr]) extends Expr.Terms(expressions)


class VarList(vars: List[Var]) extends Expr.Xs(vars):
    def prime: List[Var] = vars map (_.prime)

    def names: List[Name] = vars map (_.name)


sealed trait Expr extends Expr.Term


object Expr extends Alpha[Expr, Var]:
    override type Term = util.alpha.Term[Expr, Var]
    override type X = util.alpha.X[Expr, Var]


    given exprShow: Show[Expr] with
        override def show(value: Expr): String = value match
            case Var(name) => s"$name"

    given [Sub <: Expr](using s: Show[Expr]): Show[Sub] with
        override def show(value: Sub): String = s.show(value)


case class App(fun: Name, args: List[Expr]) extends Expr:
    override def free: Set[Var] = args.free

    override def rename(re: Map[Var, Var]): App =
        App(fun, args rename re)

    override def subst(su: Map[Var, Expr]): App =
        App(fun, args subst su)


case class Var(name: Name) extends Expr with Expr.X:
    override def fresh(index: Int): Var = Var(name.withIndex(index))

    def prime: Var = Var(name.withName(name.name + "^"))

case class Lit(any: Any) extends Expr:
    override def free: Set[Var] = Set()

    override def rename(re: Map[Var, Var]): Expr = this

    override def subst(su: Map[Var, Expr]): Expr = this

case class Bind(
                   quantifier: Quantifier,
                   formals: List[Var],
                   body: Expr,
               ) extends Expr with Expr.BindT[Bind]:
    override def free: Set[Var] = body.free -- formals

    override def bound: Set[Var] = Set(formals: _*)

    override def rename(a: Map[Var, Var], re: Map[Var, Var]): Bind =
        Bind(quantifier, formals rename a, body rename re)

    override def subst(a: Map[Var, Var], su: Map[Var, Expr]): Bind =
        Bind(quantifier, formals rename a, body subst su)

    def refresh(avoid: Iterable[Var]): Bind =
        val xs = avoid filter bound
        val re = Expr.fresh(xs)
        rename(re)

