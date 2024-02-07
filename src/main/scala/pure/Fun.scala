package pure

import Conversions.given


case class Fun(name: Name, params: List[Param], args: List[Type], res: Type):
  def bound: Set[Param] = params.toSet

  def rename(f: Name => Name): Fun = Fun(f(name), params, args, res)

  def apply(args: Expr*): App = App(this, args.toList)

  def generic: Inst = Inst(this, Type.fresh(params))

  def in(expr: Expr): Boolean = expr match
    case _: Lit | _: Var =>
      false
    case App(Inst(fun, _), _) if fun == this =>
      true
    case App(_, args) =>
      args exists in


case class Inst(fun: Fun, ty: Map[Param, Type]):
  def params: List[Type] = fun.params subst ty

  def args: List[Type] = fun.args subst ty

  def res: Type = fun.res subst ty

  def apply(args: Expr*): App = App(this, args.toList)

  def subst(su: Map[Param, Type]): Inst = Inst(fun, ty map { case (p, t) => (p, t subst su) })
  