package biabduce

import biabduce.Symbolic.Exists

enum Symbolic extends Expression.BindT[Symbolic]:
  case Exists(vars: List[Expression.LogicalVar], body: QuantFree)

  def bound: Set[Expression] = this match
    case Exists(vars, _) => vars.toSet

  override def rename(a: Map[Expression, Expression], re: Map[Expression, Expression]) = this match
    case Symbolic.Exists(vars, body) => 
      Exists(vars map (_.rename(a).asInstanceOf[Expression.LogicalVar]), body rename re)


  override def subst(a: Map[Expression, Expression], su: Map[Expression, Expression]) = this match
    case Symbolic.Exists(vars, body) => 
      Exists(vars map (_.rename(a).asInstanceOf[Expression.LogicalVar]), body subst su)


object `∃`:
  def unapply(ex: Exists): (List[Expression.LogicalVar], QuantFree) =
    (ex.vars, ex.body)

  def apply(vars: List[Expression.LogicalVar])(body: QuantFree) = Exists(vars, body)


