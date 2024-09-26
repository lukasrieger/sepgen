package biabduce

import biabduce.Symbolic.Exists

enum Symbolic extends Expression.BindT[Symbolic]:
  case Exists(vars: List[Expression.LogicalVar], body: Prop)

  override def toString: String = this match
    case Symbolic.Exists(vars, body) =>
      val piStr = if (body.pi.isEmpty) then List(Pure.True) else body.pi
      val sigmaStr = if (body.sigma.isEmpty) then List(Spatial.Emp) else body.sigma
      val propStr = s"${piStr.mkString(" ∧ ")} ∧ ${sigmaStr.mkString(" * ")}"
      s"∃${vars.mkString(", ")} . ($propStr)"

  def bound: Set[Expression] = this match
    case Exists(vars, _) => vars.toSet

  override infix def rename(a: Map[Expression, Expression], re: Map[Expression, Expression]) = this match
    case Symbolic.Exists(vars, body) =>
      Exists(vars map (_.rename(a).asInstanceOf[Expression.LogicalVar]), body ) // rename re)


  override infix def subst(a: Map[Expression, Expression], su: Map[Expression, Expression]) = this match
    case Symbolic.Exists(vars, body) => 
      Exists(vars map (_.rename(a).asInstanceOf[Expression.LogicalVar]), body subst su)


object `∃`:
  def unapply(ex: Exists): (List[Expression.LogicalVar], Prop) =
    (ex.vars, ex.body)

  def apply(vars: List[Expression.LogicalVar])(body: Prop) = Exists(vars, body)


