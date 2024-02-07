package pure

object Conversions:
  given Conversion[List[Expr], Expr.ExprList] = (expressions: List[Expr]) => new Expr.ExprList(expressions)
  given Conversion[List[Type], TypeList] = (types: List[Type]) => new TypeList(types)
  given Conversion[List[Var], VarList] = (vars: List[Var]) => new VarList(vars)
