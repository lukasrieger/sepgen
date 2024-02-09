package pure

object Conversions:
    given Conversion[List[Expr], ExprList] = (expressions: List[Expr]) => new ExprList(expressions)

    given Conversion[List[Var], VarList] = (vars: List[Var]) => new VarList(vars)
