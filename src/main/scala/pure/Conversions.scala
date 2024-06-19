package pure

object Conversions:
  given Conversion[List[Expr], ExprList] = (expressions: List[Expr]) => new ExprList(expressions)

  given Conversion[List[Var], VarList] = (vars: List[Var]) => new VarList(vars)

  given Conversion[List[Assert], AssertList] = (asserts: List[Assert]) => AssertList(asserts)

  given Conversion[Int, Lit] = (i: Int) => Lit(i)

  given Conversion[Boolean, Lit] = (b: Boolean) => Lit(b)