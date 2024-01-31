import Expr.Var

enum Program:
  case Assign(x: Var, expr: Expr)
  case Load(x: Expr, pointer: Expr)
  case Store(pointer: Expr, arg: Expr)
  case Alloc(pointer: Expr)
  case Free(pointer: Expr)
  case Block(programs: List[Program])
  case If(test: Expr, left: Program, right: Program)
  case While(test: Expr, inv: Assert, body: Program)
