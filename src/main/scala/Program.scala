import Expression.Var

enum Program:
  case Assign(x: Var, expr: Expression)
  case Load(x: Expression, pointer: Expression)
  case Store(pointer: Expression, arg: Expression)
  case Alloc(pointer: Expression)
  case Free(pointer: Expression)
  case Block(programs: List[Program])
  case If(test: Expression, left: Program, right: Program)
  case While(test: Expression, inv: Assert, body: Program)
