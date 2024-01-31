enum Assert:
  case SepAnd(left: Assert, right: Assert)
  case SepImp(left: Assert, right: Assert)
  case CoImp(left: Assert, right: Assert)
  case Septract(left: Assert, right: Assert)
  case PointsTo(pointer: Expr, arg: Expr)