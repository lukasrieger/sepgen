package biabduce

import pure.Name
import util.Alpha


enum Expression extends Expression.Term with Expression.X:
  case ProgramVar(v: Name)
  case LogicalVar(v: Name)
  case AnyTerm(t: Any)

  override def fresh(index: Int): Expression = this match
    case Expression.ProgramVar(v) => Expression.ProgramVar(v.withIndex(index))
    case Expression.LogicalVar(v) => Expression.LogicalVar(v.withIndex(index))
    case Expression.AnyTerm(t) => Expression.AnyTerm(t)

object Expression extends Alpha[Expression, Expression]:
  override type Term = util.alpha.Term[Expression, Expression]
  override type X = util.alpha.X[Expression, Expression]