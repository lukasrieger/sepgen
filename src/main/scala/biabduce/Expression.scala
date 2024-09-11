package biabduce

import pure.Name
import util.Alpha

enum Op(val symbol: String):
  override def toString: String = symbol

  case Plus extends Op("+")
  case Minus extends Op("-")
  case Mul extends Op("*")
  case Div extends Op("/")
  case Lt extends Op("<")
  case Gt extends Op(">")
  case Eq extends Op("==")
  case Neq extends Op("!=")
  case Not extends Op("!")

enum Expression extends Expression.Term with Expression.X:
  case ProgramVar(v: Name)
  case LogicalVar(v: Name)
  case AnyTerm(t: Any)
  case Const(const: Int)
  case BinOp(left: Expression, op: Op, right: Expression)
  case UnOp(op: Op, expr: Expression)

  override def fresh(index: Int): Expression = this match
    case Expression.ProgramVar(v) => Expression.ProgramVar(v.withIndex(index))
    case Expression.LogicalVar(v) => Expression.LogicalVar(v.withIndex(index))
    case other => other

object Expression extends Alpha[Expression, Expression]:
  override type Term = util.alpha.Term[Expression, Expression]
  override type X = util.alpha.X[Expression, Expression]
  
object Special:
  def Null = Expression.AnyTerm("null")
  def Nil  = Expression.AnyTerm("nil")