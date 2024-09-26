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


  override infix def rename(re: Map[Expression, Expression]): Expression = this match
    case Expression.ProgramVar(v) => super.rename(re)
    case Expression.LogicalVar(v) => super.rename(re)
    case Expression.AnyTerm(t) => super.rename(re)
    case Expression.Const(const) => Const(const)
    case Expression.BinOp(left, op, right) => BinOp(left rename re, op, right rename re)
    case Expression.UnOp(op, expr) => UnOp(op, expr rename re)

  override infix def subst(re: Map[Expression, Expression]): Expression = this match
    case Expression.ProgramVar(v) => super.subst(re)
    case Expression.LogicalVar(v) => super.subst(re)
    case Expression.AnyTerm(t) => super.subst(re)
    case Expression.Const(const) => Const(const)
    case Expression.BinOp(left, op, right) => BinOp(left subst re, op, right subst re)
    case Expression.UnOp(op, expr) => UnOp(op, expr subst re)


  override def toString: String = this match
    case Expression.ProgramVar(v) => s"$v"
    case Expression.LogicalVar(v) => s"%$v"
    case Expression.AnyTerm(t) => s"$t"
    case Expression.Const(const) => s"$const"
    case Expression.BinOp(left, op, right) => s"($left $op $right)"
    case Expression.UnOp(op, expr) => s"($op$expr)"

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