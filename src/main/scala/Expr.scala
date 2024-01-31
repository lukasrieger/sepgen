import Expr.Var

enum Expr:
  case Var(name: String, index: Option[Int] = None)



given exprShow: Show[Expr] with
  override def show(value: Expr): String = value match
    case Var(name, index) => s"$name$$$index"

given [Sub <: Expr] (using s: Show[Expr]) : Show[Sub] with
  override def show(value: Sub): String = s.show(value)



