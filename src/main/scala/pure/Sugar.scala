package pure

private enum Assoc:
    def reduce[A](xs: Iterable[A], f: (A, A) => A): A = this match
        case Left => xs.reduceLeft(f)
        case Right => xs.reduceRight(f)

    def fold[A](xs: Iterable[A], z: A, f: (A, A) => A): A = this match
        case Left => xs.foldLeft(z)(f)
        case Right => xs.foldRight(z)(f)

    case Left
    case Right


package sugar:

    class Binder(val quantifier: Quantifier) extends ((List[Var], Expr) => Expr):
        def unapply(expr: Bind): Option[(List[Var], Expr)] =
            expr match {
                case Bind(quantifier, formals, body) => Some((formals, body))
                case _ => None
            }

        def apply(formals: List[Var], body: Expr): Expr = {
            val formals_ = formals filter body.free
            if (formals_.isEmpty) body
            else Bind(quantifier, formals_, body)
        }
