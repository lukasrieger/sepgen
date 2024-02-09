package pure

import cats.Applicative
import monocle.Traversal
import monocle.function.Plated
import cats.syntax.functor._
import util.Alpha


enum Assert:
    case SepAnd(left: Assert, right: Assert)
    case SepImp(left: Assert, right: Assert)
    case CoImp(left: Assert, right: Assert)
    case Septract(left: Assert, right: Assert)
    case PointsTo(pointer: Expr, arg: Expr)
    case Imp(left: Assert, right: Assert)
    case Exists(x: Var, body: Assert) extends Assert, Expr.BindT[Exists]
    case ForAll(x: Var, body: Assert) extends Assert, Expr.BindT[ForAll]
    case Pure(expr: Expr)
    case Pred(pred: Name, args: List[Expr]) // Pred("p", Vars*)


object Assert:
    given assertPlated: Plated[Assert] = Plated(
        new Traversal[Assert, Assert]:
            override def modifyA[F[_] : Applicative](using f: Assert => F[Assert])(s: Assert): F[Assert] = s match
                case Assert.SepAnd(l, r) => appTraverse(l, r, Assert.SepAnd.apply)
                case Assert.SepImp(l, r) => appTraverse(l, r, Assert.SepImp.apply)
                case Assert.CoImp(l, r) => appTraverse(l, r, Assert.CoImp.apply)
                case Assert.Septract(l, r) => appTraverse(l, r, Assert.Septract.apply)
                case Assert.Imp(l, r) => appTraverse(l, r, Assert.Imp.apply)
                case Assert.Exists(x, e) => f(e).map(Assert.Exists(x, _))
                case Assert.ForAll(x, e) => f(e).map(Assert.ForAll(x, _))
                case _ => Applicative[F].pure(s)
    )

    private def appTraverse[F[_] : Applicative](
                                                   l: Assert,
                                                   r: Assert,
                                                   ctor: (Assert, Assert) => Assert
                                               )(using f: Assert => F[Assert]): F[Assert] =
        Applicative[F]
            .product(f(l), f(r))
            .map(ctor.tupled)
