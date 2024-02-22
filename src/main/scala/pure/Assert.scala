package pure

import cats.Applicative
import monocle.Traversal
import monocle.function.Plated
import cats.syntax.functor._


sealed trait Assert

case class SepAnd(left: Assert, right: Assert) extends Assert:
    override def toString: String = s"($left ** $right)"
case class SepImp(left: Assert, right: Assert) extends Assert:
    override def toString: String = s"($left --* $right)"
case class CoImp(left: Assert, right: Assert) extends Assert:
    override def toString: String = s"($left ~~> $right)"
case class Septract(left: Assert, right: Assert) extends Assert:
    override def toString: String = s"($left ~~@ $right)"
case class PointsTo(pointer: Expr, arg: Expr) extends Assert:
    override def toString: String = s"($pointer |-> $arg)"
case class Imp(left: Assert, right: Assert) extends Assert:
    override def toString: String = s"($left ==> $right)"


case class Exists(x: Var, body: Assert) extends Assert, Expr.BindT[Exists]:
    override def bound: Set[Var] = Set(x)
    override def rename(a: Map[Var, Var], re: Map[Var, Var]): Exists = this
    override def subst(a: Map[Var, Var], su: Map[Var, Expr]): Exists = this

    override def toString: String = s"∃$x . ($body)"

case class ForAll(x: Var, body: Assert) extends Assert, Expr.BindT[ForAll]:
    override def bound: Set[Var] = Set(x)

    override def rename(a: Map[Var, Var], re: Map[Var, Var]): ForAll = this

    override def subst(a: Map[Var, Var], su: Map[Var, Expr]): ForAll = this

    override def toString: String = s"∀$x . ($body)"



case class Pure(expr: Expr) extends Assert
case class Pred(pred: Name, args: List[Expr])  extends Assert:
    override def toString: String = args.toString()


object Assert:
    given assertPlated: Plated[Assert] = Plated(
        new Traversal[Assert, Assert]:
            override def modifyA[F[_] : Applicative](using f: Assert => F[Assert])(s: Assert): F[Assert] = s match
                case SepAnd(l, r) => appTraverse(l, r, SepAnd.apply)
                case SepImp(l, r) => appTraverse(l, r, SepImp.apply)
                case CoImp(l, r) => appTraverse(l, r, CoImp.apply)
                case Septract(l, r) => appTraverse(l, r, Septract.apply)
                case Imp(l, r) => appTraverse(l, r, Imp.apply)
                case Exists(x, e) => f(e).map(Exists(x, _))
                case ForAll(x, e) => f(e).map(ForAll(x, _))
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
