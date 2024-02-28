package pure

import cats.Applicative
import monocle.Traversal
import monocle.function.Plated
import cats.syntax.functor._
import pure.Conversions.given_Conversion_List_ExprList


sealed trait Assert:

    def rename(re: Map[Var, Var]): Assert
    def subst(su: Map[Var, Expr]): Assert

case class SepAnd(left: Assert, right: Assert) extends Assert:
    override def toString: String = s"($left ** $right)"

    override def rename(re: Map[Var, Var]) =
        SepAnd(left rename re, right rename re)

    override def subst(su: Map[Var, Expr]) =
        SepAnd(left subst su, right subst su)

case class SepImp(left: Assert, right: Assert) extends Assert:
    override def toString: String = s"($left --* $right)"

    override def rename(re: Map[Var, Var]) =
        SepImp(left rename re, right rename re)

    override def subst(su: Map[Var, Expr]) =
        SepImp(left subst su, right subst su)

case class CoImp(left: Assert, right: Assert) extends Assert:
    override def toString: String = s"($left ~~> $right)"

    override def rename(re: Map[Var, Var]) = ???

    override def subst(su: Map[Var, Expr]) = ???

case class Septract(left: Assert, right: Assert) extends Assert:
    override def toString: String = s"($left ~~@ $right)"

    override def rename(re: Map[Var, Var]) =
        Septract(left rename re, right rename re)

    override def subst(su: Map[Var, Expr]) =
        Septract(left subst su, right subst su)

case class PointsTo(pointer: Expr, arg: Expr) extends Assert:
    override def toString: String = s"($pointer |-> $arg)"

    override def rename(re: Map[Var, Var]) =
        PointsTo(pointer rename re, arg rename re)

    override def subst(su: Map[Var, Expr]) =
        PointsTo(pointer subst su, arg subst su)
case class Imp(left: Assert, right: Assert) extends Assert:
    override def toString: String = s"($left ==> $right)"

    override def rename(re: Map[Var, Var]) = ???

    override def subst(su: Map[Var, Expr]) = ???


case class Exists(x: Var, body: Assert) extends Assert, Expr.BindT[Exists]:
    override def bound: Set[Var] = Set(x)

    override def rename(a: Map[Var, Var], re: Map[Var, Var]) =
        Exists(x rename a, body rename re)

    override def subst(a: Map[Var, Var], su: Map[Var, Expr]) =
        Exists(x rename a, body subst su)

    override def toString: String = s"∃$x . ($body)"

case class ForAll(x: Var, body: Assert) extends Assert, Expr.BindT[ForAll]:
    override def bound: Set[Var] = Set(x)

    override def rename(a: Map[Var, Var], re: Map[Var, Var]) =
        ForAll(x rename a, body rename re)

    override def subst(a: Map[Var, Var], su: Map[Var, Expr]) =
        ForAll(x rename a, body subst su)

    override def toString: String = s"∀$x . ($body)"



case class Pure(expr: Expr) extends Assert:

    override def rename(re: Map[Var, Var]) = ???

    override def subst(su: Map[Var, Expr]) = ???

case class Pred(pred: Name, args: List[Expr])  extends Assert:
    override def toString: String = args.toString()

    override def rename(re: Map[Var, Var]) =
        Pred(pred, args rename re)

    override def subst(su: Map[Var, Expr]) =
        Pred(pred, args subst su)


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
