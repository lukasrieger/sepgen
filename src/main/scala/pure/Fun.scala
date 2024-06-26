package pure

import cats.{Applicative, Foldable, Monoid, Traverse}
import monocle.Traversal
import monocle.function.Plated
import cats.syntax.functor.*
import pure.Assert.appTraverse
import cats.syntax.all.toTraverseOps
import monocle.Monocle.{transform, universe}


def head(of: Expr) =
  App(
    fun = Name("head"),
    args = List(of)
  )

def tail(of: Expr) =
  App(
    fun = Name("tail"),
    args = List(of)
  )

given assertTraversal: Traversal[Assert, Assert] =
  new Traversal[Assert, Assert]:
    override def modifyA[F[_] : Applicative](using f: Assert => F[Assert])(s: Assert): F[Assert] =
      s match
      case SepAnd(l, r) => appTraverse(l, r, SepAnd.apply)
      case SepImp(l, r) => appTraverse(l, r, SepImp.apply)
      case CoImp(l, r) => appTraverse(l, r, CoImp.apply)
      case Septract(l, r) => appTraverse(l, r, Septract.apply)
      case Imp(l, r) => appTraverse(l, r, Imp.apply)
      case Exists(x, e) => f(e).map(Exists(x, _))
      case ForAll(x, e) => f(e).map(ForAll(x, _))
      case AssertList(asserts) => asserts.traverse(f).map(AssertList.apply)
      case other =>
        Applicative[F].pure(s)

given assertPlated: Plated[Assert] = Plated(assertTraversal)

private def appTraverse[F[_] : Applicative](
                                             l: Assert,
                                             r: Assert,
                                             ctor: (Assert, Assert) => Assert
                                           )(using f: Assert => F[Assert]): F[Assert] =
  Applicative[F]
    .product(f(l), f(r))
    .map(ctor.tupled)


case class LSRef(
                nextPtr: Option[Expr],
                valuePtr: Option[Expr]
                )

given lsRefMonoid: Monoid[LSRef] = Monoid.instance(
  emptyValue = LSRef(None, None),
  cmb = (a, b) => LSRef(
    nextPtr = a.nextPtr.orElse(b.nextPtr),
    valuePtr = a.valuePtr.orElse(b.valuePtr)
  )
)

def findSymolicRefs(assert: Assert): Map[Expr, LSRef] =
  Foldable[LazyList].foldMap(universe(assert)) {
      case PointsTo(pointer, field, arg) if field.contains("value") =>
        Map(pointer -> LSRef(None, Some(arg)))
      case PointsTo(pointer, field, arg) if field.contains("next") =>
        Map(pointer -> LSRef(Some(arg), None))
      case other =>
        Map.empty
    }


def rewriteSymbolicRefs(assert: Assert, rewrites: Map[Expr, LSRef]): Assert =
  transform[Assert](a =>
    a match
      case Emp => ???
      case SepAnd(left, right) => ???
      case SepImp(left, right) => ???
      case CoImp(left, right) => ???
      case Septract(left, right) => ???
      case PointsTo(pointer, field, arg) => ???
      case Imp(left, right) => ???
      case Exists(x, body) if isLSRef(x, rewrites) => ???
      case ForAll(x, body) => ???
      case Pure(expr) => ???
      case Pred(pred, args) => ???
      case And(left, right) => ???
      case Case(test, ifTrue, ifFalse) => ???
      case AssertList(asserts) => ???
  )(assert)