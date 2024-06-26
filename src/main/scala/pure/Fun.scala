package pure

import cats.syntax.all.toTraverseOps
import cats.syntax.functor.*
import cats.{Applicative, Foldable, Monoid, Traverse}
import monocle.Monocle.{transform, universe}
import monocle.Traversal
import monocle.function.Plated
import pure.Assert.appTraverse


def head(of: Expr): Expr =
  App(
    fun = Name("head"),
    args = List(of)
  )

def tail(of: Expr): Expr =
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
        case Case(test, ifTrue, ifFalse) => appTraverse(ifTrue, ifFalse, (a, b) => Case(test, a, b))
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
                  nextPtr: Option[Var],
                  valuePtr: Option[Var]
                )

given lsRefMonoid: Monoid[LSRef] = Monoid.instance(
  emptyValue = LSRef(None, None),
  cmb = (a, b) => LSRef(
    nextPtr = a.nextPtr.orElse(b.nextPtr),
    valuePtr = a.valuePtr.orElse(b.valuePtr)
  )
)

type LSRefContainer = Map[Var, LSRef]


extension (cont: LSRefContainer)

  private def names = List(
    "xs",
    "ys",
    "zs",
    "qs",
    "as",
    "bs"
  )

  def toAbstractParams: Map[Expr, Name] =
    val localIter = names.iterator
    val x = cont.keys.toList.reverse.map(e => e -> Name(localIter.next()))

    x.toMap


def findSymolicRefs(assert: Assert): LSRefContainer =
  Foldable[LazyList].foldMap(universe(assert)) {
    case PointsTo(pointer: Var, field, arg: Var) if field.contains("value") =>
      Map(pointer -> LSRef(None, Some(arg)))
    case PointsTo(pointer: Var, field, arg: Var) if field.contains("next") =>
      Map(pointer -> LSRef(Some(arg), None))
    case _ =>
      Map.empty
  }


def reprOf(ptr: Expr): Var = Var(name = Name(name = s"_REPR_$ptr"))

extension (cont: LSRefContainer)
  def isBoundValueVar(x: Var): Boolean =
    cont.values.exists(ls => ls.valuePtr.contains(x))

  def isBoundNextVar(x: Var): Boolean =
    cont.values.exists(ls => ls.nextPtr.contains(x))

  def reprForNext(x: Var): Var =
    cont.find((_, ls) => ls.nextPtr.contains(x)).get._1


def rewriteSymbolicRefs(assert: Assert, rewrites: LSRefContainer): Assert =
  transform[Assert] {
    case Exists(x, body) if rewrites.isBoundValueVar(x) => body
    case Pred(pred, args) =>
      val reprArgs = args.flatMap {
        case arg@Var(_) =>
          if (rewrites.contains(arg))
            Some(reprOf(arg))
          else if (rewrites.isBoundNextVar(arg))
            Some(tail(reprOf(rewrites.reprForNext(arg))))
          else
            None
        case _ => None
      }
      Pred(pred, args ::: reprArgs)
    case other => rewrites.foldLeft(other) {
      case (assert, (ptr, ls)) =>

        val valueSubst = ls.valuePtr.fold(
          ifEmpty = assert
        )(
          f = ptrValue => assert.subst(Map(ptrValue -> head(reprOf(ptr))))
        )


        // Unsure if this makes sense since it replaces *ALL* occurrences of the original pointer by
        // its abstract representation. (e.g. list.next -> tail(xs) becomes xs.next -> tail(xs), which makes no sense)
//        val selfSubst = valueSubst subst Map(ptr -> reprOf(ptr))


        // Maybe this is wrong as well? Unsure, should talk about that.
        val selfSubst = valueSubst match
          case c@Case(test, _, _) => c.copy(test subst Map(ptr -> reprOf(ptr)))
          case _ => valueSubst


        selfSubst
    }
  }(assert)

extension (pure: Pure)
  def isNullCheck: Boolean =
    pure.expr match
      case Eq(left: Var, _) if left.name.name.startsWith("_REPR_") => true
      case Eq(_, right: Var) if right.name.name.startsWith("_REPR_") => true
      case _ => false

  def extractRepr(): Var =
    pure.expr match
      case Eq(left: Var, _) if left.name.name.startsWith("_REPR_") => left
      case Eq(_, right: Var) if right.name.name.startsWith("_REPR_") => right
      case _ => throw UnsupportedOperationException("Unreachable.")


def rewriteNullCond(assert: Assert): Assert =
  transform[Assert] {
    case Case(test, ifTrue, ifFalse) if test.isNullCheck =>
      val abs = test.extractRepr()

      Case(
        Pure(Eq(abs, Lit("Nil"))),
        ifTrue,
        ifFalse
      )
    case other => other
  }(assert)