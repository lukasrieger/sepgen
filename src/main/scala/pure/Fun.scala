package pure

import cats.syntax.all.toTraverseOps
import cats.syntax.functor.*
import cats.{Applicative, Foldable, Monoid, Traverse}
import cats.derived.*
import cats.derived.auto.monoid.given
import cats.derived.DerivedMonoid.Strict.product
import monocle.Monocle.{transform, universe}
import monocle.Traversal
import monocle.function.Plated
import scala.util.chaining.scalaUtilChainingOps


def head(of: Expr): Expr = App(fun = Name("head"), args = List(of))
def tail(of: Expr): Expr = App(fun = Name("tail"), args = List(of))

case class LSRef(nextPtr: Option[Var], valuePtr: Option[Var]) derives Monoid

opaque type LSRefContainer = Map[Var, LSRef]


extension (cont: LSRefContainer)
  private def names = List("xs", "ys", "zs", "qs", "as", "bs")

  def toAbstractParams: Map[Expr, Name] =
    val localIter = names.iterator
    cont.keys.toList.reverse.map(e => e -> Name(localIter.next())).toMap

  def isBoundValueVar(x: Var): Boolean =
    cont.values.exists(ls => ls.valuePtr.contains(x))

  def isBoundNextVar(x: Var): Boolean =
    cont.values.exists(ls => ls.nextPtr.contains(x))

  def reprForNext(x: Var): Var =
    cont.find((_, ls) => ls.nextPtr.contains(x)).get._1

extension (assert: Assert)
  def collectSymbolicReferences: LSRefContainer =
    Foldable[LazyList].foldMap(universe(assert)) {
      case PointsTo(pointer: Var, field, arg: Var) if field.contains("value") =>
        Map(pointer -> LSRef(None, Some(arg)))
      case PointsTo(pointer: Var, field, arg: Var) if field.contains("next") =>
        Map(pointer -> LSRef(Some(arg), None))
      case _ =>
        Map.empty
    }

  private def applyRewrites(cs: Case, rewrites: LSRefContainer) =
    rewrites.foldLeft(cs):
      case (c, (ptr, _)) => c.copy(c.test subst Map(ptr -> reprOf(ptr)))


  def toAbstractRepr: (Assert, LSRefContainer) =
    val symbols = assert.collectSymbolicReferences
    assert.rewriteSymbolicRefs(symbols) -> symbols

  def rewriteSymbolicRefs(rewrites: LSRefContainer): Assert =
    transform[Assert]:
      case Exists(x, body) if rewrites.isBoundValueVar(x) => body
      case Pred(pred, args) =>
        val reprArgs = args.flatMap:
          case arg@Var(_) if rewrites.contains(arg) => Some(reprOf(arg))
          case arg@Var(_) if rewrites.isBoundNextVar(arg) => Some(tail(reprOf(rewrites.reprForNext(arg))))
          case _ => None

        Pred(pred, args ::: reprArgs)

      case cs@Case(_, _, _) => applyRewrites(cs, rewrites) match
        case c@Case(Pure(ReprCheck(abs)), _, _) => c.copy(test = Pure(Eq(abs, Lit("Nil"))))
        case other => other

      case other => rewrites.foldLeft(other):
        case (assert, (ptr, ls)) =>
          ls.valuePtr match
            case Some(ptrValue) => assert.subst(Map(ptrValue -> head(reprOf(ptr))))
            case None => assert
    .apply(assert)


def reprOf(ptr: Expr): Var = Var(name = reprNameOf(ptr))
def reprNameOf(ptr: Expr): Name = Name(name = s"_REPR_$ptr")

private object ReprVar:
  def unapply(v: Var): Option[Var] =
    if (v.name.name.startsWith("_REPR_")) Some(v) else None

private object ReprCheck:
  def unapply(repr: Expr): Option[Var] = repr match
    case Eq(ReprVar(left), Lit(0)) => Some(left)
    case Eq(Lit(0), ReprVar(right)) => Some(right)
    case _ => None
