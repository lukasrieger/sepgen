package pure

import cats.{Foldable, Monoid}
import inductive.Head
import monocle.Monocle.{transform, universe}

import scala.collection.immutable


case class LSRef(nextPtr: Option[Var], valuePtr: Option[Var])
//  derives Monoid

given lsRefMonoid: Monoid[LSRef] = Monoid.instance(
  emptyValue = LSRef(None, None),
  cmb = (a, b) => LSRef(b.nextPtr.orElse(a.nextPtr), b.valuePtr.orElse(a.valuePtr))
)

opaque type LSRefContainer = Map[Var, LSRef]

def singular(name: Name) =
  name match
    case Name("xs", _) => Name("x")
    case Name("ys", _) => Name("y")
    case Name("zs", _) => Name("z")
    case Name("qs", _) => Name("q")
    case Name("as", _) => Name("a")
    case Name("bs", _) => Name("b")
    case _ => ???


extension (cont: LSRefContainer)
  private def names = List("xs", "ys", "zs", "qs", "as", "bs")

  def toAbstractParams: Map[Expr, Name] =
    val localIter = names.iterator
    cont.keys.toList.reverse.map(e => e -> Name(localIter.next())).toMap

  def isBoundValueVar(x: Seq[Var]): Boolean =
    cont.values.flatMap(_.valuePtr).exists(x.contains)
  
  def isBoundNextVar(x: Var): Boolean =
    cont.values.exists(ls => ls.nextPtr.contains(x))

  def reprForNext(x: Var): Var =
    cont.find((_, ls) => ls.nextPtr.contains(x)).get._1

extension (assert: Assert)
  def collectSymbolicReferences: LSRefContainer =
    Foldable[LazyList].foldMap(universe(assert)):
      case PointsTo(pointer: Var, field, arg: Var) if field.contains("value") =>
        Map(pointer -> LSRef(None, Some(arg)))
      case PointsTo(pointer: Var, field, arg: Var) if field.contains("next") =>
        Map(pointer -> LSRef(Some(arg), None))
      case _ =>
        Map.empty


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

      case cs@Case(_, _, _) =>
        rewrites.foldLeft(cs):
          case (c, (ptr, _)) => c.copy(c.test subst Map(ptr -> reprOf(ptr)))
        match
          case c@Case(Pure(ReprCheck(abs)), _, _) => c.copy(test = Pure(Eq(abs, Lit.Nil)))
          case other => other

      case other => rewrites.foldLeft(other):
        case (assert, (ptr, ls)) =>
          ls.valuePtr match
            case Some(ptrValue) => assert.subst(Map(ptrValue -> head(reprOf(ptr))))
            case None => assert
    .apply(assert)

  def rewriteSymbolicRefsNonApp(rewrites: LSRefContainer): Assert =
    transform[Assert]:
      case Exists(x, body) if rewrites.isBoundValueVar(x) => body
      case Pred(pred, args) =>
        val reprArgs = args.flatMap:
          case arg@Var(_) if rewrites.contains(arg) => Some(reprOf(arg))
          case arg@Var(_) if rewrites.isBoundNextVar(arg) => Some(tail_(reprOf(rewrites.reprForNext(arg))))
          case _ => None

        Pred(pred, args ::: reprArgs)

      case cs@Case(_, _, _) =>
        rewrites.foldLeft(cs):
          case (c, (ptr, _)) => c.copy(c.test subst Map(ptr -> reprOf(ptr)))
        match
          case c@Case(Pure(ReprCheck(abs)), _, _) => c.copy(test = Pure(Eq(abs, Lit.Nil)))
          case other => other

      case other => rewrites.foldLeft(other):
        case (assert, (ptr, ls)) =>
          ls.valuePtr match
            case Some(ptrValue) => assert.subst(Map(ptrValue -> head_(reprOf(ptr))))
            case None => assert
    .apply(assert)

  def toAbstractReprNonApp: (Assert, LSRefContainer) =
    val symbols = assert.collectSymbolicReferences
    assert.rewriteSymbolicRefsNonApp(symbols) -> symbols



def headReprOf(ptr: Expr): Name = Name(name = s"%HEAD_REPR_$ptr")
def tailReprOf(ptr: Expr): Name = Name(name = s"%TAIL_REPR_$ptr")

def reprOf(ptr: Expr): Var = Var(name = reprNameOf(ptr))
def reprNameOf(ptr: Expr): Name = Name(name = s"_REPR_$ptr")

object ReprVar:
  def unapply(v: Var): Option[Var] =
    if (v.name.name.startsWith("_REPR_")) Some(v) else None

private object ReprCheck:
  def unapply(repr: Expr): Option[Var] = repr match
    case Eq(ReprVar(left), Lit.Null) => Some(left)
    case Eq(Lit.Null, ReprVar(right)) => Some(right)
    case _ => None


