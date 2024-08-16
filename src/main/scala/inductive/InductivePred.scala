package inductive

import cats.implicits.{catsKernelStdMonoidForOption, catsSyntaxSemigroup}
import cats.{Foldable, Semigroup}
import inductive.Pattern.Free
import monocle.Monocle.{transform, universe}
import pure.*

import scala.collection.mutable.ListBuffer

case class InductivePred(
                          name: Name,
                          arity: Int,
                          constructors: Seq[(Head, Assert)]
                        ):
  assert(
    constructors.forall(_._1.elements.size == arity),
    s"All predicate constructors must have the same arity ($arity)"
  )

  def renameHead(re: Map[Var, Var]) =
    InductivePred(
      name,
      arity,
      constructors.map((head, body) => (head rename re) -> (body rename re))
    )

  override def toString: String =
    val constructorsPretty = constructors
      .map((head, body) => s"  | $head := $body")
      .mkString("\n")

    s"inductive $name/$arity where \n$constructorsPretty"

object InductivePred:
  def fromPred(name: Name, predicate: Assert): InductivePred =
    val params = freeVars(predicate)
    val (trans, symbols) = predicate.toAbstractReprNonApp
    val abstracts = symbols.toAbstractParams
    val renameMap = symbols.foldLeft(Map.empty[Var, Var]):
      case (acc, (ptr, _)) =>
        acc ++ Map(Var(name = tailReprOf(ptr)) -> Var(abstracts(ptr)))
          ++ Map(Var(name = headReprOf(ptr)) -> Var(singular(abstracts(ptr))))

    val renamed = trans rename renameMap
    val paths = renamed.paths.flatMap((head, assert) => head.map(_ -> assert))

    val patternParams = params.map(n => n -> Pattern.Free(n))
    val withParams = paths.map((head, assert) =>
      Head(patternParams ++ head.elements) -> liftExistential(assert)
    )

    InductivePred(
      name = name,
      arity = params.size + paths.head._1.elements.size,
      constructors = withParams
    ) renameHead renameMap


  private def freeVars(assert: Assert): List[Var] =
    Foldable[LazyList].foldMap(universe(assert)):
      case PointsTo(pointer: Var, _, _) => Map(pointer -> VarKind.Free)
      case Exists(x, _) => x.map (_ -> VarKind.Bound).toMap
      case _ => Map.empty
    .filter(_._2 == VarKind.Free).keys.toList


  private def liftExistential(assert: Assert): Assert =
    val ex: ListBuffer[Var] = ListBuffer.empty
    val lifted = transform[Assert]:
      case Exists(x, body) =>
        ex ++= x
        body
      case other => other
    .apply(assert)

    if (ex.nonEmpty) Exists(ex.toSeq, lifted) else assert






enum VarKind:
  case Free
  case Bound

object VarKind:
  given kindSemigroup: Semigroup[VarKind] = (x: VarKind, y: VarKind) => (x, y) match
    case (VarKind.Free, VarKind.Free) => VarKind.Free
    case (VarKind.Free, VarKind.Bound) => VarKind.Bound
    case (VarKind.Bound, VarKind.Free) => VarKind.Bound
    case (VarKind.Bound, VarKind.Bound) => VarKind.Bound


extension (assert: Assert)
  private def paths: Seq[(Option[Head], Assert)] = assert match
    case Emp =>
      Seq(None -> Emp)
    case SepAnd(left, right) =>
      for
        (h1, l1) <- left.paths
        (h2, l2) <- right.paths
      yield (h1 |+| h2) -> SepAnd(l1, l2)
    case SepImp(left, right) =>
      for
        (h1, l1) <- left.paths
        (h2, l2) <- right.paths
      yield (h1 |+| h2) -> SepImp(l1, l2)
    case CoImp(left, right) =>
      for
        (h1, l1) <- left.paths
        (h2, l2) <- right.paths
      yield (h1 |+| h2) -> CoImp(l1, l2)
    case Septract(left, right) =>
      for
        (h1, l1) <- left.paths
        (h2, l2) <- right.paths
      yield (h1 |+| h2) -> Septract(l1, l2)
    case PointsTo(pointer, field, arg) =>
      Seq(None -> PointsTo(pointer, field, arg))
    case Imp(left, right) =>
      for
        (h1, l1) <- left.paths
        (h2, l2) <- right.paths
      yield (h1 |+| h2) -> Imp(l1, l2)
    case Exists(x, body) =>
      for
        (h1, b1) <- body.paths
      yield h1 -> Exists(x, b1)
    case ForAll(x, body) =>
      for
        (h1, b1) <- body.paths
      yield h1 -> ForAll(x, b1)
    case Pure(expr) =>
      Seq(None -> Pure(expr))
    case Pred(pred, args) =>
      Seq(None -> Pred(pred, args))
    case And(left, right) =>
      for
        (h1, l1) <- left.paths
        (h2, l2) <- right.paths
      yield (h1 |+| h2) -> And(l1, l2)
    case Case(test, ifTrue, ifFalse) =>
      (for (h1, p1) <- ifTrue.paths yield (test.toHead |+| h1) -> p1) ++
        (for (h2, p2) <- ifFalse.paths yield (Pure(Not(test.expr)).toHead |+| h2) -> p2)
    case AssertList(_) => sys.error("Unsupported.")

extension (test: Pure)
  private def toPattern: Option[(Var, Pattern)] = test.expr match
    case BinOp(_, _, _) => None
    case Not(neg) =>
      Pure(neg).toPattern match
        case Some(pat) => pat match
          case (v, Pattern.Nil) =>
            Some(v -> Pattern.Cons(head_(v), tail_(v)))
          case (v, Pattern.Cons(_, _)) => Some(v -> Pattern.Nil)
          case (v, Pattern.Null) => Some(v -> Free(v))
          case (v, Pattern.Free(_)) => Some(v -> Pattern.Null)
        case None => None

    case Eq(Var(name), Lit.Nil) => Some(Var(name) -> Pattern.Nil)
    case Eq(_, _) => None
    case pure.App(_, _) => None
    case Var(_) => None
    case Lit(_) => None
    case Bind(_, _, _) => None


  private def toHead: Option[Head] =
    test.toPattern match
      case Some(value) => Some(Head(Seq(value)))
      case None => None