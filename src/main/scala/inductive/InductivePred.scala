package inductive

import cats.implicits.{catsKernelStdMonoidForOption, catsSyntaxSemigroup}
import cats.{Foldable, Semigroup}
import inductive.Pattern.Free
import monocle.Monocle.{transform, universe}
import pure.*
import pure.renamePred
import util.info
import util.globalLogger

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

case class InductivePred(
                          name: Name,
                          arity: Int,
                          constructors: Seq[(Head, Assert)]
                        ):
//  assert(
//    constructors.forall(_._1.elements.size == arity),
//    s"All predicate constructors must have the same arity ($arity)"
//  )

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

  def fromPred(name: Name, predicate: Predicate): InductivePred =
    fromPred(name, predicate.body.renamePred(predicate.name.name, name.name))

  def fromPred(name: Name, predicate: Assert): InductivePred =
    val params = freeVars(predicate)
    val (trans, symbols) = predicate.toAbstractReprNonApp
    val abstracts = symbols.toAbstractParams
    val renameMap = symbols.foldLeft(Map.empty[Var, Var]):
      case (acc, (ptr, _)) =>
        acc ++ Map(Var(name = tailReprOf(ptr)) -> Var(abstracts(ptr)))
          ++ Map(Var(name = headReprOf(ptr)) -> Var(singular(abstracts(ptr))))


    val renamed = trans rename renameMap
    val paths = renamed.paths(using params ++ renameMap.keys).flatMap((head, assert) => head.map(_ -> assert))

    val patternParams: List[(Var, Pattern.Free)] = params.map(n => n -> Pattern.Free(n))
    val withParams = freeToSucc(paths).map((head, assert) =>
      (head unify patternParams.map(_._2)) -> (packToStructs compose liftExistential) (assert)
    )

    InductivePred(
      name = name,
      arity = params.size + paths.head._1.elements.size,
      constructors = withParams
    ) renameHead renameMap


  private def freeVars(assert: Assert): List[Var] =
    val free: List[Var] = Foldable[LazyList].foldMap(universe(assert)):
//      case Pred(_, args) => args.collect:
//        case v: Var => v -> VarKind.Free
//      .toMap
      case Pure(Eq(Var(Name("result", _)), _)) => Map(Var(Name("result")) -> VarKind.Free)
      case Case(Pure(Eq(v1: Var, v2: Var)), _, _) => Map(v1 -> VarKind.Free, v2 -> VarKind.Free)
      case Case(Pure(Eq(v1: Var, _)), _, _) => Map(v1 -> VarKind.Free)
      case PointsTo(pointer: Var, _, _) => Map(pointer -> VarKind.Free)
      case Exists(x, _) => x.map (_ -> VarKind.Bound).toMap
      case _ => Map.empty
    .filter(_._2 == VarKind.Free).keys.toList

    // TODO: This is super ugly and doesnt work for predicates with more than one result. FIX THIS.
    if free.contains(Var(Name("result", None))) then
      free.filterNot(_.name == Name("result", None)) :+ Var(Name("result", None))
    else
      free

  private def liftExistential(assert: Assert): Assert =
    val ex: ListBuffer[Var] = ListBuffer.empty
    val lifted = transform[Assert]:
      case Exists(x, body) =>
        ex ++= x
        body
      case other => other
    .apply(assert)

    if (ex.nonEmpty) Exists(ex.toSeq, lifted) else assert


  private def packToStructs(assert: Assert): Assert =
    transform[Assert]:
      case PointsTo(p1, Some(fieldA), expA) ** PointsTo(p2, Some(fieldB), expB) if p1 == p2 =>
        Struct(p1, List(fieldA -> expA, fieldB -> expB))
      case a ** PointsTo(p1, Some(fieldA), expA) ** PointsTo(p2, Some(fieldB), expB) if p1 == p2 =>
        SepAnd(Struct(p1, List(fieldA -> expA, fieldB -> expB)), a)
      case PointsTo(p1, Some(fieldA), expA) ** (PointsTo(p2, Some(fieldB), expB) ** a) if p1 == p2 =>
        SepAnd(Struct(p1, List(fieldA -> expA, fieldB -> expB)), a)
      case other => other
    .apply(assert)

  private def freeToSucc(heads: Seq[(Head, Assert)]): Seq[(Head, Assert)] =
    val zeros =
      for
        (head, _) <- heads
        (v, pattern) <- head.elements
        if pattern == Pattern.Zero
      yield v

    heads.map: (head, assert) =>
      val updated = head.elements.map: (v, pattern) =>
        if zeros.contains(v) && pattern.isInstanceOf[Pattern.Free] then
          (v, Pattern.Succ(v))
        else
          (v, pattern)

      Head(updated, head.guard) -> assert



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
  private def paths(using params: List[Var]): Seq[(Option[Head], Assert)] = assert match
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
    case Struct(pointer, fields) =>
      Seq(None -> Struct(pointer, fields))
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
    case Case(test, ifTrue, ifFalse) if test dependsPurelyOn params =>
      (for (h1, p1) <- ifTrue.paths yield (test.toHead |+| h1) -> p1) ++
        (for (h2, p2) <- ifFalse.paths yield (Pure(Not(test.expr)).toHead |+| h2) -> p2)
    case Case(test, ifTrue, ifFalse) =>
      (for (h1, p1) <- ifTrue.paths yield (test.toHead |+| h1 |+| test.toGuard) -> p1) ++
        (for (h2, p2) <- ifFalse.paths yield (Pure(Not(test.expr)).toHead |+| h2 |+| Pure(Not(test.expr)).toGuard) -> p2)

    case AssertList(_) => sys.error("Unsupported.")

extension (test: Pure)

  private def toGuard(using params: List[Var]): Option[Head] =
    if test dependsPurelyOn params then
      None
    else
      Some(Head(Seq.empty, Some(Seq(test.expr))))

  private infix def dependsPurelyOn(params: List[Var]): Boolean = test.expr match
    case Eq(v, _: Lit) if params.contains(v) => true
    case Eq(ReprVar(_), _: Lit) => true
    case Eq(v1, v2) if params.contains(v1) && params.contains(v2) => true
    case Eq(ReprVar(_), ReprVar(_))  => true
    case _ => false

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
          case (v, Pattern.Zero) => Some(v -> Pattern.Free(v))
          case (v, Pattern.Succ(_)) => Some(v -> Pattern.Zero)
        case None => None
    case Eq(Var(name), Lit.Nil) => Some(Var(name) -> Pattern.Nil)
    case Eq(Var(name), Lit.Zero) => Some(Var(name) -> Pattern.Zero)
    case Eq(_, _) => None
    case pure.App(_, _) => None
    case Var(_) => None
    case Lit(_) => None
    case Bind(_, _, _) => None


  private def toHead: Option[Head] =
    test.toPattern match
      case Some(value) => Some(Head(Seq(value), None))
      case None => None

extension (head: Head)

  infix def unify (params: List[Pattern.Free]): Head = params match
    case v :: tail =>
      head.elements.find(_._1 == v.variable) match
        case Some((_, Pattern.Zero)) =>
          val xxx = Head(head.elements.filterNot(_._1 == v.variable).toList, head.guard) unify tail
          Head(Seq(v.variable -> Pattern.Zero) ++ xxx.elements, xxx.guard)
        case Some(_, Pattern.Succ(n)) =>
          val xxx = Head(head.elements.filterNot(_._1 == v.variable).toList, head.guard) unify tail
          Head(Seq(v.variable -> Pattern.Succ(n)) ++ xxx.elements, xxx.guard)
        case Some(_,_) => Head(Seq(v.variable -> v) ++ (head unify tail).elements, head.guard)
        case None => Head(Seq(v.variable -> v) ++ (head unify tail).elements, head.guard)
    case immutable.Nil => head