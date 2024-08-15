package inductive

import cats.{Foldable, Semigroup}
import inductive.Pattern.{Cons, Free, Nil}
import monocle.Monocle.universe
import pure.{And, Assert, AssertList, BinOp, Bind, Case, CoImp, Emp, Eq, Exists, ForAll, Imp, Lit, Name, Not, PointsTo, Pred, Predicate, Pure, SepAnd, SepImp, Septract, Var}
import pure.*

enum Pattern:
  case Nil
  case Cons(head: Name, tail: Name)
  case Null
  case Free(variable: Name)


  def rename(re: Map[Var, Var]) = this match
    case Pattern.Nil =>
      Pattern.Nil
    case Pattern.Cons(head, tail) =>
      Pattern.Cons(
        re.getOrElse(Var(head), Var(head)).name,
        re.getOrElse(Var(tail), Var(tail)).name
      )
    case Pattern.Null =>
      Pattern.Null
    case Pattern.Free(variable) =>
      Pattern.Free(re.getOrElse(Var(variable), Var(variable)).name)

  override def toString: String = this match
    case Pattern.Null => "null"
    case Pattern.Nil => "[]"
    case Pattern.Cons(head, tail) => s"($head :: $tail)"
    case Pattern.Free(variable) => s"$variable"


case class Head(elements: Seq[(Var, Pattern)]):

  def rename(re: Map[Var, Var]): Head =
    Head(
      elements.map((v, pat) => (v rename re) -> (pat rename re))
    )

  override def toString: String = elements.map(_._2).mkString(" ")


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
    val defHead = s"inductive $name/$arity where"
    val constructorsPretty = constructors
      .map( (head, body) => s"  | $head := $body" )
      .mkString("\n")

    defHead + "\n" + constructorsPretty


object InductivePred:

  def mergeHeads(a: Option[Head], b: Option[Head]) = (a, b) match
    case (None, None) => None
    case (Some(aa), None) => Some(aa)
    case (None, Some(bb)) => Some(bb)
    case (Some(aa), Some(bb)) => Some(Head(aa.elements ++ bb.elements))

  def paths(assert: Assert): Seq[(Option[Head], Assert)] = assert match
    case Emp =>
      Seq(None -> Emp)
    case SepAnd(left, right) =>
      for
        (h1, l1) <- paths(left)
        (h2, l2) <- paths(right)
      yield mergeHeads(h1, h2) -> SepAnd(l1, l2)
    case SepImp(left, right) =>
      for
        (h1, l1) <- paths(left)
        (h2, l2) <- paths(right)
      yield mergeHeads(h1, h2) -> SepImp(l1, l2)
    case CoImp(left, right) =>
      for
        (h1, l1) <- paths(left)
        (h2, l2) <- paths(right)
      yield mergeHeads(h1, h2) -> CoImp(l1, l2)
    case Septract(left, right) =>
      for
        (h1, l1) <- paths(left)
        (h2, l2) <- paths(right)
      yield mergeHeads(h1, h2) -> Septract(l1, l2)
    case PointsTo(pointer, field, arg) =>
      Seq(None -> PointsTo(pointer, field, arg))
    case Imp(left, right) =>
      for
        (h1, l1) <- paths(left)
        (h2, l2) <- paths(right)
      yield mergeHeads(h1, h2) -> Imp(l1, l2)
    case Exists(x, body) =>
      for
        (h1, b1) <- paths(body)
      yield h1 -> Exists(x, b1)
    case ForAll(x, body) =>
      for
        (h1, b1) <- paths(body)
      yield h1 -> ForAll(x, b1)
    case Pure(expr) =>
      Seq(None -> Pure(expr))
    case Pred(pred, args) =>
      Seq(None -> Pred(pred, args))
    case And(left, right) =>
      for
        (h1, l1) <- paths(left)
        (h2, l2) <- paths(right)
      yield mergeHeads(h1, h2) -> And(l1, l2)
    case Case(test, ifTrue, ifFalse) =>
      val truePaths = for
        (h1, p1) <- paths(ifTrue)
      yield mergeHeads(toInductiveHead(test), h1) -> p1
      val falsePaths = for
        (h2, p2) <- paths(ifFalse)
      yield mergeHeads(toInductiveHead(Pure(Not(test.expr))), h2) -> p2

      truePaths ++ falsePaths
    case AssertList(_) => ???


  def toPattern(test: Pure): Option[(Var, Pattern)] = test.expr match
    case BinOp(_, _, _) => None
    case Not(neg) =>
      toPattern(Pure(neg)) match
        case Some(pat) => pat match
          case (v, Pattern.Nil) =>
            Some(v -> Pattern.Cons(head_(v).name, tail_(v).name))
          case (v, Pattern.Cons(_, _)) => Some(v -> Nil)
          case (v, Pattern.Null) => Some(v -> Free(v.name))
          case (v, Pattern.Free(_)) => Some(v -> Pattern.Null)
        case None => None

    case Eq(Var(name), Lit("Nil")) => Some(Var(name) -> Nil)
    case Eq(_, _) => None
    case pure.App(_, _) => None
    case Var(_) => None
    case Lit(_) => None
    case Bind(_, _, _) => None

  def toInductiveHead(test: Pure): Option[Head] =
    toPattern(test) match
      case Some(value) => Some(Head(Seq(value)))
      case None => None

  def fromPred(name: Name, predicate: Assert): InductivePred =
    val params = freeVars(predicate)
    val (trans, symbols) = predicate.toAbstractReprNonApp
    val abstracts = symbols.toAbstractParams
    val renameMap = symbols.foldLeft(Map.empty[Var, Var]):
      case (acc, (ptr, _)) =>
        acc ++ Map(Var(name = tailReprOf(ptr)) -> Var(abstracts(ptr)))
            ++ Map(Var(name = headReprOf(ptr)) -> Var(singular(abstracts(ptr))))

    val renamed = trans rename renameMap

    val paths = InductivePred
      .paths(renamed)
      .flatMap((head, assert) => head.map(_ -> assert))
    
    val patternParams = params.map( n => n -> Pattern.Free(n.name))
    val withParams = paths.map((head, assert) => 
        Head(patternParams ++ head.elements) -> assert
      )
    
    InductivePred(
      name = name,
      arity = params.size + paths.head._1.elements.size,
      constructors = withParams
    ) renameHead renameMap

  enum VarKind:
    case Free
    case Existential

  given kindSemigroup: Semigroup[VarKind] = (x: VarKind, y: VarKind) => (x, y) match
    case (VarKind.Free, VarKind.Free) => VarKind.Free
    case (VarKind.Free, VarKind.Existential) => VarKind.Existential
    case (VarKind.Existential, VarKind.Free) => VarKind.Existential
    case (VarKind.Existential, VarKind.Existential) => VarKind.Existential

  def freeVars(assert: Assert): List[Var] =
    Foldable[LazyList].foldMap(universe(assert)):
      case PointsTo(pointer: Var, _, _) => Map(pointer -> VarKind.Free)
      case Exists(x, _) => Map(x -> VarKind.Existential)
      case _ =>  Map.empty
    .filter(_._2 == VarKind.Free).keys.toList
