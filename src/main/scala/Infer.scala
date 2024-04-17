import pure.*
import pure.Program.*
import pure.Syntax.**

import scala.annotation.tailrec


def sumProgram: Program =
  block(
    If(
      test = Eq(Var(Name("p")), Lit(0)),
      left = Return(Lit(0)),
      right = block(
        Load(Var(Name("x")), Var(Name("p")), field = Some("value")),
        Load(Var(Name("n")), Var(Name("p")), field = Some("next")),
        Call(Name("rec"), Var(Name("n")), Var(Name("y"))),
        Return(BinOp(Var(Name("y")), Op.Plus, Var(Name("x"))))
      )
    )
  )


type Heap = List[Assert]

def inferPre(program: Program): Assert =
  inferPre(List(program))(List.empty)

def inferPre(program: Program)(heap: Heap): Assert =
  inferPre(List(program))(heap)

private def inferPre(proc: List[Program])(heap: Heap): Assert =
  proc match
    case Program.Assign(x, expr) :: rest =>
      inferPre(rest)(heap) subst Map(x -> expr)
    case Program.Load(x, pointer, field) :: rest =>
      heap load pointer match
        case Some(value) =>
          inferPre(rest)(heap) subst Map(x -> value)

        case None =>
          val fresh = x.prime
          val pto = PointsTo(pointer, field, fresh)
          val heap_ = pto *** heap

          val y = inferPre(rest)(heap_)
          val _y = y subst Map(x -> fresh)
          val __y = pto ** _y

          Exists(fresh, __y)
    case Program.Store(pointer, arg, field) :: rest =>
      heap load pointer match
        case Some(_) =>
          val pto = PointsTo(pointer, field, arg)
          val heap_ = heap store pto

          pto ** inferPre(rest)(heap_)
        case None =>
          val fresh = Var.any
          val pto = PointsTo(pointer, None, arg)
          val heap_ = pto *** heap

          val y = inferPre(rest)(heap_)

          PointsTo(pointer, None, fresh) ** y

    case Program.Alloc(pointer) :: rest => ???
    case Program.Free(pointer) :: rest => ???
    case Program.Block(programs) :: rest => inferPre(programs)(heap) ** inferPre(rest)(heap)
    case Program.If(test, left, right) :: rest =>
      Case(
        test = Pure(test),
        ifTrue = inferPre(left)(heap),
        ifFalse = inferPre(right)(heap)
      ) ** inferPre(rest)(heap)
    case Program.While(test, inv, body) :: rest => ???
    case Program.Call(name, arg, rt) :: rest =>
      // recursive call
      Pred(Name("pre"), List(arg)) ** inferPre(rest)(heap)
    case Program.Return(_) :: rest => Emp ** inferPre(rest)(heap)
    case Nil => Emp

def inferPost(program: Program): Assert =
  inferPost(List(program))(List.empty)

def inferPost(program: Program)(heap: Heap): Assert =
  inferPost(List(program))(heap)

private def inferPost(proc: List[Program])(heap: Heap): Assert =
  proc match
    case Program.Assign(x, expr) :: rest =>
      inferPost(rest)(heap) subst Map(x -> expr)
    case Program.Load(x, pointer, field) :: rest =>
      heap load pointer match
        case Some(value) =>
          inferPost(rest)(heap) subst Map(x -> value)

        case None =>
          val fresh = x.prime
          val pto = PointsTo(pointer, field, fresh)
          val heap_ = heap :+ pto

          val y = inferPost(rest)(heap_)
          val _y = y subst Map(x -> fresh)
//          val __y = pto ** _y

          Exists(fresh, _y)
    case Program.Store(pointer, arg, field) :: rest =>
      heap load pointer match
        case Some(_) =>
          val pto = PointsTo(pointer, field, arg)
          val heap_ = heap store pto

          pto ** inferPost(rest)(heap_)
        case None =>
          val fresh = Var.any
          val pto = PointsTo(pointer, None, arg)
          val heap_ = heap :+ pto

          val y = inferPost(rest)(heap_)

          PointsTo(pointer, None, fresh) ** y
    case Program.Alloc(pointer) :: rest => ???
    case Program.Free(pointer) :: rest => ???
    case Program.Block(programs) :: rest =>
      inferPost(programs)(heap) ** inferPost(rest)(heap)
    case Program.If(test, left, right) :: rest =>
      Case(
        test = Pure(test),
        ifTrue = inferPost(left)(heap),
        ifFalse = inferPost(right)(heap)
      ) ** inferPost(rest)(heap)
    case Program.While(test, inv, body) :: rest => ???
    case Program.Call(name, arg, rt) :: rest =>
      val heap_ = heap :+ Pred(Name("post"), List(arg, rt))
      inferPost(rest)(heap_)
    case Program.Return(ret) :: rest =>
      PointsTo(Var(Name("result")), None, ret) ** inferPost(rest)(heap)
    case Nil =>
      if heap.isEmpty then
        Emp
      else
        heap.reduce(_ ** _)


def simplify(assertion: Assert): Assert =
  val simplified = assertion match
    case SepAnd(Emp, Emp) => Emp
    case SepAnd(left, Emp) => simplify(left)
    case SepAnd(Emp, right) => simplify(right)
    case SepAnd(left, right) => simplify(left) ** simplify(right)
    case Case(test, ifTrue, ifFalse) => Case(test, simplify(ifTrue), simplify(ifFalse))
    case Exists(x, body) => Exists(x, simplify(body))
    case _ => assertion

  if simplified != assertion then
    simplify(simplified)
  else
    simplified


extension (assertion: Assert)
  infix def ***(heap: Heap): Heap = assertion :: heap


extension (heap: Heap)
  @tailrec infix def load(pointer: Expr, field: Option[String] = None): Option[Expr] =
    heap match
      case PointsTo(`pointer`, `field`, value) :: _ => Some(value)
      case _ :: tail => tail load(pointer, field)
      case Nil => Option.empty

  @tailrec infix def store(pto: PointsTo, seen: Heap = List.empty): Heap =
    heap match
      case PointsTo(pto.pointer, pto.field, _) :: rest =>
        seen ::: pto :: rest
      case other :: tail => tail store(pto, seen :+ other)
      case Nil => heap
