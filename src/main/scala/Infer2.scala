import pure.Syntax.**
import pure.{Assert, Case, Emp, Eq, Exists, Expr, Name, PointsTo, Pred, Procedure, Program, Pure, SepAnd, Var}

import scala.annotation.tailrec

private type Heap = List[Assert]

type Pre = Assert
type Post = Assert

def infer(proc: Procedure): (Pre, Post) =
  infer(proc.body)

def infer(program: Program): (Pre, Post) =
  infer(List(program))(List.empty)

private def infer(program: Program, heap: Heap): (Pre, Post) =
  infer(List(program))(heap)

private def infer(program: List[Program])(heap: Heap): (Pre, Post) =
  program match
    case Program.Assign(x, expr) :: rest =>
      infer(rest)(heap) map (_ subst Map(x -> expr))
    case Program.Load(x, pointer, field) :: rest =>
      heap load2 (pointer, field) match
        case Some(value) =>
          infer(rest)(heap) map (_ subst Map(x -> value))
        case None =>
          val fresh = x.prime
          val pto = PointsTo(pointer, field, fresh)
          val heap_ = heap :+ pto


          val y = infer(rest)(heap_) map (_ subst Map(x -> fresh))

          y bimap (
            pre = a => Exists(fresh, pto ** a),
            post = Exists(fresh, _)
          )
      
    case Program.Store(pointer, arg, field) :: rest =>
      heap load2 (pointer, field) match
        case Some(value) =>
          val pto = PointsTo(pointer, field, arg)


          val heap_ = heap store2 pto
          println("Store heap Some :")
          println(heap_)
          infer(rest)(heap_)

        case None =>
          val fresh = Var.any
          val pto = PointsTo(pointer, field, arg)
          val heap_ = heap :+ pto

          println("Store heap None :")
          println(heap_)

          infer(rest)(heap_) bimap (
            pre = PointsTo(pointer, field, fresh) ** _,
            post = identity
          )


    case Program.Alloc(pointer) :: rest => ???
    case Program.Free(pointer) :: rest=> ???
    case Program.Block(programs) :: rest =>
      val (_pre, _post) = infer(rest)(heap)
      infer(programs)(heap) bimap (
        pre = _pre ** _,
        post = identity
      )
    case Program.If(test, left, right) :: rest =>
      val (leftPre, leftPost) = infer(left :: rest)(heap)
      val (rightPre, rightPost) = infer(right :: rest)(heap)

      Case(
        test = Pure(test),
        ifTrue = leftPre,
        ifFalse = rightPre
      ) -> Case(
        test = Pure(test),
        ifTrue = leftPost,
        ifFalse = rightPost
      )
    case Program.While(test, inv, body) :: rest => ???
    case Program.Call(name, args, rt) :: rest =>
      val heap_ = heap :+ Pred(Name("post"), args ::: rt)
      infer(rest)(heap_) bimap (
        pre = Pred(Name("pre"), args) ** _,
        post = identity
      )
    case Program.Return(ret) :: rest =>
      val resPred: List[Assert] = ret.zipWithIndex.map((r, i) => Pure(Eq(Var(Name("result").withIndex(i)), r)))
      
      infer(rest)(heap) bimap (
        pre = identity,
        post = (resPred reduce ( _ ** _)) ** _
      )
    case Nil =>
      if heap.isEmpty then Emp -> Emp else Emp -> heap.reduceRight(_ ** _)





extension (prePost : (Pre, Post))
  infix def map(f: Assert => Assert): (Pre, Post) =
    f(prePost._1) -> f(prePost._2)

  infix def bimap(pre: Assert => Assert, post: Assert => Assert): (Pre, Post) =
    pre(prePost._1) -> post(prePost._2)

extension (assertion: Assert)
  def simplify(): Assert =
    val simplified = assertion match
      case SepAnd(Emp, Emp) => Emp
      case SepAnd(left, Emp) => left.simplify()
      case SepAnd(Emp, right) => right.simplify()
      case SepAnd(left, right) => left.simplify() ** right.simplify()
      case Case(test, ifTrue, ifFalse) => Case(test, ifTrue.simplify(), ifFalse.simplify())
      case Exists(x, body) => Exists(x, body.simplify())
      case _ => assertion

    if simplified != assertion then
      simplified.simplify()
    else
      simplified

extension (heap: Heap)
  @tailrec infix def load2(pointer: Expr, field: Option[String]): Option[Expr] =
    heap match
      case PointsTo(`pointer`, `field`, value) :: _ => Some(value)
      case _ :: tail => tail load2(pointer, field)
      case Nil => Option.empty

  @tailrec infix def store2(pto: PointsTo, seen: Heap = List.empty): Heap =
    heap match
      case PointsTo(pto.pointer, pto.field, _) :: rest =>
        seen ::: pto :: rest
      case other :: tail => tail store2(pto, seen :+ other)
      case Nil => heap
