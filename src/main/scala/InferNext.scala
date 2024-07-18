import pure.Program.Block
import pure.ProgramDsl.=:=
import pure.Syntax.**
import pure.*

import scala.annotation.tailrec


extension (assert: Assert)
  def toHeap: Heap = assert match
    case SepAnd(left, right) => left.toHeap ::: right.toHeap
    case AssertList(asserts) => asserts.flatMap(_.toHeap)
    case other => List(other)

extension (heap: Heap)
  def toAssert: Assert = simplify(heap.foldLeft[Assert](Emp)(_ ** _))

def inferNext(
               procedure: Procedure,
               pre: Predicate
             ): Assert =
  inferNext(
    procName = procedure.signature.name,
    hypName = pre.name,
    program = procedure.body,
    pre = pre.body
  )

def inferNext(procName: Name, hypName: Name, program: Program, pre: Assert): Assert =
  unfold(pre) match
    case Unfolded.Cases(cases) =>
      val inferred = cases
        .map: (cond, heap) =>
          println("Working with heap:")
          println(heap)
          cond -> inferNext(
            path = cond.toHeap,
            heap = heap,
            program = program,
            hypName = hypName,
            procName = procName
          )
        .toList

      assert(inferred.size == 2)

      println("After inference::")
      println(inferred)
      println("---")

      val (cond, trueBody, falseBody) =
        (inferred.head._1, inferred.head._2, inferred.tail.head._2)

      Case(
        test = cond,
        ifTrue = trueBody,
        ifFalse = falseBody
      )

    case Unfolded.Just(heap) => inferNext(
      path = Heap.empty,
      heap = heap,
      program = program,
      hypName = hypName,
      procName = procName
    )

def inferNext(
               path: Heap,
               heap: Heap,
               program: Program,
               hypName: Name,
               procName: Name
             ): Assert = program match
  case Block(programs) => inferNext(path, heap, programs, hypName, procName)
  case other => inferNext(path, heap, List(other), hypName, procName)


@tailrec
def inferNext(
               path: Heap,
               heap: Heap,
               program: List[Program],
               hypName: Name,
               procName: Name
             ): Assert =
  program match
    case Nil => heap.toAssert
    case pure.Program.Assign(x, expr) :: rest => inferNext(
      path,
      heap :+ Pure(x =:= expr),
      rest,
      hypName,
      procName
    )
    case p@pure.Program.Load(x, pointer, field) :: rest =>
      heap load2(pointer, field) match
        case Some(value) => inferNext(path, heap :+ Pure(x =:= value), rest, hypName, procName)
        case None =>
          println("Load missing heap :")
          println(p)
          inferNext(path, heap, rest, hypName, procName)

    case pure.Program.Store(pointer, arg, field) :: rest =>
      heap load2(pointer, field) match
        case Some(_) => inferNext(
          path,
          heap store2 PointsTo(pointer, field, arg),
          rest,
          hypName,
          procName
        )
        case None => ???
    case pure.Program.Alloc(pointer) :: rest => ???
    case pure.Program.Free(pointer) :: rest => ???
    case pure.Program.Block(programs) :: rest =>
      programs.foldLeft[Assert](Emp): (acc, pr) =>
        acc ** inferNext(path, heap, pr, hypName, procName)
    case pure.Program.If(test, left, right) :: rest =>
      val caseEval = if eval(
        condition = test,
        under = path ::: heap
      ) then
        inferNext(
          path = path ::: List(Pure(test)),
          heap = heap,
          program = left,
          hypName = hypName,
          procName = procName
        )
      else
        inferNext(
          path = path ::: List(Pure(Not(test))),
          heap = heap,
          program = right,
          hypName = hypName,
          procName = procName
        )

      inferNext(path, List(caseEval), rest, hypName, procName)
    case pure.Program.While(test, inv, body) :: rest => ???
    case pure.Program.Call(name, args, rt) :: rest =>
      assume(rt.size == 1)

      val hypothesis = heap.hypothesis().get
      val q = Pred(Name("Q"), args ::: List(hypothesis) ::: List(rt.head))

      inferNext(path, (heap dropHypothesis hypName) :+ Exists(rt.head, q), rest, hypName, procName)
    case pure.Program.Return(ret) :: rest =>
      inferNext(
        path,
        heap :+ ret.zipWithIndex
          .map[Assert]((r, i) => Pure(Var("result", i) =:= r))
          .reduceRight(_ ** _),
        rest,
        hypName,
        procName
      )