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
             )(context: Set[Procedure] = Set()): (Assert, ContextAssert) =
  inferNext(
    procName = procedure.signature.name,
    hypName = pre.name,
    program = procedure.body,
    pre = pre.body,
    context
  )

def inferNext(procName: Name, hypName: Name, program: Program, pre: Assert, context: Set[Procedure]): (Assert, ContextAssert) =
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
            procName = procName,
            contextAssert = Map()
          )(using context)
        .toList

      assert(inferred.size == 2)

      println("After inference::")
      println(inferred)
      println("---")

      val (cond, trueBodyT, falseBodyT) =
        (inferred.head._1, inferred.head._2, inferred.tail.head._2)

      val (trueBody, falseBody) = (trueBodyT._1, falseBodyT._1)

      Case(
        test = cond,
        ifTrue = trueBody,
        ifFalse = falseBody
      ) -> (trueBodyT._2 ++ falseBodyT._2)

    case Unfolded.Just(heap) => inferNext(
      path = Heap.empty,
      heap = heap,
      program = program,
      hypName = hypName,
      procName = procName,
      contextAssert = Map()
    )(using context)


type ContextAssert = Map[Name, Assert]

def inferNext(
               path: Heap,
               heap: Heap,
               program: Program,
               hypName: Name,
               procName: Name,
               contextAssert: ContextAssert
             )(using context: Set[Procedure]): (Assert, ContextAssert) = program match
  case Block(programs) => inferNext(path, heap, programs, hypName, procName, contextAssert)
  case other => inferNext(path, heap, List(other), hypName, procName, contextAssert)




@tailrec
def inferNext(
               path: Heap,
               heap: Heap,
               program: List[Program],
               hypName: Name,
               procName: Name,
               contextAssert: ContextAssert
             )(using context: Set[Procedure]): (Assert, ContextAssert) =
  program match
    case Nil => heap.toAssert -> contextAssert
    case pure.Program.Assign(x, expr) :: rest => inferNext(
      path,
      heap :+ Pure(x =:= expr),
      rest,
      hypName,
      procName,
      contextAssert
    )
    case p@pure.Program.Load(x, pointer, field) :: rest =>
      heap load2(pointer, field) match
        case Some(value) =>
          inferNext(path, heap :+ Pure(x =:= value), rest, hypName, procName, contextAssert)
        case None =>
          println("Load missing heap :")
          println(p)
          inferNext(path, heap, rest, hypName, procName, contextAssert)

    case pure.Program.Store(pointer, arg, field) :: rest =>
      heap load2(pointer, field) match
        case Some(_) => inferNext(
          path,
          heap store2 PointsTo(pointer, field, arg),
          rest,
          hypName,
          procName,
          contextAssert
        )
        case None => ???
    case pure.Program.Alloc(pointer) :: rest => ???
    case pure.Program.Free(pointer) :: rest => ???
    case pure.Program.Block(programs) :: rest =>
      programs.foldLeft[(Assert, ContextAssert)](Emp -> contextAssert): (acc, pr) =>
        val (accAssert, contextAcc) = acc
        val (inferAss, inferCtx) = inferNext(path, heap, pr, hypName, procName, contextAcc)
        (accAssert ** inferAss) -> (contextAcc ++ inferCtx)
    case pure.Program.If(test, left, right) :: rest =>
      val (caseEval, contextAssert2: ContextAssert) = if eval(
        condition = test,
        under = path ::: heap
      ) then
        inferNext(
          path = path ::: List(Pure(test)),
          heap = heap,
          program = left,
          hypName = hypName,
          procName = procName,
          contextAssert
        )
      else
        inferNext(
          path = path ::: List(Pure(Not(test))),
          heap = heap,
          program = right,
          hypName = hypName,
          procName = procName,
          contextAssert
        )

      inferNext(path, List(caseEval), rest, hypName, procName, contextAssert ++ contextAssert2)
    case pure.Program.While(test, inv, body) :: rest => ???
    case pure.Program.Call(name, args, rt) :: rest if name == procName =>
      println(name)
      assume(rt.size == 1 || rt.isEmpty)

      val hypothesis = heap.hypothesis().get

      val q = Pred(Name("Q"), args ::: List(hypothesis) ::: rt.headOption.toList)

      inferNext(
        path,
        if (rt.isEmpty) (heap dropHypothesis hypName) :+ q else (heap dropHypothesis hypName) :+ Exists(rt.head, q),
        rest,
        hypName,
        procName,
        contextAssert)
    case pure.Program.Call(name, args, rt) :: rest =>
      assume(rt.size == 1 || rt.isEmpty)

      val name_ = name.withIndex(scala.util.Random.between(1, 20))
      val (pred, newContext) = inferNext(
        procName = name,
        hypName = name_,
        program = context.find(_.signature.name == name).getOrElse(sys.error(s"Missing proc in context: $name")).body,
        pre = (path ::: heap).toAssert,
        context
      )

      val q = Pred(name_, args ::: rt.headOption.toList)

      inferNext(
        path,
        if (rt.isEmpty) heap :+ q else heap :+ Exists(rt.head, q),
        rest,
        hypName,
        procName,
        contextAssert ++ newContext ++ Map(name_ -> pred)
      )


    case pure.Program.Return(ret) :: rest =>
      inferNext(
        path,
        heap :+ ret.zipWithIndex
          .map[Assert]((r, i) => Pure(Var("result", i) =:= r))
          .reduceRight(_ ** _),
        rest,
        hypName,
        procName,
        contextAssert
      )