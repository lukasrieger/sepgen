package pure

import pure.Syntax.*

enum Program:
  case Assign(x: Var, expr: Expr)
  case Load(x: Var, pointer: Expr, field: Option[String] = None)
  case Store(pointer: Expr, arg: Expr, field: Option[String] = None)
  case Alloc(pointer: Var)
  case Free(pointer: Expr)
  case Block(programs: List[Program])
  case If(test: Expr, left: Program, right: Program)
  case While(test: Expr, inv: Assert, body: Program)
  case Call(name: Name, args: List[Var], rt: Var)
  case Return(ret: Expr)

package PrgDsl:
  import scala.collection.mutable.ListBuffer

  case class PartialWhen(cond: Expr, ifTrue: Program)
  case class StructPointer(variable: Var, field: String)
  case class PartialStore(arg: Expr)

  trait ProgramScope:
    val program: ListBuffer[Program] = ListBuffer()


  def program(init: ProgramScope ?=> Unit): Program =
    given s: ProgramScope = new ProgramScope {}
    val _ = init
    Program.Block(s.program.toList)

  extension(s: String)
    def v: Var = Var(Name(s))

  extension (v: Var)
    infix def |-> (field: String): StructPointer =
      StructPointer(v, field)

  def assign(x: Var, expr: Expr)(using s: ProgramScope): Unit =
    s.program += Program.Assign(x, expr)

  def load(x: Var, pointer: Expr, field: Option[String] = None)(using s: ProgramScope): Unit =
    s.program += Program.Load(x, pointer, field)

  def load(x: Var, partial: StructPointer)(using s: ProgramScope): Unit =
    s.program += Program.Load(x, partial.variable, Some(partial.field))

  def store(pointer: Expr, arg: Expr, field: Option[String] = None)(using s: ProgramScope): Unit =
    s.program += Program.Store(pointer, arg, field)

  def store(partial: StructPointer, arg: Expr)(using s: ProgramScope): Unit =
    s.program += Program.Store(partial.variable, arg, Some(partial.field))

  def store(arg: Expr): PartialStore =
    PartialStore(arg)

  def when(test: Expr)(ifTrue: ProgramScope ?=> Unit): PartialWhen =
    given subScope: ProgramScope = new ProgramScope {}
    val _ = ifTrue
    if subScope.program.size == 1 then
      PartialWhen(test, subScope.program.head)
    else
      PartialWhen(test, Program.Block(subScope.program.toList))

  def call(name: Name, args: List[Var], rt: Var)(using s: ProgramScope): Unit =
    s.program += Program.Call(name, args, rt)

  def call(name: String, args: List[Var], rt: Var)(using s: ProgramScope): Unit =
    s.program += Program.Call(Name(name), args, rt)

  def call(name: String)(args: Var*)(rt: Var)(using s: ProgramScope): Unit =
    s.program += Program.Call(Name(name), args.toList, rt)

  def returns(ret: Expr)(using s: ProgramScope): Unit =
    s.program += Program.Return(ret)


  extension (v: Var)
    def <-- (pointer: Expr)(using s: ProgramScope): Unit = load(v, pointer, None)
    
    def <-- (partial: StructPointer)(using s: ProgramScope): Unit = load(v, partial)

  extension (partial: PartialStore)
    infix def in (pointer: Expr)(using s: ProgramScope) = store(pointer, partial.arg, None)
    infix def in (pointer: StructPointer)(using s: ProgramScope) = store(pointer, partial.arg)

  extension (when: PartialWhen)
    infix def otherwise(ifFalse: ProgramScope ?=> Unit)(using s: ProgramScope): Unit =
      given subScope: ProgramScope = new ProgramScope {}
      val _ = ifFalse
      if subScope.program.size == 1 then
        s.program += Program.If(when.cond, when.ifTrue, subScope.program.head)
      else
        s.program += Program.If(when.cond, when.ifTrue, Program.Block(subScope.program.toList))

  extension (e: Expr)
    infix def eq(other: Expr) = Eq(e, other)

    infix def eq(other: Int) = Eq(e, Lit(other))

    infix def + (other: Expr) = BinOp(e, Op.Plus, other)