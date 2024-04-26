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
  import scala.language.dynamics

  case class PartialWhen(cond: Expr, ifTrue: Program)
  case class StructPointer(variable: Var, field: String)
  case class PartialStore(arg: Expr)

  trait ProgramScope:
    val _program: ListBuffer[Program] = ListBuffer()

  trait VarScope extends Dynamic:
    def selectDynamic(name: String): Var = Var(Name(name))


  def program(init: ProgramScope ?=> Unit): Program =
    given s: ProgramScope = new ProgramScope {}
    val _ = init
    Program.Block(s._program.toList)
  
  def v(using s: ProgramScope): VarScope = new VarScope {}

  def assign(x: Var, expr: Expr)(using s: ProgramScope): Unit =
    s._program += Program.Assign(x, expr)

  def load(x: Var, pointer: Expr, field: Option[String] = None)(using s: ProgramScope): Unit =
    s._program += Program.Load(x, pointer, field)

  def load(x: Var, partial: StructPointer)(using s: ProgramScope): Unit =
    s._program += Program.Load(x, partial.variable, Some(partial.field))

  def store(pointer: Expr, arg: Expr, field: Option[String] = None)(using s: ProgramScope): Unit =
    s._program += Program.Store(pointer, arg, field)

  def store(partial: StructPointer, arg: Expr)(using s: ProgramScope): Unit =
    s._program += Program.Store(partial.variable, arg, Some(partial.field))

  def store(arg: Expr): PartialStore =
    PartialStore(arg)

  def when(test: Expr)(ifTrue: ProgramScope ?=> Unit)(using s: ProgramScope): PartialWhen =
    given subScope: ProgramScope = new ProgramScope {}
    val _ = ifTrue
    if subScope._program.size == 1 then
      PartialWhen(test, subScope._program.head)
    else
      PartialWhen(test, Program.Block(subScope._program.toList))

  def call(name: Name, args: List[Var], rt: Var)(using s: ProgramScope): Unit =
    s._program += Program.Call(name, args, rt)

  def call(name: String, args: List[Var], rt: Var)(using s: ProgramScope): Unit =
    s._program += Program.Call(Name(name), args, rt)

  def call(name: String)(args: Var*)(rt: Var)(using s: ProgramScope): Unit =
    s._program += Program.Call(Name(name), args.toList, rt)

  def returns(ret: Expr)(using s: ProgramScope): Unit =
    s._program += Program.Return(ret)


  extension (v: Var)
    infix def |->(field: String): StructPointer = StructPointer(v, field)
    infix def <-- (pointer: Expr)(using s: ProgramScope): Unit = load(v, pointer, None)
    infix def <-- (partial: StructPointer)(using s: ProgramScope): Unit = load(v, partial)

  extension (partial: PartialStore)
    infix def in (pointer: Expr)(using s: ProgramScope) = store(pointer, partial.arg, None)
    infix def in (pointer: StructPointer)(using s: ProgramScope) = store(pointer, partial.arg)

  extension (when: PartialWhen)
    infix def otherwise(ifFalse: ProgramScope ?=> Unit)(using s: ProgramScope): Unit =
      given subScope: ProgramScope = new ProgramScope {}
      val _ = ifFalse
      if subScope._program.size == 1 then
        s._program += Program.If(when.cond, when.ifTrue, subScope._program.head)
      else
        s._program += Program.If(when.cond, when.ifTrue, Program.Block(subScope._program.toList))

  extension (e: Expr)
    infix def eq(other: Expr) = Eq(e, other)
    infix def eq(other: Int) = Eq(e, Lit(other))
    infix def =:= (other: Expr): Eq = Eq(e, other)
    infix def =:= (other: Int): Eq  = Eq(e, Lit(other))
    infix def + (other: Expr) = BinOp(e, Op.Plus, other)