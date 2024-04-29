package pure

import pure.Syntax.*
import scala.compiletime.constValue

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
  case Return(ret: List[Expr])

package ProgramDsl:

  import scala.Tuple.{Filter, Size}
  import scala.collection.mutable.ListBuffer
  import scala.compiletime.error
  import scala.language.dynamics

  case class PartialWhen(cond: Expr, ifTrue: Program)

  case class StructPointer(variable: Var, field: String)

  case class PartialStore(arg: Expr)

  case class DynamicSymbol(symbol: String) extends Dynamic:
    def selectDynamic(field: String): StructPointer =
      StructPointer(Var(Name(symbol)), field)

  given Conversion[DynamicSymbol, Var] = (s: DynamicSymbol) => Var(Name(s.symbol))
  given Conversion[DynamicSymbol, Expr] = (s: DynamicSymbol) => Var(Name(s.symbol))

  trait ProgramScope[T <: Int](val procedure: ProcSignature):
    val _program: ListBuffer[Program] = ListBuffer()

  trait VarScope extends Dynamic:
    def selectDynamic(name: String): DynamicSymbol = DynamicSymbol(name)

  inline def program[T <: Int](name: String)(args: String*)(init: ProgramScope[T] ?=> Unit): Procedure =
    given s: ProgramScope[T] = new ProgramScope[T](
      ProcSignature(
        Name(name),
        args.map(n => Var(Name(n))).toList,
        constValue[T]
      )
    ) {}

    val _ = init
    Procedure(s.procedure, Program.Block(s._program.toList))

  def $(using s: ProgramScope[_]): VarScope = new VarScope {}

  def assign(x: Var, expr: Expr)(using s: ProgramScope[_]): Unit =
    s._program += Program.Assign(x, expr)

  def load(x: Var, pointer: Expr, field: Option[String] = None)(using s: ProgramScope[_]): Unit =
    s._program += Program.Load(x, pointer, field)

  def load(x: Var, partial: StructPointer)(using s: ProgramScope[_]): Unit =
    s._program += Program.Load(x, partial.variable, Some(partial.field))

  def store(pointer: Expr, arg: Expr, field: Option[String] = None)(using s: ProgramScope[_]): Unit =
    s._program += Program.Store(pointer, arg, field)

  def store(partial: StructPointer, arg: Expr)(using s: ProgramScope[_]): Unit =
    s._program += Program.Store(partial.variable, arg, Some(partial.field))

  def store(arg: Expr): PartialStore =
    PartialStore(arg)

  inline def when[T <: Int](test: Expr)(ifTrue: ProgramScope[T] ?=> Unit)(using s: ProgramScope[T]): PartialWhen =
    given subScope: ProgramScope[T] = new ProgramScope[T](s.procedure) {}

    val _ = ifTrue
    if subScope._program.size == 1 then
      PartialWhen(test, subScope._program.head)
    else
      PartialWhen(test, Program.Block(subScope._program.toList))

  def call(name: Name, args: List[Var], rt: Var)(using s: ProgramScope[_]): Unit =
    s._program += Program.Call(name, args, rt)

  def call(name: String, args: List[Var], rt: Var)(using s: ProgramScope[_]): Unit =
    s._program += Program.Call(Name(name), args, rt)

  def call(name: String)(args: Var*)(rt: Var)(using s: ProgramScope[_]): Unit =
    s._program += Program.Call(Name(name), args.toList, rt)

  def call_rec(args: Var*)(rts: Var*)(using s: ProgramScope[_]): Unit =
    s._program += Program.Call(s.procedure.name, args.toList, rts.head)

  inline def returns(ret: Expr)(using s: ProgramScope[1]) =
    s._program += Program.Return(List(ret))

  type IsExpr[X] <: Boolean = X match
    case Expr => true
    case Int  => true // necessary to give scala a chance to apply the implicit conversion Int => Lit
    case _    => false

  inline def returns[R <: Tuple, T <: Int](ret: R)
                                                  (using s: ProgramScope[T])
                                                  (using Size[R] =:= T)
                                                  (using Size[Filter[R, IsExpr]] =:= T)  =
    val normed2 = ret.productIterator.map {
      case s@(_: Expr) => s
      case s@(_: Int) => Lit(s)
      case _ => throw RuntimeException("Wrong type.")
    }
    s._program += Program.Return(normed2.toList)



  extension (v: Var)
    infix def |->(field: String): StructPointer = StructPointer(v, field)
    infix def <--(pointer: Expr)(using s: ProgramScope[_]): Unit = load(v, pointer, None)
    infix def <--(partial: StructPointer)(using s: ProgramScope[_]): Unit = load(v, partial)

  extension (partial: PartialStore)
    infix def in(pointer: Expr)(using s: ProgramScope[_]) = store(pointer, partial.arg, None)
    infix def in(pointer: StructPointer)(using s: ProgramScope[_]) = store(pointer, partial.arg)

  extension (when: PartialWhen)
    inline infix def otherwise[T <: Int](ifFalse: ProgramScope[T] ?=> Unit)(using s: ProgramScope[T]): Unit =
      given subScope: ProgramScope[T] = new ProgramScope[T](s.procedure) {}

      val _ = ifFalse
      if subScope._program.size == 1 then
        s._program += Program.If(when.cond, when.ifTrue, subScope._program.head)
      else
        s._program += Program.If(when.cond, when.ifTrue, Program.Block(subScope._program.toList))

  extension (e: Expr)
    infix def eq(other: Expr) = Eq(e, other)
    infix def eq(other: Int) = Eq(e, Lit(other))
    infix def =:=(other: Expr): Eq = Eq(e, other)
    infix def =:=(other: Int): Eq = Eq(e, Lit(other))
    infix def +(other: Expr) = BinOp(e, Op.Plus, other)
