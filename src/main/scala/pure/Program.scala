package pure

import cats.Applicative
import monocle.Traversal
import cats.syntax.functor.*
import cats.syntax.all.toTraverseOps
import monocle.function.Plated

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
  case Call(name: Name, args: List[Var], rt: List[Var])
  case Return(ret: List[Expr])


  
object Program:
  given programTraversal: Traversal[Program, Program] =
    new Traversal[Program, Program]:
      override def modifyA[F[_]](f: Program => F[Program])(s: Program)(using app: Applicative[F]): F[Program] =
        s match
          case Program.Block(programs) => programs.traverse(f).map(Program.Block.apply)
          case Program.If(test, left, right) => app.product(f(left), f(right)).map((a, b) => Program.If(test, a, b))
          case Program.While(test, inv, body) => f(body).map(b => Program.While(test, inv, b))
          case _ => app.pure(s)

  given programPlated: Plated[Program] = Plated(programTraversal)

package ProgramDsl:

  import scala.Tuple.{Filter, Size}
  import scala.collection.mutable.ListBuffer
  import scala.language.dynamics

  case class PartialWhen(cond: Expr, ifTrue: Program)

  case class StructPointer(variable: Var, field: String)

  case class PartialStore(arg: Expr)

  case class PartialCall(rts: List[Var])

  case class DynamicSymbol(symbol: String) extends Dynamic:
    def selectDynamic(field: String): StructPointer =
      StructPointer(Var(Name(symbol)), field)

  given symToVar: Conversion[DynamicSymbol, Var] = (s: DynamicSymbol) => Var(Name(s.symbol))
  given symToExp: Conversion[DynamicSymbol, Expr] = (s: DynamicSymbol) => Var(Name(s.symbol))

  trait ProgramScope(val procedure: ProcSignature):
    val _program: ListBuffer[Program] = ListBuffer()

  trait VarScope extends Dynamic:
    def selectDynamic(name: String): DynamicSymbol = DynamicSymbol(name)

  inline def program(name: String)(args: String*)(init: ProgramScope ?=> Unit): Procedure =
    given s: ProgramScope = new ProgramScope(
      ProcSignature(
        Name(name),
        args.map(n => Var(Name(n))).toList,
        0
      )
    ) {}

    val _ = init
    val program = s._program.result()
    val retCount = accumulateReturns(program)
    Procedure(s.procedure.copy(returnCount = retCount), Program.Block(program))

  def $(using s: ProgramScope): VarScope = new VarScope {}

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

  inline def when(test: Expr)(ifTrue: ProgramScope ?=> Unit)(using s: ProgramScope): PartialWhen =
    given subScope: ProgramScope = new ProgramScope(s.procedure) {}

    val _ = ifTrue
    if subScope._program.size == 1 then
      PartialWhen(test, subScope._program.head)
    else
      PartialWhen(test, Program.Block(subScope._program.toList))

  def call(name: Name, args: List[Var])(rt: Var*)(using s: ProgramScope): Unit =
    s._program += Program.Call(name, args, rt.toList)

  def call(name: String, args: List[Var], rt: Var*)(using s: ProgramScope): Unit =
    s._program += Program.Call(Name(name), args, rt.toList)

  def call(name: String)(args: Var*)(rt: Var*)(using s: ProgramScope): Unit =
    s._program += Program.Call(Name(name), args.toList, rt.toList)

  def call(name: String)(args: Var*)(using s: ProgramScope): Unit =
    s._program += Program.Call(Name(name), args.toList, List.empty)

  def call_rec(args: Var*)(rts: Var*)(using s: ProgramScope): Unit =
    s._program += Program.Call(s.procedure.name, args.toList, rts.toList)

  def call_rec(args: Var*)(using s: ProgramScope): Unit =
    s._program += Program.Call(s.procedure.name, args.toList, List.empty)

  inline def returns(ret: Expr)(using s: ProgramScope) =
    s._program += Program.Return(List(ret))

  type IsExpr[X] <: Boolean = X match
    case Expr => true
    case Int => true // necessary to give scala a chance to apply the implicit conversion Int => Lit
    case DynamicSymbol => true
    case _ => false

  inline def returns(ret: Tuple)(using s: ProgramScope) =
    s._program += Program.Return(
      ret.productIterator.map:
        case s@(_: Expr) => s
        case s@(_: Int) => Lit(s)
        case s@(_: DynamicSymbol) => Var(Name(s.symbol))
        case _ => throw RuntimeException("Wrong type.")
      .toList
    )

  extension (v: Var)
    infix def |->(field: String): StructPointer = StructPointer(v, field)
    infix def <--(pointer: Expr)(using s: ProgramScope): Unit = load(v, pointer, None)
    infix def <--(partial: StructPointer)(using s: ProgramScope): Unit = load(v, partial)
    infix def :=(expr: Expr)(using s: ProgramScope): Unit = assign(v, expr)

  extension (partial: PartialStore)
    infix def in(pointer: Expr)(using s: ProgramScope) = store(pointer, partial.arg, None)
    infix def in(pointer: StructPointer)(using s: ProgramScope) = store(pointer, partial.arg)

  extension (when: PartialWhen)
    inline infix def otherwise(ifFalse: ProgramScope ?=> Unit)(using s: ProgramScope): Unit =
      given subScope: ProgramScope = new ProgramScope(s.procedure) {}

      val _ = ifFalse
      if subScope._program.size == 1 then
        s._program += Program.If(when.cond, when.ifTrue, subScope._program.head)
      else
        s._program += Program.If(when.cond, when.ifTrue, Program.Block(subScope._program.toList))

  extension (e: Expr)
    infix def eq(other: Expr) = Eq(e, other)
    infix def eq(other: Int) = Eq(e, Lit(other))
    infix def =:=(other: Expr) = Eq(e, other)
    infix def =:=(other: Int) = Eq(e, Lit(other))
    infix def =/=(other: Expr) = Not(Eq(e, other))
    infix def +(other: Expr) = BinOp(e, Op.Plus, other)
    infix def /(other: Expr) = BinOp(e, Op.Div, other)
    infix def -(other: Expr) = BinOp(e, Op.Minus, other)
    infix def >(other: Expr) = BinOp(e, Op.Gt, other)
    infix def <(other: Expr) = BinOp(e, Op.Lt, other)

  extension(d: DynamicSymbol)
    infix def =:= (other: Expr): Eq = Eq(symToVar(d),other)
    infix def =:= (other: Int): Eq = Eq(symToVar(d), Lit(other))
    infix def =:= (other: DynamicSymbol): Eq = Eq(symToVar(d), symToVar(other))


private def accumulateReturns(programs: List[Program]): Int =
  programs match
    case Program.Block(programs) :: rest => accumulateReturns(programs) max accumulateReturns(rest)
    case Program.If(_, left, right) :: rest => accumulateReturns(List(left, right)) max accumulateReturns(rest)
    case Program.While(_, _, body) :: rest => accumulateReturns(List(body)) max accumulateReturns(rest)
    case Program.Return(ret) :: rest => ret.size max accumulateReturns(rest)
    case _ :: rest=> accumulateReturns(rest)
    case _ => 0