package biabduce.dsl

import biabduce.ComplexCommand.*
import biabduce.{AtomicAccess, AtomicMod, Command, ComplexCommand, Expression, Op, Proc}
import biabduce.Expression.*
import pure.Name

import scala.collection.mutable.ListBuffer
import scala.language.dynamics


case class PartialWhen(cond: Expression, ifTrue: Command)

case class StructPointer(variable: ProgramVar, field: String)

case class PartialStore(arg: Expression)

case class PartialCall(rts: List[ProgramVar])

case class DynamicSymbol(symbol: String) extends Dynamic:
  def selectDynamic(field: String): StructPointer =
    StructPointer(ProgramVar(Name(symbol)), field)

given symToVar: Conversion[DynamicSymbol, ProgramVar] = (s: DynamicSymbol) => ProgramVar(Name(s.symbol))
given symToExp: Conversion[DynamicSymbol, Expression] = (s: DynamicSymbol) => ProgramVar(Name(s.symbol))


class ProgramScope(val proc: Proc):
  val _commands: ListBuffer[Command.S] = ListBuffer()

trait VarScope extends Dynamic:
  def selectDynamic(name: String): DynamicSymbol = DynamicSymbol(name)

def program(name: String)(args: String*)(init: ProgramScope ?=> Unit): Proc =
  val formals: List[ProgramVar] =
    args.map(n => ProgramVar(Name(n)).asInstanceOf[ProgramVar]).toList
  given scope: ProgramScope = new ProgramScope(
    Proc(
      name = Name(name),
      formals = formals,
      returnVar = ProgramVar(Name("temp")),
      body = List.empty
    )
  )

  val _ = init
  val prog = scope._commands.result()
  Proc(
    name = Name(name),
    formals = formals,
    returnVar = ProgramVar(Name("temp")),
    body = prog
  )

def $(using s: ProgramScope): VarScope = new VarScope {}

def assign(x: ProgramVar, expr: Expression)(using s: ProgramScope): Unit =
  s._commands += AtomicMod.Assign(x, expr)

def load(x: ProgramVar, pointer: Expression, field: Option[String] = None)(using s: ProgramScope): Unit =
  s._commands += AtomicAccess.Load(x,  field, pointer)

def load(x: ProgramVar, partial: StructPointer)(using s: ProgramScope): Unit =
  s._commands += AtomicAccess.Load(x, Some(partial.field), partial.variable)

def store(pointer: Expression, arg: Expression, field: Option[String] = None)(using s: ProgramScope): Unit =
  s._commands += AtomicAccess.Store(pointer, field, arg)

def store(partial: StructPointer, arg: Expression)(using s: ProgramScope): Unit =
  s._commands += AtomicAccess.Store(partial.variable, Some(partial.field), arg)

def store(arg: Expression): PartialStore =
  PartialStore(arg)

def when(test: Expression)(ifTrue: ProgramScope ?=> Unit)(using s: ProgramScope): PartialWhen =
  given subScope: ProgramScope = new ProgramScope(s.proc) {}

  val _ = ifTrue
  subScope._commands.toList match
    case head :: Nil =>
      PartialWhen(test, head)
    case head :: tail =>
      PartialWhen(
        test,
        tail.foldLeft[ComplexCommand]
          (head.asInstanceOf[ComplexCommand])
          ((acc, c) => AndThen(acc, c)))
    case Nil =>
      PartialWhen(test, NoOp)


def call(name: Name, args: List[ProgramVar])(using s: ProgramScope): Unit =
  s._commands += Call(name, args)

def call(name: String, args: List[ProgramVar])(using s: ProgramScope): Unit =
  s._commands += Call(Name(name), args)

def call(name: String)(args: ProgramVar*)(using s: ProgramScope): Unit =
  s._commands += Call(Name(name), args.toList)

def call_rec(args: ProgramVar*)(using s: ProgramScope): Unit =
  s._commands += Call(s.proc.name, args.toList)


extension (v: ProgramVar)
  infix def |->(field: String): StructPointer = StructPointer(v, field)
  infix def <--(pointer: Expression)(using s: ProgramScope): Unit = load(v, pointer, None)
  infix def <--(partial: StructPointer)(using s: ProgramScope): Unit = load(v, partial)
  infix def :=(expr: Expression)(using s: ProgramScope): Unit = assign(v, expr)

extension (partial: PartialStore)
  infix def in(pointer: Expression)(using s: ProgramScope) = store(pointer, partial.arg, None)
  infix def in(pointer: StructPointer)(using s: ProgramScope) = store(pointer, partial.arg)

extension (when: PartialWhen)
  infix def otherwise(ifFalse: ProgramScope ?=> Unit)(using s: ProgramScope): Unit =
    given subScope: ProgramScope = new ProgramScope(s.proc)

    val _ = ifFalse
    subScope._commands.toList match
      case head :: Nil =>
       s._commands += If(when.cond, when.ifTrue, head)
      case head :: tail =>
        s._commands += If(
          when.cond, 
          when.ifTrue,
          tail.foldLeft[ComplexCommand]
            (head.asInstanceOf[ComplexCommand])
            ((acc, c) => AndThen(acc, c))
        )
      case Nil =>
        s._commands += If(when.cond, when.ifTrue, NoOp)

extension (e: Expression)
  infix def eq(other: Expression) = BinOp(e, Op.Eq, other)
  infix def eq(other: Int) = BinOp(e, Op.Eq, Const(other))
  infix def =:=(other: Expression) = BinOp(e, Op.Eq, other)
  infix def =:=(other: Int) = BinOp(e, Op.Eq, Const(other))
  infix def =/=(other: Expression) = BinOp(e, Op.Neq, other)
  infix def +(other: Expression) = BinOp(e, Op.Plus, other)
  infix def /(other: Expression) = BinOp(e, Op.Div, other)
  infix def -(other: Expression) = BinOp(e, Op.Minus, other)
  infix def >(other: Expression) = BinOp(e, Op.Gt, other)
  infix def <(other: Expression) = BinOp(e, Op.Lt, other)

extension (d: DynamicSymbol)
  infix def =:=(other: Expression) = BinOp(symToVar(d), Op.Eq, other)
  infix def =:=(other: Int) = BinOp(symToVar(d), Op.Eq, Const(other))
  infix def =:=(other: DynamicSymbol) = BinOp(symToVar(d), Op.Eq, symToVar(other))

