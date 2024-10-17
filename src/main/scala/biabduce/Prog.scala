package biabduce


import biabduce.Expression.ProgramVar
import pure.Name
import util.Alpha

type Command = Atomic | ComplexCommand

enum ComplexCommand:
  case Call(name: Name, args: List[Expression])
  case If(condition: Expression, trueBranch: Command, falseBranch: Command)
  case AndThen(first: Command, second: Command)
  case NoOp

object Command extends HasListRepr[Command]:
  type S = ComplexCommand.Call | ComplexCommand.If | Atomic
  type L = List[S]

private def commandToL(command: Command): Command.L = command match
  case ComplexCommand.AndThen(first, second) => commandToL(first) ::: commandToL(second)
  case other => List(other.asInstanceOf[Command.S])

given Conversion[Command, Command.L] = (command: Command) => commandToL(command)
given Conversion[Command.L, Command] = (pureL: Command.L) => ???


type Atomic = AtomicAccess | AtomicMod

enum AtomicAccess:
  case Store(pointer: Expression, field: Option[String], value: Expression)
  case Free(pointer: Expression)
  case Load(v: ProgramVar, field: Option[String], value: Expression)

enum AtomicMod:
  case Assign(variable: ProgramVar, value: Expression)
