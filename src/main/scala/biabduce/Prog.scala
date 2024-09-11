package biabduce

import biabduce.ProgExpression.ProgramVar
import pure.Name

type Command = Atomic | ComplexCommand

enum ComplexCommand:
  case Call(name: Name, args: List[ProgExpression])
  case If(condition: BoolExp, trueBranch: Command, falseBranch: Command)
  case AndThen(first: Command, second: Command)
  
object Command extends HasListRepr[Command]:
  type S = ComplexCommand.Call | ComplexCommand.If | Atomic
  type L = List[S]

given Conversion[Command, Command.L] = (pure: Command) => ???
given Conversion[Command.L, Command] = (pureL: Command.L) => ???

enum BoolExp:
  case Eq(left: ProgExpression, right: ProgExpression)
  case InEq(left: ProgExpression, right: ProgExpression)

type Atomic = AtomicAccess | AtomicMod

enum AtomicAccess:
  case Store(pointer: ProgExpression, field: Option[String], value: ProgExpression)
  case Free(pointer: ProgExpression)
  case Load(v: ProgramVar, field: Option[String], value: ProgExpression)


enum AtomicMod:
  case Assign(variable: ProgramVar, value: ProgExpression)

enum ProgExpression:
  case ProgramVar(v: Name)
  case Term(t: Any)
  case Bool(b: BoolExp)

given Conversion[ProgExpression, Expression] = ???