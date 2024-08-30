package biabduce

import biabduce.ProgExpression.ProgramVar
import pure.Name

type Command = Atomic | ComplexCommand

enum ComplexCommand:
  case Call(name: Name, args: List[ProgExpression])
  case If(condition: Boolean, trueBranch: Command, falseBranch: Command)
  case AndThen(first: Command, second: Command)

enum Boolean:
  case Eq(left: ProgExpression, right: ProgExpression)
  case InEq(left: ProgExpression, right: ProgExpression)

type Atomic = AtomicAccess | AtomicMod

enum AtomicAccess:
  case Store(pointer: ProgExpression, field: Option[String], value: ProgExpression)
  case Free(pointer: ProgExpression)
  case Load(pointer: ProgramVar, value: ProgExpression)


enum AtomicMod:
  case Assign(variable: ProgramVar, value: ProgExpression)

enum ProgExpression:
  case ProgramVar(v: Name)
  case Term(t: Any)
  case Bool
//  case If(test: )

