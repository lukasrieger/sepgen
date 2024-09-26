package biabduce

import biabduce.Expression.ProgramVar
import pure.Name
import biabduce.ComplexCommand.*
import biabduce.AtomicAccess.*
import biabduce.AtomicMod.*


object C:
  var counter: Int = 0

  def getCounter: Int =
    counter = counter + 1
    counter

case class Proc(
               name: Name,
               formals: List[ProgramVar],
               returnVar: ProgramVar,
               body: Command.L
               ):

  def renameUnique(n: Int): Proc =
    val su: Map[Expression, Expression] = formals.map(f => (f, f.fresh(n))).toMap
    val x: Command.L = body.map:
      case Call(name, args) =>
        Call(name, args map (_ subst su))
      case If(cond, tr, fl) =>
        If(cond subst su, tr, fl)
      case Store(ptr, field, value) =>
        Store(ptr subst su, field, value subst su)
      case Load(v, field, vl) =>
        Load(v.subst(su).asInstanceOf[ProgramVar], field, vl subst su)
      case Assign(va, valu) =>
        Assign(va.subst(su).asInstanceOf[ProgramVar], valu subst su)
      case biabduce.AtomicAccess.Free(_) => ???

    copy(
      formals = formals.map(_.fresh(n).asInstanceOf[ProgramVar]),
      body = x
    )

  def rename(su_ : Map[Expression, Expression]) =

    var su: Map[Expression, Expression] = su_

    def renameCommand(command: Command): Command =
      println(s"Renaming $command")
      command match
        case AndThen(l, r) =>
          AndThen(renameCommand(l), renameCommand(r))
        case Call(name, args) =>
          Call(name, args map (_ rename su))
        case If(cond, tr, fl) =>
          If(cond rename su, renameCommand(tr), renameCommand(fl))
        case Store(ptr, field, value) =>
          Store(ptr rename su, field, value rename su)
        case Load(v, field, vl) =>
          val renV = v rename su
          if (renV == v && renV.isInstanceOf[ProgramVar]) then
            su = su + (renV -> renV.fresh(C.getCounter))
          else
            ()

          Load(v.rename(su).asInstanceOf[ProgramVar], field, vl.rename(su))
        case Assign(va, valu) =>
          Assign(va.rename(su).asInstanceOf[ProgramVar], valu rename su)
        case biabduce.AtomicAccess.Free(_) => ???
        case NoOp => NoOp

    val x: Command.L = body.map: c =>

      renameCommand(c).asInstanceOf[Command.S]

    copy(
      formals = formals,
      body = x
    )


type ProcTable = scala.collection.mutable.Map[Name, Proc]

extension (procTable: ProcTable)
  infix def lookupProc(name: Name) =
    procTable(name)