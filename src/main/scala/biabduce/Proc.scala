package biabduce

import biabduce.Expression.ProgramVar
import pure.Name


case class Proc(
               name: Name,
               formals: List[ProgramVar],
               returnVar: ProgramVar,
               body: Command.L
               )


type ProcTable = Map[Name, Proc]

extension (procTable: ProcTable)
  infix def lookupProc(name: Name) =
    procTable(name)