package biabduce

import biabduce.ComplexCommand.NoOp
import biabduce.Expression.{LogicalVar, ProgramVar, UnOp}
import biabduce.Spatial.PointsTo
import pure.Name

import scala.annotation.tailrec
import scala.language.experimental.saferExceptions

object SymExec2:
  var counter: Int = 0

  def getCounter: Int =
    counter = counter + 1
    counter
    
  extension (prop: Prop)
    infix def updateInductive(via: Spatial.Pred): Prop =
      @tailrec
      def go(p: Spatial.Pred, seen: List[Spatial.S])(sigma: Spatial.L): Spatial.L =
        sigma match
          case Spatial.Pred(name, params) :: tail if name == p.name => (seen :+ p) ::: tail
          case other :: tail => go(p, seen :+ other)(tail)
          case Nil => seen :+ p
      
      prop updateSigma go(via, List.empty)

  def symExeProc_(proc: Proc, qName: Name)(using specTable: SpecTable, procTable: ProcTable): List[Specification] =
    try
      symExecProc2(proc, PropSet.initial, qName).extractSpecs
    catch case e: SymException => sys.error(s"Symbolic execution failed with $e")

  def symExecProc2(
                    proc: Proc,
                    propSet: PropSet,
                    qName: Name
                  )(using specTable: SpecTable, procTable: ProcTable): PropSet throws SymException =
    symExecTop(proc.name)(proc.body)(propSet)(qName)


  def symExecTop(procName: Name)
                (command: Command.L)
                (propSet: PropSet)
                (qName: Name)
                (using specTable: SpecTable, procTable: ProcTable): PropSet throws SymException =
    println(command)
    command match
      case ComplexCommand.Call(name, args) :: tail if name == procName =>
        val newPropSet = for
          prop <- propSet
          prop_ = prop updateInductive Spatial.Pred(qName, args.map(_ subst prop.sub))
        yield prop_
        symExecTop(procName)(tail)(newPropSet)(qName)
      case ComplexCommand.Call(name, args) :: tail =>
        val su: Map[Expression, Expression] = procTable(name).formals.zip(args).toMap
        val uniqueBody = procTable(name).rename(su)
        val newPropSet = symExecProc2(uniqueBody, propSet, qName)
        val newPropSet_ = newPropSet.map(_ subst2 su)
        symExecTop(procName)(tail)(newPropSet_)(qName)
      case ComplexCommand.If(condition, trueBranch, falseBranch) :: tail =>
        val propSetTrue = symExecTop(procName)(trueBranch)(propSet pruneBy condition)(qName)
        val propSetFalse = symExecTop(procName)(falseBranch)(propSet pruneBy UnOp(Op.Not, condition))(qName)
        val newPropSet = propSetTrue.union(propSetFalse)
        symExecTop(procName)(
          command = tail
        )(propSet = newPropSet)(qName)
      case (atomic: Atomic) :: tail =>
        symExecTop(procName)(tail)(propSet map symExecInstr(atomic))(qName)
      case NoOp :: tail =>
        symExecTop(procName)(tail)(propSet)(qName)
      case Nil => propSet


  def symExeFunctionCall(fnName: Name, actualParams: List[Expression])
                        (prop: Prop)
                        (using specTable: SpecTable, procTable: ProcTable): PropSet =
    val spec = specTable lookupSpec fnName
    val proc = procTable lookupProc fnName
    val results = spec map (exeSpec(fnName, prop, _, actualParams, proc.formals))
    results.toSet

