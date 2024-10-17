package biabduce

import biabduce.ComplexCommand.NoOp
import biabduce.Expression.{LogicalVar, ProgramVar, UnOp}
import biabduce.Spatial.PointsTo
import pure.Name
import scala.collection.View.unfold

import scala.annotation.tailrec
import scala.language.experimental.saferExceptions

object SymExec2:
  var counter: Int = 0

  def getCounter: Int =
    counter = counter + 1
    counter

    
  extension (prop: Prop)
    infix def updateInductive(old: Name, via: Spatial.Pred): Prop =
      @tailrec
      def go(p: Spatial.Pred, seen: List[Spatial.S])(sigma: Spatial.L): Spatial.L =
        sigma match
          case Spatial.Pred(name, params) :: tail if name == old => (seen :+ p) ::: tail
          case other :: tail => go(p, seen :+ other)(tail)
          case Nil => seen :+ p
      
      prop updateSigma go(via, List.empty)

  extension (capture: Set[Spatial.Pred])
    infix def selectCandidate(vars: Set[Expression]): (Spatial.Pred, Set[Spatial.Pred]) =
      val candidate = capture.toList
        .sortWith((a, b) => a.params.size < b.params.size)
        .find(p => p.params.contains(vars.head))
        .head

      (candidate, capture - candidate)


//  def runSymbolicTop(proc: Proc, qName: Name)(using specTable: SpecTable, procTable: ProcTable): List[Specification]


  def symExeProc_(proc: Proc, qName: Name)(using specTable: SpecTable, procTable: ProcTable): List[Specification] =
    try
      val result = symExecProc2(proc, PropSet.initial, qName, None).extractSpecs
      specTable.update(proc.name, result)

      result
    catch case e: SymException => sys.error(s"Symbolic execution failed with $e")


  def symExePred(
                  proc: Proc,
                  propSet: PropSet,
                  qName: Name,
                  params: List[Expression],
                  updateSpec: Boolean,
                  indName: Option[Name]
                )(using specTable: SpecTable, procTable: ProcTable): Spatial.Pred throws SymException =
    val result = symExecProc2(proc, propSet, qName, indName)

    if updateSpec then
      println(s"Previous specTable entry for $qName :: $result")
      specTable.update(qName, result.extractSpecs)
      println(s"Updated specTable for $qName with $result")
    Spatial.Pred(qName, params)

  def symExecProc2(
                    proc: Proc,
                    propSet: PropSet,
                    qName: Name,
                    indName: Option[Name]
                  )(using specTable: SpecTable, procTable: ProcTable): PropSet throws SymException =
    symExecTop(proc.name)(proc.body)(propSet)(qName)(indName)


  type CaptureSet = Set[Spatial.Pred]

  def symExecTop(procName: Name)
                (command: Command.L)
                (propSet: PropSet)
                (qName: Name)
                (indName: Option[Name])
                (using procTable: ProcTable, specTable: SpecTable): PropSet throws SymException =
    println(command)
    println(propSet)
    command match
      case (c@ComplexCommand.Call(name, args)) :: tail if name == procName =>
        println(s"Executing recursive call for: $name under $propSet")
        val newPropSet = for
          prop <- propSet
          capture = prop.captureSet(args.toSet)
        yield
          if capture.isEmpty then
            println(s"Capture set is empty for recursive call $name")
            // arguments are not captured by any other predicates on the heap
            prop updateInductive (indName.getOrElse(qName), Spatial.Pred(qName, args.map(_ subst prop.sub)))
          else
            println(s"Capture set is not empty for $c  $capture!")
            // there is another predicate that captures some of the arguments
            val (predicate, captureFrame) = capture.selectCandidate(args.toSet)

            // BUT: The selected predicate may be insufficient to establish the current one.
            // -> Run recursively with selected predicate
            val predRes = symExeFunctionCall(c, Some(predicate), false)
            prop updateInductive (indName.getOrElse(qName), predRes)

            // TODO: Use capture set to update frame accordingly

        println("Done executing recursive call")
        symExecTop(procName)(tail)(newPropSet)(qName)(indName)
      case (c@ComplexCommand.Call(name, args)) :: tail =>
        val newPropSet = for
          prop <- propSet
          capture = prop.captureSet(args.toSet)
        yield
          if capture.isEmpty then
            // Foreign function call with parameters that are not captured elsewhere
            prop extendSigma symExeFunctionCall(c, None, true)
          else

            val (predicate, captureFrame) = capture.selectCandidate(args.toSet)
            println(s"CAPTURE SET NOT EMPTY FOR CALL OF ${c} :: $capture  $predicate")
            println(s"REMOVING PREDICATE AFTER USE IN:: $name")
            val rr = prop.removeSigma(predicate)
            println(s"AFTER REMOVAL :: $rr")
            rr extendSigma symExeFunctionCall(c, Some(predicate), true)
            // TODO: Use capture set to update frame accordingly


        println(s"AFTER CALL NEW PROPSET: $newPropSet")
        symExecTop(procName)(tail)(newPropSet)(qName)(indName)
      case ComplexCommand.If(condition, trueBranch, falseBranch) :: tail =>
        val propSetTrue = symExecTop(procName)(trueBranch)(propSet pruneBy condition)(qName)(indName)
        val propSetFalse = symExecTop(procName)(falseBranch)(propSet pruneBy UnOp(Op.Not, condition))(qName)(indName)
        val newPropSet = propSetTrue.union(propSetFalse)
        symExecTop(procName)(
          command = tail
        )(propSet = newPropSet)(qName)(indName)
      case (atomic: Atomic) :: tail =>
        symExecTop(procName)(tail)(propSet map symExecInstr(atomic))(qName)(indName)
      case NoOp :: tail =>
        symExecTop(procName)(tail)(propSet)(qName)(indName)
      case Nil =>
        indName match
          case Some(indName_) =>
            propSet.map: prop =>
              prop.updateSigma: sigma =>
                sigma.filterNot:
                  case Spatial.Pred(`indName_`, _) => true
                  case _ => false
          case None => propSet


  def symExeFunctionCall(fn: ComplexCommand.Call,
                         context: Option[Spatial.Pred] = None,
                         updateSpec: Boolean
                        )(using specTable: SpecTable, procTable: ProcTable): Spatial.Pred throws SymException =
    val prog = procTable.lookupProc(fn.name)
    val prog_ = prog.rename(procTable(fn.name).formals.zip(fn.args).toMap)
    val context_ = context.map(specTable.resolvePred).getOrElse(PropSet.initial)

    symExePred(prog_, context_, fn.name.nextInc, fn.args, updateSpec, context.map(_.name))
