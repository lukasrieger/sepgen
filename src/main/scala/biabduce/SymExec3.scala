package biabduce

import biabduce.ComplexCommand.NoOp
import biabduce.Expression.UnOp
import pure.Name
import monocle.Traversal
import cats.implicits.*
import biabduce.SymExec2.{symExePred, symExecProc2, updateInductive}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

type Captures = Set[Spatial.Pred]

case class CaptureTower(captures: Captures, next: Option[CaptureTower]):

  def isEmpty: Boolean = captures.isEmpty && (next match
    case Some(n) => n.isEmpty
    case None    => true)

  def current: Captures = captures

  def backlinkByParam(index: Expression): (Option[Spatial.Pred], CaptureTower) =
    captures.find(_.params.contains(index)) match
      case Some(pred) => (Some(pred), this.copy(captures = captures - pred))
      case None => next match
        case Some(captureT) => captureT.backlinkByParam(index) match
          case (None, _) => (None, this)
          case (Some(pred), captureTower) => (Some(pred), this.copy(next = Some(captureTower)))
        case None => (None, this)

  infix def extendBy(captures: Captures): CaptureTower =
    CaptureTower(captures, Some(this))

  infix def unify(other: CaptureTower): CaptureTower =
    val captureT = for
      thisNext <- next
      otherNext <- other.next
    yield CaptureTower(captures & other.captures, Some(thisNext unify otherNext))

    captureT.getOrElse(CaptureTower(captures & other.captures, None))


extension (prop: Prop)
  infix def frameDiff(original: Captures, current: Captures): Prop =
    def wasConsumed(sigma: Spatial.S) = sigma match
      case p@Spatial.Pred(_, _) if original.contains(p) && !current.contains(p)  => true
      case _ => false

    prop.copy(sigma = prop.sigma.filterNot(wasConsumed))


extension (exp: Expression)
  @tailrec
  infix def needsUnfoldingUnder(captureTower: CaptureTower): (Option[Spatial.Pred], CaptureTower) = exp match
    case pvar@biabduce.Expression.ProgramVar(v) => captureTower.backlinkByParam(pvar)
    case lVar@biabduce.Expression.LogicalVar(v) => captureTower.backlinkByParam(lVar)
    case biabduce.Expression.BinOp(left, op, right) => left.needsUnfoldingUnder(captureTower)
    case biabduce.Expression.UnOp(op, expr) => expr.needsUnfoldingUnder(captureTower)
    case _ => (None, captureTower)

private object CaptureTower:
  def initial(captures: Captures = Set.empty): CaptureTower = CaptureTower(captures, None)


def symExe3Top(procedure: Proc, predName: Name)
              (using specTable: SpecTable, procTable: ProcTable): List[Specification] =
  try
    val (propSet, _) = symExecRec(
      procedure.name,
      procedure.body,
      PropSet.initial,
      predName,
      None,
      CaptureTower.initial()
    )
    val result = propSet.extractSpecs
    specTable.update(procedure.name, result)
    result

  catch case e: SymException => sys.error(s"Symbolic execution failed with $e")

def symExePred(
                procedure: Proc,
                captures: CaptureTower,
                predName: Name,
                params: List[Expression],
                updateSpec: Boolean,
                callArgs: List[Expression]
              )(using specTable: SpecTable, procTable: ProcTable): (Spatial.Pred, CaptureTower) =
  val (result, remaining) = symExecRec(procedure.name, procedure.body, PropSet.initial, predName, None, captures, Some(callArgs))

  if updateSpec then
    println(s"Previous specTable entry for $predName :: $result")
    specTable.update(predName, result.extractSpecs)
    println(s"Updated specTable for $predName with $result")

  (Spatial.Pred(predName, params), remaining)


def symExecRec(
              procedureName: Name,
              commands: Command.L,
              propSet: PropSet,
              predName: Name,
              hypothesis: Option[Name],
              captures: CaptureTower,
              programParams: Option[List[Expression]] = None
              )(using procTable: ProcTable, specTable: SpecTable): (PropSet, CaptureTower) =
  println(s"Currently executing ${commands.headOption}")
  commands match
    case (c@ComplexCommand.Call(name, args)) :: tail if name == procedureName =>
      println(s"PropSet size: ${propSet.size}")
      val programParams_ = programParams.getOrElse(sys.error("Program parameters could not be resolved"))
      println("EXECUTING SELF RECURSIVE CALL")
      val remaining = ListBuffer[CaptureTower]()
      val newPropSet = for
        prop <- propSet
        capture = captures extendBy prop.captureSet(args.toSet)
      yield
        if capture.isEmpty then
          println("CAPTURE SET IS EMPTY!")
          prop.updateInductive(
            hypothesis.getOrElse(predName), Spatial.Pred(predName, args.map(_ subst prop.sub))
          )
        else
          println("CAPTURE SET IS NOT EMPTY!")
          println(s"GOT: $capture ")
          println("IS NEW CAPTURE EQUAL TO OLD?")
          println(captures == capture)
          println(s"OLD CAPTURES: $captures ")

          val canAssertAnyways = args.map(_ subst prop.sub).forall:
            case v@Expression.ProgramVar(n) =>
              println(s"CHECKING PROGRAM PARAMETER $v AGAINST $programParams_")
              programParams_.contains(v) || n.index.isDefined
            case _ => true

          if canAssertAnyways then
            println("YEP AM ALLOWED TO JUST ASSERT YAY")
            println(s"UNDER CURRENT PROP OF $prop")
            prop.updateInductive(
              hypothesis.getOrElse(predName), Spatial.Pred(predName, args.map(_ subst prop.sub))
            )
          else
            println("NOPE, CANT ASSERT ANYWAYS")
            println(s"GOT PROGRAM PARAMS OF: $programParams")
            println(s"GOT CALL ARGS OF $args")
            val (predRes, remainingCaptures) = symExeFunctionCall3(ComplexCommand.Call(name, args.map(_ subst prop.sub)), capture, false)

            remaining += remainingCaptures
            prop
              .updateInductive(hypothesis.getOrElse(predName), predRes)
              .frameDiff(capture.current, remainingCaptures.current)

      symExecRec(
        procedureName,
        tail,
        newPropSet,
        predName,
        hypothesis,
        remaining.result.foldLeft(captures)(_ unify _), programParams
      )

    case (c@ComplexCommand.Call(name, args)) :: tail =>
      println("EXECUTING NON SELF RECURSIVE CALL")
      val remaining = ListBuffer[CaptureTower]()
      val newPropSet = for
        prop <- propSet
        capture = captures extendBy prop.captureSet(args.toSet)
      yield
        if capture.isEmpty then
          println("I AM ALLOWED TO JUST ASSERT THE PRED")
          println(s"UNDER CURRENT PROP (SIGMA) OF ${prop.sigma} ")
          val (pred, remainingCaptures) = symExeFunctionCall3(c, capture, true)
          remaining += remainingCaptures
          prop extendSigmaAndFootprint pred
        else
          println(s"NEED TO SYM EXEC CALL FOR ARGS OF $args")
          val (predRes, remainingCaptures) = symExeFunctionCall3(c, capture, true)

          remaining += remainingCaptures
          prop
            .updateInductive(hypothesis.getOrElse(predName), predRes)
            .frameDiff(capture.current, remainingCaptures.current)


      println("REMAINING PREDICATES ALLOWED TO STAY::")
      println(remaining)
      symExecRec(
        procedureName,
        tail,
        newPropSet,
        predName,
        hypothesis,
        remaining.result.foldLeft(captures)(_ unify _), programParams
      )

    case ComplexCommand.If(condition, trueBranch, falseBranch) :: tail => 
      val (oPred, tower) = condition.needsUnfoldingUnder(captures)

      val propSet_ = oPred match
        case Some(pred) =>
          println(s"Condition requires predicate $pred unfolding")
          println(s"Current propset: $propSet")
          val pp = propSet combine unfoldPred(pred)
          println(s"RESULTING propSet: $pp")
          pp
        case None => propSet

      val (pTrue_, cTrueRem)   = symExecRec(
        procedureName, trueBranch, propSet_ pruneBy condition,
        predName, hypothesis.orElse(oPred.map(_.name)), tower, programParams)
      
      val (pFalse_, cFalseRem) = symExecRec(
        procedureName, falseBranch, propSet_ pruneBy UnOp(Op.Not, condition),
        predName, hypothesis.orElse(oPred.map(_.name)), tower, programParams)

      symExecRec(
        procedureName,
        tail,
        pTrue_ union pFalse_,
        predName,
        hypothesis.orElse(oPred.map(_.name)),
        cTrueRem unify cFalseRem, programParams
      )
      
    case (atomic: Atomic) :: tail => atomic match
      case AtomicAccess.Store(pointer, field, value) =>
        val (propSet_, tower) = pointer needsUnfoldingUnder captures match
          case (Some(pred), t) => (propSet combine unfoldPred(pred), t)
          case (None, tower)   => (propSet, tower)

        symExecRec(
          procedureName, tail, propSet_ map symExecInstr(atomic),
          predName, hypothesis, tower, programParams
        )

      case AtomicAccess.Load(variable, field, value) =>
        val (propSet_, tower) = variable needsUnfoldingUnder captures match
          case (Some(pred), t) => (propSet combine unfoldPred(pred), t)
          case (None, tower)   => (propSet, tower)

        symExecRec(
          procedureName, tail, propSet_ map symExecInstr(atomic),
          predName, hypothesis, tower, programParams
        )

      case _ => symExecRec(
        procedureName, tail, propSet map symExecInstr(atomic), predName, hypothesis, captures, programParams)

    case NoOp :: tail =>
      symExecRec(procedureName, tail, propSet, predName, hypothesis, captures, programParams)

    case Nil =>
      hypothesis match
        case Some(indName_) =>
          (propSet.map: prop =>
            prop.updateSigma: sigma =>
              sigma.filterNot:
                case Spatial.Pred(`indName_`, _) => true
                case _ => false) -> captures
        case None => (propSet, captures)


def unfoldPred(pred: Spatial.Pred)(using specTable: SpecTable) =
  specTable.resolvePred(pred)

def symExeFunctionCall3(fn: ComplexCommand.Call,
                       context: CaptureTower,
                       updateSpec: Boolean
                      )(using specTable: SpecTable, procTable: ProcTable): (Spatial.Pred, CaptureTower) =
  val prog = procTable.lookupProc(fn.name)
  val prog_ = prog.rename(procTable(fn.name).formals.zip(fn.args).toMap)

  symExePred(prog_, context, fn.name.nextInc, fn.args, updateSpec, fn.args)

