package biabduce

import biabduce.ComplexCommand.NoOp
import biabduce.Expression.{LogicalVar, ProgramVar, UnOp}
import biabduce.Spatial.PointsTo
import pure.Name

import scala.annotation.tailrec
import scala.language.experimental.saferExceptions

enum SymException extends Exception:
  case BiAbductionFailure(state: QuantFree, failAt: Spatial)
  case Other(reason: Any)
  
object Counter:
  var counter: Int = 0
  
  def getCounter =
    counter = counter + 1
    counter


extension (prop: Prop)
  
  infix def load(pointer: Expression, field: Option[String]): (Expression, Prop) =
    @tailrec
    def go(pointer: Expression, field: Option[String])(sigma: Spatial.L): (Expression, Boolean) =
      sigma match
        case Spatial.PointsTo(`pointer`, `field`, value) :: _ => value -> false
        case _ :: tail => go(pointer, field)(tail)
        case Nil =>
          val log = LogicalVar(field.map(s => Name(s.appended('_'))).getOrElse(Name.someLogical)).fresh(Counter.getCounter)
          log -> true

    go(pointer, field)(prop.sigma) match
      case (exp, false) => (exp, prop)
      case (exp, true) => (exp, prop extendSigmaAndFootprint PointsTo(pointer, field, exp))



  infix def store(pointer: PointsTo): Prop =
    @tailrec
    def go(pointer: PointsTo, seen: List[Spatial.S] = List.empty)(sigma: Spatial.L): Spatial.L =
      sigma match
        case PointsTo(pointer.pointer, pointer.field, _) :: tail =>
          (seen :+ pointer) ::: tail
        case other :: tail => go(pointer, seen :+ other)(tail)
        case Nil => sys.error(s"Failed to store $pointer, matching |-> doesnt exist even though it should.")

    prop updateSigma go(pointer)


def symExecInstr(command: Atomic)
                (prop: Prop): Prop =
  command match
    case biabduce.AtomicAccess.Store(pointer, field, newValue) =>
      val (_, prop_) = prop load(pointer, field)
      prop_ store PointsTo(pointer, field, newValue)
    case biabduce.AtomicAccess.Free(pointer) =>
      ???
    case biabduce.AtomicAccess.Load(v, field, pointer) =>
      val (value, prop_) = prop load(pointer, field)
      prop_ conjoinEq(v, value)
    case biabduce.AtomicMod.Assign(x, expr) =>
      prop conjoinEq(x, expr)

def symExeProc(proc: Proc)(using specTable: SpecTable, procTable: ProcTable): List[Specification] =
  try
    specTable.get(proc.name) match
      case Some(specs) => specs
      case None =>
        println("Running symbolic execution")
        val specs = symExecTop(proc.name)(proc.body)(PropSet.initial).extractSpecs
        specTable.update(proc.name, specs)
        specs
  catch case e: SymException => sys.error(s"Symbolic execution failed with $e")


def symExecTop(procName: Name)
              (command: Command.L)
              (propSet: PropSet)
              (using specTable: SpecTable, procTable: ProcTable): PropSet throws SymException =
  println(s"Executing $command")
  command match
    case ComplexCommand.Call(name, args) :: tail if name == procName =>
      val newPropSet = for
        prop <- propSet
        prop_ = prop.extendSigmaAndFootprint(Spatial.Pred(procName, args.map(_ subst prop.sub)))
      yield prop_
      symExecTop(procName)(tail)(newPropSet)
    case ComplexCommand.Call(name, args) :: tail =>
      val results = for
        prop <- propSet
        res = symExeFunctionCall(name, args)(prop)
      yield symExecTop(procName)(tail)(res)
      results.flatten
    case ComplexCommand.If(condition, trueBranch, falseBranch) :: tail =>
      val propSetTrue = symExecTop(procName)(trueBranch)(propSet pruneBy condition)
      val propSetFalse = symExecTop(procName)(falseBranch)(propSet pruneBy UnOp(Op.Not, condition))
      val newPropSet = propSetTrue.union(propSetFalse)
      symExecTop(procName)(
        command = tail
      )(propSet = newPropSet)
    case (atomic: Atomic) :: tail =>
      symExecTop(procName)(tail)(propSet map symExecInstr(atomic))
    case NoOp :: tail =>
      symExecTop(procName)(tail)(propSet)
    case Nil => propSet


def symExeFunctionCall(fnName: Name, actualParams: List[Expression])
                      (prop: Prop)
                      (using specTable: SpecTable, procTable: ProcTable): PropSet =
  val spec = specTable lookupSpec fnName
  val proc = procTable lookupProc fnName
  val results = spec map (exeSpec(fnName, prop, _, actualParams, proc.formals))
  results.toSet


def exeSpec(
             name: Name,
             prop: Prop,
             spec: Specification,
             actualParams: List[Expression],
             formalParams: List[ProgramVar]
           )(using specTable: SpecTable, procTable: ProcTable): Prop =

  def combine(post: Prop, prop: Prop, splitting: Splitting) =
    val sub = splitting.sub._1 concat splitting.sub._2
    val newFootprintPi = splitting.missingPi subst sub
    val newFootprintSigma = splitting.missingSigma subst sub
    val instantiatedFrame = splitting.frame subst sub

    println(s"FOOTPRINT SIGMA :: $newFootprintSigma")
    val instantiatedPostPred = Prop.empty.extendSigma(Spatial.Pred(name, actualParams)) subst sub

//    val instantiatedPost = post subst sub
    val instantiatedPost = instantiatedPostPred
    val post_1 = prop.copyFootprintPureInto(instantiatedPost).extendSigma(instantiatedFrame)
    val post_2 = newFootprintPi.foldLeft(post_1)((p, s) => p.atomAnd(s))

    post_2.addFootprintPiSigma(newFootprintPi, newFootprintSigma)

  def instantiateFormals =
    val params = formalParams zip actualParams
    val instantiated: List[Spatial.S] = params.map: (actual, formal) =>
      Spatial.PointsTo(formal, None, actual)

    println(s"GOT INSTANTIATION : $instantiated")
    prop //extendSigma instantiated

  val inst = instantiateFormals
  val specPrePred = Prop.empty.extendSigma(Spatial.Pred(name, actualParams))
  
  println(s"RUNNING BI-ABDUCTION with $inst |- $specPrePred")
  runBiabduction(inst, specPrePred) match
    case Some(split) => combine(spec.post, inst, split)
    case None => sys.error("Bi-abduction seems to have failed")
