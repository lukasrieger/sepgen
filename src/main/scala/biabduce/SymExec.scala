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


extension (prop: Prop)

/*
We either expose the value on the heap if it exists,
OR we instantiate a new logical variable as a placeholder.
This will then be picked up during bi-abduction.
 */

  infix def load(pointer: Expression, field: Option[String]): (Expression, Prop) =
    @tailrec
    def go(pointer: Expression, field: Option[String])(sigma: Spatial.L): (Expression, Boolean) =
      sigma match
        case Spatial.PointsTo(`pointer`, `field`, value) :: _ => value -> false
        case _ :: tail => go(pointer, field)(tail)
        case Nil =>
          val log = LogicalVar(field.map(s => Name(s.appended('_'))).getOrElse(Name.someLogical))
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

/*
TODO: I think we need to adjust the semantics of store and load such that they always have
      a spatial postcodnition that simply asserts that some var is now logically equal to the pointer contents
      + the existance of the pointer
 */

/*
IMPORTANT  TODO: !!NORMALIZATION!!
 */

def symExecInstr(command: Atomic)
                (prop: Prop): Prop throws SymException =
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

case class DefEnv(defs: Map[Name, Command.L])

def symExecTop(command: Command.L)
              (propSet: PropSet)
              (using specTable: SpecTable, procTable: ProcTable): PropSet throws SymException =
  command match
    case ComplexCommand.Call(name, args) :: tail =>
      val results = for
        prop <- propSet
        res = symExeFunctionCall(name, args)(prop)
      yield symExecTop(tail)(res)
      results.flatten
    case ComplexCommand.If(condition, trueBranch, falseBranch) :: tail =>
      symExecTop(
        command = tail
      )(propSet = symExecTop(trueBranch)(propSet pruneBy condition) union
        symExecTop(falseBranch)(propSet pruneBy UnOp(Op.Not, condition))
      )
    case (atomic: Atomic) :: tail =>
      symExecTop(tail)(propSet map symExecInstr(atomic))

    case NoOp :: tail => symExecTop(tail)(propSet)
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
           ): Prop =

  def combine(post: Prop, prop: Prop, splitting: Splitting) =
    val sub = splitting.sub._1 concat splitting.sub._2
    val newFootprintPi = splitting.missingPi subst sub
    val newFootprintSigma = splitting.missingSigma subst sub
    val instantiatedFrame = splitting.frame subst sub
    val instantiatedPost = post subst sub
    val post_1 = prop.copyFootprintPureInto(instantiatedPost).extendSigma(instantiatedFrame)
    val post_2 = newFootprintPi.foldLeft(post_1)((p, s) => p.atomAnd(s))

    post_2.addFootprintPiSigma(newFootprintPi, newFootprintSigma)

  def instantiateFormals =
    val params = actualParams zip formalParams
    val instantiated: List[Spatial.S] = params.map: (actual, formal) =>
      Spatial.PointsTo(formal, None, actual)
    prop extendSigma instantiated

  val inst = instantiateFormals
  runBiabduction(spec.pre, inst) match
    case Some(split) => combine(spec.post, inst, split)
    case None => sys.error("Bi-abduction seems to have failed")
