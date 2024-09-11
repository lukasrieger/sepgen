package biabduce

import biabduce.Expression.{LogicalVar, ProgramVar}
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

  infix def load(pointer: ProgExpression, field: Option[String]): (Expression, Prop) =
    @tailrec
    def go(pointer: ProgExpression, field: Option[String])(sigma: Spatial.L): (Expression, Boolean) =
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

def symExecInstr(command: Atomic)(prop: Prop): Prop throws SymException = command match
  case biabduce.AtomicAccess.Store(pointer, field, newValue) =>
    val (_, prop_) = prop load (pointer, field)
    prop_ store PointsTo(pointer, field, newValue)
  case biabduce.AtomicAccess.Free(pointer) =>
    ???
  case biabduce.AtomicAccess.Load(v, field, pointer)  =>
    val (value, prop_) = prop load (pointer , field)
    prop_ conjoinEq (v, value)
  case biabduce.AtomicMod.Assign(x, expr)  =>
    prop conjoinEq (x, expr)
      


def symExecTop(command: Command.L)(prop: Prop): Seq[Prop] throws SymException = command match
  case ComplexCommand.Call(name, args) :: tail => ???
  case ComplexCommand.If(condition, trueBranch, falseBranch) :: tail  =>
    val enumerateCases: Seq[Prop] = ???
    for
      caseProp <- enumerateCases
      res <- symExecTop(tail)(caseProp)
    yield res
  case (atomic: Atomic) :: tail  => symExecTop(tail)(symExecInstr(atomic)(prop))
  case Nil => Seq(prop)
  
  
