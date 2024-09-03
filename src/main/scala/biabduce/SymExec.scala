package biabduce

import biabduce.Expression.{LogicalVar, ProgramVar}
import biabduce.Spatial.PointsTo
import pure.Name

import scala.annotation.tailrec
import biabduce.given_Conversion_ProgramVar_Expression

import language.experimental.saferExceptions

enum SymException extends Exception:
  case BiAbductionFailure(state: QuantFree, failAt: Spatial)
  case Other(reason: Any)


extension (sigma: Spatial.L)

  @tailrec
  infix def load(pointer: ProgExpression, field: Option[String]): Option[Expression] =
    sigma match
      case Spatial.PointsTo(`pointer`, `field`, value) :: _ => Some(value)
      case _ :: tail => tail load (pointer, field)
      case Nil => None

  @tailrec infix def store(
                            pointer: PointsTo & Spatial.S,
                            seen: List[Spatial.S] = List.empty
                          ): Spatial.L =
    sigma match
      case PointsTo(pointer.pointer, pointer.field, _) :: rest =>
        ((seen :+ pointer) ::: rest).asInstanceOf[Spatial.L]
      case other :: tail =>
        tail store(pointer, seen :+ other)
      case Nil => sigma



def symExecInstr(command: Command.L)(prop: QuantFree): (QuantFree, QuantFree) throws SymException = command match
  case biabduce.AtomicAccess.Store(pointer, field, value) :: tail => ???
  case biabduce.AtomicAccess.Free(pointer) :: tail => ???
  case biabduce.AtomicAccess.Load(v, field, value) :: tail =>
    prop.sigma.convert load(value, field) match
      case Some(value) => 
        val (pre, post) = symExecInstr(tail)(prop)
        (pre subst Map(v.convert -> value), post subst Map(v.convert -> value))
      case None => 
        // prop is insufficient -> run bi-abduction
        val toProve = PointsTo(ProgramVar(v.v), field.map(Name.apply(_, None)), LogicalVar(v.v.increment))
        runBiabduction(
          prop,
          QuantFree.QAnd(
            pi = Pure.True,
            sigma = toProve
          )
        ) match
          case Some((subs, frame, missing)) => 
            // We may now assume our post-condition + the computed frame
            val nextProp = prop.copy(sigma = Spatial.SepAnd(frame, toProve) subst subs._2)
            val (pre: QuantFree, post: QuantFree) = symExecInstr(tail)(nextProp)
            val newPre = pre.copy(
              pi = Pure.&(pre.pi, missing.missingPi) subst sub._1,
              sigma = Spatial.SepAnd(pre.sigma, missing.missingSigma) subst subs._1
            )
            (newPre, post)
            
          case None => throw SymException.BiAbductionFailure(prop, toProve)
  case biabduce.AtomicMod.Assign(x, expr) :: tail => 
    val (pre, post) = symExecInstr(tail)(prop)
    (pre subst Map(x.convert -> expr.convert)) -> (post subst Map(x.convert -> expr.convert))
  case biabduce.ComplexCommand.Call(_, _) :: tail => ???
  case biabduce.ComplexCommand.If(_, _, _) :: tail => ???
  case Nil => ???
      
    