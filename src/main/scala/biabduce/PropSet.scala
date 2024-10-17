package biabduce

import biabduce.Expression.{AnyTerm, BinOp, Const, LogicalVar, ProgramVar, UnOp}

import scala.annotation.tailrec

type PropSet = Set[Prop]


object PropSet:
  def initial: PropSet = Set(Prop.empty)

  def empty: PropSet = Set.empty

  def of(prop: Prop): PropSet = Set(prop)

  def of(propList: List[Prop]): PropSet = Set.from(propList)



      

extension (propSet: PropSet)

  infix def combine(other: PropSet): PropSet = for
    prop1 <- propSet
    prop2 <- other
  yield
    prop1 combineWith prop2

  def extractSpecs: List[Specification] =
    propSet.map(_.extractSpec).map(Specification.apply).toList


  @tailrec
  infix def prunePolarity(positive: Boolean, condition: Expression): PropSet = condition match
    case ProgramVar(_) | LogicalVar(_) =>
      propSet prunePolarity (true, BinOp(condition, if positive then Op.Eq else Op.Neq, Const(0)))
    case AnyTerm(t) => propSet
    case Const(0) => PropSet.empty
    case Const(_) => propSet
    case UnOp(Op.Not, expr) => propSet prunePolarity (!positive, expr)
    case UnOp(_, _) => ???
    case BinOp(left, Op.Eq, right) => 
      def f(currPropSet: PropSet, prop: Prop) =
        val isInconsistent = 
          if positive then checkDisequal(prop, left, right)
          else checkEqual(prop, left, right)
        if isInconsistent then currPropSet
        else 
          val newProp = 
            if positive then prop conjoinEq (left, right, true)
            else prop conjoinNeq (left, right, true)
          
          if checkInconsistency(newProp) then currPropSet
          else currPropSet + newProp
      propSet.foldLeft(PropSet.empty)(f)
    case BinOp(left, Op.Neq, right) =>
      def f(currPropSet: PropSet, prop: Prop) =
        val isInconsistent =
          if positive then checkEqual(prop, left, right)
          else checkDisequal(prop, left, right)

        if isInconsistent then currPropSet
        else
          val newProp =
            if positive then prop conjoinNeq (left, right, true)
            else prop conjoinEq (left, right, true)

          if checkInconsistency(newProp) then currPropSet
          else currPropSet + newProp
      propSet.foldLeft(PropSet.empty)(f)
    case BinOp(left, Op.Gt, right) => ???
    case BinOp(left, op, right) => propSet  

  infix def pruneBy(condition: Expression): PropSet =
    val (setTrue, setUnknown) = propSet.foldLeft((PropSet.empty, PropSet.empty)):
      case ((setTrue, setUnknown), prop) =>
        prop expNormalizeProp condition match
          case Const(0) => (setTrue, setUnknown)
          case Const(_) => (setTrue + prop, setUnknown)
          case _ => (setTrue, setUnknown + prop)

    val ss = setTrue union (setUnknown prunePolarity (true, condition))
    ss
