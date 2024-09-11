package biabduce

import biabduce.Expression.{AnyTerm, BinOp, Const, LogicalVar, ProgramVar}

import scala.annotation.tailrec

type PropSet = Set[Prop]


object PropSet:
  def empty: PropSet = Set.empty

  def of(prop: Prop): PropSet = Set(prop)

  def of(propList: List[Prop]): PropSet = Set.from(propList)


extension (propSet: PropSet)

  @tailrec
  def prunePolarity(polarity: Boolean, condition: Expression): PropSet = condition match
    case ProgramVar(_) | LogicalVar(_) =>
      propSet prunePolarity BinOp(condition, if positive then Op.Eq else Op.Neq, Const(0))
    case AnyTerm(t) => pset
    case Const(0) => PropSet.empty
    case Const(const) => propSet
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
      propSet foldLeft (PropSet.empty, f)
      
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
      propSet foldLeft (PropSet.empty, f)
    case BinOp(left, Op.Gt, right) => ???
    case BinOp(left, op, right) => propSet  

  def pruneBy(condition: Expression): PropSet =
    val (setTrue, setUnknown) = propSet.foldLeft((PropSet.empty, PropSet.empty)): (sets, prop) =>
      val (setTrue, setUnknown) = sets
      prop expNormalizeProp condition match
        case Const(0) => (setTrue, setUnknown) // Zero equals false here
        case Const(_) => (setTrue + prop, setUnknown)
        case _ => (setTrue, setUnknown + prop)

    setTrue union (setUnknown prunePolarity (true, condition))
