package biabduce

import biabduce.QuantFree.QAnd

enum QuantFree:
  case QAnd(left: Pure, right: Spatial)

  def rename(re: Map[Expression, Expression]): QuantFree = this match
    case QuantFree.QAnd(left, right) => QAnd(left rename re, right rename re)
  
  def subst(su: Map[Expression, Expression]): QuantFree = this match
    case QuantFree.QAnd(left, right) => QAnd(left subst su, right subst su)


object `^`:
  def unapply(qAnd: QuantFree): (Pure, Spatial) = qAnd match
    case QAnd(left, right) => (left, right)

  def unapply(and: Pure): Option[(Pure, Pure)] = and match
    case Pure.And(left, right) => Some(left -> right)
    case _ => None