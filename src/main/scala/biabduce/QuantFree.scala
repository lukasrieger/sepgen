package biabduce

import biabduce.QuantFree.QAnd

enum QuantFree:
  case QAnd(left: Pure, right: Spatial)

  infix def rename(re: Map[Expression, Expression]): QuantFree = this match
    case QuantFree.QAnd(left, right) => QAnd(left rename re, right rename re)

  infix def subst(su: Map[Expression, Expression]): QuantFree = this match
    case QuantFree.QAnd(left, right) => QAnd(left subst su, right subst su)


extension (quantFree: QuantFree)
  def refine: (Pure.L, Spatial.L) = quantFree match
    case QuantFree.QAnd(left, right) => (left, right)

object `^`:
  def unapply(qAnd: QuantFree): (Pure, Spatial) = qAnd match
    case QAnd(left, right) => (left, right)

  def unapply(and: Pure): Option[(Pure, Pure)] = and match
    case Pure.&(left, right) => Some(left -> right)
    case _ => None
    
given Conversion[QuantFree, QuantFree.QAnd] = 
  case q@QuantFree.QAnd(_, _) => q
