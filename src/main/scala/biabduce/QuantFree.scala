package biabduce

import biabduce.QuantFree.QAnd


enum QuantFree:
  case QAnd(
             pi: Pure,
             sigma: Spatial
           )

  infix def rename(re: Map[Expression, Expression]): QuantFree = this match
    case QuantFree.QAnd(pi, sigma) => QAnd(pi rename re, sigma rename re)

  infix def subst(su: Map[Expression, Expression]): QuantFree = this match
    case QuantFree.QAnd(pi, sigma) => QAnd(pi subst su, sigma subst su)


extension (quantFree: QuantFree)
  def refine = quantFree match
    case QuantFree.QAnd( pi, sigma) => (pi, sigma)

object `^`:
  def unapply(qAnd: QuantFree) = qAnd match
    case QuantFree.QAnd(pi, sigma) => (pi, sigma)

  def unapply(and: Pure): Option[(Pure, Pure)] = and match
    case Pure.&(left, right) => Some(left -> right)
    case _ => None
    
given Conversion[QuantFree, QuantFree.QAnd] = 
  case q@QuantFree.QAnd(_, _) => q
