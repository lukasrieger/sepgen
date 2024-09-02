package biabduce

import biabduce.QuantFree.QAnd

case class Footprint(
                    pi: Pure.L,
                    sigma: Spatial.L
                    )

enum QuantFree:
  case QAnd(
             sub: Subst,
             pi: Pure,
             sigma: Spatial,
             footprint: Option[Footprint]
           )

  infix def rename(re: Map[Expression, Expression]): QuantFree = this match
    case QuantFree.QAnd(sub, pi, sigma, footprint) => QAnd(sub, pi rename re, sigma rename re, footprint)

  infix def subst(su: Map[Expression, Expression]): QuantFree = this match
    case QuantFree.QAnd(sub, pi, sigma, footprint) => QAnd(sub, pi subst su, sigma subst su, footprint)


extension (quantFree: QuantFree)
  def refine = quantFree match
    case QuantFree.QAnd(sub, pi, sigma, footprint) => (sub, pi, sigma, footprint)

object `^`:
  def unapply(qAnd: QuantFree) = qAnd match
    case QuantFree.QAnd(_, pi, sigma, _) => (pi, sigma)

  def unapply(and: Pure): Option[(Pure, Pure)] = and match
    case Pure.&(left, right) => Some(left -> right)
    case _ => None
    
given Conversion[QuantFree, QuantFree.QAnd] = 
  case q@QuantFree.QAnd(_, _, _, _) => q
