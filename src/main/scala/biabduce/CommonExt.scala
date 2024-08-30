package biabduce

import biabduce.QuantFree.*
import biabduce.Spatial.{Emp, SepAnd}
import biabduce.Symbolic.Exists


object `▷`:
  def unapply(deltaH: (QuantFree, Symbolic)): (QuantFree, Symbolic) =
    (deltaH._1, deltaH._2)

object `**`:
  def unapply(sAnd: Spatial): Option[(Spatial, Spatial)] = sAnd match
    case Spatial.SepAnd(left, right) => Some(left, right)
    case _ => None

  /**
   * Decompose a quantifier-free formula into its rightmost spatial element and the preceding rest
   */
  def unapply(quantFree: QuantFree): (QAnd, Spatial) = quantFree match
    case QAnd(pure, left ** (right@SepAnd(_, _))) =>
      QAnd(pure, right) match
        case QAnd(_, rightQ) ** rightmost => QAnd(pure, left * rightQ) -> rightmost
    case QAnd(pure: Pure, left ** right) => QAnd(pure, left) -> right
    case QAnd(pure, spatial) => QAnd(pure, Emp) -> spatial

extension (symbolic: Symbolic)
  infix def *(s: Symbolic): Symbolic = (symbolic, s) match
    case (Exists(vars1, body1), Exists(vars2, body2)) => Exists(vars1 ++ vars2, body1 * body2)

  infix def *(spatial: Spatial): Symbolic = (symbolic, spatial) match
    case(Exists(vars, body), sp) => Exists(vars, body * sp)


extension (quantFree: QuantFree)
  infix def *(q: QuantFree): QuantFree = (quantFree, q) match
    case (pi1 ^ sigma1, pi2 ^ sigma2) => (pi1 and pi2) and (sigma1 * sigma2)

  infix def *(s: Spatial): QuantFree = (quantFree, s) match
    case (pi1 ^ sigma1, sp) => pi1 and (sp * sigma1)


extension (spatial: Spatial)
  infix def *(sp: Spatial): Spatial = SepAnd(spatial, sp)