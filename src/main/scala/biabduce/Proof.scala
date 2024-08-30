package biabduce

import Spatial.*

object `▷`:
  def unapply(deltaH: (QuantFree, Symbolic)): (QuantFree, Symbolic) =
    (deltaH._1, deltaH._2)


def abduce(delta: QuantFree, H: Symbolic): Symbolic = (delta, H) match
  case (_ ^ Emp) ▷ ∃(vars, pi ^ Emp) => ∃(vars)(pi and Emp) // BaseEmp
  case _ ▷ ∃(vars, pi ^ True) => ∃(vars)(pi and Emp) // BaseTrue
  case other => ???
