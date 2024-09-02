package biabduce

import Spatial.*


//def abduce(delta: QuantFree, H: Symbolic): Symbolic = (delta, H) match
//  case (_ ^ Emp) ▷ ∃(vars, pi ^ Emp) => ∃(vars)(pi and Emp) // BaseEmp
//  case _ ▷ ∃(vars, pi ^ True) => ∃(vars)(pi and Emp) // BaseTrue
//  case (delta1 ** PointsTo(ptr0, f1, e0)) ▷ ∃(vars, delta2 ** PointsTo(ptr1, f2, e1))
//    if ptr0 == ptr1 && f1 == f2 => abduce(e0 =:= e1 and delta1, ∃(vars) (delta2)) // |-> - match
//  case other => ???
