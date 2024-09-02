package biabduce

import scala.annotation.tailrec

case class PropIter[A](
                        pitSub: Subst,
                        pitPi: Pure.L,
                        pitOld: Spatial.L,
                        pitCurr: Spatial.S,
                        pitState: Option[A],
                        pitNew: Spatial.L,
                        pitFootprint: Option[Footprint]
                      )

def createPropIter[A](prop: QuantFree): Option[PropIter[A]] = prop match
  case QuantFree.QAnd(sub, pi, sigma, footprint) => sigma.convert match
    case hpred :: sigma_ => Some(
      PropIter(
        pitSub = sub,
        pitPi  = pi,
        pitOld = List.empty,
        pitCurr = hpred,
        pitState = None,
        pitNew   = sigma_,
        pitFootprint = footprint
      )
    )
    case Nil => None
    
    
    
extension [A] (iter: PropIter[A])
  
  
  def removeCurrToProp: QuantFree =
    val sigma: Spatial.L = iter.pitOld ::: iter.pitNew
    
    QuantFree.QAnd(
      sub = iter.pitSub,
      pi = iter.pitPi.convert,
      sigma = sigma,
      footprint = iter.pitFootprint
    )
  
  def next[B]: Option[PropIter[B]] = iter.pitNew match
    case hpred_ :: new_ => Some(
      iter.copy(
        pitOld = iter.pitCurr ::: iter.pitOld,
        pitCurr = hpred_,
        pitState = None,
        pitNew = new_
      )
    )
    case Nil => None

  @tailrec
  def find[B](filter: Spatial.S => Option[B]): Option[PropIter[B]] = filter(iter.pitCurr) match
    case Some(st) => Some(iter.copy(pitState = Some(st)))
    case None => iter.next match
      case Some(iter_) => iter_.find(filter)
      case None => None