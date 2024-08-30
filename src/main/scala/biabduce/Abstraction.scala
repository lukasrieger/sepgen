package biabduce


import Expression.*
import Pure.*


def abstractionSt1(quantFree: QuantFree): QuantFree = quantFree match
  case Eq(e, l@LogicalVar(_)) ^ spatial => True and spatial.subst(l -> e)
  case Eq(l@LogicalVar(_), e) ^ spatial => True and spatial.subst(l -> e)
  case Eq(e, l@LogicalVar(_)) ^ right ^ spatial => right and spatial.subst(l -> e)
  case Eq(l@LogicalVar(_), e) ^ right ^ spatial => right and spatial.subst(l -> e)
  case _ => quantFree
