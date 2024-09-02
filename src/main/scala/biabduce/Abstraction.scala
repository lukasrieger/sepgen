package biabduce


import Expression.*
import Pure.*


def abstractionSt1(quantFree: QuantFree): QuantFree = quantFree match
  case =:=(e, l@LogicalVar(_)) ^ spatial => True and spatial.subst(l -> e)
  case =:=(l@LogicalVar(_), e) ^ spatial => True and spatial.subst(l -> e)
  case =:=(e, l@LogicalVar(_)) ^ right ^ spatial => right and spatial.subst(l -> e)
  case =:=(l@LogicalVar(_), e) ^ right ^ spatial => right and spatial.subst(l -> e)
  case _ => quantFree
