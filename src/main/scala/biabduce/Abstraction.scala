package biabduce


import biabduce.Expression.LogicalVar
import biabduce.Pure.Eq


def abstractionSt1(quantFree: QuantFree): QuantFree = quantFree match
  case Eq(e, l@LogicalVar(_)) ^ spatial => Pure.True and spatial.subst(l -> e)
  case Eq(l@LogicalVar(_), e) ^ spatial => Pure.True and spatial.subst(l -> e)
  case Pure.And(Eq(e, l@LogicalVar(_)), right) ^ spatial => right and spatial.subst(l -> e)
  case Pure.And(Eq(l@LogicalVar(_), e), right) ^ spatial => right and spatial.subst(l -> e)
  case _ => quantFree
