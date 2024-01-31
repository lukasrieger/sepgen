import Assert.{CoImp, PointsTo, SepAnd, SepImp, Septract}

type Syntax = Syntax.type
private object Syntax:
  extension (self: Assert)
    infix def **  (that: Assert): Assert = SepAnd(self, that)
    infix def ==> (that: Assert): Assert = SepImp(self, that)
    infix def ~~> (that: Assert): Assert = CoImp(self, that)
    infix def ~@  (that: Assert): Assert = Septract(self, that)

  extension (expr: Expression)
    infix def |->(that: Expression): Assert = PointsTo(expr, that)


def dsl(fn: Syntax ?=> Assert): Assert =
  given s: Syntax = Syntax
  fn
