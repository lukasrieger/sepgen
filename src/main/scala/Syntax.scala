import Assert.{CoImp, PointsTo, SepAnd, SepImp, Septract}
import Program.Block

type Syntax = Syntax.type
private object Syntax:
  extension (self: Assert)
    infix def **  (that: Assert): Assert = SepAnd(self, that)
    infix def ==> (that: Assert): Assert = SepImp(self, that)
    infix def ~~> (that: Assert): Assert = CoImp(self, that)
    infix def ~@  (that: Assert): Assert = Septract(self, that)

  extension (expr: Expr)
    infix def |->(that: Expr): Assert = PointsTo(expr, that)


  extension (prog: Program)
    infix def `;` (next: Program): Program =
      def flatten = (prog, next) match
        case (_@left, Block(right)) => left :: right
        case (Block(left), _@right) => left ::: List(right)
        case _ => List(prog, next)

      Block(flatten)


def dsl(fn: Syntax ?=> Assert): Assert =
  given s: Syntax = Syntax
  fn
