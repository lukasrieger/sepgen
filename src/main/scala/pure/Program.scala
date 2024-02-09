package pure

import Assert.*
import pure.Assert.{CoImp as ~~>, Imp as -->, SepAnd as **, Septract as ~@, PointsTo as |->}
import pure.Syntax.*

enum Program:
    case Assign(x: Var, expr: Expr)
    case Load(x: Var, pointer: Expr)
    case Store(pointer: Expr, arg: Expr)
    case Alloc(pointer: Expr)
    case Free(pointer: Expr)
    case Block(programs: List[Program])
    case If(test: Expr, left: Program, right: Program)
    case While(test: Expr, inv: Assert, body: Program)


object Program:

    def backwards(prg: Program, post: Assert): Assert = prg match
        case Assign(x, expr) => post subst Map(x -> expr)
        case Load(y, ptr) =>
            val x: Var = y.prime
            Exists(x, (ptr |-> x) ** ((ptr |-> x) --* (post rename Map(y -> x))))


