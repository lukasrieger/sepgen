package pure

import Assert.*
import pure.{CoImp as ~~>, Imp as -->, SepAnd as **, Septract as ~@, PointsTo as |->}
import pure.Syntax.*

enum Program:
    case Assign(x: Var, expr: Expr)
    case Load(x: Var, pointer: Expr)
    case Store(pointer: Expr, arg: Expr)
    case Alloc(pointer: Var)
    case Free(pointer: Expr)
    case Block(programs: List[Program])
    case If(test: Expr, left: Program, right: Program)
    case While(test: Expr, inv: Assert, body: Program)

def block(programs: Program*): Program.Block = Program.Block(programs = programs.toList)

object Program:
    
    
    def swapProgram: Program =
        block(
            Alloc(Var(Name("P1"))),
            Alloc(Var(Name("P2"))),
            Load(Var(Name("t")), Var(Name("P1"))),
            Load(Var(Name("b")), Var(Name("P2"))),
            Store(Var(Name("P1")), Var(Name("b"))),
            Store(Var(Name("P2")), Var(Name("t")))
        )


    def collectVars(prg: Program): Pred =
        prg match
            case Program.Assign(x, expr) => 
                Pred(Name("v"), List(x))
            case Program.Load(x, pointer) => 
                Pred(Name("v"), List(x))
            case Program.Store(pointer, arg) => 
                Pred(Name("v"), List.empty)
            case Program.Alloc(pointer) => 
                Pred(Name("v"), List(pointer))
            case Program.Free(pointer) => 
                Pred(Name("v"), List.empty)
            case Program.Block(programs) => 
                programs
                .map(collectVars)
                .foldRight[Pred](Pred(Name("v"), List.empty)) { (v, acc) =>
                    Pred(Name("v"), v.args ::: acc.args)
                }
            case Program.If(test, left, right) => 
                Pred(Name("v"), collectVars(left).args ::: collectVars(right).args)
            case Program.While(test, inv, body) => 
                collectVars(body)


    def valid(ptr: Expr): Assert =
        val x = Var(Name("_")) 
        Exists(x, ptr |-> x)

    def backwards(prg: Program)(post: Assert = collectVars(prg)): Assert = prg match
        case Assign(x, expr) => 
            val y = Exists(Var(Name("?")), post) subst Map(x -> expr)
            post
        case Load(y, ptr) =>
            val x: Var = y.prime
            Exists(x, (ptr |-> x) ** ((ptr |-> x) --* post)) rename Map(y -> x)
        case Free(ptr) =>
            valid(ptr) ** post
        case Store(ptr, arg) =>
            valid(ptr) ** ( (ptr |-> arg) --* post)
        case Alloc(ptr) =>
            val ptrP = ptr.prime
            ForAll(ptrP, (ptrP |-> Var(Name("_"))) ** post) rename Map(ptr -> ptrP)
        case Block(programs) => programs.foldRight(post) { (prg, postP) => 
            backwards(prg)(postP)    
        }
            


// https://en.wikipedia.org/wiki/Predicate_transformer_semantics


/**

 ForAll(
    Var(P1^),
    SepAnd(
        PointsTo(
            Var(P1^),
            Var(_)
        ),
        ForAll(
            Var(P2^),
            SepAnd(
                PointsTo(Var(P2^),Var(_)),Exists(Var(t^),SepAnd(PointsTo(Var(P1),Var(t^)),SepImp(PointsTo(Var(P1),Var(t^)),Exists(Var(b^),SepAnd(PointsTo(Var(P2),Var(b^)),SepImp(PointsTo(Var(P2),Var(b^)),SepAnd(Exists(Var(_),PointsTo(Var(P1),Var(_))),SepImp(PointsTo(Var(P1),Var(b)),SepAnd(Exists(Var(_),PointsTo(Var(P2),Var(_))),SepImp(PointsTo(Var(P2),Var(t)),Pred(v,List(Var(P1), Var(P2), Var(t), Var(b)))))))))))))))))




**/