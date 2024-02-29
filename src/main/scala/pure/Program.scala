package pure

import Assert.*
import pure.{CoImp as ~~>, Imp as -->, PointsTo as |->, SepAnd as **, Septract as ~@}
import pure.Syntax.*

import scala.::
import scala.annotation.tailrec

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
            Load(Var(Name("t")), Var(Name("P1"))),
            Load(Var(Name("b")), Var(Name("P2"))),
            Store(Var(Name("P1")), Var(Name("b"))),
            Store(Var(Name("P2")), Var(Name("t")))
        )

    def collectVars(prg: Program): Pred =
        prg match
            case Program.Assign(x, expr) => 
                Pred(Name("p"), List(x))
            case Program.Load(x, pointer) => 
                Pred(Name("p"), List(x))
            case Program.Store(pointer, arg) => 
                Pred(Name("p"), List.empty)
            case Program.Alloc(pointer) => 
                Pred(Name("p"), List(pointer))
            case Program.Free(pointer) => 
                Pred(Name("p"), List.empty)
            case Program.Block(programs) => 
                programs
                .map(collectVars)
                .foldRight[Pred](Pred(Name("v"), List.empty)) { (v, acc) =>
                    Pred(Name("p"), v.args ::: acc.args)
                }
            case Program.If(test, left, right) => 
                Pred(Name("p"), collectVars(left).args ::: collectVars(right).args)
            case Program.While(test, inv, body) => 
                collectVars(body)

    def valid(ptr: Expr): Assert =
        val x = Var(Name("_")) 
        ptr |-> x

    def backwards(prg: Program)(post: Assert = collectVars(prg)): Assert = prg match
        case Assign(x, expr) =>
            post subst Map(x -> expr)
        case Load(y, ptr) =>
            val x: Var = y.prime
            val post_ = post rename Map(y -> x)
            val body = (ptr |-> x) ** ((ptr |-> x) --* post_)
            Exists(x, body)
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


    @tailrec def abduce(
                         assumptions: List[Assert],
                         premises: List[Assert],
                         conclusion: List[Assert]
                       ): (List[Assert], List[Assert]) =

        conclusion match
            case (e @ Exists(x, PointsTo(p, y: Var))) :: rest if x == y => // does that work?
                val resolved = premises resolve p

                resolved match
                    case Some(e) =>
                        val premises_ : List[Assert] = premises without p
                        abduce(assumptions, premises_, rest.map(_ subst Map(y -> e)))

                    case None =>
                        val assumptions_ = assumptions :+ e
                        val premises_ = premises
                        abduce(assumptions_, premises_, rest)

            case c :: rest =>
                val assumptions_ = assumptions :+ c
                val premises_ = premises

                abduce(assumptions_, premises, rest)



extension (premises: List[Assert])
    infix def resolve(p: Expr): Option[Expr] =
        premises
          .find { case PointsTo(`p`, e) => true }
          .map  { case PointsTo(`p`, e) => e }

    infix def without(p: Expr): List[Assert] =
        premises.filterNot { case PointsTo(`p`, _) => true }


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