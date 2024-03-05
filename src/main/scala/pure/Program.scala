package pure

import Assert.*
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
        case If(test, left, right) => ???
        case While(test, inv, body) => ???

    def abduce(conclusion: Assert): (List[Assert], List[Assert]) = 
        abduce(List(), List(), List(conclusion))


    @tailrec def abduce(
                         assumptions: List[Assert],
                         premises: List[Assert],
                         conclusion: List[Assert]
                       ): (List[Assert], List[Assert]) =

        conclusion match
            case (e @ Exists(x, PointsTo(p, y: Var))) :: rest if x == y => // does that work?
                val resolved = premises resolve p // use load

                resolved match
                    case Some(e) =>
                        val premises_ : List[Assert] = premises without p
                        abduce(assumptions, premises_, rest subst Map(y -> e))

                    case None =>
                        val assumptions_ = assumptions :+ e
                        val premises_ = premises
                        abduce(assumptions_, premises_, rest)

            case SepAnd(left, right) :: rest => abduce(assumptions, premises, left :: right :: rest)

            case c :: rest =>
                val assumptions_ = assumptions :+ c
                val premises_ = premises without c

                abduce(assumptions_, premises, rest)

            case Nil => (assumptions, premises)




extension (la: List[Assert])
    infix def resolve(p: Expr): Option[Expr] =
        la collectFirst { case PointsTo(`p`, e) => e}

    infix def without(p: Expr): List[Assert] =
        la filterNot {
            case PointsTo(`p`, _) => true
            case _ => false
        }

    infix def subst(su: Map[Var, Expr]): List[Assert] =
        la map (_ subst su)


    infix def without(p: PointsTo): Option[List[Assert]] = la match
        case `p` :: next => Some(next)
        case Nil => ???

    infix def load(p: Expr): (Option[PointsTo], List[Assert]) = ???

// https://en.wikipedia.org/wiki/Predicate_transformer_semantics

