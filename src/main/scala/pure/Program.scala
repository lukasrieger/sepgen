package pure

import pure.Syntax.*
import pure.Conversions.given_Conversion_List_AssertList
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
  case Call(name: Name, arg: Var, rt: Var)
  case Return(ret: Expr)

def block(programs: Program*): Program.Block = Program.Block(programs = programs.toList)

object Program:

  def swapProgram: Program =
    block(
      Load(Var(Name("t")), Var(Name("P1"))),
      Load(Var(Name("b")), Var(Name("P2"))),
      Store(Var(Name("P1")), Var(Name("b"))),
      Store(Var(Name("P2")), Var(Name("t")))
    )

  def sumProgram: Program =
    block(
      If(
        test = Eq(Var(Name("p")), Lit(0)),
        left = Return(Lit(0)),
        right = block(
          Load(Var(Name("x")), Var(Name("p"), field = Some(Var(Name("value"))))),
          Load(Var(Name("n")), Var(Name("p"), field = Some(Var(Name("next"))))),
          Call(Name("rec"), Var(Name("n")), Var(Name("y"))),
          Return(Var(Name("y")))
        )
      )
    )

  def collectVars(prg: Program): Pred =
    prg match
      case Program.Assign(x, _) =>
        Pred(Name("post"), List(x))
      case Program.Load(x, _) =>
        Pred(Name("post"), List(x))
      case Program.Store(_, _) =>
        Pred(Name("post"), List.empty)
      case Program.Alloc(pointer) =>
        Pred(Name("post"), List(pointer))
      case Program.Free(_) =>
        Pred(Name("post"), List.empty)
      case Program.Block(programs) =>
        programs
          .map(collectVars)
          .foldRight[Pred](Pred(Name("v"), List.empty)) { (v, acc) =>
            Pred(Name("post"), v.args ::: acc.args)
          }
      case Program.If(_, left, right) =>
        Pred(Name("post"), collectVars(left).args ::: collectVars(right).args)
      case Program.While(_, inv, body) =>
        collectVars(body)
      case Call(_, _, ret) => Pred(Name("post"), List(ret))
      case Return(_) => Pred(Name("post"), List.empty)


  def valid(ptr: Expr): Assert =
    val x = Var(Name("_"))
    ptr |-> x

  def backwards(prg: Program)(post: Assert = collectVars(prg)): Assert = prg match
    case Assign(x, expr) =>
      post subst Map(x -> expr)
    case Load(y, ptr) =>
      val x: Var = y.prime
      val post_ = post rename Map(y -> x) // ?????
      val body = (ptr |-> x) ** ((ptr |-> x) --* post_)

      val existsX = Exists(x, ptr |-> x)
      val body_ = existsX ** ((ptr |-> x) --* post_)

      body_
    case Free(ptr) =>
      valid(ptr) ** post
    case Store(ptr, arg) =>
      valid(ptr) ** ((ptr |-> arg) --* post)
    case Alloc(ptr) =>
      val ptrP = ptr.prime
      ForAll(ptrP, (ptrP |-> Var(Name("_"))) ** post) rename Map(ptr -> ptrP)
    case Block(programs) => programs.foldRight(post) { (prg, postP) =>
      backwards(prg)(postP)
    }
    case If(test, left, right) => 
      Case(
        test = Pure(test),
        ifTrue = backwards(left)(post),
        ifFalse = backwards(right)(post)
      )
      
    case While(test, inv, body) => ???

    case Call(Name("rec", _), arg, ret) =>
      val pre = Pred(Name("pre"), List(arg))
      val post = Pred(Name("post"), List(arg, ret))

      pre ** (post --* post)

    case Return(ret) => Emp

  def abduce(conclusion: Assert): (List[Assert], List[Assert]) =
    abduce(List(), List(), List(conclusion), List.empty)


  def abduce(
                       assumptions: List[Assert],
                       premises: List[Assert],
                       conclusion: List[Assert],
                       conclusionsRest: List[Assert]
                     ): (List[Assert], List[Assert]) =

    conclusion match
      case (e@Exists(x, ptr@PointsTo(p, _, y: Var))) :: rest if x == y =>
        println("Asserting that pointer points to something")
        premises load p match
          case (None, tail) =>
            println("But found no match in premises")
            //            abduce(assumptions, ptr :: tail, rest, conclusionsRest) // This is wrong
            //            abduce(assumptions, tail, rest, conclusionsRest :+ e)
            abduce(assumptions :+ e, tail, rest, conclusionsRest)
          case (Some(PointsTo(_, _, e)), tail) =>
            println(s"Match in premises found, substituting with $e")
            abduce(assumptions, tail, rest subst Map(y -> e), conclusionsRest)


      case SepAnd(left, right) :: rest =>
        abduce(assumptions, premises, left :: right :: rest, conclusionsRest)

      case SepImp(left, right) :: rest =>
        abduce(assumptions, premises :+ left, right :: rest, conclusionsRest) // P => Q -* R --> P * Q => R

      case Case(test, ifTrue, ifFalse) :: rest =>
        val (ifTrueAbduced: List[Assert], _) =
          abduce(List(), test :: premises, List(ifTrue), List())
        val (ifFalseAbduced: List[Assert], _) =
          abduce(List(), Pure(Not(test.expr)) :: premises, List(ifFalse), List())
        
        val assumptions_ = Case(test, ifTrueAbduced, ifFalseAbduced) :: assumptions
        
        abduce(assumptions_, premises, rest, conclusionsRest)
        

      case c :: rest =>
        val assumptions_ = assumptions :+ c

        premises cancel c match
          case (Some(_), tail) =>
            println("Found matching")
            abduce(assumptions_, tail, rest, conclusionsRest)
          case (None, _) =>
            println(s"?????: $c")
            abduce(assumptions :+ c, premises, rest, conclusionsRest)

      
      case Nil =>
        println(s"Reached end, left with the following premises $premises")
        (assumptions, conclusionsRest)


extension (la: List[Assert])

  infix def cancel(p: Assert): (Option[Assert], List[Assert]) =
    go(List.empty, la) { a =>
      if a == p then Some(a) else None
    }

  infix def subst(su: Map[Var, Expr]): List[Assert] =
    la map (_ subst su)

  infix def load(p: Expr): (Option[PointsTo], List[Assert]) =
    go(List.empty, la) {
      case ptr@PointsTo(`p`, _, _) => Some(ptr)
      case _ => None
    }

  @tailrec private def go[T](
                              prefix: List[Assert],
                              rest: List[Assert]
                            )(extract: Assert => Option[T]): (Option[T], List[Assert]) =
    rest match
      case head :: tail if extract(head).isDefined =>
        (extract(head), prefix ::: tail)
      case other :: tail =>
        go(prefix :+ other, tail)(extract)
      case Nil =>
        (None, rest)


// https://en.wikipedia.org/wiki/Predicate_transformer_semantics


