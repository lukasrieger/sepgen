import pure.Syntax.**
import pure.{And, Assert, AssertList, BinOp, Bind, Case, CoImp, Emp, Eq, Exists, Expr, ForAll, Imp, Lit, Name, Not, PointsTo, Pred, Predicate, Procedure, Program, Pure, SepAnd, SepImp, Septract, Var}
import scala.annotation.tailrec



enum Unfolded:
  case Cases(cases: Map[Pure, Heap])
  case Just(heap: Heap)


def unfold(pred: Assert): Unfolded =
  def unfold_(pred: Assert): Heap = pred match
    case SepAnd(left, right) => unfold_(left) ::: unfold_(right)
    case other => List(other)

  pred match
    case Case(test @ Pure(e), ifTrue, ifFalse) =>
      Unfolded.Cases(
        Map(
          test -> unfold_(ifTrue),
          Pure(Not(e)) -> unfold_(ifFalse)
        )
      )
    case other => Unfolded.Just(unfold_(other))

def infer3(proc: Procedure, pre: Predicate): Assert =
  infer3(proc.body, pre.body)

def infer3(prg: Program, pre: Assert): Assert =
  val unfolded = unfold(pre)

  unfolded match
    case Unfolded.Cases(cases) =>
      val x = cases.map( (cond, heap) => 
        inferU(prg, List(cond) ::: heap)
      ).map(simplify)
      AssertList(x.toList)
    case Unfolded.Just(heap) =>
      inferU(prg, heap)


def inferU(prg: Program, pre: Heap): Assert =
  println(prg)
  prg match
    case Program.Assign(x, expr) => Emp // todo
    case Program.Load(x, pointer, field) => Emp
    case Program.Store(pointer, arg, field) => Emp
    case Program.Alloc(pointer) => Emp
    case Program.Free(pointer) => Emp
    case Program.Block(programs) => programs.foldLeft(Emp: Assert)((acc, pr) =>
      acc ** inferU(pr, pre)
    )
    case Program.If(test, left, right) =>
      println("Checking condition:")
      println(test)
       eval(test, pre) match
          case true =>
            println("TRUE under:")
            println(pre)
            inferU(left, Pure(test) :: pre)
          case false =>
            println("FALSE under")
            println(pre)
            inferU(right, Pure(Not(test)) :: pre)

    case Program.While(test, inv, body) => ???
    case Program.Call(name, args, rt) =>
      assume(rt.size == 1)

      println("Searching for hypothesis in: ")
      println(pre)
      val p = pre.hypothesis().get
      val q = Pred(Name("Q"), args ::: List(p) ::: List(rt.head))


      pre.reduceRight(_ ** _) ** Exists(rt.head, q)
    case Program.Return(ret) =>
      ret.zipWithIndex.map((r, i) => Pure(Eq(Var(Name("result").withIndex(i)), r)))
        .reduceRight(_ ** _)


def eval(pure: Expr, pre: Heap) = pure match
  case NullCheck(on) if pre isNullPtr on => true
  case NullCheck(on) if pre defines on => false
  case GeneralEq(on, expr) if pre defines on =>
    pre.load2(on, None) match
      case Some(`expr`) => true
      case None => false
  case _ => false


extension (heap: Heap)
  @tailrec infix def defines(pointer: Expr): Boolean =
    println(heap)
    heap match
      case PointsTo(`pointer`, None, Lit("null")) :: _ =>
        false
      case PointsTo(`pointer`, None, _) :: _ =>
        true
      case _ :: tail => tail defines pointer
      case Nil =>
        false

  infix def isNullPtr(pointer: Expr): Boolean =
    heap match
      case PointsTo(`pointer`, None, Lit("null")) :: _ =>
        true
      case PointsTo(`pointer`, None, _) :: _ =>
        false
      case _ :: tail => tail isNullPtr pointer
      case Nil => false


  def hypothesis(): Option[Var] = heap match
    case SepAnd(l, r) :: tail => List(l).hypothesis().orElse(List(r).hypothesis()).orElse(tail.hypothesis())
    case Exists(_, body) :: tail => List(body).hypothesis().orElse(tail.hypothesis())
    case Pred(_, args) :: _ => Some(args.last.asInstanceOf[Var])
    case _ :: tail => tail.hypothesis()
    case Nil => None


object NullCheck:
  def unapply(repr: Expr): Option[Expr] =
    repr match
      case Eq(left, Lit(0)) => Some(left)
      case Eq(Lit(0), right) => Some(right)
      case _ => None


object GeneralEq:
  def unapply(repr: Expr): Option[(Var, Expr)] =
    repr match
      case Eq(left: Var, right) => Some((left, right))
      case Eq(left, right: Var) => Some(right, left)
      case _ => None