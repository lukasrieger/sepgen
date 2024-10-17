package biabduce

import biabduce.Expression.{AnyTerm, BinOp, Const, LogicalVar, ProgramVar, UnOp}
import biabduce.Spatial.SepAnd
import biabduce.Pure.*
import biabduce.Subst.rangeMap
import pure.Name

import scala.annotation.tailrec

/*
Used to carry along the pre-condition during symbolic execution.
 */

case class Footprint(
                    pi: Pure.L,
                    sigma: Spatial.L
                    )

object Footprint:
  def empty: Footprint = Footprint(pi = List.empty, sigma = List.empty)

case class Prop(
                sub: Subst,
                pi: Pure.L,
                sigma: Spatial.L,
                footprint: Footprint
                ):


  def captureSet(vars: Set[Expression]): Set[Spatial.Pred] =
    @tailrec
    def go(
            vars: Set[Expression],
            captures: Set[Spatial.Pred] = Set.empty
          )(sigma: Spatial.L): (Set[Expression], Set[Spatial.Pred]) =
      sigma match
        case (p@Spatial.Pred(_, params)) :: tail if vars.exists(params.contains) =>
          go(vars ++ params, captures + p)(tail)
        case _ :: tail =>
          go(vars, captures)(tail)
        case Nil =>
          vars -> captures

    @tailrec
    def fix(vars: Set[Expression])
           (captures: Set[Spatial.Pred] = Set.empty): (Set[Expression], Set[Spatial.Pred]) =
      val (vars_, captures_) = go(vars, captures)(this.sigma)
      if captures != captures_ then
        fix(vars_)(captures_)
      else
        (vars, captures)

    fix(vars)(Set.empty)._2


  override def toString: String =
    s"""
      |Prop:
      |Prop:
      |   sub:   $sub
      |   pi:    ${pi.mkString(" ∧ ")}
      |   sigma: ${sigma.mkString(" * ")}
      |   footprint: $footprint
      |""".stripMargin

  def addFootprintPiSigma(pi: Pure.L, sigma: Spatial.L): Prop =
    val footprint_ = Footprint(
      pi = pi ::: this.footprint.pi,
      sigma = sigma ::: this.footprint.sigma
    )
    val prop1 = this.copy(footprint = footprint_)
    val prop2 = pi.foldRight(prop1)((eq, p) => p atomAnd eq )
    prop2

  
  infix def combineWith(other: Prop): Prop =
    val prop_ = this.addPiSigma(other.pi, other.sigma)
    prop_.copy(sub = prop_.sub ++ other.sub)

  def addPiSigma(pi: Pure.L, sigma: Spatial.L): Prop =
    val prop_ = pi.foldRight(this)((eq, p) => p atomAnd eq)
    prop_.copy(sigma = prop_.sigma ::: sigma)

  def copyFootprintPureInto(p2: Prop): Prop =
    this.pi.foldLeft(p2.copy(footprint = this.footprint))((p, s) => p.atomAnd(s))


  def extractSpec: (Prop, Prop) =
    val footprint0 = Prop(
      sub = Subst.empty,
      pi = List.empty,
      sigma = this.footprint.sigma normalize Subst.empty,
      footprint = Footprint(pi = List.empty, sigma = List.empty)
    )

    val prop0 = footprint.pi.foldLeft(footprint0)((p, s) => p.atomAnd(s))
    val pre = prop0
    val post = this.copy(footprint = Footprint(pi = List.empty, sigma = List.empty))
    (pre, post)

  
  infix def subst(sub: Subst): Prop =
    val pi: Pure.L = (this.pi ::: this.sub.toList.map(Pure.=:=(_, _))).asInstanceOf[Pure.L]
    val pi_ = pi subst sub
    val sigma_ = this.sigma subst sub
    val prop0 = Prop(
      sub = Subst.empty,
      pi = List.empty,
      sigma = sigma_,
      footprint = Footprint(pi = List.empty, sigma = List.empty)
    )
    pi_.foldLeft(prop0)((p, s) => p.atomAnd(s) )

  infix def subst2(sub: Subst): Prop =
    val pi: Pure.L = (this.pi ::: this.sub.toList.map(Pure.=:=(_, _))).asInstanceOf[Pure.L]
    val pi_ = pi subst sub
    val sigma_ = this.sigma subst sub
    val prop0 = Prop(
      sub = Subst.empty,
      pi = List.empty,
      sigma = sigma_,
      footprint = this.footprint
    )
    pi_.foldLeft(prop0)((p, s) => p.atomAnd(s))

  def toQuantFree: QuantFree = QuantFree.QAnd(pi = this.pi, sigma = this.sigma)

  infix def extendPi(pi: Pure.S): Prop =
    copy(pi = Pure.&(pi, this.pi))

  infix def extendSigma(sigma: Spatial.S): Prop =
    copy(sigma = SepAnd(sigma, this.sigma))

  infix def extendSigma(sigma: Spatial.L): Prop =
    copy(sigma = (sigma ::: this.sigma).asInstanceOf[Spatial.L] subst this.sub)

  infix def removeSigma(sigma: Spatial.S): Prop =
    copy(
      sigma = this.sigma.filterNot(_ == sigma)
    )

  infix def updateSigma(fn: Spatial.L => Spatial.L): Prop =
    copy(sigma = fn(sigma))

  infix def extendFootprintSigma(sigma: Spatial.S): Prop =
    copy(footprint = footprint.copy(sigma = SepAnd(sigma, footprint.sigma)))

  infix def extendFootprintPi(pi: Pure.S): Prop =
    copy(footprint = footprint.copy(pi = Pure.&(pi, footprint.pi)))


  infix def extendSigmaAndFootprint(spatial: Spatial.S): Prop =
    this extendSigma spatial extendFootprintSigma spatial

  infix def removePredicate(name: Name): Prop = ???


  infix def conjoinEq(exp1: Expression, exp2: Expression, footprint: Boolean = false): Prop =
    atomAnd(Pure.=:=(exp1, exp2), footprint)
  
  infix def conjoinNeq(exp1: Expression, exp2: Expression, footprint: Boolean = false): Prop =
    atomAnd(Pure.=!=(exp1, exp2), footprint)

  infix def atomAnd(atom: Pure.S, footprint: Boolean = false): Prop =
    val atom_ = atom normalize this.sub

    if this.pi contains atom_ then this
    else
      val p_ = atom_ match
        case Pure.True => this
        case left =:= right if left == right => this
        case left =:= right =>
          val subL = left -> right
          val sub_ = Map(subL) ++ (this.sub rangeMap (_ subst Map(subL)))
          val (npi_, nsigma_) = (
            this.pi normalize sub_, this.sigma normalize sub_
          )
          this.copy(sub = sub_, pi = npi_, sigma = nsigma_)

        case left =!= right =>
          val pi_ = (atom_ ::: this.pi).asInstanceOf[Pure.L] normalize this.sub
          this.copy(pi = pi_)
        case _ =>
          val pi_ = (atom_ ::: this.pi).asInstanceOf[Pure.L] normalize this.sub
          this.copy(pi = pi_)

      if footprint then
        p_.extendFootprintPi(atom_.asInstanceOf[Pure.S])
      else
        p_


  infix def expNormalizeProp(exp: Expression) =
    symEval(exp subst this.sub)

  infix def pureNormalizeProp(atom: Pure) =
    atom normalize (this.sub)


  infix def renameNoNormalize(sub: Subst): Prop =
    val pi: Pure.L = this.sub.toList.map(Pure.=:=(_, _)) ::: this.pi
    val pi_ = pi subst sub
    val sigma_ = this.sigma subst sub
    this.copy(sub = Subst.empty, pi = pi_, sigma = sigma_)

def symEval(e: Expression): Expression = e match
  case ProgramVar(v) => e
  case LogicalVar(v) => e
  case AnyTerm(t) if e == Special.Null => Const(0)
  case AnyTerm(t) => e
  case Const(const) => e
  case UnOp(op, e) =>
    val e_ = symEval(e)
    (op, e_) match
      case (Op.Not, Const(0)) => Const(1)
      case (Op.Not, Const(_)) => Const(0)
      case _ => UnOp(op, e_)

  case BinOp(left, op, right) =>
    val _left = symEval(left)
    val _right = symEval(right)

    (op, _left, _right) match
      case (Op.Plus, Const(0), _) => _right
      case (Op.Plus, _, Const(0)) => _left
      case (Op.Plus, Const(n), Const(m)) => Const(n+m)
      case (Op.Minus, Const(n), Const(0)) => Const(n)
      case (Op.Mul, Const(n), Const(1)) => Const(n)
      case (Op.Mul, Const(1), Const(n)) => Const(n)
      case (Op.Div, Const(n), Const(1)) => Const(n)
      case _ => BinOp(_left, op, _right)



object Prop:
  def empty = Prop(sub = Subst.empty, pi = List.empty, sigma = List.empty, footprint = Footprint.empty)