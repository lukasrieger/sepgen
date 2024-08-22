package inductive

import cats.Monoid
import cats.implicits.catsSyntaxSemigroup
import inductive.Pattern.{Cons, Free, Nil}
import pure.*

import scala.collection.immutable
import scala.collection.immutable.{AbstractSeq, LinearSeq}

enum Pattern:
  case Nil
  case Cons(head: Var, tail: Var)
  case Null
  case Free(variable: Var)
  case Zero
  case Succ(n: Var)

  def rename(re: Map[Var, Var]) = this match
    case Pattern.Nil =>
      Pattern.Nil
    case Pattern.Cons(head, tail) =>
      Pattern.Cons(
        re.getOrElse(head, head),
        re.getOrElse(tail, tail)
      )
    case Pattern.Null =>
      Pattern.Null
    case Pattern.Free(variable) =>
      Pattern.Free(re.getOrElse(variable, variable))
    case Zero =>
      Pattern.Zero
    case Succ(n) =>
      Pattern.Succ(re.getOrElse(n, n))

  override def toString: String = this match
    case Pattern.Null => "null"
    case Pattern.Nil => "[]"
    case Pattern.Cons(head, tail) => s"($head :: $tail)"
    case Pattern.Free(variable) => s"$variable"
    case Pattern.Zero => "0"
    case Pattern.Succ(n) => s"$n"


case class Head(
                 elements: Seq[(Var, Pattern)],
                 guard: Option[Seq[Expr]]
               ):
  def rename(re: Map[Var, Var]): Head =
    Head(
      elements.map((v, pat) => (v rename re) -> (pat rename re)),
      guard.map(_ map(_ rename re))
    )

  override def toString: String =
    val params = elements.map(_._2).mkString(" ")
    val guard_ = guard.map(_.mkString("&"))

    guard_ match
      case Some(g) => s"$params when $g"
      case None => params


object Head:
  given headMonoid: Monoid[Head] = new Monoid[Head]:
    override def empty: Head = Head(Seq.empty, None)
    override def combine(x: Head, y: Head): Head = Head(
      x.elements ++ y.elements,
      x.guard combine y.guard
    )
