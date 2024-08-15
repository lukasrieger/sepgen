package inductive

import cats.Monoid
import inductive.Pattern.{Cons, Free, Nil}
import pure.*

enum Pattern:
  case Nil
  case Cons(head: Var, tail: Var)
  case Null
  case Free(variable: Var)

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

  override def toString: String = this match
    case Pattern.Null => "null"
    case Pattern.Nil => "[]"
    case Pattern.Cons(head, tail) => s"($head :: $tail)"
    case Pattern.Free(variable) => s"$variable"


case class Head(elements: Seq[(Var, Pattern)]):
  def rename(re: Map[Var, Var]): Head =
    Head(elements.map((v, pat) => (v rename re) -> (pat rename re)))

  override def toString: String = elements.map(_._2).mkString(" ")

object Head:
  given headMonoid: Monoid[Head] = new Monoid[Head]:
    override def empty: Head = Head(Seq.empty)
    override def combine(x: Head, y: Head): Head = Head(x.elements ++ y.elements)
