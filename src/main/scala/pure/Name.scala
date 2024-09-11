package pure

import util.StringSyntax.*

import scala.util.Random




case class Name(name: String, index: Option[Int] = None):
  def withName(name_ : String): Name = Name(name_, index)

  def withIndex(index_ : Int): Name = Name(name, Some(index_))
  
  def increment: Name = Name(name, Some(index.getOrElse(0) + 1))

  def toLabel: String = name ~~ index

  override def toString: String = name __ index.flatMap(i => if (i > 0) Some(i) else None)


object Name:
  def someLogical: Name =
    val r = Random.nextPrintableChar()
    Name(r.toString)