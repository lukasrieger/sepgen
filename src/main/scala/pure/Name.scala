package pure

import util.StringSyntax.*

case class Name(name: String, index: Option[Int] = None):
  def withName(name_ : String): Name = Name(name_, index)
  def withIndex(index_ : Int): Name = Name(name, Some(index_))

  def toLabel: String = name ~~ index
  override def toString: String = name __ index

