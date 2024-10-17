package pure

import util.StringSyntax.*

import scala.util.Random


private object NameCounter:
  var counter: Int = 0

  def getCounter: Int =
    counter = counter + 1
    counter

case class Name(name: String, index: Option[Int] = None):
  def withName(name_ : String): Name = Name(name_, index)

  def withIndex(index_ : Int): Name = Name(name, Some(index_))
  
  def increment: Name = Name(name, Some(index.getOrElse(0) + 1))
  
  def nextInc: Name = Name(name, Some(NameCounter.getCounter))

  def toLabel: String = name ~~ index

  override def toString: String = name __ index.flatMap(i => if (i > 0) Some(i) else None)


object Name:
  def someLogical: Name =
    val r = Random.nextPrintableChar()
    Name(r.toString)