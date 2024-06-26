package pure

import scala.util.chaining._


case class Predicate(
                      name: Name,
                      params: List[Name],
                      abstractReprs: Option[List[Name]],
                      body: Assert
                    ):

  def toAbstractRepr: Predicate =
    val (transformed, symbols) = body.toAbstractRepr
    val abstracts = symbols.toAbstractParams
    val renamed = symbols.foldLeft(transformed):
      case (body, (ptr, _)) => body.rename(
        Map(Var(name = reprNameOf(ptr)) -> Var(abstracts(ptr)))
      )
      
    copy(
      abstractReprs = Some(abstracts.values.toList),
      body = renamed
    )
  
  override def toString: String = s"${name.name}${params.pretty()}${abstractReprs.pretty()} <== $body"



extension (abstractReprs: Option[List[Name]])
  private def pretty(): String =
    abstractReprs.map(_.pretty()).getOrElse("")

extension (params: List[Name])
  private def pretty(): String =
    if (params.isEmpty)
      ""
    else
      params.mkString("(", ", ", ")")


extension (prePost: (Predicate, Predicate))
  infix def tapPre(pre: Predicate => Unit): (Predicate, Predicate) =
    pre(prePost._1)
    prePost

  infix def tapPost(post: Predicate => Unit): (Predicate, Predicate) =
    post(prePost._2)
    prePost

object Predicate:
  def fromPre(proc: Procedure, body: Assert) =
    val name = s"${proc.signature.name}Pre"
    Predicate(
      proc.signature.name.copy(name),
      proc.signature.params.map(_.name),
      None,
      body.renamePred("pre", s"$name")
    )

  def fromPost(proc: Procedure, body:Assert) =
    val name = s"${proc.signature.name}Post"
    Predicate(
      proc.signature.name.copy(name),
      proc.signature.params.map(_.name) ++ buildReturns(proc.signature.returnCount),
      None,
      body.renamePred("post", s"$name"))


private def buildReturns(returnCount: Int): List[Name] =
  List.tabulate(returnCount): i =>
    Name("result", Some(i))


extension (body: Assert)
  private def renamePred(original: String, name: String): Assert = body match
      case SepAnd(left, right) => SepAnd(left.renamePred(original, name), right.renamePred(original, name))
      case SepImp(left, right) => SepImp(left.renamePred(original, name), right.renamePred(original, name))
      case CoImp(left, right) => CoImp(left.renamePred(original, name), right.renamePred(original, name))
      case Septract(left, right) => Septract(left.renamePred(original, name), right.renamePred(original, name))
      case Imp(left, right) => Imp(left.renamePred(original, name), right.renamePred(original, name))
      case Exists(x, body) => Exists(x, body.renamePred(original, name))
      case ForAll(x, body) =>  ForAll(x, body.renamePred(original, name))
      case Pred(pred, args) if pred.name == original => Pred(Name(name, index = pred.index), args)
      case And(left, right) => And(left.renamePred(original, name), right.renamePred(original, name))
      case Case(test, ifTrue, ifFalse) => Case(test, ifTrue.renamePred(original, name), ifFalse.renamePred(original, name))
      case AssertList(asserts) => AssertList(asserts.map(_.renamePred(original, name)))
      case other => other
