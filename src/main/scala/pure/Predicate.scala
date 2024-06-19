package pure


// Idea: Split by constructors
case class Predicate(name: Name, params: List[Name], body: Assert):
  override def toString: String = s"${name.name}${pretty(params)} <== $body"


def pretty(names: List[Name]): String =
  names.mkString("(", ", ", ")")


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
      renamePred(body, "pre", s"${name}Pre")
    )

  def fromPost(proc: Procedure, body:Assert) =
    val name = s"${proc.signature.name}Post"
    Predicate(
      proc.signature.name.copy(name),
      proc.signature.params.map(_.name) ++ buildReturns(proc.signature.returnCount),
      renamePred(body, "post", s"${name}Post"))


private def buildReturns(returnCount: Int): List[Name] =
  List.tabulate(returnCount): i =>
    Name("result", Some(i))


private def renamePred(body: Assert, original: String, name: String): Assert = body match
    case SepAnd(left, right) => SepAnd(renamePred(left, original, name), renamePred(right, original, name))
    case SepImp(left, right) => SepImp(renamePred(left, original, name), renamePred(right, original, name))
    case CoImp(left, right) => CoImp(renamePred(left, original, name), renamePred(right, original, name))
    case Septract(left, right) => Septract(renamePred(left, original, name), renamePred(right, original, name))
    case Imp(left, right) => Imp(renamePred(left, original, name), renamePred(right, original, name))
    case Exists(x, body) => Exists(x, renamePred(body, original, name))
    case ForAll(x, body) =>  ForAll(x, renamePred(body, original, name))
    case Pred(pred, args) if pred.name == original => Pred(Name(name, index = pred.index), args)
    case And(left, right) => And(renamePred(left, original, name), renamePred(right, original, name))
    case Case(test, ifTrue, ifFalse) => Case(test, renamePred(ifTrue, original, name), renamePred(ifFalse, original, name))
    case AssertList(asserts) => AssertList(asserts.map(renamePred(_, original, name)))
    case other => other
