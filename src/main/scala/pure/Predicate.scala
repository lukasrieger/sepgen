package pure


// Idea: Split by constructors
case class Predicate(name: Name, params: List[Name], body: Assert):
  override def toString: String = s"${name.name}${pretty(params)} <== $body"


def pretty(names: List[Name]): String =
  names.map(_.name).mkString("(", ", ", ")")


extension (prePost: (Predicate, Predicate))
  infix def tapPre(pre: Predicate => Unit): (Predicate, Predicate) =
    pre(prePost._1)
    prePost

  infix def tapPost(post: Predicate => Unit): (Predicate, Predicate) =
    post(prePost._2)
    prePost

object Predicate:
  def fromPre(name: Name, params: List[Name], body: Assert) =
    Predicate(Name(s"${name.name}Pre", name.index), params, renamePred(body, "pre", s"${name.name}Pre"))

  def fromPost(name: Name, params: List[Name], body:Assert) =
    Predicate(Name(s"${name.name}Post", name.index), params, renamePred(body, "post", s"${name.name}Post"))



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
