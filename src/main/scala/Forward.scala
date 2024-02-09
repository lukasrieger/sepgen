import cats.Applicative
import monocle.Traversal
import monocle.function.Plated
import pure.Assert
import pure.Syntax.*
import pure.Assert.{CoImp as ~~>, Imp as ==>, SepAnd as **, Septract as ~~@}
import cats.syntax.all._


enum Rule(val apply: PartialFunction[Assert, Assert]):
    case Curry extends Rule(
        { case (p ~~@ q) ==> r => q ==> (p ~~> r) }
    )
    case DeCurry extends Rule(
        { case q ==> (p ~~> r) => (p ~~@ q) ==> r }
    )
    case Cancel extends Rule(
        { case q ~~@ (q_ ~~> p) if q == q_ => p }
    )

private val transform: Assert => Option[Assert] =
    Rule.values.map(_.apply).fold(PartialFunction.empty)(_ orElse _).lift

def simplify(assert: Assert): Assert = Plated.rewrite(transform)(assert)