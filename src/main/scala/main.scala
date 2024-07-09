import util.{info, initLogger}
import wvlet.log.Logger
import pure.{Assert, Case, Eq, Exists, Lit, Name, PointsTo, Pred, Predicate, Pure, SepAnd, Var, collectSymbolicReferences, tapPost, tapPre}

given globalLogger: Logger = initLogger()

val ls = Predicate(
  name = Name("ls"),
  params = List(Name("p")),
  abstractReprs = Some(List(Name("xs"))),
  body = Case(
      test = Pure(Eq(Var(Name("xs")), Lit("Nil"))),
      ifTrue = PointsTo(Var(Name("p")), None, Lit("null")),
      ifFalse = Exists(
        Var(Name("n'")),
        SepAnd(
          PointsTo(Var(Name("p")), Some("next"), Var(Name("n'"))),
          Pred(Name("ls"), List(Var(Name("n'")), Var(Name("tail(xs)"))))
        )
      )
    )
)

@main
def main(): Unit =
  println(ls)
  val q = infer3(Examples.listLength, ls)

  println("DONE.")
  info(q)
//  Examples.all.foreach: proc =>
//    infer(proc)
//      .tapPre: p =>
//        info(s"${proc.signature.name} :")
//        info(s"  $p")
//      .tapPost: q =>
//        info(s"  $q")
//      .tapPre: p =>
//        info(s"  With abstract repr:")
//        info(s"    ${p.toAbstractRepr}")
//      .tapPost: q =>
//        info(s"    ${q.toAbstractRepr}")

