import inductive.Pattern.{Cons, Free, Nil}
import inductive.{Head, InductivePred}
import util.{info, initLogger}
import wvlet.log.Logger
import pure.{Case, Eq, Exists, Lit, Name, PointsTo, Pred, Predicate, Pure, SepAnd, Var, collectSymbolicReferences, tapPost, tapPre}
import wvlet.log.Logger.rootLogger.{debug, trace}

given globalLogger: Logger = initLogger()

val ls = Predicate(
  name = Name("ls"),
  params = List(Name("p")),
  abstractReprs = Some(List(Name("xs"))),
  body = Case(
      test = Pure(Eq(Var(Name("p")), Lit(0))),
      ifTrue = PointsTo(Var(Name("p")), None, Lit("null")),
      ifFalse = Exists(
        Var(Name("n'")),
        Exists(Var(Name("v'")),
          SepAnd(
            PointsTo(Var(Name("p")), Some("value"), Var(Name("v'"))),
            SepAnd(
              PointsTo(Var(Name("p")), Some("next"), Var(Name("n'"))),
              Pred(Name("ls"), List(Var(Name("n'"))))
            )
          )
        )
      )
    )
)

val doubleLs = Predicate(
  name = Name("lsD"),
  params = List(Name("p"), Name("q")),
  abstractReprs = Some(List(Name("xs"), Name("ys"))),
  body = SepAnd(
    Case(
      test = Pure(Eq(Var(Name("p")), Lit(0))),
      ifTrue = PointsTo(Var(Name("p")), None, Lit("null")),
      ifFalse = Exists(
        Var(Name("n'")),
        Exists(Var(Name("v'")),
          SepAnd(
            PointsTo(Var(Name("p")), Some("value"), Var(Name("v'"))),
            SepAnd(
              PointsTo(Var(Name("p")), Some("next"), Var(Name("n'"))),
              Pred(Name("ls"), List(Var(Name("n'"))))
            )
          )
        )
      )
    ),

    Case(
      test = Pure(Eq(Var(Name("q")), Lit(0))),
      ifTrue = PointsTo(Var(Name("q")), None, Lit("null")),
      ifFalse = Exists(
        Var(Name("n2'")),
        Exists(Var(Name("v2'")),
          SepAnd(
            PointsTo(Var(Name("q")), Some("value"), Var(Name("v2'"))),
            SepAnd(
              PointsTo(Var(Name("q")), Some("next"), Var(Name("n2'"))),
              Pred(Name("ls"), List(Var(Name("n2'"))))
            )
          )
        )
      )
    )
  )
)

val tripleLs = Predicate(
  name = Name("lsT"),
  params = List(Name("p"), Name("q"), Name("w")),
  abstractReprs = Some(List(Name("xs"), Name("ys"), Name("ws"))),
  body =
    SepAnd(
      SepAnd(
        Case(
          test = Pure(Eq(Var(Name("p")), Lit(0))),
          ifTrue = PointsTo(Var(Name("p")), None, Lit("null")),
          ifFalse = Exists(
            Var(Name("n'")),
            Exists(Var(Name("v'")),
              SepAnd(
                PointsTo(Var(Name("p")), Some("value"), Var(Name("v'"))),
                SepAnd(
                  PointsTo(Var(Name("p")), Some("next"), Var(Name("n'"))),
                  Pred(Name("ls"), List(Var(Name("n'"))))
                )
              )
            )
          )
        ),
        Case(
          test = Pure(Eq(Var(Name("q")), Lit(0))),
          ifTrue = PointsTo(Var(Name("q")), None, Lit("null")),
          ifFalse = Exists(
            Var(Name("n2'")),
            Exists(Var(Name("v2'")),
              SepAnd(
                PointsTo(Var(Name("q")), Some("value"), Var(Name("v2'"))),
                SepAnd(
                  PointsTo(Var(Name("q")), Some("next"), Var(Name("n2'"))),
                  Pred(Name("ls"), List(Var(Name("n2'"))))
                )
              )
            )
          )
        )
      ),
  Case(
    test = Pure(Eq(Var(Name("w")), Lit(0))),
    ifTrue = PointsTo(Var(Name("w")), None, Lit("null")),
    ifFalse = Exists(
      Var(Name("n3'")),
      Exists(Var(Name("v3'")),
        SepAnd(
          PointsTo(Var(Name("w")), Some("value"), Var(Name("v3'"))),
          SepAnd(
            PointsTo(Var(Name("w")), Some("next"), Var(Name("n3'"))),
            Pred(Name("ls"), List(Var(Name("n3'"))))
          )
        )
      )
    )
  )
)
)



def testInductiveLs = InductivePred(
  name = Name("ls"),
  arity = 2,
  constructors = Seq(
    Head(Seq(Var(Name("_")) -> Free(Var(Name("p"))), Var(Name("_")) ->  Nil)) -> PointsTo(Var(Name("p")), None, Lit("null")),
    Head(Seq(Var(Name("_")) -> Free(Var(Name("p"))), Var(Name("_")) -> Cons(Var(Name("x")), Var(Name("xs"))))) -> Exists(
      Var(Name("n'")),
      SepAnd(
        PointsTo(Var(Name("p")), Some("next"), Var(Name("n'"))),
        Pred(Name("ls"), List(Var(Name("n'")), Var(Name("xs"))))
      )
    )
  )
)

@main
def main(): Unit =
  info("---HANDWRITTEN---")
  info(testInductiveLs)
  info("---DERIVED FROM---")
  info(tripleLs)
  info("---RESULTING INDUCTIVE PREDICATE---")
  info(InductivePred.fromPred(Name("ls"), tripleLs.body))
//  println(ls)
//  val qOld = infer3(Examples.listLength, ls)
//  val qNext = inferNext(Examples.listLength, ls)()
//
//  println("DONE.")
//  info(qOld)
//  info(qNext)
//
//  val qNext2 = inferNext(
//    Examples.sequenceOfPrgs,
//    ls
//  )(context = Set(Examples.listLength, Examples.appendList))
//
//  info("----------------------------")
//  info(qNext2)

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

