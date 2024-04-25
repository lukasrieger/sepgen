

package Examples {

  import pure.Program.*
  import pure.*

  val listSum: Program =
    block(
      If(
        test = Eq(Var(Name("p")), Lit(0)),
        left = Return(Lit(0)),
        right = block(
          Load(Var(Name("x")), Var(Name("p")), field = Some("value")),
          Load(Var(Name("n")), Var(Name("p")), field = Some("next")),
          Call(Name("rec"), Var(Name("n")), Var(Name("y"))),
          Return(BinOp(Var(Name("y")), Op.Plus, Var(Name("x"))))
        )
      )
    )

  val listLength: Program =
    block(
      If(
        test = Eq(Var(Name("p")), Lit(0)),
        left = Return(Lit(0)),
        right = block(
          Load(Var(Name("n")), Var(Name("p")), field = Some("next")),
          Call(Name("rec"), Var(Name("n")), Var(Name("y"))),
          Return(BinOp(Var(Name("y")), Op.Plus, Lit(1)))
        )
      )
    )
    
    
  val listReverse: Program =
    block(
      Load(Var(Name("next")), Var(Name("p")), field = Some("next")),
      If(
        test = Eq(Var(Name("next")), Lit(0)),
        left = Return(Var(Name("p"))),
        right = block(
          Store(Var(Name("p")), Lit(0), field = Some("next")),
          Call(Name("rec"), Var(Name("next")), Var(Name("head"))),
          Store(Var(Name("head")), Var(Name("p")), field = Some("next")),
          Return(Var(Name("head")))
        )
      )
    )
}
