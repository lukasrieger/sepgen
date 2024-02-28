import pure.*
import pure.Expr.*
import pure.Program.Free
import util.show

//@main
//def main(): Unit = {
    //  val test = dsl {
    //    (Var("a") |-> Var("b")) ~~> (Var("c") |-> Var("d"))
    //
    //    Free(Var("a")) `;` Free(Var("a"))
    //
    //    (Var("a") |-> Var("b")) ~~> (Var("c") |-> Var("d")) ** (Var("e") |-> Var("f"))
    //  }
    //
    //  println(Var("test").show)
    //  println(test)
//}


@main
def main(): Unit = {
    val result = Program.backwards(Program.swapProgram)()

    println(result)
    println(simplify(result))
}




def abduce(assumptions: List[Assert], premises: List[Assert], conclusion: List[Assert]): (List[Assert], List[Assert]) =

    conclusion match
        case PointsTo(ptr, expr) :: rest =>
            val assumptions_ = ???
            val premises_ = ???

            ???

            abduce(assumptions_, premises_, rest)
        case _ =>
