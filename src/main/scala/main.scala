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
//    val result = Program.backwards(Program.swapProgram)()
//
//    println(result)
//
//    println(Program.abduce(result))
//
//    val sumResult = Program.backwards(Program.sumProgram)()
//
//    println(sumResult)
//
//    println(Program.abduce(sumResult))

    val inferredPre: Assert = inferPre(sumProgram)
    val inferredPost: Assert = inferPost(sumProgram)

//    println("<<Generated Pre>>")
//    println(inferredPre)
//
//    println("<<Generated Post>>")
//    println(inferredPost)

    println("Given program:")
    println(sumProgram)

    println("<<Simplified Pre>>")
    println(inferredPre.simplify())

    println("<<Simplified Post>>")
    println(inferredPost.simplify())


    println("Are equal?")
    val xx = inferredPre.simplify() -> inferredPost.simplify()
    val yy = infer(sumProgram) map (_.simplify())

    println(xx == yy)

    println(s"${yy._1} ==> ${yy._2}")

}

