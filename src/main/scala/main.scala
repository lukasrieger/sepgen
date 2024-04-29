
import pure.*

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
//
//    val inferredPre: Assert = inferPre(sumProgram)
//    val inferredPost: Assert = inferPost(sumProgram)

//    println("<<Generated Pre>>")
//    println(inferredPre)
//
//    println("<<Generated Post>>")
//    println(inferredPost)
//    println("Given program:")
//    println(sumProgram)
//
//
//    println("<<Simplified Pre>>")
//    println(inferredPre.simplify())
//
//    println("<<Simplified Post>>")
//    println(inferredPost.simplify())
//
//
//    println("Are equal?")
//    val xx = inferredPre.simplify() -> inferredPost.simplify()
//    val yy = infer(sumProgram) map (_.simplify())
//
//    println(xx == yy)
//
//    println(s"${yy._1} ==> ${yy._2}")
//
    val (revP, revQ) = infer(Examples.listReverse) map (_.simplify())
    println(s"Reverse Pre: $revP")
    println(s"Reverse Post: $revQ")

    val (listLenP, listLenQ) = infer(Examples.listLength) map (_.simplify())
    println(s"List length Pre: $listLenP")
    println(s"List length Post: $listLenQ")

    val (listSumP, listSumQ) = infer(Examples.listSum) map (_.simplify())
    println(s"List sum Pre: $listSumP")
    println(s"List sum Post: $listSumQ")

    val (dll_bst_P, dll_bst_Q) = infer(Examples.dll_to_bst) map (_.simplify())
    println(s"DLL to BST Pre: $dll_bst_P")
    println(s"DLL to BST Post: $dll_bst_Q")
    
   
    

}

