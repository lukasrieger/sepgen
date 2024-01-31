import Expression.Var

@main
def main(): Unit = {
  val test = dsl {
    Var("a") |-> Var("b")
  }

  println(test)
}