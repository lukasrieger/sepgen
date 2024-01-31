import Expression.Var
import Program.Free

@main
def main(): Unit = {
  val test = dsl {
    (Var("a") |-> Var("b")) ~~> (Var("c") |-> Var("d"))

    Free(Var("a")) `;` Free(Var("a"))

    (Var("a") |-> Var("b")) ~~> (Var("c") |-> Var("d")) ** (Var("e") |-> Var("f"))
  }

  println(test)
}