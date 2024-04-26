
object Examples:
  import pure.*
  import pure.PrgDsl.*
  import scala.language.implicitConversions
  import pure.Conversions.given_Conversion_Int_Lit

  val listSum = program:
    when (v.p =:= 0):
      returns (0)
    .otherwise:
      v.x <-- (v.p |-> "value")
      v.n <-- (v.p |-> "next")
      call("rec")(v.n)(v.y)
      returns (v.y + v.x)

  val listLength = program:
    when (v.p =:= 0):
      returns (0)
    .otherwise:
      v.n <-- (v.p |-> "next")
      call("rec")(v.n)(v.y)
      returns (v.y + 1)

  val listReverse = program:
    v.next <-- (v.curr |-> "next")
    when (v.next =:= 0):
      returns (v.curr)
    .otherwise:
      store (v.prev) in (v.curr |-> "next")
      call("rec")(v.curr, v.next)(v.head)
      returns (v.head)
