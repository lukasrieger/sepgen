

object Examples:
  import pure.*
  import pure.PrgDsl.*
  import pure.Conversions.given_Conversion_Int_Lit

  val listSum = program:
    when("p".v eq 0):
      returns (0)
    .otherwise:
      load("x".v, "p".v |-> "value")
      load("n".v, "p".v |-> "next")
      call("rec", List("n".v), "y".v)
      returns ("y".v + "x".v)

  val listLength = program:
    when("p".v eq 0):
      returns (0)
    .otherwise:
      load("n".v, "p".v |-> "next")
      call("rec", List("n".v), "y".v)
      returns ("y".v + 1)

  val listReverse: Program = program:
    load("next".v, "curr".v |-> "next")
    when("next".v eq 0):
      returns ("curr".v)
    .otherwise:
      store ("prev".v) in ("curr".v |-> "next")
      call("rec", List("curr".v, "next".v), "head".v)
      returns ("head".v)
