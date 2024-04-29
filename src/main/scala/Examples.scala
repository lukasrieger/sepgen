
object Examples:
  import pure.*
  import pure.ProgramDsl.*
  import scala.language.implicitConversions
  import pure.Conversions.given_Conversion_Int_Lit
  import pure.ProgramDsl.given_Conversion_DynamicSymbol_Var



  val listSum = program[2]("listSum")("p"):
    when ($.p =:= 0):
      returns (Lit(0), Lit(1))
    .otherwise:
      $.x <-- $.p.value
      $.n <-- $.p.next
      call_rec($.n)($.y)
      returns ($.y + $.x, 2)

  val listLength = program("listLength")("p"):
    when ($.p =:= 0):
      returns (0)
    .otherwise:
      $.n <-- $.p.next
      call_rec($.n)($.y)
      returns ($.y + 1)

  val listReverse = program("listReverse")("curr"):
    $.next <-- $.curr.next
    when ($.next =:= 0):
      returns ($.curr)
    .otherwise:
      store ($.prev) in $.curr.next
      call_rec($.curr, $.next)($.head)
      returns ($.head)
