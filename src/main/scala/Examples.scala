
object Examples:
  import pure.*
  import pure.ProgramDsl.*
  import scala.language.implicitConversions
  import pure.Conversions.given_Conversion_Int_Lit
  import pure.ProgramDsl.given_Conversion_DynamicSymbol_Var



  val listSum = program("listSum")("p"):
    when ($.p =:= 0):
      returns (0)
    .otherwise:
      $.x <-- $.p.value
      $.n <-- $.p.next
      call_rec($.y)($.n)
      returns ($.y + $.x)

  val listLength = program("listLength")("p"):
    when ($.p =:= 0):
      returns (0)
    .otherwise:
      $.n <-- $.p.next
      call_rec($.y)($.n)
      returns ($.y + 1)

  val listReverse = program("listReverse")("curr"):
    $.next <-- $.curr.next
    when ($.next =:= 0):
      returns ($.curr)
    .otherwise:
      store ($.prev) in $.curr.next
      call_rec($.head)($.curr, $.next)
      returns ($.head)


  val dll_to_bst = program("dll_to_bst_rec")("head", "n"):
    when($.n > 0):
      $.nLeft := $.n / 2
      call_rec($.left, $.root) using ($.head, $.nLeft)

      store ($.left) in $.root.prev

      $.nRight := $.n - (($.n / 2) - 1)
      $.next <-- $.root.next
      call_rec($.temp, $.right) using ($.next, $.nRight)

      store ($.temp) in $.root.next
      returns ($.root, $.right)
    .otherwise:
      returns (0, $.head)


