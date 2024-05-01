
object Examples:

  import pure.*
  import pure.Conversions.given_Conversion_Int_Lit
  import pure.ProgramDsl.symToVar
  import pure.ProgramDsl.*
  
  import scala.language.implicitConversions
  
  val listSum = program("listSum")("p"):
    when($.p =:= 0):
      returns(0)
    .otherwise:
      $.x <-- $.p.value
      $.n <-- $.p.next
      call_rec($.n)($.y)
      returns($.y + $.x)

  val listLength = program("listLength")("p"):
    when($.p =:= 0):
      returns(0)
    .otherwise:
      $.n <-- $.p.next
      call_rec($.n)($.y)
      returns($.y + 1)

  val listReverse = program("listReverse")("curr"):
    $.next <-- $.curr.next
    when($.next =:= 0):
      returns($.curr)
    .otherwise:
      store($.prev) in $.curr.next
      call_rec($.curr, $.next)($.head)
      returns($.head)
  
  val dll_to_bst = program("dll_to_bst_rec")("head", "n"):
    when($.n > 0):
      $.nLeft := $.n / 2
      call_rec($.head, $.nLeft)($.left, $.root)

      store($.left) in $.root.prev

      $.nRight := $.n - (($.n / 2) - 1)
      $.next <-- $.root.next
      call_rec($.next, $.nRight)($.temp, $.right)

      store($.temp) in $.root.next
      returns($.root, $.right)
    .otherwise:
      returns(0, $.head)

  val all = List(listSum, listLength, listReverse, dll_to_bst)