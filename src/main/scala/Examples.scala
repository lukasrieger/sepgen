
object Examples:

  import pure.*
  import pure.Conversions.given_Conversion_Int_Lit
  import pure.Conversions.given_Conversion_Boolean_Lit
  import pure.ProgramDsl.symToVar
  import pure.ProgramDsl.*
  
  import scala.language.implicitConversions

  val swapPtrs = program("swapPointers")("x", "y"):
    store($.y) in $.x
    store($.x) in $.y
  
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

  val incrementAll = program("increment")("list"):
    when($.list =:= 0):
      ()
    .otherwise:
      $.next <-- $.list.next
      $.curVal <-- $.list.value
      $.newVal := $.curVal + 1
      store($.newVal) in $.list.value
      call_rec($.next)


  val removeElement = program("remove")("list", "element"):
    when($.list =:= 0):
      returns($.list)
    .otherwise:
      $.curVal <-- $.list.value
      $.next <-- $.list.next
      when($.curVal =:= $.element):
        returns($.next)
      .otherwise:
        call_rec($.next, $.element)($.newNext)
        store ($.newNext) in $.list.next
        returns($.list)

  val findElement = program("find")("list, element"):
    when($.list =:= 0):
      returns(false)
    .otherwise:
      $.value <-- $.list.value
      when($.value =:= $.element):
        returns(true)
      .otherwise:
        $.next <-- $.list.next
        call_rec($.next, $.element)($.found)
        returns ($.found)

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

  val appendList = program("appendList")("first","second"):
    $.next <-- $.first.next
    when($.next =:= 0):
      store($.second) in $.first.next
    .otherwise:
      call_rec($.next, $.second)($.ignored)



  val all = List(listSum, listLength, listReverse, dll_to_bst, appendList, swapPtrs, findElement, incrementAll, removeElement)

  // TODO: FIND + REMOVE + INCREMENT ALL + (ALL FOLD/REDUCES/FILTERS)
  // TODO : Abstract before talk (goals of thesis etc.)
  //
