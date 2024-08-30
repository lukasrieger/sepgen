import pure.Lit.Null

object Examples:

  import pure.*
  import pure.Conversions.given_Conversion_Int_Lit
  import pure.Conversions.given_Conversion_Boolean_Lit
  import pure.ProgramDsl.symToVar
  import pure.ProgramDsl.*
  
  import scala.language.implicitConversions


  val sumUntilGasLimit = program("sumUntilGasLimit")("p", "g"):
    when($.p =:= Null):
      returns (0)
    .otherwise:
      when($.g =:= 0):
        returns (0)
      .otherwise:
        $.v <-- $.p.value
        $.n <-- $.p.next
        $.gas := $.g - 1
        call_rec($.n, $.gas)($.y)
        returns ($.y + $.v)


  
  val sequenceOfPrgs = program("sequence")("p", "q"):
    call("listLength")($.p)($.a)
    call("appendList")($.p, $.q)
    returns($.p)

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
    when($.p =:= Null):
      returns(0)
    .otherwise:
      $.n <-- $.p.next
      call_rec($.n)($.y)
      returns($.y + 1)

  val incrementAll = program("increment")("list"):
    when($.list =:= Null):
      ()
    .otherwise:
      $.next <-- $.list.next
      $.curVal <-- $.list.value
      store($.curVal + 1) in $.list.value
      call_rec($.next)


  val removeElement = program("remove")("list", "el"):
    when($.list =:= Null):
      returns($.list)
    .otherwise:
      $.curVal <-- $.list.value
      $.next <-- $.list.next
      when($.curVal =:= $.el):
        returns($.next)
      .otherwise:
        call_rec($.next, $.el)($.newNext)
        store ($.newNext) in $.list.next
        returns($.list)

  val findElement = program("find")("list, el"):
    when($.list =:= Null):
      returns(false)
    .otherwise:
      $.value <-- $.list.value
      $.next <-- $.list.next
      when($.value =:= $.el):
        returns(true)
      .otherwise:
//        $.found <--local
        call_rec($.next, $.el)($.found)
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

  val appendList = program("appendList")("p","q"):
    when($.p =:= Lit.Null):
      ()
    .otherwise:
      $.next <-- $.p.next
      when($.next =:= Lit.Null):
        store($.q) in $.first.next
      .otherwise:
        call_rec($.next, $.q)

  val testHead = program("headTest")("list1", "list2"):
    $.sNext <-- $.list1.next
    $.sValue <-- $.list1.value

    $.ssNext <-- $.list2.next
    $.ssValue <-- $.list2.value

    call_rec($.sNext, $.ssNext)



  val all = List(
    listSum,
    listLength,
    listReverse,
    dll_to_bst,
    appendList,
    swapPtrs,
    findElement,
    incrementAll,
    removeElement,
    testHead
  )