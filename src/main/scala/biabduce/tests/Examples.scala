package biabduce.tests


import biabduce.Special
import biabduce.dsl.*
import biabduce.dsl.symToVar


val sequenceOfPrgs = program("sequence")("p", "q"):
  // emp, footprint: P(p)
  call("listLength")($.p)
  // P(p, r_1) | .. * .. * P(p.next, ...)
  call("listLength")($.q)
  // P(p, r_1) * Q(q, r_2) || NOT: PQ(p, q, r_1, r2, ...)
  call("appendList")($.p, $.q)
  // A(p, q) * Q(q, r_2)
  call("listLength")($.p)

  // f(params) | -> heap - ind(...) + P(params)

val listSum = program("listSum")("p"):
  when($.p =:= Special.Null):
    ()
  .otherwise:
    $.x <-- $.p.value
    $.n <-- $.p.next
    call_rec($.n)

val listLength = program("listLength")("ptr"):
  when($.ptr =:= Special.Null):
    ()
  .otherwise:
    $.n <-- $.ptr.next
    $.v <-- $.ptr.value
    call_rec($.n)


val appendList = program("appendList")("ls1", "ls2"):
  when($.ls1 =:= Special.Null):
    ()
  .otherwise:
    $.next <-- $.ls1.next
    when($.next =:= Special.Null):
      store($.ls2) in $.ls1.next
    .otherwise:
      call_rec($.next, $.ls2)