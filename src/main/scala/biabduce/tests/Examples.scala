package biabduce.tests

import biabduce.Special
import biabduce.dsl.*
import biabduce.dsl.symToVar


val sequenceOfPrgs = program("sequence")("p", "q"):
  call("listLength")($.p)
  call("listLength")($.q)
  call("appendList")($.p, $.q)


val listLength = program("listLength")("p"):
  when($.p =:= Special.Null):
    ()
  .otherwise:
    $.n <-- $.p.next
    $.v <-- $.p.value
    call_rec($.n)