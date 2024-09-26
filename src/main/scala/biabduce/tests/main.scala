package biabduce.tests

import biabduce.SymExec2.symExeProc_

import scala.collection.mutable
import scala.collection.mutable.Map as MutableMap
import biabduce.{ProcTable, SpecTable, symExeProc, show}
import pure.Name


//def main(): Unit =
//  val procTable: ProcTable = mutable.Map(
//    listLength.name -> listLength,
//    sequenceOfPrgs.name -> sequenceOfPrgs,
//    listSum.name -> listSum,
//    appendList.name -> appendList
//  )
//  val specTable: SpecTable = MutableMap.empty
//
//  val specs = symExeProc(sequenceOfPrgs)(using specTable, procTable)
//
//  specTable.foreach: (name, entry) =>
//    println(s"SPECTABLE ENTRY OF $name")
//    println(entry)
//
//  specs.foreach: spec =>
//    val pre = spec.pre
//    val post = spec.post
//    print("PRE")
//    print(pre)
//    print("POST")
//    print(post)


def main(): Unit =
  val procTable: ProcTable = mutable.Map(
    listLength.name -> listLength,
    sequenceOfPrgs.name -> sequenceOfPrgs,
    listSum.name -> listSum,
    appendList.name -> appendList
  )

  val specTable: SpecTable = MutableMap.empty
  val result = symExeProc_(sequenceOfPrgs, Name("Q"))(using specTable, procTable)

  val symRes = result.map(_.existQuantify)

  println("-----------------------------")
  symRes.map(r => s"${r.show}").foreach(println)
  println("-----------------------------")