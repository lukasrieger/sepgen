package biabduce.tests

import biabduce.SymExec2.symExeProc_

import scala.collection.mutable
import scala.collection.mutable.Map as MutableMap
import biabduce.{ProcTable, SpecTable, show, symExe3Top, symExeProc}
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
    sequence1.name -> sequence1,
    listSum.name -> listSum,
    appendList.name -> appendList,
    incr.name -> incr
  )

  val specTable: SpecTable = MutableMap.empty
  val result = symExe3Top(sequence1, Name("Q"))(using specTable, procTable)

  val symRes = result.map(_.existQuantify)

  println("-----------------------------")
  symRes.map(r => s"${r.show}").foreach(println)
  println("-----------------------------")

  println("SPEC TABLE:")
  specTable.foreach(r => println(r))


  println("------------- SEQUENCE 2 ------------")

  val procTable2: ProcTable = mutable.Map(
    listLength.name -> listLength,
    sequence2.name -> sequence2,
    listSum.name -> listSum,
    appendList.name -> appendList,
    incr.name -> incr
  )

  val specTable2: SpecTable = MutableMap.empty
  val result2 = symExe3Top(sequence2, Name("Q"))(using specTable2, procTable2)

  val symRes2 = result2.map(_.existQuantify)

  println("-----------------------------")
  symRes2.map(r => s"${r.show}").foreach(println)
  println("-----------------------------")

  println("SPEC TABLE:")
  specTable2.foreach(r => println(r))