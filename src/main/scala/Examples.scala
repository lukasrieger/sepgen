

object Examples {
  
  import pure.*
  import pure.PrgDsl.*
  import pure.Conversions.given_Conversion_Int_Lit
  
  
  val listSum = program {
    when(v("p") eq 0) {
      returns (0)
    } otherwise {
      load("x".v, "p".v |-> "value")
      load("n".v, "p".v |-> "next")
      call("rec", List("n".v), "y".v)
      returns("y".v + "x".v)
    }
  }
    
  val listLength = program {
    when("p".v eq 0) {
      returns (0)
    } otherwise {
      load("n".v, v("p") |-> "next")
      call("rec", List(v("n")), v("y"))
      returns (v("y") + 1)
    }
  }


  val listReverse: Program = program {
    load(v("next"), v("curr") |-> "next")
    when(v("next") eq 0) {
      returns (v("curr"))
    } otherwise {
      store ("prev".v) in ("curr".v |-> "next")
      call("rec", List(v("curr"), v("next")), v("head"))
      returns(v("head"))
    }
  }
}
