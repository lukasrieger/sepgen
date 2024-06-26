import util.{info, initLogger}
import wvlet.log.Logger
import pure.{findSymolicRefs, tapPost, tapPre}

given globalLogger: Logger = initLogger()

@main
def main(): Unit =
  
  val x = infer(Examples.testHead)
  println(findSymolicRefs(x._1.body))

  Examples.all.foreach: proc =>
    infer(proc)
      .tapPre: p =>
        info(s"${proc.signature.name} :")
        info(s"  $p")
      .tapPost: q =>
        info(s"  $q")
