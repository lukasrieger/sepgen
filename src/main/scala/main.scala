import util.{info, initLogger}
import wvlet.log.Logger
import pure.{collectSymbolicReferences, tapPost, tapPre}

given globalLogger: Logger = initLogger()

@main
def main(): Unit =
//  val (pre, post) = infer(Examples.testHead)
//  val preT = pre.abstractRepr()
//
//  println(preT)

  Examples.all.foreach: proc =>
    infer(proc)
      .tapPre: p =>
        info(s"${proc.signature.name} :")
        info(s"  $p")
      .tapPost: q =>
        info(s"  $q")
      .tapPre: p =>
        info(s"  With abstract repr:")
        info(s"    ${p.toAbstractRepr}")
      .tapPost: q =>
        info(s"    ${q.toAbstractRepr}")

