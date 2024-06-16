import util.{info, initLogger}
import wvlet.log.Logger

given globalLogger: Logger = initLogger()

@main
def main(): Unit =

  Examples.all.foreach: proc =>
    infer(proc)
      .pre: p =>
        info(s"${proc.signature.name} Pre:")
        info(s"  $p")
      .post: q =>
        info(s"${proc.signature.name} Post:")
        info(s"  $q")


// 