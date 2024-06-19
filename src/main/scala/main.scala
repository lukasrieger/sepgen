import util.{info, initLogger}
import wvlet.log.Logger
import pure.tapPre
import pure.tapPost

given globalLogger: Logger = initLogger()

@main
def main(): Unit =

  Examples.all.foreach: proc =>
    infer(proc)
      .tapPre: p =>
        info(s"${proc.signature.name} :")
        info(s"  $p")
      .tapPost: q =>
        info(s"  $q")
