import wvlet.log.Logger

val logger = Logger("SepGen")

@main
def main(): Unit = 
  Examples.all.foreach: proc =>
    infer(proc)
      .pre: p =>
        logger.info(s"${proc.signature.name} Pre:")
        logger.info(s"  $p")
      .post: q =>
        logger.info(s"${proc.signature.name} Post:")
        logger.info(s"  $q")
