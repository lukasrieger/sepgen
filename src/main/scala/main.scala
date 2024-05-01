import wvlet.log.Logger

val logger = Logger("SepGen")

@main
def main(): Unit =
  infer(Examples.listLength)
    .pre: p =>
      logger.info("List length Pre:")
      logger.info(s"  $p")
    .post: q =>
      logger.info("List length Post:")
      logger.info(s"  $q")

  infer(Examples.listSum)
    .pre: p =>
      logger.info("List sum Pre:")
      logger.info(s"  $p")
    .post: q =>
      logger.info("List sum Post:")
      logger.info(s"  $q")

  infer(Examples.listReverse)
    .pre: p =>
      logger.info("Reverse Pre:")
      logger.info(s"  $p")
    .post: q =>
      logger.info("Reverse Post:")
      logger.info(s"  $q")

  infer(Examples.dll_to_bst)
    .pre: p =>
      logger.info("DLL to BST Pre:")
      logger.info(s"  $p")
    .post: q =>
      logger.info("DLL to BST Post:")
      logger.info(s"  $q")
