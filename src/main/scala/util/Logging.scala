package util

import wvlet.log.{LogFormatter, LogLevel, Logger}

given globalLogger: Logger = initLogger()

def initLogger(): Logger =
  val logger = Logger("SepGen")
  logger.setFormatter(LogFormatter.AppLogFormatter)
  logger

inline def info(inline message: Any)(using logger: Logger): Unit = logger.info(message)