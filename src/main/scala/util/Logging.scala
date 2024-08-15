package util

import wvlet.log.{LogFormatter, LogLevel, Logger}

def initLogger(): Logger =
  Logger.setDefaultLogLevel(LogLevel.ALL)
  val logger = Logger("SepGen")
  logger.setFormatter(LogFormatter.AppLogFormatter)
  logger

inline def info(inline message: Any)(using logger: Logger): Unit = logger.info(message)