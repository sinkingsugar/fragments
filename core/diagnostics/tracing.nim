import times, streams, strutils, macros, locks

type
  Logger = ref LoggerObj

  LoggerObj = object
    buffer: seq[byte]
    flushLock: Lock

  Event = object
    text*: string

  EventInfo = object
    time: float

  Severity* = enum
    Critical
    Error
    Warning
    Information
    Verbose
    Debug

var
  globalBuffer: seq[byte]
  globalLock: Lock
  currentLogger {.threadvar.}: Logger
  loggers: seq[Logger]

const
  logThreshold = 1000

initLock(globalLock)

proc getCurrentLogger(): Logger =
  if currentLogger == nil:
    new(currentLogger)
    initLock(currentLogger.flushLock)
    loggers.add(currentLogger)

  return currentLogger

proc flushInternal(logger: Logger) =
  withLock logger.flushLock:
    globalBuffer.add(logger.buffer)
    logger.buffer.setLen(0)

proc flush(logger: Logger) =
  withLock globalLock:
    logger.flushInternal()
  
proc flushLoggers() =
  withLock globalLock:
    for logger in loggers.mitems:
      logger.flushInternal()

proc log(logger: Logger; event: Event; time: float) =
  echo event.text

  if logger.buffer.len > logThreshold:
    logger.flush()

proc initEvent(formatString: string): Event =
  result.text = formatString

template log*(formatString: string; args: varargs[untyped]): untyped =
  var event {.global.}: Event = initEvent(formatString)
  getCurrentLogger().log(event, epochTime())

proc beginEvent*() =
  discard

proc endEvent*() =
  discard

template traced() {.pragma.}

template traced(head: untyped; body: untyped): untyped =
  beginEvent()
  try:
    body
  except:
    let exception = getCurrentException()
    # flush?
    raise
  finally:
    endEvent()

when isMainModule:
  type Info = object
    data: int

  for i in 0..2:

    traced "Some scoped message $#":

      log("Some message $#", 5.0)
      log("Some other message $# $#", a: 4, b: 1.5)
      log("Some compilcated message $# $#", Info(data: 3))
        
      let x {.traced.} = getTime()

      # debug(...)
      # verbose(...)
      # info(...)
      # warning(...)
      # error(...)
      # fatal(...)