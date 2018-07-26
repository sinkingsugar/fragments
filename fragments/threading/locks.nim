import cpuinfo, std/locks
import atomics

const
  maxSpinCount = 10

let
  allowBusyWaiting = countProcessors() > 1

type
  SpinWait* = object
    count: int

  SpinLock* = object
    state: AtomicFlag

  ManualResetEvent* = object
    isSet: bool
    lock: Lock
    condition: Cond

  AutoResetEvent* = object
    isSet: bool
    lock: Lock
    condition: Cond

  Event = ManualResetEvent | AutoResetEvent

proc isNextSpinYield*(self: SpinWait): bool {.inline.} =
  return self.count >= maxSpinCount or not allowBusyWaiting

proc spinOnce*(self: var SpinWait) {.inline.} =
  if self.isNextSpinYield:
    cpuRelax()
  inc(self.count)

proc enter*(self: var SpinLock): bool {.inline.} =
  var wait: SpinWait
  while self.state.testAndSet():
    wait.spinOnce()
  return true

proc exit*(self: var SpinLock) {.inline.} =
  self.state.clear()

template withLock*(self: var SpinLock; body: untyped): untyped =
  let isLockTaken = self.enter()
  try:
    body
  finally:
    if isLockTaken:
      self.exit()

proc init*(self: var Event) =
  initLock(self.lock)
  initCond(self.condition)

# Cannot use Event generic
proc `=destroy`*(self: var ManualResetEvent) =
  deinitLock(self.lock)
  deinitCond(self.condition)

# Cannot use Event generic
proc `=destroy`*(self: var AutoResetEvent) =
  deinitLock(self.lock)
  deinitCond(self.condition)

proc signal*(self: var Event) =
  withLock(self.lock): 
    self.isSet = true
  self.condition.signal()

proc reset*(self: var Event) =
  withLock(self.lock):
    self.isSet = false

proc waitOne*(self: var Event) =
  withLock(self.lock):
    while not self.isSet:
      self.condition.wait(self.lock)
    when Event is AutoResetEvent:
      self.isSet = false

when isMainModule:
  import threadpool

  var manualEvent: ManualResetEvent
  manualEvent.init()

  proc myManualThread() =
    echo "Entering wait"
    manualEvent.waitOne()
    echo "Exited wait"
    assert manualEvent.isSet == true

  spawn myManualThread()
  echo "Signaling thread"
  manualEvent.signal()

  sync()

  var autoEvent: AutoResetEvent
  autoEvent.init()

  proc myAutoThread() =
    echo "Entering wait"
    autoEvent.waitOne()
    echo "Exited wait"
    assert autoEvent.isSet == false

  spawn myAutoThread()
  echo "Signaling thread"
  autoEvent.signal()

  sync()
