import cpuinfo
import std/locks

const
  maxSpinCount = 10

let
  allowBusyWaiting = countProcessors() > 1

type
  SpinWait* = object
    count: int

  SpinLock* = object
    state: bool

  ManualResetEvent* = object
    isSet: bool
    lock: Lock
    condition: Cond

proc isNextSpinYield*(self: SpinWait): bool {.inline.} =
  return self.count >= maxSpinCount or not allowBusyWaiting

proc spinOnce*(self: var SpinWait) {.inline.} =
  if self.isNextSpinYield:
    cpuRelax()
  inc(self.count)

proc enter*(self: var SpinLock): bool {.inline.} =
  var wait: SpinWait
  while not cas(addr(self.state), false, true):
    wait.spinOnce()
  return true

proc exit*(self: var SpinLock) {.inline.} =
  self.state = false

template withLock*(self: var SpinLock; body: untyped): untyped =
  let isLockTaken = self.enter()
  try:
    body
  finally:
    if isLockTaken:
      self.exit()

proc set*(self: var ManualResetEvent) =
  withLock(self.lock): 
    self.condition.signal()

proc reset*(self: var ManualResetEvent) =
  withLock(self.lock):
    self.isSet = false

proc waitOne*(self: var ManualResetEvent) =
  withLock(self.lock):
    while not self.isSet:
      self.condition.wait(self.lock)
    #self.isSet = false