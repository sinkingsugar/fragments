import deques

when defined(js):
  import asyncjs
  type Resolver = proc()
else:
  import asyncdispatch, threading_primitives
  type Resolver = Future[void]  

type
  AsyncSemaphore* = object
    resolvers: Deque[Resolver]
    currentCount: int
    when not defined(js):
      lock: SpinLock

  AsyncLock* = object
    semaphore: AsyncSemaphore

proc init*(self: var AsyncSemaphore; initialCount: int = 0) =
  self.resolvers = initDeque[Resolver]()
  self.currentCount = initialCount

when defined(js):
  proc waitAsync*(self: var AsyncSemaphore): Future[void] {.async.} =
    if self.currentCount > 0:
      dec self.currentCount
    else:
      await newPromise do (resolve: proc()) -> void:
        self.resolvers.addLast(resolve)
else:
  proc waitAsync*(self: var AsyncSemaphore): Future[void] =
    withLock self.lock:
      if self.currentCount > 0:
        dec self.currentCount
      else:
        result = newFuture[void]()
        self.resolvers.addLast(result)

when defined(js):
  proc signal*(self: var AsyncSemaphore) =
    if self.resolvers.len > 0:
      let resolver = self.resolvers.popFirst()
      resolver()
    else:
      inc(self.currentCount)
else:
  proc signal*(self: var AsyncSemaphore) =
    var resolver: Resolver = nil
    withLock self.lock:
      if self.resolvers.len > 0:
        resolver = self.resolvers.popFirst()
      else:
        inc(self.currentCount)
    resolver.complete()
  
proc init*(self: var AsyncLock) =
  self.semaphore.init(1)

proc lock*(self: var AsyncLock): Future[void] {.inline.} =
  self.semaphore.waitAsync()

proc release*(self: var AsyncLock) {.inline.} = 
  self.semaphore.signal()

when defined(js):
  template withLock*(self: var AsyncLock; body: untyped): untyped {.dirty.} =
    await self.lock()
    defer: self.release()
    body
else:
  template withLock*(self: var AsyncLock; body: untyped): untyped {.dirty.} =
    yield self.lock()
    try:
      body
    finally:
      self.release()