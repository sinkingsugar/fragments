import asyncjs, deques

type
  AsyncSemaphore* = ref object
    resolvers: Deque[proc()]
    currentCount: int

  AsyncLock* = ref object
    semaphore: AsyncSemaphore

proc newAsyncSemaphore*(initialCount: int = 0): AsyncSemaphore =
  new(result)
  result.resolvers = initDeque[proc()]()
  result.currentCount = initialCount

proc waitAsync*(self: AsyncSemaphore): Future[void] {.async.} =
  #withLock lock:
    if self.currentCount > 0:
      dec(self.currentCount)
    else:
      await newPromise do (resolve: proc()) -> void:
        self.resolvers.addLast(resolve)

proc signal*(self: AsyncSemaphore) =
  var resolver: proc() = nil
  block: #withLock lock:
    if self.resolvers.len > 0:
      resolver = self.resolvers.popFirst()
    else:
      inc(self.currentCount)
  if resolver != nil:
    resolver()
  
proc newAsyncLock*(initialCount: int = 0): AsyncLock =
  new(result)
  result.semaphore = newAsyncSemaphore(1)

proc lock*(self: AsyncLock): Future[void] {.async, inline.} =
  await self.semaphore.waitAsync()

proc release*(self: AsyncLock) {.inline.} = 
  self.semaphore.signal()

template withLock*(self: AsyncLock; body: untyped): untyped =
  await self.lock()
  # try:
  #   body
  # finally:
  #   self.release()
  defer: self.release()
  body