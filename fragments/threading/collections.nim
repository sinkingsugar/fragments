import std/locks, concurrency/atomics
import threading_primitives

# type
#   Segment[T] = object
#     items: seq[T]
#     low: int
#     high: int
#     count: int
#     next: ref Segment[T]

#   ConcurrentQueue*[T] = object
#     resizeLock: Lock
#     head: ref Segment[T]
#     tail: ref Segment[T]

# proc init[T](self: var ConcurrentQueue[T]) =
#   initLock(self.resizeLock)

# proc newConcurrentQueue*[T](): ref ConcurrentQueue[T] =
#   new(result)
#   init(result)

# proc enqueue*[T](self: ConcurrentQueue[T]; item: T) =

#   while true:
#     let localTail = self.tail
#     let count = localTail.count

#     # If the segment was full, allocate and append a new, bigger one.
#     if count == localTail.items.len:
#       withLock(resizeLock):
#         if tail.next == nil and count == tail.items.len:
#           tail.next = new Segment[T]
#           tail = tail.next
#           tail.items = newSeq(count shl 1)

#     elif cas(localTail.count, count, count + 1):

#       # If there was space for another item and we were able to reserve it, move the
#       # write index forward and get the index of the slot we can write into.
#       let localHigh = atomicInc(localTail.high) - 1

#       # Modulo items.len to calculate the actual index.
#       let mask = localTail.items.len - 1
#       let index = localHigh and mask

#       # Write the item. Spin until the slot has been cleared by pending calls to Acquire.
#       var spinWait: SpinWait
#       while localTail.items[index] != nil: # TODO: Do we need atomic exchange here?
#         spinWait.spinOnce()

#       localTail.items[index] = item
#       return

# proc tryDequeue*[T](self: ConcurrentQueue[T]; value: var T): bool =

#   while true:
#     let localHead = self.head
#     let count = localHead.count

#     if count == 0:

#       # If the first segment is empty, but there is at least one other, move the head forward.
#       if localHead.next != nil:
#         withLock(resizeLock):
#           if head.next != nil and head.count == 0:
#             head = head.next

#       # If there was only one segment and it was empty, there are no items
#       else:
#         return false

#     elif cas(localHead.count, count, count - 1):

#       # If there were any items and we could reserve one of them, move the
#       # read index forward and get the index of the item we can acquire.
#       let localLow = atomicInc(localHead.low) - 1

#       # Modulo items.len to calculate the actual index.
#       let mask = localHead.items.len - 1
#       let index = localLow and mask

#       # Take the item. Spin until the slot has been written by pending calls to Release.
#       var spinWait: SpinWait
#       while localHead.items[index] == nil: # TODO: Do we need atomic exchange here?
#         spinWait.spinOnce()

#       localHead.items[index] = nil
#       return true

type
  WorkStealingQueue* = object
    items: seq[proc()]
    head, tail: Atomic[int]
    foreignLock: SpinLock

proc init*(self: var WorkStealingQueue) =
  newSeq(self.items, 1 shl 2)

proc enqueue*(self: var WorkStealingQueue; item: proc()) =

  var currentTail = self.tail.load(moRelaxed)
  var mask = self.items.len - 1 # items.len is a power of 2, so `and mask` is equivalent to `mod items.len`

  # When the tail overflows, remap head and tail to the actual range
  if currentTail == int.high:
    withLock(self.foreignLock):

      if self.tail.load(moRelaxed) == int.high:
        self.head.store(self.head.load(moRelaxed) and mask, moRelaxed)
        currentTail = self.tail.load(moRelaxed) and mask
        self.tail.store(currentTail, moRelaxed)

  # If there is space (at least 2 elements worth), append.
  # This is done only by the owning thread and doesn't need to be synchronized
  if currentTail < self.head.load(moRelaxed) + mask:
    self.items[currentTail and mask] = item
    self.tail.store(currentTail + 1, moRelaxed)
    
  # Otherwise resize first.
  else:
    withLock(self.foreignLock):

      var currentHead = self.head.load(moRelaxed)
      let count = self.tail.load(moRelaxed) - currentHead

      if count >= mask:
        # Reallocate the array and make items contiguous
        let items = self.items
        self.items = newSeq[proc()](items.len shl 1)
        for i in 0 ..< count:
          self.items[i] = items[(currentHead + i) and mask]

        # Reset head and tail
        mask = self.items.len - 1
        currentTail = count
        self.head.store(0, moRelaxed)
        self.tail.store(currentTail, moRelaxed)

      fence(moRelease)
      self.items[currentTail and mask] = item
      self.tail.store(currentTail + 1, moRelaxed)

proc dequeue*(self: var WorkStealingQueue): proc() =

  while true:

    let mask = self.items.len - 1
    var localTail = self.tail.load(moRelaxed)

    # Queue is empty
    if self.head.load(moRelaxed) >= localTail:
      return nil

    # Reserve the tail
    localTail = self.tail.fetchSub(1, moRelaxed) - 1

    # If there is still some item left, there was no
    # interaction with a steal, so take the fast path
    if self.head.load(moRelaxed) <= localTail:
      let index = self.tail.load(moRelaxed) and mask
      let item = self.items[index]
      fence(moAcquire)

      if item == nil:
        continue

      self.items[index] = nil
      return item

    # Othersise synchronize with stealing threads
    else:
      withLock(self.foreignLock):

        if self.head.load(moRelaxed) <= localTail:
          let index = localTail and mask
          let item = self.items[index]
          fence(moAcquire)

          if (item == nil):
            continue

          self.items[index] = nil
          return item

        # The element was stolen. Restore the tail
        else:
          self.tail.store(localTail + 1, moRelaxed)
          return nil

proc steal*(self: var WorkStealingQueue): tuple[item: proc(); missedSteal: bool] =

  while true:
    # Queue is empty
    if self.head.load(moRelaxed) >= self.tail.load(moRelaxed):
      return

    # TODO: timeout and missed steal
    # Synchronize with resizing and dequeuing by the owning thread
    withLock(self.foreignLock):

      # Reserve the head
      let localHead = self.head.fetchAdd(1, moRelaxed) - 1

      if localHead < self.tail.load(moRelaxed):
        let mask = self.items.len - 1
        let index = localHead and mask
        let item = self.items[index]
        fence(moAcquire)

        if item == nil:
          continue

        self.items[index] = nil
        result.item = item
        return

      # We missed the steal. Restore the head
      else:
        self.head.store(localHead, moRelaxed)
        result.missedSteal = true

      return

when isMainModule:

  var
    queue: WorkStealingQueue
    items = newSeq[proc()]()

  for i in 0 ..< 10:
    closureScope:
      let local = i
      items.add(proc() = echo local)

  queue.init()
  for item in items:
    queue.enqueue(item)

  while true:
    let item = queue.dequeue()
    if item == nil:
      break
    item()

    let (item2, missedSteal) = queue.steal()
    if item2 == nil:
      break
    item2()    
