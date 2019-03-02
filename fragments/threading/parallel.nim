import cpuinfo, locks, algorithm, concurrency/atomics
import threadpool, threading_primitives

let MaxDegreeOfParallelism = countProcessors()

type
  State = object
    startInclusive: Atomic[int]
    activeWorkerCount: Atomic[int]
    finished: ManualResetEvent

  SortRange = tuple
    left: int
    right: int

  SortState = object
    partitions: seq[SortRange]
    activeWorkerCount: Atomic[int]
    finished: ManualResetEvent

proc executeBatch(fromInclusive, toExclusive: int; action: proc(value: int)) =
# try:
#   initialize()
  for i in fromInclusive ..< toExclusive:
    action(i)
# finally:
#   finalize()

proc fork(endExclusive, batchSize, maxDegreeOfParallelism: int; action: proc(value: int), state: ptr State) =

  # Reserve one batch to process on the current thread
  var start = state.startInclusive.fetchAdd(batchSize, moRelaxed)

  # Other threads already processed all work before this one started. activeWorkerCount is already 0
  if start >= endExclusive:
    return

  #  This thread is now actively processing work items, meaning there might be work in progress
  discard state.activeWorkerCount.fetchAdd(1, moRelaxed)

  # Kick off another worker if there's any work left
  if maxDegreeOfParallelism > 1 and start + batchSize < endExclusive:
    queueWorkItem do ():
      fork(endExclusive, batchSize, maxDegreeOfParallelism - 1, action, state)

  try:
    # Process batches synchronously as long as there are any
    while start < endExclusive:
      executeBatch(start, min(endExclusive, start + batchSize), action)
      start = state.startInclusive.fetchAdd(batchSize, moRelaxed)

  finally:
    # If this was the last batch, signal
    if (state.activeWorkerCount.fetchSub(1, moRelaxed) == 1):
      state.finished.signal()


proc parallelFor(fromInclusive, toExclusive: int; action: proc(value: int)) =

  let count = toExclusive - fromInclusive
  let batchCount = min(MaxDegreeOfParallelism, count)

  var state: State
  state.startInclusive.store(fromInclusive, moRelaxed)

  if MaxDegreeOfParallelism <= 1 or batchCount == 1:
    executeBatch(fromInclusive, toExclusive, action)

  else:
    let batchSize = (count + (batchCount - 1)) div batchCount

    # Kick off a worker, then perform work synchronously
    fork(toExclusive, batchSize, MaxDegreeOfParallelism, action, addr(state))

    # Wait for all workers to finish
    if state.activeWorkerCount.load(moRelaxed) != 0:
      state.finished.waitOne()

proc parallelFor[T](collection: openArray[T]; action: proc(item: T)) {.inline.} =
  parallelFor(0, collection.len, proc(index: int) = action(collection[index]))

# macro parallelFor*(head: untyped; ; body: untyped): untyped =
#   return quote do:
#     #var loopInfo {.global.}: LoopInfo
#     prallelFor(fromInclusive, toExclusive)

proc parallelSort[T](collection: var openArray[T]; maxDegreeOfParallelism: int, state: ptr SortState)

proc swap[T](collection: var openArray[T], left, right: int) {.inline.} =
  let temp = collection[left];
  collection[left] = collection[right]
  collection[right] = temp

proc partition[T](collection: var openArray[T]; left, right: int): int =

  var
    i = left
    j = right
    mid = (left + right) div 2

  if collection[right] < collection[left]: swap(collection, left, right)
  if collection[mid] < collection[left]: swap(collection, left, mid)
  if collection[right] < collection[mid]: swap(collection, mid, right)

  while i <= j:
    let pivot = collection[mid]
    while collection[i] < pivot: inc(i)
    while collection[j] > pivot: dec(j)
    
    if i <= j:
      inc(i)
      dec(j)
      swap(collection, i, j)

  return mid

proc parallelSort[T](collection: var openArray[T]; maxDegreeOfParallelism: int, state: ptr SortState) =

  const sequentialThreshold = 2048;

  # Other threads already processed all work before this one started. ActiveWorkerCount is already 0
  if state.partitions.len == 0:
    return

  var hasChild = false

  try:
    var partition: SortRange
    while state.partitions.tryDequeue(partition):

      if partition.right - partition.left < sequentialThreshold:
        sort(collection[partition.left .. partition.right])

      else:
        let pivot = partition(collection, partition.left, partition.right)

        #  Add work items
        if (pivot - 1 > partition.left):
          state.partitions.enqueue((partition.left, pivot - 1))

        if (partition.right > pivot + 1):
          state.partitions.enqueue((pivot + 1, partition.right))

        # Add a new worker if necessary
        if maxDegreeOfParallelism > 1 and not hasChild:
          queueWorkItem(proc() = parallelSort(collection, maxDegreeOfParallelism - 1, state))
          hasChild = true

  finally:
    # If this was the last batch, signal
    if (state.activeWorkerCount.fetchSub(1, moRelaxed) == 1):
      state.finished.set()

  # This thread is now actively processing work items, meaning there might be work in progress
  state.activeWorkerCount.fetchAdd(1, moRelaxed)

proc parallelSort[T](collection: var openArray[T]) =

  if collection.len == 0:
    return

  var state: SortState

  # Initial partition
  state.partitions.enqueue((0, collection.len - 1))

  # Sort recursively
  parallelSort(collection, MaxDegreeOfParallelism, state)

  # Wait for all work to finish
  if (state.activeWorkerCount.load(moRelaxed) != 0):
    state.finished.waitOne()

when isMainModule:

  import os
  import times

  let startTime = epochTime()
  
  parallelFor(0, 100) do (i: int) -> void:
    sleep(10)
    echo $i, " ", $getThreadId()

  echo epochTime() - startTime
