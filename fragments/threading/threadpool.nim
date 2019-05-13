import locks, cpuinfo, random, concurrency/atomics
import threading_primitives, collections, ../memory

when not defined nimV2:
  {.error: "Legacy run-time is not supported".}

var 
  MaxThreadCount = countProcessors() + 2

type
  ThreadPoolWorker = ref object
    localWorkItems: WorkStealingQueue
    threadPool: ThreadPool
    thread: Thread[ThreadPoolWorker]

  ThreadPool* = ref object
    globalWorkItems: WorkStealingQueue
    workers: seq[owned ThreadPoolWorker]
    spinLock: SpinLock
    workAvailable: Semaphore
    workerCount: Atomic[int]

proc newThreadPool*(): owned ThreadPool =
  new result
  result.globalWorkItems.init()
  result.workers.setLen(MaxThreadCount)

var
  threadPool = newThreadPool()
  threadPoolWorker {.threadvar.}: ThreadPoolWorker

proc processWorkItems(self: ThreadPoolWorker) {.thread.}

proc newThreadPoolWorker*(): owned ThreadPoolWorker =
  new result
  result.localWorkItems.init()
  result.threadPool = threadPool
  createThread(result.thread, processWorkItems, result)

proc requestWorker(self: ThreadPool) =

  self.workAvailable.signal()

  # If the pool is not full, create a new worker
  while true:
    var count = self.workerCount.load(moRelaxed)
    if count >= MaxThreadCount:
      break

    if self.workerCount.compareExchangeWeak(count, count + 1, moRelaxed):
      self.workers[count] = newThreadPoolWorker()
      break      

proc markRequestSatisfied(self: ThreadPool) =
  discard
  # TODO: Decrease semaphore counter

proc processWorkItems(self: ThreadPoolWorker) {.thread.} =

  threadPoolWorker = self

  {.gcsafe.}:
    while true:
      var spinWait: SpinWait

      while true:
        threadPool.markRequestSatisfied()

        # Try to get work from this threads local queue
        var workItem = self.localWorkItems.dequeue()

        # If there is no local work, try to get some from the global queue
        var missedSteal = false
        if workItem == nil:
          (workItem, missedSteal) = threadPool.globalWorkItems.steal()
          
        # If there is no global work either, try to steal from a random worker
        if workItem == nil:
          let
            count = threadPool.workerCount.load(moRelaxed)
            offset = rand(count - 1)

          for i in 0 ..< count:
            let worker = threadPool.workers[(offset + i) mod count]
            if worker == nil or worker == self:
              continue

            # TODO: check for *any* missed steal, instead of the last?
            (workItem, missedSteal) = worker.localWorkItems.steal()
            if workItem != nil:
              break

        if workItem == nil:
          # If we missed a steal, there might be more work, so try again
          if missedSteal:
            break

          # Wait until more work arrives
          if spinWait.isNextSpinYield:
            threadPool.workAvailable.waitOne()
            break

          # Wait a little, in case more work arrives
          else:
            spinWait.spinOnce()

        else:
          # If we found work, there might be more, so request another worker
          threadPool.requestWorker()
          workItem()
          break

proc queueWorkItem*(self: var ThreadPool; workItem: proc()) =

  if threadPoolWorker != nil:
    threadPoolWorker.localWorkItems.enqueue(workItem)
  else:
    self.globalWorkItems.enqueue(workItem)

  self.requestWorker()

proc queueWorkItem*(workItem: proc()) =
  threadPool.queueWorkItem(workItem)

when isMainModule:

  import os, times, math

  proc main() =

    let startTime = epochTime()
    let maxLevel = 3
    let childCount = 10
    var count = new Atomic[int]

    proc createWorkRecursive(level: int) =
      if level > 0:
        for i in 0 ..< childCount:
          closureScope:
            let n = i
            queueWorkItem do () -> void:
              createWorkRecursive(level - 1)
              # let startTime = epochTime()
              # while epochTime() - startTime < 0.02:
              #   discard
              # echo $level, " ", $n, " " & $getThreadId()

      discard count[].fetchAdd(1, moRelaxed)
      sleep(20)

    createWorkRecursive(maxLevel)

    let totalCount = (1 - childCount ^ (maxLevel + 1)) div (1 - childCount)
    while not (count[].load(moRelaxed) == totalCount):
      sleep(10)

    echo epochTime() - startTime

  main()