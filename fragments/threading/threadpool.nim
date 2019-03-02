import locks, cpuinfo, random, concurrency/atomics
import threading_primitives, collections, ../memory

var 
  MaxThreadCount = countProcessors() + 2

type
  ThreadPoolWorker = object
    localWorkItems: WorkStealingQueue
    threadPool: ptr ThreadPool
    thread: Thread[ptr ThreadPoolWorker]

  ThreadPool* = object
    globalWorkItems: WorkStealingQueue
    workers: seq[ptr ThreadPoolWorker]
    spinLock: SpinLock
    workAvailable: Semaphore
    workerCount: Atomic[int]

proc init*(self: var ThreadPool) =
  self.globalWorkItems.init()
  self.workers.setLen(MaxThreadCount)

var
  threadPool: ThreadPool
  threadPoolWorker {.threadvar.}: ptr ThreadPoolWorker

threadPool.init()

proc processWorkItems(self: ptr ThreadPoolWorker) {.thread.}

proc init*(self: var ThreadPoolWorker) =
  self.localWorkItems.init()
  self.threadPool = addr threadPool
  createThread(self.thread, processWorkItems, addr self)

proc requestWorker(self: var ThreadPool) =

  self.workAvailable.signal()

  # If the pool is not full, create a new worker
  while true:
    var count = self.workerCount.load(moRelaxed)
    if count >= MaxThreadCount:
      break

    if self.workerCount.compareExchangeWeak(count, count + 1, moRelaxed):
      let worker = alloc0 ThreadPoolWorker
      worker[].init()
      self.workers[count] = worker
      break      

proc markRequestSatisfied(self: var ThreadPool) =
  discard
  # TODO: Decrease semaphore counter

proc processWorkItems(self: ptr ThreadPoolWorker) {.thread.} =

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
            if worker == nil:
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