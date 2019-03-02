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
    workAvailable: ManualResetEvent
    spinLock: SpinLock
    workerRequestCount: Atomic[int]

proc init*(self: var ThreadPool) =
  self.globalWorkItems.init()

var
  threadPool: ThreadPool
  threadPoolWorker {.threadvar.}: ptr ThreadPoolWorker

threadPool.init()

proc init*(self: var ThreadPoolWorker) =
  self.localWorkItems.init()
  self.threadPool = addr threadPool

proc processWorkItems(self: ptr ThreadPoolWorker) {.thread.}

proc requestWorker(self: var ThreadPool) =
  var count = self.workerRequestCount.load(moRelaxed)

  while count < MaxThreadCount:
    if self.workerRequestCount.compareExchangeWeak(count, count + 1, moRelaxed):

      # TODO: Lock this
      if self.workers.len < MaxThreadCount:
        let worker = alloc0 ThreadPoolWorker
        worker[].init()
        self.workers.add(worker)
        createThread(worker[].thread, processWorkItems, worker)

      else:
        self.workAvailable.signal()

      break

    count = self.workerRequestCount.load(moRelaxed)


proc markRequestSatisfied(self: var ThreadPool) =

  var count = self.workerRequestCount.load(moRelaxed)
  while count > 0 and not self.workerRequestCount.compareExchangeWeak(count, count - 1, moRelaxed):
    count = self.workerRequestCount.load(moRelaxed)

proc waitForWork(self: var ThreadPool) =

  self.workAvailable.waitOne()

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
            count = threadPool.workers.len
            offset = rand(count - 1)
          for i in 0 ..< count:
            # TODO: check for *any* missed steal, instead of the last?
            (workItem, missedSteal) = threadPool.workers[(offset + i) mod count].localWorkItems.steal()
            if workItem != nil:
              break

        if workItem == nil:
          # If we missed a steal, there might be more work, so try again
          if missedSteal:
            break

          # Wait until more work arrives
          if spinWait.isNextSpinYield:
            threadPool.waitForWork()
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

  import os
  import times

  proc main() =

    let startTime = epochTime()
    var count = new Atomic[int]

    proc createWorkRecursive(level: int) =
      if level > 0:
        for i in 0 ..< 10:
          closureScope:
            let n = i
            queueWorkItem do () -> void:
              createWorkRecursive(level - 1)
              # let startTime = epochTime()
              # while epochTime() - startTime < 0.02:
              #   discard
              sleep(20)
              discard count[].fetchAdd(1, moRelaxed)
              #echo $level, " ", $n, " " & $getThreadId()

    createWorkRecursive(3)

    # for i in 0 ..< 10:
    #   closureScope:
    #     let n = i
    #     queueWorkItem do () -> void:
    #       echo $n & " " & $getThreadId()
    #       sleep(100)

    while not (count[].load(moRelaxed) == 1110):
      sleep(10)
    #echo count[].load(moRelaxed)
    echo epochTime() - startTime

  main()