# Originally:
#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Rokas Kupstys
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#
## Nim coroutines implementation supports several context switching methods:
## ucontext: available on unix and alike (default)
## setjmp:   available on unix and alike (x86/64 only)
## Fibers:   available and required on windows.
##
## -d:nimCoroutines              Required to build this module.
## -d:nimCoroutinesUcontext      Use ucontext backend.
## -d:nimCoroutinesSetjmp        Use setjmp backend.
## -d:nimCoroutinesSetjmpBundled Use bundled setjmp implementation.

# Fragments:
## Remove old GC garbage hacks and simplified
  
import os
import lists
include system/timers

const defaultStackSize = 512 * 1024

const
  CORO_BACKEND_UCONTEXT = 0
  CORO_BACKEND_SETJMP = 1
  CORO_BACKEND_FIBERS = 2

when defined(windows):
  const coroBackend = CORO_BACKEND_FIBERS
  when defined(nimCoroutinesUcontext):
    {.warning: "ucontext coroutine backend is not available on windows, defaulting to fibers.".}
  when defined(nimCoroutinesSetjmp):
    {.warning: "setjmp coroutine backend is not available on windows, defaulting to fibers.".}
elif defined(haiku):
  const coroBackend = CORO_BACKEND_SETJMP
  when defined(nimCoroutinesUcontext):
    {.warning: "ucontext coroutine backend is not available on haiku, defaulting to setjmp".}
elif defined(nimCoroutinesSetjmp) or defined(nimCoroutinesSetjmpBundled):
  const coroBackend = CORO_BACKEND_SETJMP
else:
  const coroBackend = CORO_BACKEND_UCONTEXT

when coroBackend == CORO_BACKEND_FIBERS:
  import windows/winlean
  type
    Context = pointer

elif coroBackend == CORO_BACKEND_UCONTEXT:
  type
    stack_t {.importc, header: "<ucontext.h>".} = object
      ss_sp: pointer
      ss_flags: int
      ss_size: int

    ucontext_t {.importc, header: "<ucontext.h>".} = object
      uc_link: ptr ucontext_t
      uc_stack: stack_t

    Context = ucontext_t

  proc getcontext(context: var ucontext_t): int32 {.importc, header: "<ucontext.h>".}
  proc setcontext(context: var ucontext_t): int32 {.importc, header: "<ucontext.h>".}
  proc swapcontext(fromCtx, toCtx: var ucontext_t): int32 {.importc, header: "<ucontext.h>".}
  proc makecontext(context: var ucontext_t, fn: pointer, argc: int32) {.importc, header: "<ucontext.h>", varargs.}

elif coroBackend == CORO_BACKEND_SETJMP:
  proc coroExecWithStack*(fn: pointer, stack: pointer) {.noreturn, importc: "narch_$1", fastcall.}
  when defined(amd64):
    {.compile: "../arch/x86/amd64.S".}
  elif defined(i386):
    {.compile: "../arch/x86/i386.S".}
  else:
    # coroExecWithStack is defined in assembly. To support other platforms
    # please provide implementation of this procedure.
    {.error: "Unsupported architecture.".}

  when defined(nimCoroutinesSetjmpBundled):
    # Use setjmp/longjmp implementation shipped with compiler.
    when defined(amd64):
      type
        JmpBuf = array[0x50 + 0x10, uint8]
    elif defined(i386):
      type
        JmpBuf = array[0x1C, uint8]
    else:
      # Bundled setjmp/longjmp are defined in assembly. To support other
      # platforms please provide implementations of these procedures.
      {.error: "Unsupported architecture.".}

    proc setjmp(ctx: var JmpBuf): int {.importc: "narch_$1".}
    proc longjmp(ctx: JmpBuf, ret=1) {.importc: "narch_$1".}
  else:
    # Use setjmp/longjmp implementation provided by the system.
    type
      JmpBuf {.importc: "jmp_buf", header: "<setjmp.h>".} = object
    
    proc setjmp(ctx: var JmpBuf): int {.importc, header: "<setjmp.h>".}
    proc longjmp(ctx: JmpBuf, ret=1) {.importc, header: "<setjmp.h>".}

  type
    Context = JmpBuf

when defined(unix):
  # GLibc fails with "*** longjmp causes uninitialized stack frame ***" because
  # our custom stacks are not initialized to a magic value.
  when defined(osx):
    # workaround: error: The deprecated ucontext routines require _XOPEN_SOURCE to be defined
    const extra = " -D_XOPEN_SOURCE"
  else:
    const extra = ""
  {.passC: "-U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0" & extra.}

const
  CORO_CREATED = 0
  CORO_EXECUTING = 1
  CORO_FINISHED = 2

type
  Stack {.pure.} = object
    top: pointer      # Top of the stack. Pointer used for deallocating stack if we own it.
    bottom: pointer   # Very bottom of the stack, acts as unique stack identifier.
    size: int

  Coroutine {.pure.} = object
    execContext: Context
    customData: pointer
    fn: proc(data: pointer) {.cdecl.}
    state: int
    lastRun: Ticks
    sleepTime*: float
    stack: Stack

  CoroutinePtr* = ptr Coroutine

var currentCoro {.threadvar.}: CoroutinePtr
var mainCoro: Coroutine

proc init*() =
  mainCoro.state = CORO_EXECUTING
  when coroBackend == CORO_BACKEND_FIBERS:
    mainCoro.execContext = ConvertThreadToFiberEx(nil, FIBER_FLAG_FLOAT_SWITCH)

proc getCurrent(): CoroutinePtr = currentCoro
proc setCurrent*(co: CoroutinePtr) = currentCoro = co

proc runCurrentTask()

proc switchTo*(current, to: CoroutinePtr) =
  ## Switches execution from `current` into `to` context.
  to.lastRun = getTicks()
  # Update position of current stack so gc invoked from another stack knows how much to scan.
  var frame = getFrameState()
  block:
    # Execution will switch to another fiber now. We do not need to update current stack
    when coroBackend == CORO_BACKEND_FIBERS:
      SwitchToFiber(to.execContext)
    elif coroBackend == CORO_BACKEND_UCONTEXT:
      discard swapcontext(current.execContext, to.execContext)
    elif coroBackend == CORO_BACKEND_SETJMP:
      echo "Pre set jump"
      var res = setjmp(current.execContext)
      if res == 0:
        if to.state == CORO_EXECUTING:
          echo "Pre long jump"
          # Coroutine is resumed.
          longjmp(to.execContext, 1)
        elif to.state == CORO_CREATED:
          # Coroutine is started.
          echo "Pre exec"
          coroExecWithStack(runCurrentTask, to.stack.bottom)
          #doAssert false
    else:
      {.error: "Invalid coroutine backend set.".}
  # Execution was just resumed. Restore frame information and set active stack.
  setFrameState(frame)

proc suspend*(sleepTime: float=0) =
  ## Stops coroutine execution and resumes no sooner than after ``sleeptime`` seconds.
  ## Until then other coroutines are executed.
  var current = getCurrent()
  current.sleepTime = sleepTime
  switchTo(current, addr mainCoro)

proc runCurrentTask() =
  ## Starts execution of current coroutine and updates it's state through coroutine's life.
  var sp {.volatile.}: pointer
  sp = addr(sp)
  block:
    var current = getCurrent()
    current.stack.bottom = sp
    # Execution of new fiber just started. Since it was entered not through `switchTo` we
    # have to set active stack here as well. GC_removeStack() has to be called in main loop
    # because we still need stack available in final suspend(0) call from which we will not
    # return.
    # Activate current stack because we are executing in a new coroutine.
    current.state = CORO_EXECUTING
    try:
      current.fn(current.customData)                    # Start coroutine execution
    except:
      echo "Unhandled exception in coroutine."
      writeStackTrace()
    current.state = CORO_FINISHED
  suspend(0)
  doAssert false

proc start*(c: proc(data: pointer) {.cdecl.}, data: pointer = nil, stacksize: int = defaultStackSize): CoroutinePtr =
  ## Schedule coroutine for execution. It does not run immediately.
  var coro: CoroutinePtr
  when coroBackend == CORO_BACKEND_FIBERS:
    coro = cast[CoroutinePtr](alloc0(sizeof(Coroutine)))
    coro.execContext = CreateFiberEx(stacksize, stacksize, FIBER_FLAG_FLOAT_SWITCH, (proc(p: pointer): void {.stdcall.} = runCurrentTask()), nil)
  else:
    coro = cast[CoroutinePtr](alloc0(sizeof(Coroutine) + stacksize))
    coro.stack.top = cast[pointer](cast[ByteAddress](coro) + sizeof(Coroutine))
    coro.stack.bottom = cast[pointer](cast[ByteAddress](coro.stack.top) + stacksize)
    when coroBackend == CORO_BACKEND_UCONTEXT:
      discard getcontext(coro.execContext)
      coro.execContext.uc_stack.ss_sp = coro.stack.top
      coro.execContext.uc_stack.ss_size = stacksize
      coro.execContext.uc_link = addr(ctx.loop.execContext)
      makecontext(coro.execContext, runCurrentTask, 0)
  coro.fn = c
  coro.customData = data
  coro.stack.size = stacksize
  coro.state = CORO_CREATED
  return coro

proc cleanup*(c: CoroutinePtr) =
  when coroBackend == CORO_BACKEND_FIBERS:
    DeleteFiber(c.execContext)
  else:
    dealloc(c.stack.top)
  dealloc(c)

proc runOnce*() =
  switchTo(addr mainCoro, currentCoro)

proc alive*(c: CoroutinePtr): bool = c != nil and c[].state != CORO_FINISHED
  ## Returns ``true`` if coroutine has not returned, ``false`` otherwise.

proc wait*(c: CoroutinePtr, interval=0.01) =
  ## Returns only after coroutine ``c`` has returned. ``interval`` is time in seconds how often.
  while alive(c):
    suspend(interval)

when isMainModule:
  init()

  var
    running = true
    c1 = start(proc(data: pointer) {.cdecl.} =
      while running:
        echo "Hello"
        suspend(1)
      echo "Done"
    )
  
  setCurrent(c1)
  runOnce()
  runOnce()
  running = false
  runOnce()
  cleanup(c1)
  echo "Deleted"