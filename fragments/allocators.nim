import std/allocators
import math/math_common

type
  LinearAllocator* = ptr LinearAllocatorObj
    ## A allocator that allocates memory in a continuous block of memory and never frees it,
    ## unless `reset` is called manually.

  LinearAllocatorObj* = object of AllocatorObj
    upstream: Allocator
    buffer: seq[byte]
    offset: int

template withAllocator*(allocator: Allocator; body: untyped): untyped =
  ## Uses the specified allocator for all allocations in a block of code
  let currentAllocator = getLocalAllocator()
  setLocalAllocator(allocator)
  try:
    body
  finally:
    setLocalAllocator(currentAllocator)

template withSharedAllocator*(body: untyped): untyped =
  ## Uses the current shared allocator for all allocations in a block of code
  withAllocator(getSharedAllocator()):
    body

func allocImpl(self: var LinearAllocatorObj; size: int; alignment: int = 8): pointer {.nimcall, raises: [], tags: [], inline.} =
  let start = align(self.offset, alignment)
  let newOffset = start + size
  if newOffset > self.buffer.len:
    return nil
  return addr self.buffer[start]

func deallocImpl(self: var LinearAllocatorObj; p: pointer; size: int) {.nimcall, raises: [], tags: [], inline.} =
  discard

func reallocImpl(self: var LinearAllocatorObj; p: pointer; oldSize, newSize: int): pointer {.nimcall, raises: [], tags: [], inline.} =
  if newSize < oldSize:
    return p
  self.allocImpl(newSize) # TODO: Alignment

func deallocAllImpl(self: var LinearAllocatorObj) {.nimcall, raises: [], tags: [], inline.} =
  discard

proc initLinearAllocator*(totalSize: int; upstream: Allocator = nil): LinearAllocatorObj =
  ## Initializes a linear allocator backed by a buffer of size `totalSize`, drawn from the `upstream` allocator.
  result.upstream = upstream
  result.alloc = proc(self: Allocator; size: int; alignment: int): pointer {.nimcall, raises: [], tags: [].} = cast[LinearAllocator](self)[].allocImpl(size, alignment)
  result.dealloc = proc(self: Allocator; p: pointer; size: int) {.nimcall, raises: [], tags: [].} = cast[LinearAllocator](self)[].deallocImpl(p, size)
  result.realloc = proc(self: Allocator; p: pointer; oldSize, newSize: int): pointer {.nimcall, raises: [], tags: [].} = cast[LinearAllocator](self)[].reallocImpl(p, oldSize, newSize)
  result.deallocAll = proc(self: Allocator) {.nimcall, raises: [], tags: [].} = cast[LinearAllocator](self)[].deallocAllImpl()
  result.flags = { ThreadLocal, ZerosMem }

  withAllocator upstream:
    result.buffer.setLen(totalSize)

func reset*(self: var LinearAllocatorObj) =
  # Frees all memory allocated through a a linear allocator.
  self.offset = 0

when isMainModule:

  var linearAllocator = initLinearAllocator(1000)
  for i in 0 ..< 10:
    for i in 0 ..< 10:
      withAllocator(addr linearAllocator):
        var x = newSeq[int](10)
    echo linearAllocator.offset
    linearAllocator.reset()