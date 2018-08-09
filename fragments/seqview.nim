type
  SeqView*[T] = object
    root: ptr seq[T]
    slice: Slice[int]

proc view*[T](s: var seq[T]; slice: Slice[int]): SeqView[T] {.inline.} =
  result.root = addr s
  result.slice = slice

proc high*(v: var SeqView): int {.inline.} = v.slice.b - v.slice.a

proc len*(v: var SeqView): int {.inline.} = v.high + 1

proc `[]`*[T](v: var SeqView[T]; index: int): T {.inline.} = v.root[][v.slice.a + index]

when isMainModule:
  var s = @[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  var v = s.view(2..5)

  assert(v.high == 3, $v.len)
  assert(v.len == 4, $v.len)
  assert(v[0] == 2)
  assert(v[v.high] == 5)