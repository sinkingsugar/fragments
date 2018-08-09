type
  SeqView*[T] = object
    root: ptr T
    length: int

proc view*[T](s: var seq[T]; slice: Slice[int]): SeqView[T] {.inline.} =
  result.root = addr s[slice.a]
  result.length = slice.b - slice.a + 1

proc high*(v: SeqView): int {.inline.} = v.length - 1

proc len*(v: SeqView): int {.inline.} = v.length

proc `[]`*[T](v: SeqView[T]; index: int): T {.inline.} =
  let
    buffer = cast[int](v.root)
    begin = buffer + index * sizeof(T)
    resPtr = cast[ptr T](begin)
  return resPtr[]

proc `[]=`*[T](v: SeqView[T]; index: int; value: T) =
  let
    buffer = cast[int](v.root)
    begin = buffer + index * sizeof(T)
    valPtr = cast[ptr T](begin)
  valPtr[] = value

when isMainModule:
  var s = @[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  var v = s.view(2..5)

  assert(v.high == 3, $v.len)
  assert(v.len == 4, $v.len)
  assert(v[0] == 2)
  assert(v[v.high] == 5)
  v[2] = 11
  assert(v[2] == 11)
  assert(s[4] == 11)