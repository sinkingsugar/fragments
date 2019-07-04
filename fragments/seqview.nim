type
  SeqView*[T] = object
    root: ptr T
    length: int

proc view*[T](s: var seq[T]; slice: Slice[int]): SeqView[T] {.inline.} =
  result.root = addr s[slice.a]
  result.length = slice.b - slice.a + 1

proc view*[T](s: var seq[T]): SeqView[T] {.inline.} =
  result.root = addr s[0]
  result.length = s.len

proc high*(v: SeqView): int {.inline.} = v.length - 1

proc len*(v: SeqView): int {.inline.} = v.length

proc `[]`*[T](v: SeqView[T]; index: int): T {.inline.} =
  let
    buffer = cast[int](v.root)
    begin = buffer + index * sizeof(T)
    resPtr = cast[ptr T](begin)
  return resPtr[]

proc `[]=`*[T](v: SeqView[T]; index: int; value: T) {.inline.} =
  let
    buffer = cast[int](v.root)
    begin = buffer + index * sizeof(T)
    valPtr = cast[ptr T](begin)
  valPtr[] = value

iterator items*[T](s: SeqView[T]): T {.inline.} =
  for i in 0..s.high:
    let item = s[i]
    yield item

iterator mitems*[T](s: var SeqView[T]): var T {.inline.} =
  for i in 0..s.high:
    var item = s[i]
    yield item

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