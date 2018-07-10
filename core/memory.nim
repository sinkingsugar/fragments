# memory utils

proc alloc*[T](objtype: typedesc[T]): ptr T {.inline.} = 
  result = cast[ptr T](alloc(sizeof(T)))
  
proc alloc0*[T](objtype: typedesc[T]): ptr T {.inline.} = 
  result = cast[ptr T](alloc0(sizeof(T)))

proc newptr*[T](objtype: typedesc[T]): ptr T {.inline.} = 
  result = cast[ptr T](alloc(sizeof(T)))
  result[] = T()

proc delete*[T](p: ptr T) {.inline.} =
  when compiles(`=destroy`(p[])):
    `=destroy`(p[])
  dealloc(p)