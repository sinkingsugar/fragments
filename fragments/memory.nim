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

type
  SharedPtr* {.importcpp: "std::shared_ptr<'0>", header: "<memory>".} [T] = object

template newShared*(T: typedesc; destructor: untyped): SharedPtr[T] =
  proc deleter(self: ptr T) =
    destructor(self[])
    dealloc(self)
    
  proc construct(self: ptr T, del: pointer): SharedPtr[T] {.importcpp: "'0(@)".}
  let p = newptr(T)
  construct(p, deleter)

when isMainModule:
  type Foo = object
  proc kill(self: var Foo) {.nimcall.} = echo "bye"
  var x = newShared(Foo, kill)
  