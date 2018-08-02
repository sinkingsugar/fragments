# Fragments

## A collection of pure nim utilities.

**Disclamer, work in progress**

### ffi
* **cpp**: inspired from the official `jsffi`, using the magic of importcpp, macros and many other nim cool features, **dynamic c++**, wrappers are a thing of the past write straight cpp code from nim.
```nimrod
cppdefines("MYDEFINE", "MYDEFINE2=10")
cppincludes(".")
cppfiles("MyClass.cpp")
cpplibpaths(".")

defineCppType(MyClass, "MyClass", "MyClass.hpp")
defineCppType(MyClass2, "MyClass2", "MyClass.hpp")

var x = cppinit(MyClass, 1)
var y = cast[ptr MyClass](alloc0(sizeof(MyClass)))
var w: ref MyClass
new(w, proc(self: ref MyClass) = self.cppdtor())
var z = y.cppctor(1)
var q = w.cppctor(1)

var j: ref MyClass
cppnewref(j, 1)
j.number = 22
echo $j.number.to(cint)

var k: ptr MyClass 
cppnewptr(k, 2)
k.number = 55
echo $k.number.to(cint)
k.cppdelptr

echo $global.globalNumber.to(cint)
global.globalNumber = 102
echo $global.globalNumber.to(cint)

global.printf("Hello World\n".cstring).to(void)
y.test3().to(void)
y.test4(7, 8).to(void)

```
* **js** a collection of utilities for the js backend

### math
* **linalg**
* **math_common**
* **vectors**

### threading
* **async_primitives**
* **atomics**
* **threading_primitives**

### dsl
Macros to define custom nim DSLs, for example:
```nimrod
type
  MyBase = object of RootObj
    value: string

method testMethod(b: ptr MyBase; wow: ref int) {.base.} = echo "Base"

archetype entity:
  super: MyBase
  mustBeVar: int
  mustBeVar2: float
  proc start()
  proc run(state: int) {.async.}
  proc stop()
  method testMethod(wow: ref int)

entity MyEntity:
  param0: 10
  param1 {.public.}: 20
  var0 {.public.}: float
  var1: float = 2.0
  mustBeVar {.public.}: 10

  start:
    var ten = param0
    var ten2 = param1
    var1 = (ten + ten2).float
  
  run:
    echo param0
    echo param1
    echo state
  
  stop:
    echo param0

  testMethod:
    echo "Derived"
    echo param0

```

### memory

### serialization