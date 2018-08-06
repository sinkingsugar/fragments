# Fragments

## A collection of pure nim utilities.

> **Note: work in progress**

## Structure

- **ffi**
  - **cpp**: Write C++ straight from Nim. Inspired by `std/jsffi` and using the magic of `importcpp`, `macros` and many other cool nim features. Wrappers are a thing of the past!
  - **js**: A collection of utilities for the js backend.
  
- **math**
  - **math_common**: Commonly used math routines, similar to `std/math`.
  - **vectors**: Helpers for writing vectorized code, converting types into Array-Of-Structure-Of-Arrays (AOSOA) form and generalizing operations to wide types.
  - **linalg**: Linear algebra for 3D rendering and simulation. Designed for maximum ergonomy, performance and generalization to vectorized types and algorithms.

- **threading**
  - **async_primitives**: Synchronization primitives for asynchronous programs, integrated with `Futures` and `async/await`.
  - **atomics**: Atomic types and operations closely conforming to C++11 style atomics.
  - **threading_primitives**: Syncrhonization primitives for multi-threaded programs, such as *spin locks* and *events*.

- **dsl**: Macros to define custom Nim DSLs.

- **memory**: Custom allocators and helpers for allocating objects and managing their life-time.

- **serialization**: Fast binary serialization with automatically compile-time generated serializers.

## Examples

### ffi

#### cpp

```nimrod
# Build configuration
cppdefines("MYDEFINE", "MYDEFINE2=10")
cppincludes(".")
cppfiles("MyClass.cpp")
cpplibpaths(".")

# Define nim types for C++ types will be used directly
defineCppType(MyClass, "MyClass", "MyClass.hpp")
defineCppType(MyClass2, "MyClass2", "MyClass.hpp")

# Construct an object
var x = cppinit(MyClass, 1)
var y = cast[ptr MyClass](alloc0(sizeof(MyClass)))

# Create a C++ object tracked by the Nim GC
var j: ref MyClass
cppnewref(j, 1)
j.number = 22
echo $j.number.to(cint)

# Create an untracked C++ object
var k: ptr MyClass 
cppnewptr(k, 2)
k.number = 55
echo $k.number.to(cint)
k.cppdelptr

# Accessing global variables and functions
echo $global.globalNumber.to(cint)
global.globalNumber = 102
echo $global.globalNumber.to(cint)
global.printf("Hello World\n".cstring).to(void)

# Accessing members
y.test3().to(void)
y.test4(7, 8).to(void)
```

### math

#### vectors

```nimrod
type
  MyType = object
    f1: int
    f2: float

  MyWideType = wide MyType

var x: wide MyType
x.setLane(1, x.getLane(0))
```

#### linalg

```nimrod
var
  v2: Vector2
  v3: Vector3
  v4: Vector4
  m: Matrix4x4
  q: Quaternion

# Construction
v2 = (1.0f, 2.0f)
v4 = (v2, 3.0f, 4.0f)
v3 = Vector3.unitX
m = Matrix4x4.identity
q = Quaternion.identity

# Swizzling
v2.x = 3.0f
q.yxz = v4.wwx
v4.rgba = (v3.zxy, 1.0f)
m.m00m12m34m41 = v4
let v6 = v3.xyzxyz

# Basic transformation
let a = 
q = rotationAxisQuaternion(Vector3.unitX, PI/2)
v3 = Vector3.unitY.transform(q)
```

### threading

#### async_primitives

```nimrod
var lock: AsyncLock
lock.init()

# Yield asynchronous execution until the lock is available
withLock lock:
  await someAsynchronousOperation()
```

#### threading_primitives

**Spin-locks**
```nimrod
var spinLock: SpinLock

# In multiple threads
withLock spinLock:
  someAtomicOperation()
```

**Events**
```nimrod
var event: ManualResetEvent
event.init()

# Thread 1
event.signal()

# Thread 2
event.waitOne()
```

#### atomics

```nimrod
# Atomic flags, trivially atomic types and types that require spin-locking
var
  guard: AtomicFlag
  foo: atomic int
  bar: atomic MyComplexType

# Atomic operations
var value = 5
foo.store(value, moRelease)
assert foo.compareExchange(value, 4, moAcquireRelease)
assert foo.load(moAcquire) == 4

# Possible spin-lock impelemtation
while guard.testAndSet(moAcquire): discard
  cpuRelax()
guard.clear(moRelease)
```

### serialization

Feature:
- Atomatic serializer generation for object and tuple types
- Efficient handling of blittable types
- Reuse of objects when serializing references

```nimrod
type
  MyType = ref object
    trivial*: int
    skipped* {.serializable: false.}: int
    sequence*: seq[int]
    str*: string
    complex*: MyOtherType
    reference*: ref int
    anonymousTuple*: tuple[a: ref int]

  MyOtherType = object

var context = newSerializationContext(stream)
context.stream = newStringStream()

var x = new MyType
x.serialize(context)

var y: MyType
y.deserialize(context)
```

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