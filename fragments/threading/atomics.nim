import macros

when defined(cpp):
  {.push, header: "<atomic>".}

  type
    MemoryOrder* {.importcpp: "std::memory_order".} = enum
      Relaxed
      Consume
      Acquire
      Release
      AcquireRelease
      SequentiallyConsistent

  type
    Atomic* {.importcpp: "std::atomic".} [T] = object
    AtomicFlag* {.importcpp: "std::atomic_flag".} = object

  template atomic*(T: typedesc): typedesc =
    Atomic[T]     

  # Access operations
  #proc init*[T](location: var Atomic[T]; value: T): T {.importcpp: "std::atomic_init(&#, @)".}
  proc load*[T](location: var Atomic[T]; order: MemoryOrder = SequentiallyConsistent): T {.importcpp: "#.load(@)".}
  proc store*[T](location: var Atomic[T]; desired: T; order: MemoryOrder = SequentiallyConsistent) {.importcpp: "#.store(@)".}
  proc exchange*[T](location: var Atomic[T]; desired: T; order: MemoryOrder = SequentiallyConsistent): T {.importcpp: "#.exchange(@)".}
  proc compareExchange*[T](location: var Atomic[T]; expected: var T; desired: T; order: MemoryOrder = SequentiallyConsistent): bool {.importcpp: "#.compare_exchange_strong(@)".}
  proc compareExchange*[T](location: var Atomic[T]; expected: var T; desired: T; success, failure: MemoryOrder): bool {.importcpp: "#.compare_exchange_strong(@)".}
  proc compareExchangeWeak*[T](location: var Atomic[T]; expected: var T; desired: T; order: MemoryOrder = SequentiallyConsistent): bool {.importcpp: "#.compare_exchange_weak(@)".}
  proc compareExchangeWeak*[T](location: var Atomic[T]; expected: var T; desired: T; success, failure: MemoryOrder): bool {.importcpp: "#.compare_exchange_weak(@)".}

  # Numerical operations
  proc fetchAdd*[T: SomeInteger](location: var Atomic[T]; value: T; order: MemoryOrder = SequentiallyConsistent): T {.importcpp: "#.fetch_add(@)".}
  proc fetchSub*[T: SomeInteger](location: var Atomic[T]; value: T; order: MemoryOrder = SequentiallyConsistent): T {.importcpp: "#.fetch_sub(@)".}
  proc fetchAnd*[T: SomeInteger](location: var Atomic[T]; value: T; order: MemoryOrder = SequentiallyConsistent): T {.importcpp: "#.fetch_and(@)".}
  proc fetchOr*[T: SomeInteger](location: var Atomic[T]; value: T; order: MemoryOrder = SequentiallyConsistent): T {.importcpp: "#.fetch_or(@)".}
  proc fetchXor*[T: SomeInteger](location: var Atomic[T]; value: T; order: MemoryOrder = SequentiallyConsistent): T {.importcpp: "#.fetch_xor(@)".}

  # Flag operations
  #proc init*(location: var AtomicFlag) {.importcpp: "# = ATOMIC_FLAG_INIT".}
  proc testAndSet*(location: var AtomicFlag; order: MemoryOrder = SequentiallyConsistent): bool {.importcpp: "#.test_and_set(@)".}
  proc clear*(location: var AtomicFlag; order: MemoryOrder = SequentiallyConsistent) {.importcpp: "#.clear(@)".}

  proc fence*(order: MemoryOrder) {.importcpp: "std::atomic_thread_fence(@)".}
  #proc signalFence*(order: MemoryOrder) {.importcpp: "std::atomic_signal_fence(@)".}  

  {.pop.}
else:
  type 
    Trivial = SomeNumber | bool | ptr | pointer

  when defined(vcc):
    
    type
      MemoryOrder* = enum
        Relaxed
        Consume
        Acquire
        Release
        AcquireRelease
        SequentiallyConsistent

      AtomicObject[T] = object
        value: T
        guard: AtomicFlag

      AtomicFlag* = distinct int8

      Atomic*[T] = concept type A
        A.atomicUnderlyingType is T
        
    template atomicUnderlyingType*[T, S](_: Trivial): typedesc = T
    template atomicUnderlyingType*[T](_: typedesc[AtomicObject[T]]): typedesc = T

    template nonAtomicType(T: typedesc): typedesc =
      when sizeof(T) == 1: int8
      elif sizeof(T) == 2: int16
      elif sizeof(T) == 4: int32
      elif sizeof(T) == 8: int64

    template atomic*(T: typedesc): typedesc =
      when T is Trivial: T
      else: AtomicObject[T]

    {.push header: "<intrin.h>".}

    proc interlockedExchange(location: pointer; desired: int8): int8 {.importc: "_InterlockedExchange8".}
    proc interlockedExchange(location: pointer; desired: int16): int16 {.importc: "_InterlockedExchange".}
    proc interlockedExchange(location: pointer; desired: int32): int32 {.importc: "_InterlockedExchange16".}
    proc interlockedExchange(location: pointer; desired: int64): int64 {.importc: "_InterlockedExchange64".}

    proc interlockedCompareExchange(location: pointer; desired, expected: int8): int8 {.importc: "_InterlockedCompareExchange8".}
    proc interlockedCompareExchange(location: pointer; desired, expected: int16): int16 {.importc: "_InterlockedCompareExchange16".}
    proc interlockedCompareExchange(location: pointer; desired, expected: int32): int32 {.importc: "_InterlockedCompareExchange".}
    proc interlockedCompareExchange(location: pointer; desired, expected: int64): int64 {.importc: "_InterlockedCompareExchange64".}

    proc interlockedAnd(location: pointer; value: int8): int8 {.importc: "_InterlockedAnd8".}
    proc interlockedAnd(location: pointer; value: int16): int16 {.importc: "_InterlockedAnd16".}
    proc interlockedAnd(location: pointer; value: int32): int32 {.importc: "_InterlockedAnd".}
    proc interlockedAnd(location: pointer; value: int64): int64 {.importc: "_InterlockedAnd64".}

    proc interlockedOr(location: pointer; value: int8): int8 {.importc: "_InterlockedOr8".}
    proc interlockedOr(location: pointer; value: int16): int16 {.importc: "_InterlockedOr16".}
    proc interlockedOr(location: pointer; value: int32): int32 {.importc: "_InterlockedOr".}
    proc interlockedOr(location: pointer; value: int64): int64 {.importc: "_InterlockedOr64".}

    proc interlockedXor(location: pointer; value: int8): int8 {.importc: "_InterlockedXor8".}
    proc interlockedXor(location: pointer; value: int16): int16 {.importc: "_InterlockedXor16".}
    proc interlockedXor(location: pointer; value: int32): int32 {.importc: "_InterlockedXor".}
    proc interlockedXor(location: pointer; value: int64): int64 {.importc: "_InterlockedXor64".}

    proc fence(order: MemoryOrder): int64 {.importc: "_ReadWriteBarrier()".}
    #proc threadFence(order: MemoryOrder): int64 {.importc: "_ReadWriteBarrier()".}

    {.pop.}

    proc testAndSet*(location: var AtomicFlag; order: MemoryOrder = SequentiallyConsistent): bool =
      interlockedCompareExchange(addr(location), 1'i8, 0'i8) != 0
    proc clear*(location: var AtomicFlag; order: MemoryOrder = SequentiallyConsistent) =
      discard interlockedExchange(addr(location), 0'i8)

    macro makeCallName(name: untyped; size: static[int]): untyped =
      ident($name & "")

    template call[T](name: untyped; location: ptr T; args: varargs[untyped]): untyped =
      name(location, args)

    proc load*[T: Trivial](location: var T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](call(interlockedOr, addr(location), (nonAtomicType(T))0))
    proc store*[T: Trivial](location: var T; desired: T; order: MemoryOrder = SequentiallyConsistent) {.inline.} =
      discard call(interlockedExchange, addr(location), cast[nonAtomicType(T)](desired))

    proc exchange*[T: Trivial](location: var T; desired: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](call(interlockedExchange, addr(location), cast[int64](desired)))
    proc compareExchange*[T: Trivial](location: var T; expected: var T; desired: T; success, failure: MemoryOrder): bool {.inline.} =
      cast[T](call(interlockedCompareExchange, addr(location), cast[nonAtomicType(T)](desired), cast[nonAtomicType(T)](expected))) == expected
    proc compareExchange*[T: Trivial](location: var T; expected: var T; desired: T; order: MemoryOrder = SequentiallyConsistent): bool {.inline.} =
      compareExchange(location, expected, desired, order, order)
    proc compareExchangeWeak*[T: Trivial](location: var T; expected: var T; desired: T; success, failure: MemoryOrder): bool {.inline.} =
      compareExchange(location, expected, desired, success, failure)
    proc compareExchangeWeak*[T: Trivial](location: var T; expected: var T; desired: T; order: MemoryOrder = SequentiallyConsistent): bool {.inline.} =
      compareExchangeWeak(location, expected, desired, order, order)

    proc fetchAdd*[T: SomeInteger](location: var T; value: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      let currentValue = location.load()
      while not compareExchangeWeak(location, currentValue, currentValue + value): discard
    proc fetchSub*[T: SomeInteger](location: var T; value: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      fetchAdd(location, -value, order)
    proc fetchAnd*[T: SomeInteger](location: var T; value: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](call(interlockedAnd, addr(location), cast[nonAtomicType(T)](value)))
    proc fetchOr*[T: SomeInteger](location: var T; value: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](call(interlockedOr, addr(location), cast[nonAtomicType(T)](value)))
    proc fetchXor*[T: SomeInteger](location: var T; value: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](call(interlockedXor, addr(location), cast[nonAtomicType(T)](value)))
    
  else:
    {.push, header: "<stdatomic.h>".}

    type
      MemoryOrder* {.importc: "memory_order".} = enum
        Relaxed
        Consume
        Acquire
        Release
        AcquireRelease
        SequentiallyConsistent

    type
      #Atomic* {.importcpp: "_Atomic('0)".} [T] = object
      AtomicInt8 {.importc: "_Atomic NI8".} = object
      AtomicInt16 {.importc: "_Atomic NI16".} = object
      AtomicInt32 {.importc: "_Atomic NI32".} = object
      AtomicInt64 {.importc: "_Atomic NI64".} = object
      AtomicObject[T] = object
        value: T
        guard: AtomicFlag

      AtomicFlag* {.importc: "atomic_flag".} = object

      AtomicTrivial[T: Trivial; S] = object
        value: S

      Atomic*[T] = concept type A
        A.atomicUnderlyingType is T

    template atomicType(T: typedesc; size: int): untyped =
      when size == 1: AtomicInt8
      elif size == 2: AtomicInt16
      elif size == 4: AtomicInt32
      elif size == 8: AtomicInt64

    template atomicUnderlyingType*[T, S](_: typedesc[AtomicTrivial[T, S]]): typedesc = T
    template atomicUnderlyingType*[T](_: typedesc[AtomicObject[T]]): typedesc = T

    template nonAtomicType(T: typedesc[AtomicInt8]): typedesc = int8
    template nonAtomicType(T: typedesc[AtomicInt16]): typedesc = int16
    template nonAtomicType(T: typedesc[AtomicInt32]): typedesc = int32
    template nonAtomicType(T: typedesc[AtomicInt64]): typedesc = int64

    template atomic*(T: typedesc): typedesc =
      when T is Trivial: AtomicTrivial[T, atomicType(T, sizeof(T))]
      else: AtomicObject[T]

    #proc init*[T](location: var Atomic[T]; value: T): T {.importcpp: "atomic_init(@)".}
    proc atomic_load_explicit[T, A](location: ptr A; order: MemoryOrder): T {.importc.}
    proc atomic_store_explicit[T, A](location: ptr A; desired: T; order: MemoryOrder = SequentiallyConsistent) {.importc.}
    proc atomic_exchange_explicit[T, A](location: ptr A; desired: T; order: MemoryOrder = SequentiallyConsistent): T {.importc.}
    proc atomic_compare_exchange_strong_explicit[T, A](location: ptr A; expected: ptr T; desired: T; success, failure: MemoryOrder): bool {.importc.}
    proc atomic_compare_exchange_weak_explicit[T, A](location: ptr A; expected: ptr T; desired: T; success, failure: MemoryOrder): bool {.importc.}
      
    # Numerical operations
    proc atomic_fetch_add_explicit[T, A](location: ptr A; value: T; order: MemoryOrder = SequentiallyConsistent): T {.importc.}
    proc atomic_fetch_sub_explicit[T, A](location: ptr A; value: T; order: MemoryOrder = SequentiallyConsistent): T {.importc.}
    proc atomic_fetch_and_explicit[T, A](location: ptr A; value: T; order: MemoryOrder = SequentiallyConsistent): T {.importc.}
    proc atomic_fetch_or_explicit[T, A](location: ptr A; value: T; order: MemoryOrder = SequentiallyConsistent): T {.importc.}
    proc atomic_fetch_xor_explicit[T, A](location: ptr A; value: T; order: MemoryOrder = SequentiallyConsistent): T {.importc.}
  
    # Flag operations
    # var ATOMIC_FLAG_INIT {.importc, nodecl.}: AtomicFlag
    # proc init*(location: var AtomicFlag) {.inline.} = location = ATOMIC_FLAG_INIT
    proc testAndSet*(location: var AtomicFlag; order: MemoryOrder = SequentiallyConsistent): bool {.importc: "atomic_flag_test_and_set_explicit".}
    proc clear*(location: var AtomicFlag; order: MemoryOrder = SequentiallyConsistent) {.importc: "atomic_flag_clear_explicit".}
  
    proc fence*(order: MemoryOrder) {.importc: "atomic_thread_fence".}
    #proc signalFence*(order: MemoryOrder) {.importc: "atomic_signal_fence".}  

    {.pop.}

    proc load*[T, A](location: var AtomicTrivial[T, A]; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](atomic_load_explicit[nonAtomicType(A), A](addr(location.value), order))
    proc store*[T, A](location: var AtomicTrivial[T, A]; desired: T; order: MemoryOrder = SequentiallyConsistent) {.inline.} =
      atomic_store_explicit(addr(location.value), cast[nonAtomicType(A)](desired), order)
    proc exchange*[T, A](location: var AtomicTrivial[T, A]; desired: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](atomic_exchange_explicit(addr(location.value), cast[nonAtomicType(A)](desired), order))
    proc compareExchange*[T, A](location: var AtomicTrivial[T, A]; expected: var T; desired: T; success, failure: MemoryOrder): bool {.inline.} =
      atomic_compare_exchange_strong_explicit(addr(location.value), cast[ptr nonAtomicType(A)](addr(expected)), cast[nonAtomicType(A)](desired), success, failure)
    proc compareExchange*[T, A](location: var AtomicTrivial[T, A]; expected: var T; desired: T; order: MemoryOrder = SequentiallyConsistent): bool {.inline.} =
      compareExchange(location, expected, desired, order, order)
    proc compareExchange*[T, A](location: var AtomicObject[T]; expected: var T; desired: T; order: MemoryOrder = SequentiallyConsistent): bool {.inline.} =
      compareExchange(location, expected, desired, order, order)

    proc compareExchangeWeak*[T, A](location: var AtomicTrivial[T, A]; expected: var T; desired: T; success, failure: MemoryOrder): bool {.inline.} =
      atomic_compare_exchange_weak_explicit(addr(location.value), cast[ptr nonAtomicType(A)](addr(expected)), cast[nonAtomicType(A)](desired), success, failure)
    proc compareExchangeWeak*[T, A](location: var AtomicTrivial[T, A]; expected: var T; desired: T; order: MemoryOrder = SequentiallyConsistent): bool {.inline.} =
      compareExchangeWeak(location, expected, desired, order, order)
  
    # Numerical operations
    proc fetchAdd*[T: SomeInteger, A](location: var AtomicTrivial[T, A]; value: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](atomic_fetch_add_explicit(addr(location.value), cast[nonAtomicType(A)](value), order))
    proc fetchSub*[T: SomeInteger, A](location: var AtomicTrivial[T, A]; value: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](atomic_fetch_sub_explicit(addr(location.value), cast[nonAtomicType(A)](value), order))
    proc fetchAnd*[T: SomeInteger, A](location: var AtomicTrivial[T, A]; value: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](atomic_fetch_and_explicit(addr(location.value), cast[nonAtomicType(A)](value), order))
    proc fetchOr*[T: SomeInteger, A](location: var AtomicTrivial[T, A]; value: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](atomic_fetch_or_explicit(addr(location.value), cast[nonAtomicType(A)](value), order))
    proc fetchXor*[T: SomeInteger, A](location: var AtomicTrivial[T, A]; value: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
      cast[T](atomic_fetch_xor_explicit(addr(location.value), cast[nonAtomicType(A)](value), order))

  template withLock(location: var AtomicObject; order: MemoryOrder; body: untyped): untyped =
    while location.guard.testAndSet(): discard
    body
    location.guard.clear()

  proc load*[T](location: var AtomicObject[T]; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =      
    withLock(location, order):
      result = location.value

  proc store*[T](location: var AtomicObject[T]; desired: T; order: MemoryOrder = SequentiallyConsistent) {.inline.} =      
    withLock(location, order):
      location.value = desired

  proc exchange*[T](location: var AtomicObject[T]; desired: T; order: MemoryOrder = SequentiallyConsistent): T {.inline.} =
    withLock(location, order):
      result = location.value
      location.value = desired

  proc compareExchange*[T](location: var AtomicObject[T]; expected: var T; desired: T; success, failure: MemoryOrder): bool {.inline.} =
    withLock(location, success):
      if location.value != expected:
        return false
      swap(location.value, expected)
      return true

  proc compareExchangeWeak*[T](location: var AtomicObject[T]; expected: var T; desired: T; success, failure: MemoryOrder): bool {.inline.} =
    withLock(location, success):
      if location.value != expected:
        return false
      swap(location.value, expected)
      return true

  proc compareExchangeWeak*[T](location: var AtomicObject[T]; expected: var T; desired: T; order: MemoryOrder = SequentiallyConsistent): bool {.inline.} =
    compareExchangeWeak(location, expected, desired, order, order)

proc atomicInc*[T: SomeInteger](location: var Atomic[T]; value: T = 1) {.inline.} =
  discard location.fetchAdd(value)

proc atomicDec*[T: SomeInteger](location: var Atomic[T]; value: T = 1) {.inline.} =
  discard location.fetchSub(value)

proc `+=`*[T: SomeInteger](location: var Atomic[T]; value: T) {.inline.} =
  discard location.fetchAdd(value)

proc `-=`*[T: SomeInteger](location: var Atomic[T]; value: T) {.inline.} =
  discard location.fetchSub(value)

when isMainModule:

  type
    Foo = object
      i: int

  var
    foo: atomic float
    b: atomic int
    bar: float = 2.0
    lol: atomic Foo

  foo.store(1.0)
  echo foo.load()
  echo lol.load()
  echo foo.exchange(2.0)
  echo foo.compareExchange(bar, 3.0)
  echo foo.compareExchangeWeak(bar, 3.0)
  atomicInc b, 2
  echo foo.load()
  echo b.load()
