import macros, strutils, typetraits, tables, math

type
  Wide*[T; len: static int] = object
    ## A super-scaler primitive type, used in vectorized code
    elements*: array[len, T]

  SomeWide*[T; width: static int] = concept v, var m, type V
    ## The contract for super-scalar versions of complex types
    V.scalarTypeImpl is T
    V.laneCountImpl == width
    # v.getLaneImpl(int) is T
    # m.setLaneImpl(int, T)

  AnyWide* = concept v, var m, type V
    ## The contract for super-scalar versions of complex types
    const width = V.laneCountImpl
    type T = V.scalarTypeImpl
    # TODO: Causes compiler crash
    # v.getLaneImpl(int) is T
    # m.setLaneImpl(int, T)

  SomeVector*#[T; width: static int]# = concept #[v, var m,]# type V
    ## A wide type marked as vector. Vectors automatically support some basic operations.
    V.isVector()
    # v[int] is T
    # m[int] = T
    # V.len == width

  Vectorizable* = concept type S
    ## A type that defines a way to generate a vectorized version by implementing `wideImpl`.
    isVectorizable(S)

    # TODO: Compiler crash when matching the type in a macro parameter?
    # wideImpl(S)
    # TODO: Compiler crash when deducing the type. Generic concept doesn't work either.
    #type W = wideImpl(S)
    # W is SomeWide
    # S is scalarType(W)

  TrivialScalar* = SomeNumber | enum | bool

template restrict*(arg: untyped): untyped =
  when defined(vcc):
    let `arg` {.inject, codegenDecl: "$# __restrict $#".} = unsafeaddr `arg`
  else:
    let `arg` {.inject, codegenDecl: "$# __restrict__ $#".} = unsafeaddr `arg`

macro staticFor*(name: untyped; count: static int; body: untyped): untyped =
  result = newStmtList()
  for i in 0 ..< count:
    result.add quote do:
      block:
        const `name` = `i`
        `body`
  
  # return quote do:
  #   for `name` in 0 ..< `count`:
  #     `body`

# Helpers for mapping scalar operations to wide types
template makeUniversal*(T: typedesc, op: untyped): untyped =
  proc `op`*[U: T](value: U): U {.inline.} =
    staticFor(i, U.len):
      result[i] = `op`(value[i])

template makeUniversalBinary*(T: typedesc, op: untyped): untyped =
  proc `op`*[U: T](left, right: U): U {.inline.} =
    staticFor(i, U.len):
      result[i] = `op`(left[i], right[i])

# Mark the basic wide type as vector
template isVector*(_: type Wide): bool = true

# Indexing of wide types
func `[]`*[T; width: static int](wide: Wide[T, width]; index: int): T {.inline.} =
  wide.elements[index]

func `[]=`*[T; width: static int](wide: var Wide[T, width]; index: int; value: T) {.inline.} =
  wide.elements[index] = value

# Supported operations for all types marked as vector
makeUniversal(SomeVector, `-`)
makeUniversalBinary(SomeVector, `+`)
makeUniversalBinary(SomeVector, `-`)
makeUniversalBinary(SomeVector, `*`)
makeUniversalBinary(SomeVector, `/`)
makeUniversalBinary(SomeVector, `div`)

makeUniversal(SomeVector, `not`)
makeUniversalBinary(SomeVector, `and`)
makeUniversalBinary(SomeVector, `or`)
makeUniversalBinary(SomeVector, `xor`)

makeUniversal(SomeVector, abs)
makeUniversal(SomeVector, floor)
makeUniversal(SomeVector, ceil)
makeUniversal(SomeVector, sqrt)
makeUniversal(SomeVector, exp)
makeUniversal(SomeVector, tanh)
makeUniversalBinary(SomeVector, pow)
makeUniversalBinary(SomeVector, clamp)

# These cannot be defined using the concept, since they would conflict with the generic version in the system module
makeUniversalBinary(Wide, min)
makeUniversalBinary(Wide, max)

proc clamp*[T: SomeVector](self, min, max: T): T {.noinit, inline.} =
  staticFor(i, T.len):
    result[i] = clamp(self[i], min[i], max[i])

func `*` *[T: SomeVector](left: T; right: T.T): T {.inline.} =
  staticFor(i, T.len):
    result[i] = left[i] * right

template `/` *[T: SomeVector](left: T; right: T.T): T =
  left * (T.T)1 / right

template `+=` *(left: var SomeVector; right: SomeVector) = left = left + right
template `-=` *(left: var SomeVector; right: SomeVector) = left = left - right
template `*=` *(left: var SomeVector; right: SomeVector) = left = left * right
template `/=` *(left: var SomeVector; right: SomeVector) = left = left / right

template `*=` *(left: var SomeVector; right: SomeVector.scalarType) = left = left * right
template `/=` *(left: var SomeVector; right: SomeVector.scalarType) = left = left / right

type
  VectorizedType = object
    scalarType: NimNode
    wideType: NimNode
    width: int
    getLane: NimNode
    setLane: NimNode

  WideBuilderContext = object
    generatedTypes: seq[NimNode]
    generatedProcs: seq[NimNode]

var vectorizedTypes {.compileTime.}: seq[VectorizedType]

proc generateWideType(T: NimNode): NimNode

template wide*(T: typedesc[Vectorizable]): untyped =
  wideImpl(T)

template scalarType*(T: typedesc AnyWide): typedesc =
  scalarTypeImpl(T)

template laneCount*(T: typedesc AnyWide): int =
  laneCountImpl(T)

template getLane*(self: AnyWide; index: int): untyped =
  self.getLaneImpl(index)

template setLane*(self: var AnyWide; index: int; value: untyped): untyped =
  self.setLaneImpl(index, value)

macro wide*(T: typedesc[not Vectorizable]): untyped =
  nnkTypeOfExpr.newTree generateWideType(T.getTypeInst())

macro scalarType*(T: typedesc[not AnyWide]): untyped =
  for typeInfo in vectorizedTypes:
    # TODO: getTypeInst are not the same?
    if T.getTypeInst()[1].getType().sameType(typeInfo.wideType.getType()):
      return typeinfo.scalarType

macro laneCount*(T: typedesc[not AnyWide]): untyped =
  for typeInfo in vectorizedTypes:
    # TODO: getTypeInst are not the same?
    if T.getTypeInst()[1].getType().sameType(typeInfo.wideType.getType()):
      return newLit(typeInfo.width)

macro getLane*(self: not AnyWide; index: int): untyped =
  for typeInfo in vectorizedTypes:
    if self.getType().sameType(typeInfo.wideType.getType()):
      return newCall(typeInfo.getLane, self, index)
  error("No getLane proc generated for complex type", self)

macro setLane*(self: var (not AnyWide); index: int; value: untyped): untyped =
  for typeInfo in vectorizedTypes:
    # TODO: getTypeInst are not the same?
    if self.getType().sameType(typeInfo.wideType.getType()):
      return newCall(typeInfo.setLane, self, index, value)
  error("No setLane proc generated for complex type", self)

# Vectorized version of primitive types
template isVectorizable*(T: type TrivialScalar): bool = true
template wideImpl*(T: type TrivialScalar): untyped = Wide[T, 4]
template scalarTypeImpl*[T; width: static int](t: type Wide[T, width]): untyped = T
template laneCountImpl*[T; width: static int](t: type Wide[T, width]): int = width
template getLaneImpl*[T; width: static int](wide: Wide[T, width]; index: int): T = wide.elements[index]
template setLaneImpl*[T; width: static int](wide: var Wide[T, width]; index: int; value: T) = wide.elements[index] = value

# Vectorized version of arrays
# TODO: Constraining size to `static int`, or constrainting T to `SomeWide` seems to cause issues here
template isVectorizable*(T: type array): bool = true
template wideImpl*[T; size: static int](t: typedesc[array[size, T]]): untyped =
  type E = wide(typeof(T)) # Using the type expression directly causes 'type expected' 
  array[size, E]
template scalarTypeImpl*[size; T](t: type array[size, T]): untyped = array[size, T.scalarType]
template laneCountImpl*[size; T](t: type array[size, T]): int = T.laneCount

func getLaneImpl*[size; T](wide: array[size, T]; laneIndex: int): auto {.inline.} =
  var r: array[size, T.scalarType] # If this is the result type directly, we get "Error: cannot generate VM code for" when this calls the macro version
  for i in 0 ..< wide.len:
    r[i] = wide[i].getLane(laneIndex)
  return r

func setLaneImpl*[size; T; S](wide: var array[size, T]; laneIndex: int; value: array[size, S]) {.inline.} =
  when S isnot T.scalarType: {.error.} # Using this in the signature directly causes "Error: cannot generate VM code for" when this calls the macro version
  for i in 0 ..< wide.len:
    wide[i].setLane(laneIndex, value[i])

# Common operations on vectorized types
func gather*[T; width: static int](self: var SomeWide[T, width]; args: varargs[T]) {.inline.} =
  for i, value in pairs(args):
    self.setLane(i, value)

func scatter*(wide: SomeWide; args: var openarray[SomeWide.T]) {.inline.} =
  # var varargs is not supported
  for laneIndex, value in mpairs(args):
    value = wide.getLane(laneIndex)

func broadcast*(wide: var SomeWide; value: SomeWide.T) {.inline.} =
  for laneIndex in 0 ..< SomeWide.laneCount:
    wide.setLane(laneIndex, value)

iterator lanes*(wide: SomeWide): SomeWide.T {.inline.} =
  for laneIndex in 0..<SomeWide.laneCount:
    yield wide.getLane(laneIndex)

func equals*[T; laneCount: static int](left, right: Wide[T, laneCount]): Wide[bool, laneCount] {.inline.} =
  staticFor(i, laneCount):
    result.setLane(i, left.getLane(i) == right.getLane(i))

func `<=`*[T; laneCount: static int](left, right: Wide[T, laneCount]): Wide[bool, laneCount] {.inline.} =
  staticFor(i, laneCount):
    result.setLane(i, left.getLane(i) <= right.getLane(i))

func `<`*[T; laneCount: static int](left, right: Wide[T, laneCount]): Wide[bool, laneCount] {.inline.} =
  staticFor(i, laneCount):
    result.setLane(i, left.getLane(i) < right.getLane(i))

func `and`*[T: bool | SomeInteger, laneCount: static int](left, right: Wide[T, laneCount]): Wide[T, laneCount] {.inline.} =
  staticFor(i, laneCount):
    result.setLane(i, left.getLane(i) and right.getLane(i))

func `or`*[T: bool | SomeInteger, laneCount: static int](left, right: Wide[T, laneCount]): Wide[T, laneCount] {.inline.} =
  staticFor(i, laneCount):
    result.setLane(i, left.getLane(i) and right.getLane(i))

func `xor`*[T: bool | SomeInteger, laneCount: static int](left, right: Wide[T, laneCount]): Wide[T, laneCount] {.inline.} =
  staticFor(i, laneCount):
    result.setLane(i, left.getLane(i) xor right.getLane(i))

func `not`*[T: bool | SomeInteger, laneCount: static int](value: Wide[T, laneCount]): Wide[T, laneCount] {.inline.} =
  staticFor(i, laneCount):
    result.setLane(i, not value.getLane(i))
    
func select*[T; width: static int](mask: Wide[SomeInteger, width]; a, b: SomeWide[T, width]): SomeWide[T, width] {.inline.} =
  static: assert sizeof(SomeInteger) == sizeof(T)

  var r: type(a)
  staticFor(i, width):
    let value = (mask[i] and cast[SomeInteger](a.getLane(i))) or (not mask[i] and cast[SomeInteger](b.getLane(i)))
    r.setLane(i, cast[T](value)) # TODO: Why does this not work directly on result?
  return r

func select*[T; width: static int](condition: Wide[bool, width]; a, b: SomeWide[T, width]): SomeWide[T, width] {.inline.} =
  var r: type(a)
  staticFor(i, width):
    let value = if condition.getLane(i): a.getLane(i) else: b.getLane(i)
    r.setLane(i, value) # TODO: Why does this not work directly on result?
  return r

proc replaceSyms(node: NimNode): NimNode =
  case node.kind:
    of nnkSym, nnkOpenSymChoice, nnkClosedSymChoice:
      return ident($node)
    else:
      result = node.copy()
      for i in 0 ..< node.len:
        result[i] = result[i].replaceSyms()

proc makeWideComplexType(context: var WideBuilderContext; T: NimNode): VectorizedType {.compileTime.} =
  
  var wideGenericParams = newEmptyNode()

  let
    scalarTypeName = T
    scalarImpl = T.getTypeImpl()
  
  # TODO: Handle refs, base types, etc.
  scalarImpl.expectKind({ nnkObjectTy, nnkTupleTy })
  var fields = scalarImpl
  if scalarImpl.kind == nnkObjectTy:
    fields = scalarImpl[2]

  var
    recList = nnkRecList.newTree(newEmptyNode(), newEmptyNode())
    getters = newStmtList()
    setters = newStmtList()
    selfSym = ident("self")
    laneIndexSym = ident("laneIndex")
    resultSym = ident("result")
    valueSym = ident("value")

  # Iterate field declarations of the scalar type
  for fieldDefs in fields:
    fieldDefs.expectKind(nnkIdentDefs)
    fieldDefs.expectMinLen(2)

    # Copy over all identifiers, including visibility and pragmas
    var newFieldDefs = nnkIdentDefs.newNimNode()
    for i in 0 ..< fieldDefs.len - 2:

      # Create a copy of each field declaration
      let fieldDef = fieldDefs[i]
      #let (newFieldDef, fieldIdent) = fieldDef.copyFieldDef()
      let newFieldDef = fieldDef.replaceSyms()
      let fieldIdent = ident($newFieldDef)

      # TODO: Match the original postfix
      newFieldDefs.add(nnkPostfix.newTree(ident"*", newFieldDef))

      # Generate a lane getter and setter for each field
      getters.add(quote do: `resultSym`.`fieldIdent` = `selfSym`.`fieldIdent`.getLane(`laneIndexSym`))
      setters.add(quote do: `selfSym`.`fieldIdent`.setLane(`laneIndexSym`, `valueSym`.`fieldIdent`))

    # Vectorize the field type
    let fieldType = fieldDefs[^2]
    let newFieldType = quote do: wide(typeof(`fieldType`))
    newFieldDefs.add(newFieldType)

    newFieldDefs.add(newEmptyNode())

    # Add to the record
    recList.add(newFieldDefs)

  # Create a new symbol for the type and add it to the list
  # of known vectorized types
  let
    symbol = genSym(nskType, scalarTypeName.repr & "_Wide")
    getLane = genSym(nskFunc)
    setLane = genSym(nskFunc)

  result = VectorizedType(
    scalarType: scalarTypeName,
    width: 4,
    wideType: symbol,
    getLane: getLane,
    setLane: setLane)

  vectorizedTypes.add(result)

  context.generatedProcs.add(quote do:
    func `getLane`(`selfSym`: `symbol`; `laneIndexSym`: int): `scalarTypeName` {.inline.} = `getters`
    func `setLane`(`selfSym`: var `symbol`; `laneIndexSym`: int; `valueSym`: `scalarTypeName`) {.inline.} = `setters`
  )
  
  # Create the definition of the vectorized type
  context.generatedTypes.add(nnkTypeDef.newTree(
    symbol,
    wideGenericParams,
    nnkObjectTy.newTree(
      newEmptyNode(),
      newEmptyNode(),
      recList
    )
  ))

proc makeWideTypeRecursive(context: var WideBuilderContext; T: NimNode): VectorizedType {.compileTime.} =

  for vectorizedType in vectorizedTypes:
    # TODO: Check: We don't ever register typedescs, but nodes sometimes change typekind! For now we strip it again...
    var knownType = vectorizedType.scalarType
    if knownType.typekind == ntyTypeDesc:
      knownType = knownType.gettypeInst()[1]

    if T.sameType(knownType) and
      vectorizedType.width == 4:
      #echo "Reused type: " & (repr vectorizedType.scalarType) & " --> " & (repr vectorizedType.wideType)
      return vectorizedType

  case T.typeKind:
    of ntyGenericInvocation:
      # We need a concrete type for vectorization. Otherwise the vector-width can't be inferred.
      error("'wide' only supports concrete types.", T)

    of ntyGenericInst, ntyObject, ntyTuple: # ntyGenericBody
      # T should be a symbol, bracket expr, etc. Expanding it with getTypeImpl will yield a nnkObjectTy, etc.
      return context.makeWideComplexType(T)

    # TODO: Should distinct introspect object types? Should it handle trivial types differently?
    #of ntyDistinct, ntyEnum, ntyFloat, ...:
    else:
      error("Don't know how to vectorize type: " & T.repr, T)

proc generateWideType(T: NimNode): NimNode =
  ## Create a vectorized version of a type, used to convert code to structure-of-array form.
  
  var context: WideBuilderContext
  let rootType = context.makeWideTypeRecursive(T[1])

  result = newStmtList(
    newStmtList(nnkTypeSection.newTree(context.generatedTypes)),
    newStmtList(context.generatedProcs),
    rootType.wideType)

when isMainModule:
  # Test super scalar concept
  var f1, f2, f3: float
  var fa: array[4, float]
  f1 = 1.0
  var w, v: Wide[float, 4]
  w.broadcast(f1)
  echo w.getLane(1)
  echo w[1]
  w.gather(f3, f2, 2.0)
  w.scatter(fa)
  echo fa[2]
  for x in w.lanes: discard

  echo w + v

  assert (wide float) is SomeVector

  echo array[10, Wide[float, 4]].scalarType

  echo Wide[float, 4] is SomeWide
  echo array[10, Wide[float, 4]] is SomeWide
  echo array[10, float] isnot SomeWide
  echo Wide[float, 4] is AnyWide
  echo array[10, Wide[float, 4]] is AnyWide
  echo array[10, float] isnot AnyWide

  echo float is Vectorizable
  echo array[4, float] is Vectorizable

  # Test primitive type vectorization
  assert (wide float) is Wide

  # Test array vectorization and concept
  var a: wide array[4, float]
  #echo a.getLane(0)

  # Test complex type vectorization
  type
    Bar = object
      value*: uint64

    Foo = object
      value*, value2: int
      fValue* : float
      #tValue*: Time
      #sValue*: string
      rValue*: Bar
      aValue: array[2, Bar]

    WideBar = wide Bar

    WideFoo = wide Foo

  assert Bar isnot Vectorizable
  assert Foo isnot Vectorizable
  
  assert (wide float) is SomeWide
  assert (wide float) is SomeWide[float, 4]
  assert array[10, WideFoo] is AnyWide

  # Test type reuse
  assert WideFoo is wide Foo
  assert WideFoo.rValue is WideBar

  echo WideBar.laneCount
  echo WideFoo.scalarType.name

  var x: WideFoo
  var xa: array[10, WideBar]
  var xs: typeof(xa).scalarType
  x.setLane(0, Foo())
  discard x.getLane(0)
  xa.setLane(0, xs)
  discard xa.getLaneImpl(0)

  # # Test logical ops
  let b0 = w > v
  let b1 = not b0
  let wv = select(b1, w, v)
  echo b0
  echo wv
