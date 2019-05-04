import macros, strutils, typetraits, tables, math

type
  Wide*[T; width: static int] = object
    ## A super-scaler primitive type, used in vectorized code
    elements*: array[width, T]

  SomeWide*[T; width: static int] = concept v, var m, type V
    ## The contract for super-scalar versions of complex types
    V.scalarType is T
    V.laneCount == width
    v.getLane(int) is T
    m.setLane(int, T)
    
  SomeVector* = concept type T of SomeWide
    ## A wide type marked as vector. Vectors automatically support some basic operations.
    T.isVector()

template restrict*(arg: untyped): untyped =
  when defined(vcc):
    let `arg` {.inject, codegenDecl: "$# __restrict $#".} = unsafeaddr `arg`
  else:
    let `arg` {.inject, codegenDecl: "$# __restrict__ $#".} = unsafeaddr `arg`

# Helpers for mapping scalar operations to wide types
template makeUniversal*(T: typedesc, op: untyped): untyped =
  proc `op`*[U: T](value: U): U {.inline.} =
    for i in 0 ..< U.laneCount:
      result.setLane(i, `op`(value.getLane(i)))

template makeUniversalBinary*(T: typedesc, op: untyped): untyped =
  proc `op`*[U: T](left, right: U): U {.inline.} =
    for i in 0 ..< U.laneCount:
      result.setLane(i, `op`(left.getLane(i), right.getLane(i)))

# Mark the basic wide type as vector
template isVector*(_: type Wide): bool = true

# Supported operations for all types marked as vector
makeUniversal(SomeVector, `-`)
makeUniversalBinary(SomeVector, `+`)
makeUniversalBinary(SomeVector, `-`)
makeUniversalBinary(SomeVector, `*`)
makeUniversalBinary(SomeVector, `/`)

makeUniversal(SomeVector, `not`)
makeUniversalBinary(SomeVector, `and`)
makeUniversalBinary(SomeVector, `or`)
makeUniversalBinary(SomeVector, `xor`)

makeUniversal(SomeVector, abs)
makeUniversal(SomeVector, floor)
makeUniversal(SomeVector, ceil)
makeUniversal(SomeVector, exp)
makeUniversal(SomeVector, tanh)
makeUniversalBinary(SomeVector, pow)
makeUniversalBinary(SomeVector, clamp)

# These cannot be defined using the concept, since they would conflict with the generic version in the system module
makeUniversalBinary(Wide, min)
makeUniversalBinary(Wide, max)

proc clamp*[T: SomeVector](self, min, max: T): T {.noinit, inline.} =
  for i in 0 ..< T.laneCount():
    result.setLane(i, clamp(self.getLane(i), min.getLane(i), max.getLane(i)))

func `*` *[T: SomeVector](left: T; right: T.T): T {.inline.} =
  restrict(left)
  for i in 0 ..< T.laneCount():
    result.setLane(i, left[].getLane(i) * right)

template `/` *[T: SomeVector](left: T; right: T.T): T =
  left * (T.T)1 / right

template `+=` *(left: var SomeVector; right: SomeVector) = left = left + right
template `-=` *(left: var SomeVector; right: SomeVector) = left = left - right
template `*=` *(left: var SomeVector; right: SomeVector) = left = left * right
template `/=` *(left: var SomeVector; right: SomeVector) = left = left / right

template `*=` *(left: var SomeVector; right: SomeVector.scalarType) = left = left * right
template `/=` *(left: var SomeVector; right: SomeVector.scalarType) = left = left / right

# Vectorized version of primitive types
template scalarType*[T; width: static int](t: type Wide[T, width]): typedesc = T
template laneCount*[T; width: static int](t: type Wide[T, width]): int = width
template getLane*[T; width: static int](wide: Wide[T, width]; index: int): T = wide.elements[index]
template setLane*[T; width: static int](wide: var Wide[T, width]; index: int; value: T) = wide.elements[index] = value

# Vectorized version of arrays
template scalarType*[size: static int](t: type array[size, SomeWide]): typedesc = array[size, SomeWide.T]
template laneCount*[size: static int](t: type array[size, SomeWide]): int = SomeWide.width
func getLane*[size: static int](wide: array[size, SomeWide]; laneIndex: int): array[size, SomeWide.T] {.inline.} =
  for i in 0..<size:
    result[i] = wide[i].getLane(laneIndex)
func setLane*[size: static int](wide: var array[size, SomeWide]; laneIndex: int, value: array[size, SomeWide.T]) {.inline.} =
  for i in 0..<size:
    wide[i].setLane(laneIndex, value[i])

# Common operations on vectorized types
func gather*(wide: var SomeWide; args: varargs[SomeWide.T]) {.inline.} =
  for laneIndex, value in pairs(args):
    wide.setLane(laneIndex, value)

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

# Indexing of wide types
template `[]`*[T; width: static int](wide: Wide[T, width]; index: int): T =
  wide.elements[index]

template `[]=`*[T; width: static int](wide: var Wide[T, width]; index: int; value: T) =
  wide.elements[index] = value

func equals*(left, right: SomeWide): Wide[bool, SomeWide.laneCount] {.inline.} =
  for i in 0 ..< SomeWide.laneCount:
    result.setLane(i, left.getLane(i) == right.getLane(i))

func `<=`*(left, right: SomeWide): Wide[bool, SomeWide.laneCount] {.inline.} =
  for i in 0 ..< SomeWide.laneCount:
    result.setLane(i, left.getLane(i) <= right.getLane(i))

func `<`*(left, right: SomeWide): Wide[bool, SomeWide.laneCount] {.inline.} =
  for i in 0 ..< SomeWide.laneCount:
    result.setLane(i, left.getLane(i) < right.getLane(i))

func `and`*[laneCount: static int](left, right: Wide[bool, laneCount]): Wide[bool, laneCount] {.inline.} =
  for i in 0 ..< laneCount:
    result.setLane(i, left.getLane(i) and right.getLane(i))

func `or`*[laneCount: static int](left, right: Wide[bool, laneCount]): Wide[bool, laneCount] {.inline.} =
  for i in 0 ..< laneCount:
    result.setLane(i, left.getLane(i) and right.getLane(i))

func `xor`*[laneCount: static int](left, right: Wide[bool, laneCount]): Wide[bool, laneCount] {.inline.} =
  for i in 0 ..< laneCount:
    result.setLane(i, left.getLane(i) xor right.getLane(i))

func `not`*[laneCount: static int](value: Wide[bool, laneCount]): Wide[bool, laneCount] {.inline.} =
  for i in 0 ..< laneCount:
    result.setLane(i, not value.getLane(i))
    
func select*[T; width: static int](condition: Wide[bool, width]; a, b: SomeWide[T, width]): SomeWide[T, width] {.inline.} =
  #staticFor(i, T.laneCount):
  var r: type(a)
  for i in 0 ..< width:
    let value = if condition.getLane(i): a.getLane(i) else: b.getLane(i)
    r.setLane(i, value) # TODO: Why does this not work directly on result?
  return r

var vectorizedTypes {.compileTime.}: seq[tuple[scalar: NimNode; width: int; wide: NimNode]]

type
  WideBuilderContext = object
    generatedTypes: seq[NimNode]
    generatedProcs: seq[NimNode]
    symbolMap: seq[(NimNode, NimNode)]
    isLocal: bool

func replaceSymbols(node: NimNode; context: var WideBuilderContext): NimNode =
  if node.kind == nnkSym:
    for sym in context.symbolMap:
      if node == sym[0]:
        return sym[1]
  else:
    for i in 0 ..< node.len:
      node[i] = node[i].replaceSymbols(context)

  return node

proc makeWideTypeRecursive(context: var WideBuilderContext; T: NimNode): NimNode {.compileTime.}

proc makeWideComplexType(context: var WideBuilderContext; T: NimNode): NimNode {.compileTime.} =
  
  var
    scalarTypeName: NimNode
    scalarImpl: NimNode
    wideGenericParams = newEmptyNode()

  # For a typedef, we refer to the scalarType with it's symbol. We also take it's implementation from the typedef.
  # This is the case for generic type instantiations.
  if T.kind == nnkTypeDef:
    scalarTypeName = T[0]
    scalarImpl = T[2]

    if T[1].kind != nnkEmpty:
      wideGenericParams = nnkGenericParams.newTree()
      for scalarParam in T[1]:
        scalarParam.expectKind(nnkSym) # TODO: Why is this not nnkIdentDefs?
        let t = quote do:
          type(`scalarParam`)
        let wideParam = genSym(nskGenericParam, scalarParam.repr)
        wideGenericParams.add(nnkIdentDefs.newTree(wideParam, t, newEmptyNode()))
        context.symbolMap.add((scalarParam, wideParam))

  # Otherwise, T might be a symbol or another simple type expression. We refer to it using this expression.
  # We get a nnkObjectType/etc. with getTypeImpl()
  else:
    scalarTypeName = T
    scalarImpl = T.getTypeImpl()
    #wideGenericParams = newEmptyNode()
  
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

      proc copyFieldDef(node: NimNode): tuple[root, ident: NimNode] =
        fieldDef.expectKind({nnkIdent, nnkSym, nnkPragmaExpr, nnkPostfix})
        case node.kind:
          of nnkIdent:
            return (node, node)
          of nnkSym:
            let ident = ident($node)
            return (ident, ident)
          of nnkPostfix:
            result.root = node.copy()
            let (root, ident) = result.root[1].copyFieldDef()
            result.root[1] = root
            result.ident = ident
          of nnkPragmaExpr:
            result.root = node.copy()
            let (root, ident) = result.root[0].copyFieldDef()
            result.root[0] = root
            result.ident = ident
          else: discard

      let (newFieldDef, fieldIdent) = fieldDef.copyFieldDef()

      newFieldDefs.add(newFieldDef)

      # Generate a lane getter and setter for each field
      getters.add(quote do: `resultSym`.`fieldIdent` = `selfSym`.`fieldIdent`.getLane(`laneIndexSym`))
      setters.add(quote do: `selfSym`.`fieldIdent`.setLane(`laneIndexSym`, `valueSym`.`fieldIdent`))

    # Vectorize the field type
    let fieldType = fieldDefs[^2]
    let newFieldType = context.makeWideTypeRecursive(fieldType)
    newFieldDefs.add(newFieldType)

    newFieldDefs.add(newEmptyNode())

    # Add to the record
    recList.add(newFieldDefs)

  # Create a new symbol for the type and add it to the list
  # of known vectorized types
  var symbol = genSym(nskType, scalarTypeName.repr & "_Wide")
  vectorizedTypes.add((scalarTypeName, 4, symbol))

  if context.isLocal:
    context.generatedProcs.add(quote do:
      template scalarType(t: type `symbol`): typedesc = `scalarTypeName`
      template laneCount(t: type `symbol`): int = 4
      func getLane(`selfSym`: `symbol`; `laneIndexSym`: int): `scalarTypeName` {.inline.} = `getters`
      func setLane(`selfSym`: var `symbol`; `laneIndexSym`: int; `valueSym`: `scalarTypeName`) {.inline.} = `setters`
      template isVector(_: type `symbol`): bool = `scalarTypeName` is SomeVector
    )
  else:
  context.generatedProcs.add(quote do:
    template scalarType*(t: type `symbol`): typedesc = `scalarTypeName`
    template laneCount*(t: type `symbol`): int = 4
    func getLane*(`selfSym`: `symbol`; `laneIndexSym`: int): `scalarTypeName` {.inline.} = `getters`
    func setLane*(`selfSym`: var `symbol`; `laneIndexSym`: int; `valueSym`: `scalarTypeName`) {.inline.} = `setters`
    template isVector*(_: type `symbol`): bool = `scalarTypeName` is SomeVector
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
  
  return symbol

proc makeWideTypeRecursive(context: var WideBuilderContext; T: NimNode): NimNode {.compileTime.} =

  for vectorizedType in vectorizedTypes:
    if T.sameType(vectorizedType.scalar) and
      vectorizedType.width == 4:
    #if T == vectorizedType.scalar:
      return vectorizedType.wide

  case T.typeKind:
    of ntyTypeDesc:
      # A typedesc is a backet expression, wrapping another type
      return context.makeWideTypeRecursive(T[1])

    of ntyGenericInvocation:
      # Generic invocations are bracket expressions. We vectorize the generic type and return a matching invocation of it.
      # We pass the full nnkTypeDef, so we can replace the generic params.
      result = T.copyNimTree().replaceSymbols(context)
      let scalarTypeDef = T[0].getImpl()
      let wideTypeSym = context.makeWideComplexType(scalarTypeDef)
      result[0] = wideTypeSym
      
    of ntyArray:
      # Array types are a bracket expression of 'array', a range, and the element type
      # We simply vectorize the element type
      #T.expectKind(nnkBracketExpr)
      result = T.getTypeInst.copyNimTree().replaceSymbols(context)
      var elementType = result[2]
      result[2] = context.makeWideTypeRecursive(elementType)

    of ntyGenericInst, ntyObject, ntyTuple:
      # T should be a symbol, bracket expr, etc. Expanding it with getTypeImpl will yield a nnkObjectTy, etc.
      return context.makeWideComplexType(T)

    # TODO: Should distinct introspect object types? Should it handle trivial types differently?
    #of ntyDistinct, ntyEnum, ntyFloat, ...:
    else:
      let
        name = ident($T)
        width = 4
      return quote do:
        Wide[`name`, `width`]
        #Wide[`T`]
        #array[4, `T`]

macro wide*(T: typedesc): untyped =
  ## Create a vectorized version of a type, used to convert code to structure-of-array form.
  
  let typeDesc = T.getTypeInst()
  var context: WideBuilderContext

  # Check the ancestor of the typedesc. If we are inside some proc we can't export any
  # generated symbols.
  var owner = typeDesc[0].owner
  while owner.kind == nnkSym:
    #echo astgenrepr owner, owner.symKind
    if owner.symKind in { nskProc, nskFunc, nskMethod, nskIterator, nskConverter }:
      context.isLocal = true
      break
    owner = owner.owner
  
  let rootType = context.makeWideTypeRecursive(typeDesc)

  result = newStmtList(nnkTypeSection.newTree(context.generatedTypes))
  result.add(context.generatedProcs)
  result.add(rootType)

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

  # Test primitive type vectorization
  echo (wide float).name

  # Test array vectorization and concept
  var a: wide array[4, float]
  echo type(a).name
  echo a.getLane(0)

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

  echo type(WideFoo.fvalue).name
  assert (wide float) is SomeWide
  assert (wide float) is SomeWide[float, 4]

  # Test type reuse
  var x: WideFoo
  var y: WideBar
  x.rValue = y

  echo WideBar.laneCount
  echo WideFoo.scalarType.name
  x.setLane(0, Foo())
  discard x.getLane(0)

  # # Test logical ops
  let b0 = w > v
  let b1 = not b0
  let wv = select(b1, w, v)
  echo b0
  echo wv
