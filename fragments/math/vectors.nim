import macros, strutils, typetraits, tables, math

type
  Wide*[T; width: static int] = object
    ## A super-scaler primitive type, used in vectorized code
    elements*: array[width, T]

  SomeWide* = concept v, var m, type V
    ## The contract for super-scalar versions of complex types
    type T = V.scalarType
    const width: int = V.laneCount
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
template mapInline*[T: SomeWide](value: T, op: untyped): untyped =
  restrict(value)
  for i in 0 ..< T.laneCount():
    result.setLane(i, op(value.getLane(i)))

template mapInlineBinary*[T: SomeWide](left, right: T, op: untyped): untyped =
  restrict(left)
  restrict(right)
  for i in 0 ..< T.laneCount():
    result.setLane(i, op(left.getLane(i), right.getLane(i)))

template makeUniversal*(T: typedesc, op: untyped): untyped =
  proc op*[U: T](value: U): U {.noinit, inline.} =
    mapInline(value, op)

template makeUniversalBinary*(T: typedesc, op: untyped): untyped =
  proc op*[U: T](left, right: U): U {.noinit, inline.} =
    mapInlineBinary(left, right, op)

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

func `*` *[T: SomeVector](left: T; right: T.T): T =
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
func getLane*(wide: Wide; laneIndex: int): Wide.T {.inline.} = wide.elements[laneIndex]
func setLane*(wide: var Wide; laneIndex: int; value: Wide.T) {.inline.} = wide.elements[laneIndex] = value

# Vectorized version of arrays
template scalarType*[size: static int](t: type array[size, SomeWide]): typedesc = array[size, SomeWide.T]
template laneCount*[size: static int](t: type array[size, SomeWide]): int = SomeWide.width
func getLane*[size: static int; S: SomeWide](wide: array[size, S]; laneIndex: int): array[size, S.T] {.inline.} =
  for i in 0..<size:
    result[i] = wide[i].getLane(laneIndex)
func setLane*[size: static int; S: SomeWide](wide: array[size, S]; laneIndex: int, value: array[size, S.T]) {.inline.} =
  for i in 0..<size:
    wide[i].setLane(laneIndex, value[i])

# Common operations on vectorized types
func gather*(wide: var SomeWide; args: varargs[SomeWide.T]) =
  for laneIndex, value in pairs(args):
    wide.setLane(laneIndex, value)

func scatter*(wide: SomeWide; args: var openarray[SomeWide.T]) =
  # var varargs is not supported
  for laneIndex, value in mpairs(args):
    value = wide.getLane(laneIndex)

func broadcast*(wide: var SomeWide; value: SomeWide.T) =
  for laneIndex in 0 ..< SomeWide.width:
    wide.setLane(laneIndex, value)

iterator lanes*(wide: SomeWide): SomeWide.T =
  for laneIndex in 0..<SomeWide.width:
    yield wide.getLane(laneIndex)

# Indexing of wide types
func `[]`*(wide: Wide; index: int): Wide.T =
  wide.getLane(index)

func `[]=`*(wide: var Wide; index: int; value: Wide.T) =
  wide.setLane(index, value)

func `equals`*(left, right: SomeWide): Wide[bool, SomeWide.laneCount] {.inline.} =
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
    
func select*[T: SomeWide](condition: Wide#[bool, T.laneCount]#; a, b: T): T {.inline.} =
  # TODO: Use SomeWide directly, or constraining Wide causes compiler errors
  for i in 0 ..< T.laneCount:
    let value = if condition.getLane(i): a.getLane(i) else: b.getLane(i)
    result.setLane(i, value)

var vectorizedTypes {.compileTime.} = newSeq[tuple[scalar: NimNode; width: int; wide: NimNode]]()

proc makeWideTypeRecursive(T: NimNode; generatedTypes, generatedProcs: var seq[NimNode]): NimNode {.compileTime.} =
  
  case T.typeKind:
    of ntyTypeDesc:
      return makeWideTypeRecursive(T.getTypeInst[1], generatedTypes, generatedProcs)
      
    of ntyArray:
      # Array types are a bracket expression of 'array', a range, and the element type
      var wideType = T.getTypeInst.copyNimTree()     
      var elementType = wideType[2]
      wideType[2] = makeWideTypeRecursive(elementType, generatedTypes, generatedProcs)
      return wideType

    of ntyObject:
      case T.kind:
        of nnkSym:
          for vectorizedType in vectorizedTypes:
            if T.sameType(vectorizedType.scalar) and
              vectorizedType.width == 4:
            #if T == vectorizedType.scalar:
              return vectorizedType.wide

          var
            recList = nnkRecList.newTree(newEmptyNode(), newEmptyNode())
            getters = newStmtList()
            setters = newStmtList()
            selfSym = ident("self")
            laneIndexSym = ident("laneIndex")
            resultSym = ident("result")
            valueSym = ident("value")

          let scalarTypeDefinition = T.getImpl()

          # Iterate field declarations of the scalar type
          for fieldDefs in scalarTypeDefinition[2][2]:
            fieldDefs.expectKind(nnkIdentDefs)
            fieldDefs.expectMinLen(2)

            # Copy over all identifiers, including visibility and pragmas
            var newFieldDefs = nnkIdentDefs.newNimNode()
            for i in 0 ..< fieldDefs.len - 2:

              # Create a copy of each field declaration
              let fieldDef = fieldDefs[i]
              fieldDef.expectKind({nnkIdent, nnkPragmaExpr, nnkPostfix})
              newFieldDefs.add(fieldDef.copyNimTree())

              # Get the field identifier
              var fieldIdent = fieldDef
              if fieldIdent.kind == nnkPragmaExpr:
                fieldIdent = fieldIdent[0]
              if fieldIdent.kind == nnkPostfix:
                fieldIdent = fieldIdent[1]

              # Generate a lane getter and setter for each field
              getters.add(quote do: `resultSym`.`fieldIdent` = `selfSym`.`fieldIdent`.getLane(`laneIndexSym`))
              setters.add(quote do: `selfSym`.`fieldIdent`.setLane(`laneIndexSym`, `valueSym`.`fieldIdent`))

            # Vectorize the field type
            let fieldType = fieldDefs[^2]
            let newFieldType = makeWideTypeRecursive(fieldType, generatedTypes, generatedProcs)
            newFieldDefs.add(newFieldType)

            newFieldDefs.add(newEmptyNode())

            # Add to the record
            recList.add(newFieldDefs)

          # Create a new symbol for the type and add it to the list
          # of known vectorized types
          var symbol = genSym(nskType)
          vectorizedTypes.add((T, 4, symbol))

          # Satisfy the SomeWide concept and generate lane accessors
          generatedProcs.add(quote do:
            template scalarType*(t: type `symbol`): typedesc = `T`
            template laneCount*(t: type `symbol`): int = 4
            func getLane*(`selfSym`: `symbol`; `laneIndexSym`: int): `T` {.inline.} = `getters`
            func setLane*(`selfSym`: var `symbol`; `laneIndexSym`: int; `valueSym`: `T`) {.inline.} = `setters`
          )

          # Create the definition of the vectorized type
          generatedTypes.add(nnkTypeDef.newTree(
            symbol,
            newEmptyNode(),
            nnkObjectTy.newTree(
              newEmptyNode(),
              newEmptyNode(),
              recList
            )
          ))
          
          return symbol

        else: discard
   
    else:
      let
        name = ident($T)
        width = 4
      return quote do:
        Wide[`name`, `width`]
        #Wide[`T`]
        #array[4, `T`]

proc makeWideTypeImpl(T: NimNode): NimNode {.compileTime.} =
  var generatedTypes = newSeq[NimNode]()
  var generatedProcs = newSeq[NimNode]()
  let rootType = makeWideTypeRecursive(T, generatedTypes, generatedProcs)

  result = newStmtList(nnkTypeSection.newTree(generatedTypes))
  result.add(generatedProcs)
  result.add(rootType)

macro wide*(T: typedesc): untyped =
  ## Create a vectorized version of a type, used to convert code to structure-of-array form.
  T.getType().makeWideTypeImpl()

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

    WideBar = wide Bar

    WideFoo = wide Foo

  echo type(WideFoo.fvalue).name

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