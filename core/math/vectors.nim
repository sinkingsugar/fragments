import macros, strutils, typetraits, tables

#var types {.compileTime.} = newTable[NimNode, NimSym]()

type
  Wide*[T] = object
    elements*: array[4, T]

  SuperScalar* = concept v, type V
    type T = V.scalarType
    const width: int = V.laneCount
    v.getLane(int) is T
    v.setLane(int, T)

# Vectorized version of primitive types
template scalarType*[T](t: typedesc[Wide[T]]): typedesc = T
template laneCount*[T](t: typedesc[Wide[T]]): int = 4
func getLane*(wide: Wide; laneIndex: int): Wide.T {.inline.} = wide.elements[laneIndex]
func setLane*(wide: var Wide; laneIndex: int; value: Wide.T) {.inline.} = wide.elements[laneIndex] = value

# Vectorized version of arrays
template scalarType*[size: static[int]](t: typedesc[array[size, SuperScalar]]): typedesc = array[size, SuperScalar.T]
template laneCount*[size: static[int]](t: typedesc[array[size, SuperScalar]]): int = SuperScalar.width
func getLane*[size: static[int]; S: SuperScalar](wide: array[size, S]; laneIndex: int): array[size, S.T] {.inline.} =
  for i in 0..<size:
    result[i] = wide[i].getLane(laneIndex)
func setLane*[size: static[int]; S: SuperScalar](wide: array[size, S]; laneIndex: int, value: array[size, S.T]) {.inline.} =
  for i in 0..<size:
    wide[i].setLane(laneIndex, value[i])

# Common operations on vectorized types
func gather*(wide: var SuperScalar; args: varargs[SuperScalar.T]) =
  for laneIndex, value in pairs(args):
    wide.setLane(laneIndex, value)

func scatter*(wide: SuperScalar; args: var openarray[SuperScalar.T]) =
  # var varargs is not supported
  for laneIndex, value in mpairs(args):
    value = wide.getLane(laneIndex)

# iterator lanes*(wide: SuperScalar): SuperScalar.T =
#   for laneIndex in SuperScalar.width:
#     yield getLane(laneIndex)

var vectorizedTypes {.compileTime.} = newSeq[tuple[scalar, wide: NimNode]]()

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
            if T.sameType(vectorizedType.scalar):
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

          let scalarTypeDefinition = T.symbol.getImpl()

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
          vectorizedTypes.add((T, symbol))

          # Satisfy the SuperScalar concept and generate lane accessors
          generatedProcs.add(quote do:
            template scalarType*(t: typedesc[`symbol`]): typedesc = `T`
            template laneCount*(t: typedesc[`symbol`]): int = 4
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
      let name = ident($T)
      return quote do:
        Wide[`name`]
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
  T.getType().makeWideTypeImpl()

static:
  # Test super scalar concept
  var f1, f2, f3: float
  var fa: array[Wide[float].laneCount, float]
  f1 = 1.0
  var w: Wide[float]
  w.gather(f3, f2, f1)
  w.scatter(fa)
  echo fa[2]
  #for x in w.lanes: discard

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

    Foo {.importc.} = object
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
