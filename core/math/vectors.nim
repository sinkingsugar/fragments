import macros, strutils, typetraits, tables

#var types {.compileTime.} = newTable[NimNode, NimSym]()

type
  Wide*[T] = object
    elements*: array[4, T]

#   SuperScalar* = concept v, type V
#     type S = V.scalarType
#     v.getLane(int) is S
#     v.setLane(int, S)

# template scalarType*[T](t: typedesc[Wide[T]]): typedesc = T
# func getLane*(wide: Wide; laneIndex: int): Wide.T {.inline.} = wide.elements[laneIndex]
# func setLane*(wide: var Wide; laneIndex: int; value: Wide.T) {.inline.} = wide.elements[laneIndex] = value

# func gather*(wide: var SuperScalar; args: varargs[SuperScalar.scalarType]) =
#   for laneIndex, value in pairs(args):
#     wide.setLane(laneIndex, value)

# func scatter*(wide: SuperScalar; args: varargs[var SuperScalar.scalarType]) =
#   for laneIndex, value in pairs(args):
#     value = wide.getLane(laneIndex)

proc makeWideTypeImpl(T: NimNode): NimNode {.compileTime.} =
  
  # echo T.typeKind
  # echo T.getType.typeKind
  # echo astGenRepr(T)
  # echo astGenRepr(T.getType)
  # echo astGenRepr(T.getTypeInst[1])
  # echo astGenRepr(T.getTypeInst[1].getType)

  # T.getType().typeKind == ntyTypeDesc
  #var t = T.getTypeInst[1].getType
  
  case T.typeKind:
    of ntyTypeDesc:
      return makeWideTypeImpl(T.getTypeInst[1])
      
    of ntyArray:
      # Array types are a bracket expression of 'array', a range, and the element type
      var wideType = T.getTypeInst.copyNimTree()     
      var elementType = wideType[2]
      #wideType[2] = nnkBracketExpr.newTree(bindSym"Wide", elementType)
      wideType[2] = makeWideTypeImpl(elementType)
      return wideType

    of ntyObject:
      case T.kind:
        of nnkSym:
          let scalarTypeDefinition = T.symbol.getImpl()

          var recList = nnkRecList.newNimNode()
          recList.add(newEmptyNode())
          recList.add(newEmptyNode())

          for fieldDefs in scalarTypeDefinition[2][2]:
            fieldDefs.expectKind(nnkIdentDefs)
            fieldDefs.expectMinLen(2)

            # Copy over all identifiers, including visibility and pragmas
            var newFieldDefs = nnkIdentDefs.newNimNode()
            for i in 0 ..< fieldDefs.len - 2:
              let fieldDef = fieldDefs[i]
              fieldDef.expectKind({nnkIdent, nnkPragmaExpr, nnkPostfix})
              newFieldDefs.add(fieldDef.copyNimTree())

            # Vectorize the field type
            let fieldType = fieldDefs[^2]
            let newFieldType = makeWideTypeImpl(fieldType)
            newFieldDefs.add(newFieldType)

            newFieldDefs.add(newEmptyNode())

            # Add to the record
            recList.add(newFieldDefs)

          # Create a new symbol for the type
          var symbol = genSym(nskType)

          var wideTypeDefinition = nnkTypeDef.newTree(
            symbol,
            newEmptyNode(),
            nnkObjectTy.newTree(
              newEmptyNode(),
              newEmptyNode(),
              recList
            )
          )

          return newStmtList(
            nnkTypeSection.newTree(
              wideTypeDefinition
            ),
            symbol
          )
        else: discard

    of ntyFloat:
      return quote do: Wide[float]
    of ntyFloat32:
      return quote do: Wide[float32]
    of ntyFloat64:
      return quote do: Wide[float64]
    of ntyFloat128:
      return quote do: Wide[float128]      

    of ntyBool:
      return quote do: Wide[bool]
    of ntyChar:
      return quote do: Wide[char]

    of ntyInt:
      return quote do: Wide[int]
    of ntyInt8:
      return quote do: Wide[int8]
    of ntyInt16:
      return quote do: Wide[int16]
    of ntyInt32:
      return quote do: Wide[int32]
    of ntyInt64:
      return quote do: Wide[int64]

    of ntyUInt:
      return quote do: Wide[uint]
    of ntyUInt8:
      return quote do: Wide[uint8]
    of ntyUInt16:
      return quote do: Wide[uint16]
    of ntyUInt32:
      return quote do: Wide[uint32]
    of ntyUInt64:
      return quote do: Wide[uint64]

    else: error("Can't vectorize type " & $T)
      # return nnkBracketExpr.newTree(ident"Wide", T)
      # return quote do:
      #   array[4, `T`]
      #   Wide[`T`]

macro makeWideType(T: typed): untyped =
  result = makeWideTypeImpl(T)
  #echo astGenRepr(result)
  
macro wide*(T: typedesc): untyped =
  makeWideTypeImpl(T.getType)

when isMainModule:
  # var f1, f2, f3: float
  # var w: Wide[float]
  # w.gather(f1, f2, f3)
  # w.scatter(f3, f2, f1)

  type
    Bar = object
      value*: uint64

    Foo {.importc.} = object
      value*, value2: int
      fValue* : float
      #tValue*: Time
      #sValue*: string
      rValue*: Bar

  echo (wide float).name
  echo (wide array[4, float]).name

  # type WideFoo = makeWideType(Foo)
  # var f: WideFoo
  # echo type(bar.fvalue).name

  # echo f.value