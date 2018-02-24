import macros, streams, math.common, options

{.experimental.}

const
  highBit = 0b1000_0000
  mask = 0b0111_1111

proc writePackedUInt*(stream: Stream; value: BiggestUInt) =
  var remaining = value
  while remaining >= highBit.uint64:
    echo (value or highBit).byte
    remaining = remaining shr 7
  stream.write(remaining.byte);

proc writePackedInt*(stream: Stream; value: BiggestInt) =
  stream.writePackedUInt(value.zigZagEncode().BiggestUInt)

proc readPackedUInt*(stream: Stream): BiggestUInt =
  var 
    shift = 0
    currentByte: byte

  while true:
    currentByte = stream.readUInt8()
    result = result or ((currentByte and mask) shl shift)
    shift += 7
    if (currentByte and highBit) == 0:
      break

proc readPackedInt*(stream: Stream): BiggestInt =
  var x = stream.readPackedUInt()
  return x.BiggestInt.zigZagDecode()

template serializable*(): untyped {.pragma.}

func isBlittable(t: NimNode): bool {.compileTime.} =
  case t.typeKind:
    of ntyString, ntyCString, ntyProc, ntyPtr, ntyPointer, ntyRef, ntySequence: return false
    of ntyTypeDesc: return t.getTypeInst()[1].isBlittable     
    of ntyDistinct: return t[1].isBlittable
    of ntyArray: return t.getTypeInst()[2].isBlittable
    of ntyObject, ntyTuple:
      let inst = t.getTypeInst()
      case inst.kind:
        of nnkSym:
          let typeDef = inst.symbol.getImpl()
          let fieldDefs = if t.typeKind == ntyObject: typeDef[2][2] else: typeDef[2] # Tuples have no recList
          for field in fieldDefs:
            if not field[^2].isBlittable:
              return false
          return true
        else: return true
    else: return true

func isBlittable(t: typedesc): bool {.compileTime.} =
  t.getType().isBlittable()

type
  SerializationContext = ref object
    stream*: Stream

  Serializer = ref object of RootObj

  # TODO: Override with shallow pragma
  Blittable* = concept type T
    T.isBlittable

  # Any type that can be derived from. Takes care of {.inheritable.} and {.final.} too.
  Inheritable* = concept type T
    type
      Base = T # Using T directly doesn't work for some reason
      Derived = object of Base

  Serializable* = concept v, var m
    v.serialize(SerializationContext)
    m.deserialize(SerializationContext)

method serialize(self: Serializer; instance: ref RootObj; context: SerializationContext) {.base.} = discard 
  
method deserialize(self: Serializer; instance: var ref RootObj; context: SerializationContext) {.base.} = discard

macro generateObjectSerializer(t: typedesc): untyped =
  let typeDef = t.getTypeInst[1].getTypeInst().symbol.getImpl()
  let fieldDefs = if t.getTypeInst[1].typeKind == ntyObject: typeDef[2][2] else: typeDef[2]

  let
    context = ident"context"
    value = ident"value"

  var
    serializers = newStmtList()
    deserializers = newStmtList()

  # Call base type serializer first
  # TODO: Handle refs?
  if t.getType().typeKind == ntyObject:
    let inherit = typeDef[2][1]
    if inherit.kind == nnkOfInherit:
      let baseType = inherit[0]
      assert baseType.kind == nnkIdent
      if not eqIdent(baseType, "RootObj"):
        serializers.add quote do: `baseType`(`value`).serialize(`context`)
        deserializers.add quote do: `baseType`(`value`).deserialize(`context`)

  # Serialize fields
  for fieldDef in fieldDefs:
    for i in 0..<fieldDefs.len - 2:
      var fieldIdent = fieldDef[i]
      if fieldIdent.kind == nnkPragmaExpr:
        fieldIdent = fieldIdent[0]
      fieldIdent = fieldIdent.basename

      serializers.add quote do: `value`.`fieldIdent`.serialize(`context`)
      deserializers.add quote do: `value`.`fieldIdent`.deserialize(`context`)

  result = quote do:
    proc serialize*(`value`: `t`; `context`: SerializationContext) = `serializers`
    proc deserialize*(`value`: var `t`; `context`: SerializationContext) = `deserializers`

# Options
proc serialize*(value: Option[Serializable]; context: SerializationContext) =
  if value.isNone:
    context.stream.write(false)
  else:
    context.stream.write(true)
    value.get().serialize(context)

proc deserialize*[T: Serializable](value: var Option[T]; context: SerializationContext) =
  let isSome = context.stream.readBool()
  if isSome:
    var temp: T
    temp.deserialize(context)
    value = temp.some    

# Refs
proc serialize*(value: ref Serializable; context: SerializationContext) =
  if value == nil:
    context.stream.write(0'i8)
  else:
    let id = context.getRefId()
    if id.isSome:
      context.stream.write(1'i8)
      context.stream.write(id)
    else:
      context.stream.write(2'i8)
      context.registerRef(value)

      when Serializable is Inheritable: discard
        #let typeInfo = getTypeDescription(Serializable)
        #let serializer = context.getSerializer(typeInfo)
        #serializer.serialize(value, context)
      else:
        serialize(value.Serializable, context)     

proc deserialize*(value: var ref Serializable; context: SerializationContext) =
  let kind = context.stream.readInt8()
  case kind:
    of 0:
      value = nil
    of 1:
      let id = context.stream.readInt64()
      value = context.getRefFromId(id)
    of 2:
      when Serializable is Inheritable:
        discard
      else:
        new(value)
        context.registerRef(value)
        deserialize(value.Serializable, context)  
    else: discard

# Blittable
proc serialize*(value: Blittable; context: SerializationContext) =
  context.stream.write(value)

proc deserialize*[T: Blittable](value: var T; context: SerializationContext) =
  #context.stream.read(value) # Internal
  if context.stream.readData(addr(value), sizeof(T)) != sizeof(T):
    raise newException(IOError, "cannot read from stream")

# Lists
proc serialize*[T: Serializable](collection: seq[T]; context: SerializationContext) =
  context.stream.writePackedInt(collection.len)
  for item in collection:
    item.serialize(context)

proc deserialize*[T: Serializable](collection: var seq[T]; context: SerializationContext) =
  # TODO: seq[Blittable]
  let count = context.stream.readPackedInt()
  newSeq(collection, count)
  for index in mitems(collection):
    deserialize(index, context)

# Strings
proc serialize*(value: string; context: SerializationContext) =
  context.stream.writePackedInt(value.len)
  context.stream.write(value)

proc deserialize*(value: var string; context: SerializationContext) =
  let length = context.stream.readPackedInt()
  value = context.stream.readStr(length.int)

static:
  type
    Distint = distinct int
    NonBlittable = object
      f: ref int

    Tuple = tuple[s: string; i: int]
    Ref = ref int
    Ptr = ptr int
    Seq = seq[int]

  assert float is Blittable
  assert Distint.isBlittable # TODO: concept causes SIGSEGV?
  assert Tuple isnot Blittable
  assert NonBlittable isnot Blittable
  assert string isnot Blittable
  assert Ref isnot Blittable
  assert Ptr isnot Blittable
  assert Seq isnot Blittable

  type
    A = object of RootObj
    B = object
    C {.inheritable.} = object
    D {.final.} = object of RootObj

  assert A is Inheritable
  assert B isnot Inheritable
  assert C is Inheritable
  assert D isnot Inheritable

  assert float is (Blittable and not Inheritable)
  assert (ref float) is (ref Blittable)

when isMainModule:
  var stream = newStringStream()
    
  stream.writePackedInt(42)
  stream.writePackedUInt(42)
  stream.setPosition(0)
  discard stream.readPackedInt()
  discard stream.readPackedUInt()

  type
    Test2 = object
      s*: string

    Test = object
      a*: float
      b*: seq[int]
      s*: string
      #r*: Test2

  var context = new SerializationContext
  context.stream = stream  

  var
    r = Test(a: 1.0, b: @[1, 2, 3], s: "Hello")

  generateObjectSerializer(Test2)
  generateObjectSerializer(Test)

  stream.setPosition(0)
  serialize(r, context)
  stream.setPosition(0)
  deserialize(r, context)
  echo r
