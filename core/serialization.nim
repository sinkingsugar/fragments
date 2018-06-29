import macros, streams, math.common, options, tables

{.experimental.}

const
  highBit = 0b1000_0000
  mask = 0b0111_1111

proc writePackedUInt*(stream: Stream; value: BiggestUInt) =
  var remaining = value
  while remaining >= highBit.uint64:
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

template serializable*(shouldSerialize: bool = true): untyped {.pragma.}

proc isBlittable*(t: NimNode): bool {.compileTime.} =
  case t.typeKind:
    of ntyString, ntyCString, ntyProc, ntyPtr, ntyPointer, ntyRef, ntySequence: return false
    of ntyTypeDesc: return t.getTypeInst()[1].isBlittable     
    of ntyDistinct: return t[1].isBlittable
    of ntyArray: return t.getTypeInst()[2].isBlittable
    of ntyObject, ntyTuple:

      var typeDef = t.getTypeInst()

      if typeDef.kind == nnkSym:
        typeDef = typeDef.getImpl()

      if typeDef.kind == nnkTypeDef:
        typeDef = typeDef[2]

      let fieldDefs = if t.typeKind == ntyObject: typeDef[2] else: typeDef # Tuples have no recList
      for field in fieldDefs:
        if not field[^2].isBlittable:
          return false

      return true
    else: return true

proc isBlittable*(t: typedesc): bool {.compileTime.} =
  t.getType().isBlittable()

type
  ReferenceSerializationKind* {.pure, size: sizeof(int8).} = enum
    Nil
    Reference
    Value

  SerializationContext* = ref object
    stream*: Stream
    references: seq[pointer]
    ids: Table[pointer, int]

  # TODO: Override with shallow pragma?
  Blittable* = concept type T
    T.isBlittable

  # Any type that can be derived from. Takes care of {.inheritable.} and {.final.} too.
  Inheritable* = concept type T
    type
      Base = T # Using T directly doesn't work for some reason
      Derived = object of Base

  Serializable* = concept v, var m
    v.serializeValue(SerializationContext)
    m.deserializeValue(SerializationContext)

proc newSerializationContext*(stream: Stream): SerializationContext =
  new(result)
  result.stream = stream
  result.references = @[]
  result.ids = initTable[pointer, int]()

var serializableTypes {.compileTime.} = newSeq[tuple[serializable, serialize, deserialize: NimNode]]()

proc generateObjectSerializer*(t: NimNode): tuple[declaration, serialize, deserialize: NimNode] {.compileTime.} =
  let typeInst = t.getTypeInst()

  var typeDef = typeInst

  # Type definition
  if typeDef.kind == nnkSym:
    typeDef = typeInst.getImpl()

    let isRef = typeDef[2].kind in {nnkRefTy, nnkPtrTy}
    if isRef and typeDef[2][0].kind in {nnkSym, nnkBracketExpr}:
      typeDef = typeDef[2][0].getImpl()
    else:
      typeDef = if isRef: typeDef[2][0] else: typeDef[2]

    for serializer in serializableTypes:
      if t.sameType(serializer.serializable):
        return (nil, serializer.serialize, serializer.deserialize)

  # Anonymous tuple
  elif typeDef.kind != nnkTupleTy:
    discard

  # Objects have a record list. Tuples contain ident defs directly.
  let fieldDefs = if typeDef.kind == nnkObjectTy: typeDef[2] else: typeDef

  let
    context = ident"context"
    value = ident"value"

  var
    serializers = newStmtList()
    deserializers = newStmtList()

  # Call base type serializer first
  # TODO: Handle refs?
  # if t.getType().typeKind == ntyObject:
  #   let inherit = typeDef[2][1]
  #   if inherit.kind == nnkOfInherit:
  #     let baseType = inherit[0]
  #     assert baseType.kind == nnkIdent
  #     if not eqIdent(baseType, "RootObj"):
  #       serializers.add quote do: `baseType`(`value`).serialize(`context`)
  #       deserializers.add quote do: `baseType`(`value`).deserialize(`context`)

  # Serialize fields
  # TODO: Handle visibility and make configurable per field
  for fieldDef in fieldDefs:
    for i in 0 ..< fieldDef.len - 2:
      var shouldSerialize = true
      var fieldIdent = fieldDef[i]

      if fieldIdent.kind == nnkPragmaExpr:
        for pragma in fieldIdent[1]:
          if pragma.kind == nnkExprColonExpr and pragma[0].strVal == "serializable" and pragma[1].strVal == "false":
            shouldSerialize = false
        fieldIdent = fieldIdent[0]

      if shouldSerialize:
        # Get the field identifier without postfix/pragmas.
        # In anonymous typles, this is already a symbol.
        if fieldIdent.kind != nnkSym:
          fieldIdent = fieldIdent.basename

        serializers.add quote do: `value`.`fieldIdent`.serialize(`context`)
        deserializers.add quote do: `value`.`fieldIdent`.deserialize(`context`)

  let
    serializeSym = genSym(nskProc)
    deserializeSym = genSym(nskProc)

  serializableTypes.add((t.getTypeInst(), serializeSym, deserializeSym))

  result.serialize = serializeSym
  result.deserialize = deserializeSym
  result.declaration = quote do:
    type noref = type((
      block:
        var x: type(`t`)
        when compiles(x[]):
          x[]
        else:
          x
    ))
    proc `serializeSym`(`value`: noref; `context`: SerializationContext) = `serializers`
    proc `deserializeSym`(`value`: var noref; `context`: SerializationContext) = `deserializers`

template serialize*(value: Serializable; context: SerializationContext): untyped =
  serializeValue(value, context)

template deserialize*(value: var Serializable; context: SerializationContext): untyped =
  deserializeValue(value, context)

macro serialize*(value: not Serializable; context: SerializationContext): untyped =
  let (declaration, serialize, _) = generateObjectSerializer(value)
  result = newStmtList()
  if declaration != nil:
    result.add(declaration)
  result.add quote do:
    `serialize`(`value`, `context`)

macro deserialize*[T: not Serializable](value: var T; context: SerializationContext): untyped =
  let (declaration, _, deserialize) = generateObjectSerializer(value)
  result = newStmtList()
  if declaration != nil:
    result.add(declaration)
  result.add quote do:
    `deserialize`(`value`, `context`)

# Options
proc serializeValue*[T](value: Option[T]; context: SerializationContext) =
  if value.isNone:
    context.stream.write(false)
  else:
    context.stream.write(true)
    value.get().serialize(context)

proc deserializeValue*[T](value: var Option[T]; context: SerializationContext) =
  let isSome = context.stream.readBool()
  if isSome:
    var temp: T
    temp.deserialize(context)
    value = temp.some    

proc getRefId*(self: SerializationContext; reference: ref): Option[int] =
  let id = self.ids.getOrDefault(cast[pointer](reference), -1)
  return if id < 0: int.none else: id.some

proc registerRef*(self: SerializationContext; reference: ref) =
  let id = self.references.len
  let key = cast[pointer](reference)
  self.references.add(key)
  self.ids[key] = id

proc getRefFromId*[T](self: SerializationContext; id: int): ref T =
  cast[ref T](self.references[id])

# Refs
proc serializeValue*[T](value: ref T; context: SerializationContext) =
  if value == nil:
    context.stream.write(ReferenceSerializationKind.Nil)
  else:
    let id = context.getRefId(value)
    if id.isSome:
      context.stream.write(ReferenceSerializationKind.Reference)
      context.stream.write(id.get().int64)
    else:
      context.stream.write(ReferenceSerializationKind.Value)
      context.registerRef(value)

      when T is Inheritable: discard
        #let typeInfo = getTypeDescription(Serializable)
        #let serializer = context.getSerializer(typeInfo)
        #serializer.serialize(value, context)
      else:
        serialize(value[], context)     

proc deserializeValue*[T](value: var ref T; context: SerializationContext) =
  let kind = context.stream.readInt8().ReferenceSerializationKind
  case kind:
    of ReferenceSerializationKind.Nil:
      value = nil

    of ReferenceSerializationKind.Reference:
      let id = context.stream.readInt64()
      value = getRefFromId[T](context, id.int)

    of ReferenceSerializationKind.Value:
      when Serializable is Inheritable:
        discard
      else:
        new(value)
        context.registerRef(value)
        deserialize(value[], context)  

# Blittable
proc serializeValue*(value: Blittable; context: SerializationContext) =
  context.stream.write(value)

proc deserializeValue*[T: Blittable](value: var T; context: SerializationContext) =
  #context.stream.read(value) # Internal
  if context.stream.readData(addr(value), sizeof(T)) != sizeof(T):
    raise newException(IOError, "cannot read from stream")

# Lists
proc serializeValue*[T](collection: seq[T]; context: SerializationContext) =
  context.stream.writePackedInt(collection.len)
  for item in collection:
    item.serialize(context)

proc deserializeValue*[T](collection: var seq[T]; context: SerializationContext) =
  # TODO: seq[Blittable]
  let count = context.stream.readPackedInt()
  newSeq(collection, count)
  # when T.isBlittable():
  #   let length = sizeof(T) * count
  #   if context.stream.readData(addr collection[0], length) != length:
  #     raise newException(IOError, "cannot read from stream")
  # else:
  for index in mitems(collection):
    deserialize(index, context)

# Strings
proc serializeValue*(value: string; context: SerializationContext) =
  context.stream.writePackedInt(value.len)
  if value.len > 0:
    context.stream.write(value)

proc deserializeValue*(value: var string; context: SerializationContext) =
  let length = context.stream.readPackedInt()
  if length > 0:
    value = context.stream.readStr(length.int)
  else: value = ""

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
  assert stream.readPackedInt() == 42
  assert stream.readPackedUInt() == 42

  type
    Test2 = object
      s*: string

    Test3 = tuple
      a: int

    Test = ref object
      a1*, a2* {.serializable: false.}: float
      b*: seq[int]
      s*: string
      r*: Test2
      p*: ref float
      t1*: Test3
      t2* {.serializable: false.}: tuple[a: int]
      t3*: tuple[a: ref float]

  var
    r1 = Test(a1: 1.0, a2: 2.0, b: @[1, 2, 3], s: "Hello", p: new float, t1: (4,), t2: (5,), t3: (new float,))
    r2 = Test()
  r1.p[] = 3.0
  r1.t3.a[] = 4.0

  var context = newSerializationContext(stream)
  context.stream = stream
  stream.setPosition(0)
  serialize(r1, context)

  context = newSerializationContext(stream)
  context.stream = stream  
  stream.setPosition(0)
  deserialize(r2, context)

  echo r2
