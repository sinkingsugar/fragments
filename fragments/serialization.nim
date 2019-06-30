import macros, streams, math.math_common, options, tables

const
  highBit = 0b1000_0000
  mask = 0b0111_1111

proc writePackedUInt*(stream: Stream; value: SomeUnsignedInt) =
  var remaining = value
  while remaining >= highBit.SomeUnsignedInt:
    stream.write((remaining or highBit).byte)
    remaining = remaining shr 7
  stream.write(remaining.byte)

proc writePackedInt*(stream: Stream; value: SomeInteger) =
  stream.writePackedUInt(value.zigZagEncode())

proc readPackedUInt*(stream: Stream): BiggestUInt =
  var 
    shift = 0
    currentByte: byte

  while true:
    currentByte = stream.readUInt8()
    result = result or ((currentByte and mask).BiggestUInt shl shift)
    shift += 7
    if (currentByte and highBit) == 0:
      break

proc readPackedInt*(stream: Stream): BiggestInt =
  var x = stream.readPackedUInt()
  return x.zigZagDecode().BiggestInt

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
  FourCC* = distinct int32

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

proc toFourCC*(c1, c2, c3, c4: char): FourCC {.compileTime.} =
  return FourCC((ord(c1).cint and 255) + ((ord(c2).cint and 255) shl 8) +
    ((ord(c3).cint and 255) shl 16) + ((ord(c4).cint and 255) shl 24))

proc toFourCC*(str: string): FourCC {.compileTime.} =
  doAssert(str.len == 4, "To make a FourCC from a string the string needs to be exactly 4 chars long")
  return toFourCC(str[0], str[1], str[2], str[3])

proc `==`*(x, y: FourCC): bool {.borrow.}

proc newSerializationContext*(stream: Stream): owned SerializationContext =
  new(result)
  result.stream = stream
  result.references = @[]
  result.ids = initTable[pointer, int]()

var serializableTypes {.compileTime.}: seq[tuple[serializable, serialize, deserialize: NimNode]]

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

proc registerRef*[T](self: SerializationContext; reference: ref T) =
  let id = self.references.len
  let key = cast[pointer](reference)
  self.references.add(key)
  self.ids[key] = id

proc registerOwnedRef*[T](self: SerializationContext; reference: owned(ref T)): ref T =
  let id = self.references.len
  let key = cast[pointer](reference)
  self.references.add(key)
  self.ids[key] = id
  return cast[ref T](key)

proc getRefFromId*[T](self: SerializationContext; id: int): ref T =
  cast[ref T](self.references[id])

proc getOwnedRefFromId*[T](self: SerializationContext; id: int): owned (ref T) =
  move cast[ptr owned(ref T)](addr self.references[id])[]

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

proc deserializeValue*[T](value: var owned(ref T); context: SerializationContext) =
  let kind = context.stream.readInt8().ReferenceSerializationKind
  case kind:
    of ReferenceSerializationKind.Nil:
      value = nil

    of ReferenceSerializationKind.Reference:
      let id = context.stream.readInt64()
      value = getOwnedRefFromId[T](context, id.int)

      # TODO: Keep track which references are actually owned, and allow getOwnedRefFromId only once.
      # When serializing, registered references can never be owned, because deserializing could violate ownership.

    of ReferenceSerializationKind.Value:
      when value is Inheritable:
        raiseAssert "Not implemented"
      else:
        new(value)
        context.registerRef(value)
        deserialize(value[], context)  

when defined nimV2:
  proc deserializeValue*[T](value: var ref T; context: SerializationContext) =
    let kind = context.stream.readInt8().ReferenceSerializationKind
    case kind:
      of ReferenceSerializationKind.Nil:
        value = nil

      of ReferenceSerializationKind.Reference:
        let id = context.stream.readInt64()
        value = getRefFromId[T](context, id.int)

      of ReferenceSerializationKind.Value:
        when value is Inheritable:
          raiseAssert "Not implemented"
        else:
          let reference = new T
          value = context.registerOwnedRef(reference)
          deserialize(value[], context)  

# Blittable
proc serializeValue*(value: Blittable; context: SerializationContext) =
  context.stream.write(value)

proc deserializeValue*(value: var Blittable; context: SerializationContext) =
  #context.stream.read(value) # Internal
  if context.stream.readData(addr(value), sizeof(Blittable)) != sizeof(Blittable):
    raise newException(IOError, "cannot read from stream")

# Lists
proc serializeValue*[T](collection: seq[T]; context: SerializationContext) =
  context.stream.writePackedUInt(collection.len.uint)
  for item in collection:
    item.serialize(context)

proc deserializeValue*[T](collection: var seq[T]; context: SerializationContext) =
  # TODO: seq[Blittable]
  let count = context.stream.readPackedUInt()
  collection.setLen(count)
  # when T.isBlittable():
  #   let length = sizeof(T) * count
  #   if context.stream.readData(addr collection[0], length) != length:
  #     raise newException(IOError, "cannot read from stream")
  # else:
  for index in mitems(collection):
    deserialize(index, context)

# Strings
proc serializeValue*(value: string; context: SerializationContext) =
  context.stream.writePackedUInt(value.len.uint)
  if value.len > 0:
    context.stream.write(value)

proc deserializeValue*(value: var string; context: SerializationContext) =
  let length = context.stream.readPackedUInt()
  if length.int > 0:
    value = context.stream.readStr(length.int)
  else: value = ""

when isMainModule:
  type
    Distinct = distinct int
    NonBlittable = object
      f: ref int

    Tuple = tuple[s: string; i: int]
    Ref = ref int
    Ptr = ptr int
    Seq = seq[int]

  assert float is Serializable
  assert string is Serializable
  assert Ref is Serializable
  assert Ptr isnot Serializable
  assert Seq is Serializable

  assert float is Blittable
  static: assert Distinct.isBlittable # TODO: concept causes SIGSEGV?
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

  var stream = newStringStream()
    
  stream.writePackedInt(42)
  stream.writePackedInt(0)
  stream.writePackedInt(int.high)
  stream.writePackedInt(int.low)
  stream.writePackedUInt(42)
  stream.writePackedUInt(0)
  stream.setPosition(0)
  assert stream.readPackedInt() == 42
  assert stream.readPackedInt() == 0
  assert stream.readPackedInt() == int.high
  assert stream.readPackedInt() == int.low
  assert stream.readPackedUInt() == 42
  assert stream.readPackedUInt() == 0

  type
    Test2 = object
      s*: string

    Test3 = tuple
      a: int

    Test = ref TestObj
    TestObj = object
      a1*, a2* {.serializable: false.}: float
      b*: seq[int]
      s*: string
      r*: Test2
      p*: owned (ref float)
      p2*: (ref float)
      t1*: Test3
      t2*: tuple[a: int]
      # t3*: tuple[a: owned (ref float)]

  assert Test is Serializable

  var
    r1 = Test(a1: 1.0, a2: 2.0, b: @[1, 2, 3], s: "Hello", p: new float, t1: (4,), t2: (5,))#, t3: (new float,))
    r2: owned Test
  r1.p[] = 3.0
  r1.p2 = r1.p
  #r1.t3.a[] = 4.0

  var context = newSerializationContext(stream)
  stream.setPosition(0)
  serialize(r1, context)

  context = newSerializationContext(stream)
  stream.setPosition(0)
  deserialize(r2, context)
  
  r1.a2 = 0

  echo r1[]
  echo r2[]

  assert $r1[] == $r2[]
  assert $r1.p[] == $r2.p[]
  #assert r2.p == r2.p2

