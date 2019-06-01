import math, macros, strutils, vectors, math_common, strformat
export vectors
  
{.experimental.}

# {.emit: """/*TYPESECTION*/
# template<typename T, int I>
# using Vector = T __attribute__((ext_vector_type(I)));
# """.}

# {.passC: "-I" & staticExec("echo $PWD").}

type
  Half* = distinct uint16

  #Vector* {.importcpp: "Vector<'0, '1>", header: "linearAlgebra.h".} [T; size: static int] = object
  Vector*[T; size: static int] = object
    elements: array[size, T]

  Matrix*[T; height, width: static int] = object
    elements: array[width * height, T]

  SymmetricMatrix*[T; size: static int] = object
    elements: array[size * (size + 1) div 2, T]

  # Common vector types
  Vector2* = Vector[float32, 2]
  Vector3* = Vector[float32, 3]
  Vector4* = Vector[float32, 4]

  Half2* = Vector[Half, 2]
  Half3* = Vector[Half, 3]
  Half4* = Vector[Half, 4]

  Int2* = Vector[int32, 2]
  Int3* = Vector[int32, 3]
  Int4* = Vector[int32, 4]

  Bool2* = Vector[bool, 2]
  Bool3* = Vector[bool, 3]
  Bool4* = Vector[bool, 4]

  # Quaternion types
  QuaternionBase*[T] = object
    elements: array[4, T]
  Quaternion* = QuaternionBase[float32]

  # Common matrix types
  Matrix2x2* = Matrix[float32, 2, 2]
  Matrix3x2* = Matrix[float32, 3, 2]
  Matrix3x3* = Matrix[float32, 3, 3]
  Matrix4x3* = Matrix[float32, 4, 3]
  Matrix4x4* = Matrix[float32, 4, 4]

  SymmetricMatrix2x2* = SymmetricMatrix[float32, 2]
  SymmetricMatrix3x3* = SymmetricMatrix[float32, 3] 

# Custom vectorization. Vector-like types simply have their members serialized.
template isVectorizable*(_: type Vector): bool = true
template wideImpl*[T; size: static int](_: type Vector[T, size]): typedesc = Vector[wide(typeof(T)), size]
template scalarTypeImpl*[T; size: static int](t: type Vector[T, size]): typedesc = Vector[T.scalarType, size]
template laneCountImpl*[T; size: static int](t: type Vector[T, size]): int = T.laneCount

func getLaneImpl*[size; T](self: Vector[T, size]; laneIndex: int): auto {.inline.} =
  var r: Vector[T.scalarType, size]
  for i in 0 ..< self.elements.len:
    r.elements[i] = self.elements[i].getLane(laneIndex)
  return r

func setLaneImpl*[size; T; S](self: var Vector[T, size]; laneIndex: int; value: Vector[S, size]) {.inline.} =
  when S isnot T.scalarType: {.error.}
  for i in 0 ..< self.elements.len:
    self.elements[i].setLane(laneIndex, value.elements[i])

template isVectorizable*(_: type QuaternionBase): bool = true
template wideImpl*[T](_: type QuaternionBase[T]): typedesc = QuaternionBase[wide(typeof(T))]
template scalarTypeImpl*[T](t: type QuaternionBase[T]): typedesc = QuaternionBase[T.scalarType]
template laneCountImpl*[T](t: type QuaternionBase[T]): int = T.laneCount

func getLaneImpl*[T](self: QuaternionBase[T]; laneIndex: int): auto {.inline.} =
  var r: QuaternionBase[T.scalarType]
  for i in 0 ..< self.elements.len:
    r.elements[i] = self.elements[i].getLane(laneIndex)
  return r

func setLaneImpl*[T; S](self: var QuaternionBase[T]; laneIndex: int; value: QuaternionBase[S]) {.inline.} =
  when S isnot T.scalarType: {.error.}
  for i in 0 ..< self.elements.len:
    self.elements[i].setLane(laneIndex, value.elements[i])

template isVectorizable*(_: type Matrix): bool = true
template wideImpl*[T; height, width: static int](_: type Matrix[T, height, width]): typedesc = Matrix[wide(typeof(T)), height, width]
template scalarTypeImpl*[T; height, width: static int](t: type Matrix[T, height, width]): typedesc = Matrix[T.scalarType, height, width]
template laneCountImpl*[T; height, width: static int](t: type Matrix[T, height, width]): int = T.laneCount

func getLaneImpl*[height, width; T](self: Matrix[T, height, width]; laneIndex: int): auto {.inline.} =
  var r: Matrix[T.scalarType, height, width]
  for i in 0 ..< self.elements.len:
    r.elements[i] = self.elements[i].getLane(laneIndex)
  return r

func setLaneImpl*[height, width; T; S](self: var Matrix[T, height, width]; laneIndex: int; value: Matrix[S, height, width]) {.inline.} =
  when S isnot T.scalarType: {.error.}
  for i in 0 ..< self.elements.len:
    self.elements[i].setLane(laneIndex, value.elements[i])    

template isVectorizable*(_: type SymmetricMatrix): bool = true
template wideImpl*[T; size: static int](_: type SymmetricMatrix[T, size]): typedesc = SymmetricMatrix[wide(typeof(T)), size]
template scalarTypeImpl*[T; size: static int](t: type SymmetricMatrix[T, size]): typedesc = SymmetricMatrix[T.scalarType, size]
template laneCountImpl*[T; size: static int](t: type SymmetricMatrix[T, size]): int = T.laneCount

func getLaneImpl*[size; T](self: SymmetricMatrix[T, size]; laneIndex: int): auto {.inline.} =
  var r: SymmetricMatrix[T.scalarType, size]
  for i in 0 ..< self.elements.len:
    r.elements[i] = self.elements[i].getLane(laneIndex)
  return r

func setLaneImpl*[size; T; S](self: var SymmetricMatrix[T, size]; laneIndex: int; value: SymmetricMatrix[S, size]) {.inline.} =
  when S isnot T.scalarType: {.error.}
  for i in 0 ..< self.elements.len:
    self.elements[i].setLane(laneIndex, value.elements[i])

# Vectors inherit universal pointwise ops
template isVector*(_: type Vector): bool = true
template len*[T; size: static int](_: type Vector[T, size]): int = size

# Vector, quaternion and matrix subscript
func `[]`*[V: Vector | QuaternionBase | Matrix](self: V; index: int): V.T =
  self.elements[index]

func `[]=`*[V: Vector | QuaternionBase | Matrix](self: var V; index: int; value: V.T) =
  self.elements[index] = value

# Matrix subscript
func `[]`*(self: Matrix; row, column: int): Matrix.T =
  self.elements[row * Matrix.width + column]

func `[]=`*(self: var Matrix; row, column: int; value: Matrix.T) =
  self.elements[row * Matrix.width + column] = value

# Common wide types
type
  Vector2Wide* = wide Vector2
  Vector3Wide* = wide Vector3
  Vector4Wide* = wide Vector4

  QuaternionWide* = wide Quaternion

  Matrix4x4Wide* = wide Matrix4x4

# These cannot be defined using the concept, since they would conflict with the generic version in the system module
makeUniversalBinary(Vector, min)
makeUniversalBinary(Vector, max)

# converter toVector[T; size: static int](value: T): Vector[T, size] =
#   for i in 0..<size:
#     result[i] = value

# Matrix diagonal
func diag*[T; size: static int](self: Matrix[T, size, size]): Vector[T, size] =
  for i in 0..<size:
    result[i] = self[i, i]

func `diag=`*[T; size: static int](self: var Matrix[T, size, size]; value: Vector[T, size]) =
  for i in 0..<size:
    self[i, i] = value[i]

# Array to vector conversion
func toVector[T; width: static int](value: array[width, T]): Vector[T, width] =
  for i in 0..<width:
    result[i] = value[i]

func `$`*(self: Vector): string =
  const size = Vector.size
  when size == 1: return fmt"(X: {self.x})"
  elif size == 2: return fmt"(X: {self.x}, Y: {self.y})"
  elif size == 3: return fmt"(X: {self.x}, Y: {self.y}, Z: {self.z})"
  elif size == 4: return fmt"(X: {self.x}, Y: {self.y}, Z: {self.z}, W: {self.w})"
  else: return $self.elements

func `$`*(self: Matrix): string =
  result = "["
  for y in 0 ..< Matrix.height:
    for x in 0 ..< Matrix.width:
      result &= fmt"m{y}{x}: {self[y, x]}"
      if x < Matrix.width - 1: result &= ", "
    if y < Matrix.height - 1: result &= ",\n "
    else: result &= "]" 
  
func `$`*(self: QuaternionBase): string =
  fmt"(X: {self.x}, Y: {self.x}, Z: {self.z}, W: {self.w})"

# Vector swizzles
func getSwizzleIndex(c: char): int {.compileTime.} =
  case c:
    of 'x', 'r': return 0
    of 'y', 'g': return 1
    of 'z', 'b': return 2
    of 'w', 'a': return 3
    else: raiseAssert($c & " is not a valid vector swizzle")

# Assumes `[]` operator and the convertor from arrays on vectors
macro `.`*(self: Vector | QuaternionBase; swizzle: untyped): untyped =
  var
    cardinality = ($swizzle).len

  # For one-element swizzles, just return the scalar element
  # v.elements[0]
  if cardinality == 1:
    return nnkBracketExpr.newTree(self, newIntLitNode(($swizzle)[0].getSwizzleIndex))
    
  else:
    var
      values = newNimNode(nnkBracket)
      temp = genSym()

    # Make an array of values, one for each swizzle index
    # [temp[1], temp[0], ...]
    for c in $swizzle:
      values.add(nnkBracketExpr.newTree(temp, newIntLitNode(c.getSwizzleIndex)))

    return quote do:
      let `temp` = `self`
      toVector[`temp`.T, `cardinality`](`values`)

# Assumes `[]` and `[]=` operators on vectors
macro `.=`*(self: var Vector | QuaternionBase; swizzle: untyped; value: untyped): untyped =
  var 
    cardinality = ($swizzle).len
  
  # For single elements, just do single assignment
  if cardinality == 1:
    return newAssignment(nnkBracketExpr.newTree(self, newIntLitNode(($swizzle)[0].getSwizzleIndex)), value)

  else:
    var temp = genSym()

    # Evaluate the right-hand side into a temporary variable
    # let temp = self
    result = newStmtList(
      nnkLetSection.newTree(
        newIdentDefs(temp, newEmptyNode(), value)))

    # For each swizzle index, add an assignment of the corresponding element
    # self[swizzleIndex(c)] = temp[i]
    for i, c in $swizzle:
      result.add(
        newAssignment(
          nnkBracketExpr.newTree(self, newIntLitNode(c.getSwizzleIndex)),
          nnkBracketExpr.newTree(temp, newIntLitNode(i))))

func getMatrixSwizzles(swizzle: string): seq[(int, int)] {.compileTime.} =

  #assert(match(swizzle, re"(m\d\d)*"), "'" & swizzle & "' is not a valid matrix swizzling")
  assert(swizzle.len mod 3 == 0, "'" & swizzle & "' is not a valid matrix swizzling pattern")
  result = newSeqOfCap[(int, int)](swizzle.len div 3)

  for i in countup(0, swizzle.len - 1, 3):
    assert(swizzle[i] == 'm' and swizzle[i + 1].isDigit and swizzle[i + 2].isDigit, "'" & swizzle & "' is not a valid matrix swizzling pattern")
    result.add((parseInt($swizzle[i + 1]), parseInt($swizzle[i + 2])))

macro `.`*(self: Matrix; swizzle: untyped): untyped =
  var
    indices = getMatrixSwizzles($swizzle)
    cardinality = indices.len

  # For one-element swizzles, just return the scalar element
  # v.elements[0]
  if cardinality == 1:
    let (row, column) = indices[0]
    return nnkBracketExpr.newTree(self, newIntLitNode(row), newIntLitNode(column))
    
  else:
    var
      values = newNimNode(nnkBracket)
      temp = genSym()

    # Make an array of values, one for each swizzle index
    # [temp[1, 2], temp[0, 3], ...]
    for index in indices:
      let (row, column) = index
      values.add(nnkBracketExpr.newTree(temp, newIntLitNode(row), newIntLitNode(column)))

    return quote do:
      let `temp` = `self`
      toVector[`temp`.T, `cardinality`](`values`)

# Assumes `[]` and `[]=` operators on matrices
macro `.=`*(self: var Matrix; swizzle: untyped; value: untyped): untyped =
  var
    indices = getMatrixSwizzles($swizzle)
    cardinality = indices.len
  
  # For single elements, just do single assignment
  if cardinality == 1:
    let (row, column) = indices[0]
    return newAssignment(nnkBracketExpr.newTree(self, newIntLitNode(row), newIntLitNode(column)), value)

  else:
    var temp = genSym()

    # Evaluate the right-hand side into a temporary variable
    # let temp = self
    result = newStmtList(
      nnkLetSection.newTree(
        newIdentDefs(temp, newEmptyNode(), value)))

    # For each swizzle index, add an assignment of the corresponding element
    # self[swizzleIndex(c)] = temp[i]
    for i, pos in indices:
      let (row, column) = pos
      result.add(
        newAssignment(
          nnkBracketExpr.newTree(self, newIntLitNode(row), newIntLitNode(column)),
          nnkBracketExpr.newTree(temp, newIntLitNode(i))))

# macro `{}`*(typ: type Vector, args: varargs[untyped]): auto =
#   for x in xs.children:
#     if x.kind == nnkExprColonExpr:

# 2 element tuples converters
converter toVector*[T](value: (T, T)): Vector[T, 2] = (result.x, result.y) = value

# 3 element tuples converters
converter toVector*[T](value: (T, T, T)): Vector[T, 3] = (result.x, result.y, result.z) = value
converter toVector*[T](value: (Vector[T, 2], T)): Vector[T, 3] = (result.xy, result.z) = value

# 4 element tuples converters
converter toVector*[T](value: (T, T, T, T)): Vector[T, 4] = (result.x, result.y, result.z, result.w) = value
converter toVector*[T](value: (Vector[T, 2], T, T)): Vector[T, 4] = (result.xy, result.z, result.w) = value
converter toVector*[T](value: (Vector[T, 3], T)): Vector[T, 4] = (result.xyz, result.w) = value

proc to*[T1; width: static int](self: Vector[T1, width]; T2: type): Vector[T2, width] {.inline.} =
  for i in 0 ..< width:
    result[i] = (T2)self[i]

func identity*[T](_: type QuaternionBase[T]): QuaternionBase[T] =
  result.w = 1.T

func identity*[T; size: static int](_: type Matrix[T, size, size]): Matrix[T, size, size] =
  for i in 0..<size:
    result[i, i] = 1.T

func full*[T; width: static int](_: type Vector[T, width]; value: T): Vector[T, width] =
  for i in 0..<width:
    result[i] = value

func zero*[T; width: static int](_: type Vector[T, width]): Vector[T, width] = discard

func one*[T; width: static int](_: type Vector[T, width]): Vector[T, width] =
  full(_, 1.T)

func unitX*[T](_: type Vector[T, 3]): Vector[T, 3] =
  result.x = 1.T
  
func unitY*[T](_: type Vector[T, 3]): Vector[T, 3] =
  result.y = 1.T

func unitZ*[T](_: type Vector[T, 3]): Vector[T, 3] =
  result.z = 1.T

# func rand*[T; width: static int](maximum: Vector[T, width]): Vector[T, width] =
#   (rand(maximum.x))

func `*` *[T](left: QuaternionBase[T]; right: T): QuaternionBase[T] =
  for i in 0..<4:
    result[i] = left[i] * right

template `/` *(left: QuaternionBase; right: QuaternionBase.T): QuaternionBase =
  left * (QuaternionBase.T)1 / right

func dot*(left, right: Vector): Vector.T =
  for i in 0..<Vector.size:
    result += left[i] * right[i]

func cross*[T](left, right: Vector[T, 3]): Vector[T, 3] =
  left.yzx * right.zxy - left.zxy * right.yzx

func lengthSquared*(self: Vector): Vector.T =
  dot(self, self)

func length*[T; width: static int](self: Vector[T, width]): T =
  self.lengthSquared().sqrt()

func distanceSquared*(left, right: Vector): Vector.T =
  (left - right).lengthSquared

func distance*[T; width: static int](left, right: Vector[T, width]): T =
  (left - right).length

func normalize*[T; width: static int](self: Vector[T, width]): Vector[T, width] =
  let length = self.length
  if length > 0:
    return self / self.length
  else:
    return self

func lengthSquared*(self: QuaternionBase): QuaternionBase.T =
  self.xyzw.lengthSquared

func length*(self: QuaternionBase): QuaternionBase.T =
  sqrt(self.lengthSquared)
    
func normalize*(self: QuaternionBase): QuaternionBase =
  let length = self.length
  if length > 0:
    return self / self.length
  else:
    return self

func lerp*[T; width: static int](min, max: Vector[T, width]; amount: T): Vector[T, width] =
  for i in 0..<Vector.size:
    result[i] = lerp(min[i], max[i], amount)

func smoothstep*[T; width: static int](min, max: Vector[T, width]; amount: T): Vector[T, width] =
  var lerpAmount = saturate(amount)
  lerpAmount = lerpAmount * lerpAmount * (3 - (2 * lerpAmount))
  return lerp(min, max, lerpAmount)

func hermite*[T; width: static int](value1, value2, tangent1, tangent2: Vector[T, width], amount: T): Vector[T, width] =
  let
    squared = amount * amount
    cubed = amount * squared
    part1 = ((2 * cubed) - (3 * squared)) + 1
    part2 = (-2 * cubed) + (3 * squared)
    part3 = (cubed - (2 * squared)) + amount
    part4 = cubed - squared

  return (((value1 * part1) + (value2 * part2)) + (tangent1 * part3)) + (tangent2 * part4);

func catmullRom*[T; width: static int](value1, value2, value3, value4: Vector[T, width], amount: T): Vector[T, width] =
  let
    squared = amount * amount
    cubed = squared * amount

    factor0 = -cubed + 2 * squared - amount
    factor1 = 3 * cubed - 5 * squared + 2
    factor2 = -3 * cubed + 4 * squared + amount
    factor3 = cubed - squared

  return 0.5 * (value1 * factor0 + value2 * factor1 + value3 * factor2 + value4 * factor3)

func barycentric*[T; width: static int](value1, value2, value3: Vector[T, width]; amount1, amount2: T): Vector[T, width] =
  (value1 + (amount1 * (value2 - value1))) + (amount2 * (value3 - value1))

func changeBasis*(self, basis: Matrix): Matrix =
  basis.transpose() * self * basis

func transformNormal*[T](normal: Vector[T, 3]; transform: Matrix[T, 4, 4]): Vector[T, 3] =
  (transform.m00m01m02 * normal.x) + (transform.m10m11m12 * normal.y) + (transform.m20m21m22 * normal.z)

func transformCoordinate*[T](coordinate: Vector[T, 3]; transform: Matrix[T, 4, 4]): Vector[T, 3] =
  let invW = 1.0 / ((coordinate.x * transform.m03) + (coordinate.x * transform.m13) + (coordinate.z * transform.m23) + transform.m33)
  return (transform.transformNormal(coordinate) + transform.m41m42m43) * invW

func transform*[T](coordinate: Vector[T, 3]; transform: Matrix[T, 3, 3]): Vector[T, 3] =
  (transform.m00m01m02 * coordinate.x) + (transform.m10m11m12 * coordinate.y) + (transform.m20m21m22 * coordinate.z)

func transform*[T](vector: Vector[T, 3]; rotation: QuaternionBase[T]): Vector[T, 3] =
  # Rotates a vector v by a quaternion q, through conjugation q * v * q^-1. Concatenation of rotations is therefore multiplication
  # of quaternions with the right-most quaternion being applied first (as opposed to column matrices).
  let
    x = rotation.x + rotation.x
    y = rotation.y + rotation.y
    z = rotation.z + rotation.z
    
    wx = rotation.w * x
    wy = rotation.w * y
    wz = rotation.w * z
    xx = rotation.x * x
    xy = rotation.x * y
    xz = rotation.x * z
    yy = rotation.y * y
    yz = rotation.y * z
    zz = rotation.z * z

  result.x = ((vector.x * ((1.T - yy) - zz)) + (vector.y * (xy - wz))) + (vector.z * (xz + wy))
  result.y = ((vector.x * (xy + wz)) + (vector.y * ((1.T - xx) - zz))) + (vector.z * (yz - wx))
  result.z = ((vector.x * (xz - wy)) + (vector.y * (yz + wx))) + (vector.z * ((1.T - xx) - yy))

func project*[T](vector: Vector[T, 3]; x, y, width, height, minZ, maxZ: T; worldViewProjection: Matrix): Vector[T, 3] =
  let v = worldViewProjection.transformCoordinate(vector)
  result.x = ((1.T + v.x) * (T)0.5 * width) + x
  result.y = ((1.T - v.y) * (T)0.5 * height) + y
  result.z = (v.z * (maxZ - minZ)) + minZ

func reflect*[T](vector, normal: Vector[T, 3]): Vector[T, 3] =
  vector - normal * 2.T# * dot(vector, normal))

func `*` *(left, right: QuaternionBase): QuaternionBase =
  result.xyz = left.xyz * right.w + right.xyz * left.w + cross(left.xyz, right.xyz)
  result.w = left.w + right.w - dot(left.xyz, right.xyz)

func concatenate*(first, second: QuaternionBase): QuaternionBase =
  # Concatenate two quaternions representing rotations. The first argument is the first rotation applied.
  second * first

func fromAxisAngle*[T](_: type QuaternionBase[T]; axis: Vector[T, 3]; angle: T): QuaternionBase[T] =
  # TODO: identity if zero axis?
  let halfAngle = angle * (T)0.5;
  result.xyz = axis.normalize() * sin(halfAngle)
  result.w = cos(halfAngle)

# Matrix multiplication
func `*` *[T; height, width, count: static int](
  left: Matrix[T, height, count];
  right: Matrix[T, count, width]):
  Matrix[T, height, width] =

  restrict(left)
  restrict(right)

  for r in 0..<height:
    for c in 0..<width:
      for i in 0..<count:
        result[r, c] = result[r, c] + left[][r, i] * right[][i, c]

func transpose*[T; height, width: static int](self: Matrix[T, height, width]): Matrix[T, width, height] =
  for r in 0 ..< height:
    for c in 0 ..< width:
      result[r, c] = self[c, r]

func translation*[T](value: Vector[T, 3]): Matrix[T, 4, 4] =
  result = Matrix[T, 4, 4].identity
  result.m30m31m32 = value

func scaling*[T](value: Vector[T, 3]): Matrix[T, 4, 4] =
  result.m00m11m22 = value
  result.m33 = 1.T

func rotationAxis*[T](axis: Vector[T, 3]; angle: T): Matrix[T, 4, 4] =
  let
    sin = sin(angle)
    cos = cos(angle)

    xx = axis.x * axis.x
    yy = axis.y * axis.y
    zz = axis.z * axis.z
    xy = axis.x * axis.y
    xz = axis.x * axis.z
    yz = axis.y * axis.z

  result.m00 = xx + (cos * (1.0f - xx))
  result.m01 = (xy - (cos * xy)) + (sin * axis.z)
  result.m02 = (xz - (cos * xz)) - (sin * axis.y)
  result.m10 = (xy - (cos * xy)) - (sin * axis.z)
  result.m11 = yy + (cos * (1.0f - yy))
  result.m12 = (yz - (cos * yz)) + (sin * axis.x)
  result.m20 = (xz - (cos * xz)) + (sin * axis.y)
  result.m21 = (yz - (cos * yz)) - (sin * axis.x)
  result.m22 = zz + (cos * (1.0f - zz))
  result.m33 = 1.T

func rotationX*[T](angle: T): Matrix[T, 4, 4] =
  rotationAxis(Vector[T, 3].unitX, angle)

func rotationY*[T](angle: T): Matrix[T, 4, 4] =
  rotationAxis(Vector[T, 3].unitY, angle)

func rotationZ*[T](angle: T): Matrix[T, 4, 4] =
  rotationAxis(Vector[T, 3].unitZ, angle)

func rotationYawPitchRoll*[T](yaw, pitch, roll: T): Matrix[T, 4, 4] =
  rotationZ(roll) * rotationX(pitch) * rotationY(yaw)

func toMatrix*[T](rotation: QuaternionBase[T]): Matrix[T, 3, 3] =
  let
    xx = rotation.x * rotation.x
    yy = rotation.y * rotation.y
    zz = rotation.z * rotation.z
    xy = rotation.x * rotation.y
    zw = rotation.z * rotation.w
    zx = rotation.z * rotation.x
    yw = rotation.y * rotation.w
    yz = rotation.y * rotation.z
    xw = rotation.x * rotation.w

  result.m00 = 1.T - (2.T * (yy + zz))
  result.m01 = 2.T * (xy + zw)
  result.m02 = 2.T * (zx - yw)
  result.m10 = 2.T * (xy - zw)
  result.m11 = 1.T - (2.T * (zz + xx))
  result.m12 = 2.T * (yz + xw)
  result.m20 = 2.T * (zx + yw)
  result.m21 = 2.T * (yz - xw)
  result.m22 = 1.T - (2.T * (yy + xx))

func toQuaternion*[T](self: Matrix[T, 3, 3] | Matrix[T, 4, 4]): QuaternionBase[T] =
  let scale = self.m00 + self.m11 + self.m22
  
  if scale > 0.T:
    let
      sqrt = sqrt(scale + 1.T)
      half = (T)0.5 / sqrt
    
    result.w = (T)0.5 * sqrt
    result.xyz = (self.m12m20m01 - self.m21m02m10) * half
    
  elif (self.m00 >= self.m11) and (self.m00 >= self.m22):
    let 
      sqrt = sqrt(1.0 + self.m00 - self.m11 - self.m22)
      half = (T)0.5 / sqrt;
  
    result.x = (T)0.5 * sqrt;
    result.y = (self.m01 + self.m10) * half
    result.z = (self.m02 + self.m20) * half
    result.w = (self.m12 - self.m21) * half
  
  elif self.m11 > self.m22:
    let
      sqrt = sqrt(1.0f + self.m11 - self.m00 - self.m22)
      half = (T)0.5 / sqrt;
    
    result.x = (self.m10 + self.m01) * half
    result.y = (T)0.5 * sqrt
    result.z = (self.m21 + self.m12) * half
    result.w = (self.m20 - self.m02) * half
    
  else:
    let
      sqrt = sqrt(1.0f + self.m22 - self.m00 - self.m11)
      half = (T)0.5 / sqrt;
    
    result.x = (self.m20 + self.m02) * half
    result.y = (self.m21 + self.m12) * half
    result.z = (T)0.5 * sqrt
    result.w = (self.m01 - self.m10) * half

func perspectiveOffCenter*[T](left, right, bottom, top, near, far: T): Matrix[T, 4, 4] =

  let zRange = far / (far - near)

  result.m00 = 2.T * near / (right - left)
  result.m11 = 2.T * near / (top - bottom)
  result.m20 = -(left + right) / (left - right)
  result.m21 = -(top + bottom) / (bottom - top)
  result.m22 = -zRange
  result.m23 = -1.T
  result.m32 = -near * zRange

func perspectiveFov*[T](fov, aspect, near, far: T): Matrix[T, 4, 4] =
  let
    yScale = 1.T / tan(fov * (T)0.5)
    xScale = yScale / aspect

    halfWidth = near / xScale
    halfHeight = near / yScale
  
  return perspectiveOffCenter(-halfWidth, halfWidth, -halfHeight, halfHeight, near, far)

func orthographicOffCenter*[T](left, right, bottom, top, near, far: T): Matrix[T, 4, 4] =

  let zRange = 1.T / (far - near)
  result.m00 = 2.T / (right - left)
  result.m11 = 2.T / (top - bottom)
  result.m22 = -zRange
  result.m30 = (left + right) / (left - right)
  result.m31 = (top + bottom) / (bottom - top)
  result.m32 = -near * zRange
  result.m33 = 1.T

func orthographic*[T](width, height, near, far: T): Matrix[T, 4, 4] =
  let
    halfWidth = (T)0.5 * width
    halfHeight = (T)0.5 * height

  return orthographicOffCenter(-halfWidth, halfWidth, -halfHeight, halfHeight, near, far: T)

func lookAt*[T](eye, target, up: Vector[T, 3]): Matrix[T, 4, 4] =

  let
    zAxis = (eye - target).normalize()
    xAxis = cross(up, zAxis).normalize()
    yAxis = cross(zAxis, xAxis)
    
  result.m00m10m20 = xAxis
  result.m01m11m21 = yAxis
  result.m02m12m22 = zAxis

  result.m30 = -dot(xAxis, eye)
  result.m31 = -dot(yAxis, eye)
  result.m32 = -dot(zAxis, eye)
  result.m33 = 1.T

func abs*(self: Matrix): Matrix =
  for i in 0 ..< self.elements.len:
    result[i] = abs(self[i])

func all*[width: static int](value: Vector[bool, width]): bool =
  for element in value.elements:
    if not element:
      return false
  return true

func any*[width: static int](value: Vector[bool, width]): bool =
  for element in value.elements:
    if element:
      return true
  return false

func equals*[T; width: static int](left, right: Vector[T, width]): Vector[bool, width] {.inline.} =
  for i in 0 ..< width:
    result[i] = left[i] == right[i]

func `not`*[width: static int](value: Vector[bool, width]): Vector[bool, width] {.inline.} =
  for i in 0 ..< width:
    result[i] = not value[i]

func `==`*[T; width: static int](left, right: Vector[T, width]): bool {.inline.} =
  left.equals(right).all()

func `<=`*[T; width: static int](left, right: Vector[T, width]): Vector[bool, width] {.inline.} =
  for i in 0 ..< width:
    result[i] = left[i] <= right[i]

func `<`*[T; width: static int](left, right: Vector[T, width]): Vector[bool, width] {.inline.} =
  for i in 0 ..< width:
    result[i] = left[i] < right[i]

func `and`*[width: static int](left, right: Vector[bool, width]): Vector[bool, width] {.inline.} =
  for i in 0 ..< width:
    result[i] = left[i] and right[i]

func `or`*[width: static int](left, right: Vector[bool, width]): Vector[bool, width] {.inline.} =
  for i in 0 ..< width:
    result[i] = left[i] and right[i]

func `xor`*[width: static int](left, right: Vector[bool, width]): Vector[bool, width] {.inline.} =
  for i in 0 ..< width:
    result[i] = left[i] xor right[i]

func select*[T; width: static int](condition: Vector[bool, width]; a, b: Vector[T, width]): Vector[T, width] {.inline.} =
  for i in 0 ..< width:
    result[i] = if condition[i]: a[i] else: b[i]

when isMainModule:
  assert Vector3Wide is Vector
  assert Vector is SomeVector
  assert Vector3Wide is SomeVector

  var v: Vector[float, 4] = [1.0, 2.0, 3.0, 4.0].toVector
  var a, b, c, d: Vector3
  var w: Vector[float, 2] = [5.0, 6.0].toVector
  var i: Int3

  #var v = Vector{0, 1, 2}
  #var v2: Vector4 = 1

  echo v.z
  echo v.xy
  echo v.xyzyzx

  v.w = v.z
  v[0] = v[1] 
  v.zx = v.xy
  echo v

  w = (1.0, 2.0)
  v = (w, 1.0, 2.0)
  v = (v.xyz, 3.0)  

  echo a * b
  echo a + b * c / d - a
  echo a.reflect(b)
  
  echo i.to(float32).length()

  var m, n: Matrix4x4 = Matrix4x4.identity
  echo m[1, 2]
  echo m[2]
  m[1, 2] = 10.0f
  echo m.m21m31
  
  echo m * n

  let q = Quaternion.fromAxisAngle(Vector3.unitX, PI/2)
  echo Vector3.unitY.transform(q)

  echo q
  echo q.toMatrix().toQuaternion()

  block:
    var
      a, b: Vector3Wide
      c: wide bool
    echo a + b
    echo c.select(a, b)

type
  Color3* = distinct Vector3

  Color4* = distinct Vector4

  Color* = distinct Vector[uint8, 4]

  CompressedQuaternion* = distinct uint32

  Viewport* = tuple
    x, y, width, height, minDepth, maxDepth: float32

  Rectangle* = tuple
    x, y, width, height: int32

  BoundingBox* = object
    minimum*: Vector3
    maximum*: Vector3

  BoundingSphere* = object
    center*: Vector3
    radius*: float

  Ray* = object
    position*: Vector3
    direction*: Vector3

  RayHit* = object
    normal*: Vector3
    distance*: float32

  RigidPose* = object
    position*: Vector3
    orientation*: Quaternion

  PlaneIntersectionType* {.pure.} = enum
    Back,
    Front,
    Intersection

  ContainmentType* {.pure.} = enum
    Disjoint,
    Contains,
    Intersects

  Plane* = object
    normal*: Vector3
    distance*: float

  CubeFace* {.pure.} = enum
    Right, Left, Top, Bottom, Near, Far

  BoundingFrustum* = object
    planes*: array[CubeFace, Plane]

func normal*(face: CubeFace): Vector3 =
  case face:
    of Right: Vector3.unitX
    of Left: -Vector3.unitX
    of Top: Vector3.unitY
    of Bottom: -Vector3.unitY
    of Near: Vector3.unitZ
    of Far: -Vector3.unitZ

func center*(self: BoundingBox): Vector3 =
  (self.maximum + self.minimum) * 0.5f

func extent*(self: BoundingBox): Vector3 =
  self.maximum - self.minimum

func merge*(first, second: BoundingBox): BoundingBox =
  result.minimum = min(first.minimum, second.minimum)
  result.maximum = max(first.maximum, second.maximum)

func contains*(frustum: BoundingFrustum; box: BoundingBox): bool =
  for plane in frustum.planes:
    if dot(box.center, plane.normal) + dot(box.extent, abs(plane.normal)) <= -plane.distance:
      return false
  return true

func contains*(box: BoundingBox; point: Vector3): bool =
  all(box.minimum <= point and box.maximum >= point)

func intersects*(a, b: BoundingBox): bool =
  all(a.maximum >= b.minimum) and all(b.maximum >= a.minimum)
