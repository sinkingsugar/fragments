import math, macros, strutils, vectors, common
  
{.experimental.}


const
  DefaultVectorSize = 4

type
  Half* = distinct uint16
  
  float16* = Half

  Vector*[T; size: static[int]] = object
    elements: array[size, T]

  Matrix*[T; height, width: static[int]] = object
    elements: array[width * height, T]

  Wide = Vector[float32, DefaultVectorSize]

  # Common vector types
  Vector2* = Vector[float32, 2]
  Vector3* = Vector[float32, 3]
  Vector4* = Vector[float32, 4]
  Vector2Wide* = Vector[Wide, 2]
  Vector3Wide* = Vector[Wide, 2]
  Vector4Wide* = Vector[Wide, 2]

  Half2* = Vector[Half, 2]
  Half3* = Vector[Half, 3]
  Half4* = Vector[Half, 4]

  Int2* = Vector[int32, 2]
  Int3* = Vector[int32, 3]
  Int4* = Vector[int32, 4]

  # Quaternion types
  QuaternionBase*[T] = distinct Vector[T, 4]
  Quaternion* = distinct Vector[float32, 4]
  QuaternionWide* = distinct Vector[Wide, 4]

  # Common matrix types
  Matrix2x2* = Matrix[float32, 2, 2]
  Matrix3x2* = Matrix[float32, 3, 2]
  Matrix3x3* = Matrix[float32, 3, 3]
  Matrix4x3* = Matrix[float32, 4, 3]
  Matrix4x4* = Matrix[float32, 4, 4]

  SquareMatrix*[T; size: static[int]] = concept v
    v is Matrix[T, size, size]

  NonScalar*[T] = concept v
    v.elements[int] is T
    v.elements[int] = T

template elementwise*(op: untyped): untyped =
  proc op*(self: Vector): Vector =
    for i in 0..<Vector.size:
      result[i] = op(self[i])

template elementwiseBinary*(op: untyped): untyped =
  proc op*(left, right: Vector): Vector =
    for i in 0..<Vector.size:
      result[i] = op(left[i], right[i])

# converter toVector[T; size: static[int]](value: T): Vector[T, size] =
#   for i in 0..<size:
#     result[i] = value

# Vector, quaternion and matrix subscript
func `[]`*(self: NonScalar; index: int): NonScalar.T =
  return self.elements[index]

func `[]=`*(self: var NonScalar; index: int; value: NonScalar.T) =
  self.elements[index] = value

# Matrix subscript
func `[]`*(self: Matrix; row, column: int): Matrix.T =
  return self.elements[row * Matrix.width + column]

func `[]=`*(self: var Matrix; row, column: int; value: Matrix.T) =
  self.elements[row * Matrix.width + column] = value

# Matrix diagonal
func diag*[T; size: static[int]](self: Matrix[T, size, size]): Vector[T, size] =
  for i in 0..<size:
    result[i] = self[i, i]

func `diag=`*[T; size: static[int]](self: var Matrix[T, size, size]; value: Vector[T, size]) =
  for i in 0..<size:
    self[i, i] = value[i]

# Array to vector conversion
func toVector[T; width: static[int]](value: array[width, T]): Vector[T, width] =
  for i in 0..<width:
    result[i] = value[i]

# Vector swizzles
func getSwizzleIndex(c: char): int {.compileTime.} =
  case c:
    of 'x', 'r': return 0
    of 'y', 'g': return 1
    of 'z', 'b': return 2
    of 'w', 'a': return 3
    else: raiseAssert($c & " is not a valid vector swizzle")

# Assumes `[]` operator and the convertor from arrays on vectors
macro `.`*(self: Vector; swizzle: untyped): untyped =
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

    return newStmtList(
      # let temp = self
      nnkLetSection.newTree(newIdentDefs(temp, newEmptyNode(), self)),

      # Vector[temp.T, cardinality]([...])
      newCall(
        nnkBracketExpr.newTree(
          newIdentNode("toVector"),
          newDotExpr(temp, newIdentNode("T")),
          newIntLitNode(cardinality)
        ), values))

# Assumes `[]` and `[]=` operators on vectors
macro `.=`*(self: var Vector; swizzle: untyped; value: untyped): untyped =
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

    return newStmtList(
      # let temp = self
      nnkLetSection.newTree(newIdentDefs(temp, newEmptyNode(), self)),

      # Vector[temp.T, cardinality]([...])
      newCall(
        nnkBracketExpr.newTree(
          newIdentNode("toVector"),
          newDotExpr(temp, newIdentNode("T")),
          newIntLitNode(cardinality)
        ), values))

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

# macro `{}`*(typ: typedesc[Vector], args: varargs[untyped]): auto =
#   for x in xs.children:
#     if x.kind == nnkExprColonExpr:

# 2 element tuples converters
converter toVector*[T](value: (T, T)): Vector[T, 2] = (result.x, result.y) = value

# 3 element tuples converters
converter toVector*[T](value: (T, T, T)): Vector[T, 3] = (result.x, result.x, result.z) = value

# 4 element tuples converters
converter toVector*[T](value: (T, T, T, T)): Vector[T, 4] = (result.x, result.y, result.z, result.w) = value
converter toVector*[T](value: (Vector[T, 2], T, T)): Vector[T, 4] = (result.xy, result.z, result.w) = value
converter toVector*[T](value: (Vector[T, 3], T)): Vector[T, 4] = (result.xyz, result.w) = value

# func zero*(_: typedesc[Vector]): Vector =
#   result = 0

# func one*(_: typedesc[Vector]): Vector =
#   result = 1

func identity*(_: typedesc[QuaternionBase]): QuaternionBase =
  result.w = 1

func identity*[T; size: static[int]](_: typedesc[Matrix[T, size, size]]): Matrix[T, size, size] =
  for i in 0..<size:
    result[i, i] = (T)1
  
# Vector addition
func `+` *[T; w: static[int]](left, right: Vector[T, w]): Vector[T, w] =
  for i in 0..<w:
    result[i] = left[i] + right[i]

func `-` *(self: Vector): Vector =
  for i in 0..<Vector.size:
    result[i] = -self[i]

template `-` *(left, right: Vector): Vector =
  left + -right

# TODO: Somehow causes conflict with `.` operator
# func `/` *(left: Vector; right: Vector.T): Vector =
func `*` *[T; width: static[int]](left: Vector[T, width]; right: T): Vector[T, width] =
  for i in 0..<width:
    result[i] = left[i] * right

template `/` *(left: Vector; right: Vector.T): Vector =
  left * (Vector.T(1) / right)

func `*` *(left, right: Vector): Vector =
  for i in 0..<Vector.size:
    result[i] = left[i] * right[i]

func `/` *(left, right: Vector): Vector =
  for i in 0..<Vector.size:
    result[i] = left[i] / right[i]

func dot*(left, right: Vector): Vector.T =
  for i in 0..<Vector.size:
    result += left[i] * right[i]

func cross*[T](left, right: Vector[T, 3]): Vector[T, 3] =
  return left.yzx * right.zxz - left.zxz * right.yzx

func lengthSquared*(self: Vector): Vector.T =
  return dot(self, self)

func length*(self: Vector): Vector.T =
  return sqrt(lengthSquared)

func distanceSquared*(left, right: Vector): Vector.T =
  return (left - right).lengthSquared

func distance*(left, right: Vector): Vector.T =
  return (left - right).length

func normalized*(self: Vector): Vector =
  let lenth = self.length
  if length > 0:
    return self / self.length
  else:
    return self

func min*(left, right: Vector): Vector =
  for i in 0..<Vector.size:
    result[i] = min(left[i], right[i])

func max*(left, right: Vector): Vector =
  for i in 0..<Vector.size:
    result[i] = max(left[i], right[i])

func clamp*(value, min, max: Vector): Vector =
  return max(min, min(value, max))

func saturate*(value: Vector): Vector =
  return clamp(value, Vector.zero, Vector.one)

func lerp*(min, max: Vector; amount: Vector.T): Vector =
  for i in 0..<Vector.size:
    result[i] = lerp(min[i], max[i], amount)

func smoothstep*(min, max: Vector; amount: Vector.T): Vector =
  var lerpAmount = saturate(amount)
  var lerpAmount = lerpAmount * lerpAmount * (3 - (2 * lerpAmount))
  return lerp(min, max, lerpAmount)

func hermite*(value1, value2, tangent1, tangent2: Vector, amount: Vector.T): Vector =
  let squared = amount * amount;
  let cubed = amount * squared;
  let part1 = ((2 * cubed) - (3 * squared)) + 1;
  let part2 = (-2 * cubed) + (3 * squared);
  let part3 = (cubed - (2 * squared)) + amount;
  let part4 = cubed - squared;

  return (((value1 * part1) + (value2 * part2)) + (tangent1 * part3)) + (tangent2 * part4);

func catmullRom*(value1, value2, value3, value4: Vector, amount: Vector.T): Vector =
  let squared = amount * amount;
  let cubed = squared * amount;

  let factor0 = -cubed + 2 * squared - amount;
  let factor1 = 3 * cubed - 5 * squared + 2;
  let factor2 = -3 * cubed + 4 * squared + amount;
  let factor3 = cubed - squared;

  return 0.5 * (value1 * factor0 + value2 * factor1 + value3 * factor2 + value4 * factor3);

func barycentric*(value1, value2, value3: Vector; amount1, amount2: Vector.T): Vector =
  return (value1 + (amount1 * (value2 - value1))) + (amount2 * (value3 - value1))

func transformNormal*[T](transform: Matrix[T, 4, 4]; normal: Vector[T, 3]): Vector[T, 3] =
  return (transform.m00m01m02 * normal.x) + (transform.m10m11m12 * normal.y) + (transform.m20m21m22 * normal.z)

func transformCoordinate*[T](transform: Matrix[T, 4, 4]; coordinate: Vector[T, 3]): Vector[T, 3] =
  let invW = 1.0 / ((coordinate.x * transform.m03) + (coordinate.x * transform.m13) + (coordinate.z * transform.m23) + transform.m33)
  return (transform.transformNormal(coordinate) + transform.m41m42m43) * invW

func transform*[T](rotation: QuaternionBase[T]; vector: Vector[T, 3]): Vector[T, 3] =
  let x = rotation.x + rotation.x
  let y = rotation.y + rotation.y
  let z = rotation.z + rotation.z
  let wx = rotation.w * x
  let wy = rotation.w * y
  let wz = rotation.w * z
  let xx = rotation.x * x
  let xy = rotation.x * y
  let xz = rotation.x * z
  let yy = rotation.y * y
  let yz = rotation.y * z
  let zz = rotation.z * z

  result.x = ((vector.x * ((1.0 - yy) - zz)) + (vector.y * (xy - wz))) + (vector.z * (xz + wy))
  result.y = ((vector.x * (xy + wz)) + (vector.y * ((1.0 - xx) - zz))) + (vector.z * (yz - wx))
  result.z = ((vector.x * (xz - wy)) + (vector.y * (yz + wx))) + (vector.z * ((1.0 - xx) - yy))

func project*[T](vector: Vector[T, 3]; x, y, width, height, minZ, maxZ: T; worldViewProjection: Matrix): Vector[T, 3] =
  let v = worldViewProjection.transformCoordinate(vector)
  result.x = ((1.0 + v.x) * 0.5 * width) + x
  result.y = ((1.0 - v.y) * 0.5 * height) + y
  result.z = (v.z * (maxZ - minZ)) + minZ

func reflect*[T](vector, normal: Vector[T, 3]): Vector[T, 3] =
  return vector - normal * 2.0 * dot(vector, normal)

# Matrix multiplication
func `*` *[T; height, width, count: static[int]](
  left: Matrix[T, height, count];
  right: Matrix[T, count, width]):
  Matrix[T, height, width] =

  for r in 0..<height:
    for c in 0..<width:
      for i in 0..<count:
        result[r, c] = result[r, c] + left[r, i] * right[i, c]
              
func rotationAxis*[T](axis: Vector[T, 3]; angle: T): Matrix[T, 4, 4] =
  let sin = sin(angle)
  let cos = cos(angle)

  let xx = axis.x * axis.x
  let yy = axis.y * axis.y
  let zz = axis.z * axis.z
  let xy = axis.x * axis.y
  let xz = axis.x * axis.z
  let yz = axis.y * axis.z

  result.m00 = xx + (cos * (1.0f - xx))
  result.m01 = (xy - (cos * xy)) + (sin * axis.z)
  result.m02 = (xz - (cos * xz)) - (sin * axis.y)
  result.m10 = (xy - (cos * xy)) - (sin * axis.z)
  result.m11 = yy + (cos * (1.0f - yy))
  result.m12 = (yz - (cos * yz)) + (sin * axis.x)
  result.m20 = (xz - (cos * xz)) + (sin * axis.y)
  result.m21 = (yz - (cos * yz)) - (sin * axis.x)
  result.m22 = zz + (cos * (1.0f - zz))
  result.m33 = 1

func rotationX*[T](angle: T): Matrix[T, 4, 4] =
  return rotationAxis(Vector[T, 3](1, 0, 0), angle)

func rotationY*[T](angle: T): Matrix[T, 4, 4] =
  return rotationAxis(Vector[T, 3](0, 1, 0), angle)

func rotationZ*[T](angle: T): Matrix[T, 4, 4] =
  return rotationAxis(Vector[T, 3](0, 0, 1), angle)

func rotationYawPitchRoll*[T](yaw, pitch, roll: T): Matrix[T, 4, 4] =
  return rotationZ(roll) * rotationX(pitch) * rotationY(yaw)

static:
  var v: Vector[float, 4] = [1.0, 2.0, 3.0, 4.0].toVector
  var a, b, c, d: Vector3
  var w: Vector[float, 2] = [5.0, 6.0].toVector

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
  
  var m, n: Matrix4x4 = Matrix4x4.identity
  echo m[1, 2]
  echo m[2]
  m[1, 2] = 10.0f
  echo m.m21m31
  
  # echo m * n
  # echo m.transformCoordinate(a)
    
type
  Color3* = distinct Vector3

  Color4* = distinct Vector4

  Color* = distinct Vector[uint8, 4]

  ColorBgra* = distinct Vector[uint8, 4]

  ColorHsv* = distinct Vector4

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
    Right, Left, Top, Bottom, Far, Near

  BoundingFrustum* = object
    planes*: array[CubeFace, Plane]