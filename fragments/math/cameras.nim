import linalg
import math

const maximumPitch = PI / 2.0 * 0.99

type
  CameraKind* = enum
    Perspective
    Orthographic

  Camera* = object
    ## Helper for manipulating view/projection parameters, including operations requiring
    ## a fixed target or the world-up-axis, like orbiting and panning.
    position*: Vector3
    target*: Vector3
    up*: Vector3

    nearPlane*: float32
    farPlane*: float32

    case kind: CameraKind:
    of Perspective:
      fieldOfView*: float32 ## Vertical field of view
      aspectRatio*: float32

    of Orthographic:
      extent*: Vector2

func viewDirection*(self: Camera): Vector3 =
  normalize(self.target - self.position)

func `viewDirection=`*(self: var Camera; value: Vector3) =
  self.target = self.position + normalize(value) * distance(self.target, self.position)

func viewRay*(self: Camera): Ray =
  Ray(position: self.position, direction: self.viewDirection)

func right*(self: Camera): Vector3 =
  ## A unit vector pointing to the right of the camera.
  cross(self.viewDirection, self.up).normalize()

func horizontalViewDirection*(self: Camera): Vector3 =
  ## The view direction projected to the horizontal plane, that is, the the plane orthogonal to the up-direction.
  cross(self.up, self.right).normalize()

func horizontalFrame*(self: Camera): Matrix3x3 =
  ## The camera's local coordinate system, as if the viewing direction were orthogonal to the up-direction.
  result.m01m02m03 = self.right
  result.m11m12m13 = self.up
  result.m21m22m23 = -self.horizontalViewDirection

func default*(_: typedesc[Camera]): Camera =
  result.position = Vector3.unitZ
  result.up = Vector3.unitY

  result.fieldOfView = PI / 2.0f
  result.aspectRatio = 4.0f / 3.0f
  result.nearPlane = 1.0f
  result.farPlane = 1000.0f

func view*(self: Camera): Matrix4x4 =
  ## The camera's view matrix.
  lookAt(self.position, self.target, self.up)

func projection*(self: Camera): Matrix4x4 =
  ## The camera's projection matrix.
  if self.kind == Perspective:
    perspectiveFov(self.fieldOfView, self.aspectRatio, self.nearPlane, self.farPlane)
  else:
    orthographic(self.extent.x, self.extent.y, self.nearPlane, self.farPlane)
    
func frustum*(self: Camera): BoundingFrustum =
  ## The view frustum.
  projectionFrustum(self.view * self.projection)

func predictViewDirection*(self: Camera; yaw, pitch, roll: float32): Vector3 =
  ## Gets the direction the camera would look towards, after being rotated by the given angles.

  # TODO: Not rolling
  let
    direction = self.viewDirection

    dot = dot(direction, self.up)
    currentPitch = arccos(clamp(-dot, -1.0f, 1.0f)) - PI / 2.0f
    newPitch = clamp(currentPitch + pitch, -maximumPitch, maximumPitch)
    allowedChange = newPitch - currentPitch

  return direction.transformNormal(rotationAxis(self.right, allowedChange) * rotationAxis(self.up, yaw))

func orbit*(self: var Camera; yaw, pitch: float32) =
  ## Rotates the camera around the current target.
  let distance = distance(self.position, self.target)
  self.position = self.target - self.predictViewDirection(yaw, pitch, 0) * distance

func rotate*(self: var Camera; yaw, pitch, roll: float32) =
  ## Rotates the camera around the eye position.
  self.target = self.position + self.predictViewDirection(yaw, pitch, roll)

func moveTo*(self: var Camera; position: Vector3) =
  ## Changes the eye position, without changing the viewing direction.
  self.target = position + (self.target - self.position)
  self.position = position

func pan*(self: var Camera; offset: Vector3) =
  ## Moves the camera in it's local coordinate system.
  let translation = offset.transform(self.horizontalFrame)
  self.moveTo(self.position + translation)

func zoom*(self: var Camera; factor: float32) = 
  ## Moves towards the target by dividing the distance to the target.
  self.position = self.target + (self.position - self.target) / factor

func zoomTo*(self: var Camera; extent: Vector2) =
  ## Moves towards the target, so as to just fit the given extents on the screen.
  assert self.kind == Perspective
  
  let
    aspectRatio = extent.x / extent.y
    effectiveHeight = if aspectRatio > self.aspectRatio: extent.x / aspectRatio else: extent.y
    distance = effectiveHeight * 0.5f / tan(self.fieldOfView * 0.5f)
    offset = self.viewDirection * distance
  self.position = self.target - offset

func getExtentAt*(self: Camera; distance: float32): Vector2 =
  ## Gets the extenst of the view frustum at the given distance.
  assert self.kind == Perspective

  let
    height = tan(self.fieldOfView * 0.5f)
    width = height / self.aspectRatio
  return (width * distance, height * distance)

# func getScreenRay(self: Camera; screenPosition: Vector2): Ray =
#   # Gets the normalized view ray at the given screen coordinate.
#   let direction = unproject((screenPosition, 0.0f), 0, 0, 1, 1, 0, 1, self.view * self.projection) - self.position
#   return Ray(position: self.position, direction: direction.normalize()); 
