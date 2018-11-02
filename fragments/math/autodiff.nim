

# TODO: If a value/symbol is defined in the same loop level as a usage, it doesn't need to be loaded from the stack

# func 𝒟*[T: SomeFloat](f: proc(x: T): T): proc(x: T): T = discard
# func 𝒟*[T: SomeFloat](f: proc(x, y: T): T): proc(x, y: T): (T, T) = discard
# func 𝒟*[T: SomeFloat](f: proc(x, y, z: T): T): proc(x, y, z: T): (T, T, T) = discard

# type
#   Vector[T; size: static int] = array[size, T]
#   Matrix[T; height, width: static int] = array[height, array[width, T]]

#   Function[T; n, m: static int] = proc(x: Vector[T, n]): Vector[T, m]
#   Derivative[T; n, m: static int] = proc(x: Vector[T, n]): Matrix[T, m, n]

# func `*`*[T; n, m: static int](a: Vector[T, n]; b: Matrix[T, n, m]): Vector[T, m] = discard 

# func 𝒟*[T: SomeFloat; n, m: static int](f: Function[T, n, m]): Derivative[T, n, m] = discard

# func pullback*[T: SomeFloat; n, m: static int](
#   f: Function[T, n, m], x: Vector[T, n]): Function[T, m, n] =
#   return proc(adjoint: Vector[T, m]): Vector[T, n] = adjoint * 𝒟(f)(x)

# func differential*[T: SomeFloat; n, m: static int](
#   f: Function[T, n, m], x: Vector[T, n]): Function[T, n, m] =
#   return proc(tangent: Vector[T, n]): Vector[T, m] = 𝒟(f)(x) * tangent

# macro gradient(fn: proc; at: typed): proc =
#   discard

# macro gradient(at: untyped; body: untyped): untyped =
#   discard

# when isMainModule:
#   let a = 𝒟 do (x: float) -> float: x

#   var
#     w1 = float32.randomNormal(784, 30)
#     b1 = float32.zeros(30)
#     w2 = float32.randomNormal(30, 10)
#     b2 = float32.zeros(10)

#   for x, y in minibatches:
#     let grads = gradient(params) do (params: Parameters) -> Tensor:

#       let
#         h1 = tanh(matmul(x, params.w1) + params.b1)
#         ŷ = sigmoid(matmul(h1, params.w2) + params.b2)
#         loss = (y - ŷ).squared().mean()
#       echo fmt"Loss is {loss}"
#       return loss
  
#     optimizer.fit(&params, gradients: grads)

import macros, math

func prod(items: openarray[int]): int =
  result = 1
  for item in items:
    result *= item

type
  Tensor[T; shape: static openarray[int]] = object
    data: array[prod(shape), T]

  Matrix[T; height, width: static int] = Tensor[T, [height, width]]

  Vector[T; size: static int] = Tensor[T, [size]]

proc shape*(T: typedesc[Matrix]): array[2, int] {.inline.} = [T.height, T.width]
proc shape*(T: typedesc[Vector]): array[1, int] {.inline.} = [T.size]

var
  t: Tensor[float32, [3, 4, 5]]
  m: Matrix[float32, 4, 3]
  v: Vector[float32, 4]

# echo t.shape
# echo t.data.len
# #echo m.shape # compiler error: expr(skGenericParam); unknown symbol
# echo m.data.len
# #echo v.shape # compiler error: expr(skGenericParam); unknown symbol
# echo v.data.len

# t.data[0] = 1.0
# echo t.data[0]

func foo*(x: SomeFloat): SomeFloat =
  sin(x)

func tangentFoo*(x, dx, originalResult: SomeFloat): SomeFloat =
  cos(x) * dx

func adjointFoo*(x, dx, adjoint: SomeFloat): SomeFloat =
  cos(x) * adjoint

func tangentSin*(x, dx, originalResult: SomeFloat): SomeFloat =
  cos(x) * dx

#pullback(foo)

type
  Stack = object
    data: seq[byte]

func push[T](self: var Stack; value: T) =
  let offset = self.data.len
  self.data.setLen(offset + sizeof(T))
  cast[ptr T](addr self.data[offset])[] = value

func pop(self: var Stack; T: typedesc): T =
  let offset = self.data.len - sizeof(T)
  result = cast[ptr T](addr self.data[offset])[]
  self.data.setLen(offset)

type
  Context = ref object
    stack: NimNode
    returnSym: NimNode
    resultSym: NimNode
    independent: NimNode

proc getTangent(sym: NimNode): NimNode =
  if $sym == "foo":
    return bindSym"tangentFoo"
  elif $sym == "sin":
    return bindSym"tangentSin"

  # let
  #   procDef = sym.getImpl()
  #   resultType = procDef.params[0].getType()

  # let gradType = nnkTupleTy.newTree()
  # for i in 1 ..< procDef.params.len:
  #   let param = procDef.params[i].copyNimTree()
  #   param[^1] = newEmptyNode()
  #   gradType.add(param)

  # let tangentProcSym = genSym(nskProc)
  # let tangentProcDef = quote do:
  #   proc `tangentProcSym`(inputs: ; seed: `gradType`; originalResult: `resultType`): `inputType` =
  #     `tangentBody`

  # return tangentProcSym

type
  Result = tuple[primal, adjoint: NimNode]

proc genNode(context: Context, n: NimNode, seed: NimNode): Result

proc genCall(context: Context, primal: NimNode, seed: NimNode): Result =
  
  let tangentSym = primal[0].getTangent()
  if tangentSym == nil:
    error($primal[0] & " is not differentiable.", primal)
  
  let
    primalResultType = primal.getType()
    stack = context.stack

  let adjointCall = newCall(tangentSym)
  result.adjoint = newStmtList(adjointCall)
  for i in 1 ..< primal.len:
    let
      (primalChild, adjointChild) = context.genNode(primal[i], seed)
      paramSym = genSym(nskLet)
      adjointParam = quote do:
        let `paramSym` = `adjointChild`
    primal[i] = primalChild

    # Execute the adjoint param expressions in reverse
    result.adjoint.insert(0, adjointParam)

    # Then add them to the adjoint call in normal order
    adjointCall.add(paramSym)

  adjointCall.add(seed)
  adjointCall.add quote do:
    `stack`.pop(type(`primalResultType`))

  result.primal = quote do:
    let primalResult = `primal`
    `stack`.push(primalResult)
    primalResult

proc genIf(context: Context, primal: NimNode, seed: NimNode): Result =
  
  let
    adjoint = primal.copyNimNode()
    stack = context.stack

  for i, primalChild in primal:
    let adjointChild = primalChild.copyNimNode()
    adjoint.add(adjointChild)

    if primalChild.kind != nnkElse:
      let condition = primalChild[0]

      # Evaluate the branch condition and save the result
      primalChild[0] = quote do:
        let didEnter = `condition`
        `stack`.push(didEnter)
        didEnter

      # Load the result and take the same path
      adjointChild.add quote do:
        `stack`.pop(bool)

    let r = context.genNode(primalChild[^1], seed)
    primalChild[^1] = r.primal
    adjointChild.add(r.adjoint)

  return (primal, adjoint)

proc genNode(context: Context, n: NimNode, seed: NimNode): Result =

  case n.kind:
    of nnkStmtList, nnkStmtListExpr:
      result.primal = newStmtList()
      result.adjoint = newStmtList()
      for child in n:
        let x = context.genNode(child, seed)
        result.primal.add(x.primal)
        result.adjoint.insert(0, x.adjoint)

    of nnkLiterals: return (n, n)

    of nnkCallKinds: return context.genCall(n, seed)

    of nnkAsgn, nnkFastAsgn: discard

    of nnkIfStmt, nnkIfExpr: return context.genIf(n, seed)
    #of nnkReturnStmt: return context.returnSym

    of nnkSym:
      if n == context.independent: return (n, newLit(1.0))
      # # if n.symKind == nskResult: return (context.resultSym, newEmptyNode())
      # else: {.error: "Not implemented".}
      return (n, n)

    else: error("Unhandled node kind: " & $n.kind, n)

#macro gradient*(body: typed): untyped =
  # let def = body.getImpl()
  # echo def.astGenRepr

  # let newBody = genNode(def.body)

  # return quote do:
  #   (proc() = `newBody`)
  
func bar*(x: SomeFloat): SomeFloat = foo(x)

#let grad = gradient(bar)

macro gradient*(independent: typed; body: typed): untyped =
  #echo astGenRepr body
  #echo body.getType()
  
  let
    stack = genSym(nskVar)
    returnSym = genSym(nskLabel)
    primalResultSym = genSym(nskVar)
    primalResultType = body.getType()

  let context = Context(
    returnSym: returnSym,
    resultSym: primalResultSym,
    stack: stack,
    independent: independent)

  let
    seed = newLit(1.0)
    (primal, adjoint) = context.genNode(body, seed)

  # echo astGenRepr primal
  # echo astGenRepr adjoint

  result = quote do:
    var
      `primalResultSym`: `primalResultType`
      `stack`: Stack
    block `returnSym`:
      discard `primal`
      `adjoint`
  echo repr result

var x: float
let a = gradient x:
  foo(sin(x))
  # if true:
  #   foo(1.0)
  # else:
  #   foo(2.0)

echo a

when isMainModule:
  var stack: Stack
  stack.push(1.int)
  echo stack.pop(int)
