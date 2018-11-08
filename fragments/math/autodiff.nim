

# TODO: If a value/symbol is defined in the same loop level as a usage, it doesn't need to be loaded from the stack

# proc 𝒟*[T: SomeFloat](f: proc(x: T): T): proc(x: T): T = discard
# proc 𝒟*[T: SomeFloat](f: proc(x, y: T): T): proc(x, y: T): (T, T) = discard
# proc 𝒟*[T: SomeFloat](f: proc(x, y, z: T): T): proc(x, y, z: T): (T, T, T) = discard

# type
#   Vector[T; size: static int] = array[size, T]
#   Matrix[T; height, width: static int] = array[height, array[width, T]]

#   proction[T; n, m: static int] = proc(x: Vector[T, n]): Vector[T, m]
#   Derivative[T; n, m: static int] = proc(x: Vector[T, n]): Matrix[T, m, n]

# proc `*`*[T; n, m: static int](a: Vector[T, n]; b: Matrix[T, n, m]): Vector[T, m] = discard 

# proc 𝒟*[T: SomeFloat; n, m: static int](f: proction[T, n, m]): Derivative[T, n, m] = discard

# proc pullback*[T: SomeFloat; n, m: static int](
#   f: proction[T, n, m], x: Vector[T, n]): proction[T, m, n] =
#   return proc(adjoint: Vector[T, m]): Vector[T, n] = adjoint * 𝒟(f)(x)

# proc differential*[T: SomeFloat; n, m: static int](
#   f: proction[T, n, m], x: Vector[T, n]): proction[T, n, m] =
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

# proc prod(items: openarray[int]): int =
#   result = 1
#   for item in items:
#     result *= item

# type
#   Tensor[T; shape: static openarray[int]] = object
#     data: array[prod(shape), T]

#   Matrix[T; height, width: static int] = Tensor[T, [height, width]]

#   Vector[T; size: static int] = Tensor[T, [size]]

# proc shape*(T: typedesc[Matrix]): array[2, int] {.inline.} = [T.height, T.width]
# proc shape*(T: typedesc[Vector]): array[1, int] {.inline.} = [T.size]

# var
#   t: Tensor[float32, [3, 4, 5]]
#   m: Matrix[float32, 4, 3]
#   v: Vector[float32, 4]

# echo t.shape
# echo t.data.len
# #echo m.shape # compiler error: expr(skGenericParam); unknown symbol
# echo m.data.len
# #echo v.shape # compiler error: expr(skGenericParam); unknown symbol
# echo v.data.len

# t.data[0] = 1.0
# echo t.data[0]

type
  Dual*[T] = tuple[primal, derivative: T]

  ProcInfo = ref object
    sym: NimNode
    tangents: seq[NimNode]
    adjoints: seq[NimNode]

var procs {.compiletime.}: seq[ProcInfo]

proc getOrCreateProcInfo(sym: NimNode): ProcInfo {.compileTime.} =
  var ident = sym
  if ident.kind == nnkAccQuoted:
    ident = ident[0]
  expectKind(ident, nnkIdent)

  for p in procs:
    if p.sym == ident:
      return p
  result = ProcInfo(sym: ident)
  procs.add(result)

macro adjointOf*(sym: untyped; procDef: typed): untyped =
  expectKind(procDef, { nnkProcDef, nnkFuncDef })
  let info = getOrCreateProcInfo(sym)
  info.adjoints.add(procDef.name)

macro tangentOf*(sym: untyped; procDef: typed): untyped =
  expectKind(procDef, { nnkProcDef, nnkFuncDef })
  let info = getOrCreateProcInfo(sym)
  info.tangents.add(procDef.name)

proc adjointNeg*[T](x, originalResult, seed: T): T {.adjointOf: `-`.} =
  -seed

proc tangentNeg*[T](x: Dual[T];  originalResult, seed: T): T =
  -x.derivative

proc adjointAdd*[T](left, right, originalResult, seed: T): tuple[left, right: T] {.adjointOf: `+`.} =
  (seed, seed)

proc tangentAdd*[T](left, right: Dual[T], originalResult: T): T =
  left.derivative + right.derivative

proc adjointSub*[T](left, right, originalResult, seed: T): tuple[left, right: T] {.adjointOf: `-`.} =
  (seed, -seed)

proc tangentSub*[T](left, right: Dual[T], originalResult: T): T =
  left.derivative - right.derivative

proc adjointMul*[T](left, right, originalResult, seed: T): tuple[left, right: T] {.adjointOf: `*`.} =
  (seed * right, seed * left)

proc tangentMul*[T](left, right: Dual[T]; seed: T): T =
  (left.derivative * right.value + left.value * right.derivative)

proc adjointDiv*[T](left, right, originalResult, seed: T): tuple[left, right: T] {.adjointOf: `/`.} =
  (seed / right, -seed * left / (right * right))

proc tangentDiv*[T](left, right: Dual[T]; seed: T): T =
  (left.derivative * right.value - left.value * right.derivative) / (right.value)

proc adjointSqrt*[T](x, originalResult, seed: T): T {.adjointOf: sqrt.} =
  seed / (2 * originalResult)

proc adjointExp*[T](x, originalResult, seed: T): T {.adjointOf: exp.} =
  seed * originalResult

proc tangentExp*[T](x: Dual[T]; originalResult, seed: T): T =
  x.derivative * exp(x.value)

proc adjointLog*[T](x, originalResult, seed: T): T {.adjointOf: log.} =
  seed / x

proc tangentLog*[T](x: Dual[T]; originalResult, seed: T): T =
  x.derivative / x.value

proc adjointSin*[T](x, originalResult, seed: T): T {.adjointOf: sin.} =
  cos(x) * seed

proc tangentSin*[T](x: Dual[T]; originalResult, seed: T): T =
  x.derivative * cos(x.value)

proc adjointCos*[T](x, originalResult, seed: T): T {.adjointOf: cos.} =
  -sin(x) * seed

proc tangentCos*[T](x: Dual[T]; originalResult, seed: T): T =
  -x.derivative * sin(x.value)

proc adjointTan*[T](x, originalResult, seed: T): T {.adjointOf: tan.} =
  seed * (1 + originalResult * originalResult)

# proc adjointAsin*[T](x, originalResult, seed: T): T =
#   seed * (-x * seed + 1).rsqrt()

# proc adjointAcos*[T](x, originalResult, seed: T): T =
#   seed * -(-x * seed + 1).rsqrt()

proc adjointAtan*[T](x, originalResult, seed: T): T =
  seed / (x * x + 1)

proc adjointSinh*[T](x, originalResult, seed: T): T =
  cosh(x) * seed

proc adjointCosh*[T](x, originalResult, seed: T): T =
  -sinh(x) * seed

proc foo*[T](x: T): T =
  sin(x)

proc adjointFoo*[T](x, originalResult, seed: T): T =
  cos(x) * seed

proc adjointFoo*[T](x, seed, adjoint: T): T =
  cos(x) * adjoint

type
  Stack = object
    data: seq[byte]

proc push[T](self: var Stack; value: T) =
  let offset = self.data.len
  self.data.setLen(offset + sizeof(T))
  cast[ptr T](addr self.data[offset])[] = value

proc pop(self: var Stack; T: typedesc): T =
  let offset = self.data.len - sizeof(T)
  result = cast[ptr T](addr self.data[offset])[]
  self.data.setLen(offset)

type
  Context = ref object
    stack: NimNode
    returnSym: NimNode
    resultSym: NimNode
    adjointSymbols: seq[(NimNode, NimNode)]
    blocks: seq[Block]

  Block = object
    head: NimNode
    primal: NimNode
    adjoint: NimNode
    breakCount: int

proc currentBlock(context: Context): Block =
  context.blocks[^1]

proc isInLoop(context: Context): bool =
  for b in context.blocks:
    if b.head.kind in { nnkForStmt, nnkWhileStmt }:
      return true

proc getAdjoint(sym: NimNode): NimNode {.compileTime.} =
 
  for p in procs:
    if $sym == $p.sym:
      return nnkClosedSymChoice.newTree(p.adjoints)

  # let
  #   procDef = sym.getImpl()
  #   resultType = procDef.params[0].getType()

  # let gradType = nnkTupleTy.newTree()
  # for i in 1 ..< procDef.params.len:
  #   let param = procDef.params[i].copyNimTree()
  #   param[^1] = newEmptyNode()
  #   gradType.add(param)

  # let adjointProcSym = genSym(nskProc)
  # let adjointProcDef = quote do:
  #   proc `adjointProcSym`(inputs: ; seed: `gradType`; originalResult: `resultType`): `inputType` =
  #     `adjointBody`

  # return adjointProcSym

type
  Result = tuple[primal, adjoint: NimNode]

proc genNode(context: Context; primal: NimNode; seed: NimNode): Result

proc genPushPop(context: Context; primal: NimNode; typ: NimNode = nil): Result =
  if context.isInLoop():
    let
      stack = context.stack
      primalType = if typ != nil: typ else: primal.getType()

    result.primal = quote do:
      let temp = `primal`
      `stack`.push(temp)
      temp
    result.adjoint = quote do:
      `stack`.pop(type(`primalType`))

  else:
    let tempSym = genSym(nskLet)
    result.primal = quote do:
      let `tempSym` = `primal`
      `tempSym`
    result.adjoint = tempSym

proc genCall(context: Context; primal: NimNode; seed: NimNode): Result =
  
  let adjointSym = primal[0].getAdjoint()
  if adjointSym == nil:
    return (primal, newStmtList())
    # error($primal[0] & " is not differentiable.", primal)
  
  let
    stack = context.stack
    adjointCall = newCall(adjointSym)
    adjointResultSym = genSym(nskLet)
    adjointParams = newStmtList()
    primalResultSym = genSym(nskLet)

  var primalParams: seq[NimNode]

  result.adjoint = newStmtList()
  for i in 1 ..< primal.len:

    let paramSeed = if primal.len == 2:
        adjointResultSym
      else:
        let index = i - 1
        quote do: `adjointResultSym`[`index`]

    let
      (primalChild, adjointChild) = context.genNode(primal[i], `paramSeed`)
      (pushParam, popParam) = context.genPushPop(primalChild)
      primalParam = genSym(nskLet)
    
    primal[i] = pushParam

    primalParams.add(primalParam)
    result.adjoint.insert(0, quote do:
      let`primalParam` = `popParam`)

    adjointParams.add(adjointChild)
    
  let (pushResult, popResult) = context.genPushPop(primal)

  # Fetch the primal result first
  result.adjoint.insert(0, quote do:
    let `primalResultSym` = `popResult`)

  adjointCall.add(primalParams)
  adjointCall.add(primalResultSym)
  adjointCall.add(seed)

  result.adjoint.add quote do:
    let `adjointResultSym` = `adjointCall`
    #echo `adjointResultSym`
    `adjointParams`

  result.primal = pushResult

proc genIf(context: Context; primal: NimNode; seed: NimNode): Result =
  
  let
    adjoint = primal.copyNimNode()
    stack = context.stack

  for i, primalChild in primal:
    let adjointChild = primalChild.copyNimNode()
    adjoint.add(adjointChild)

    if primalChild.kind != nnkElse:
      let
        # TODO: Transform condition too
        (pushCondition, popCondition) = context.genPushPop(primalChild[0])

      # Evaluate the branch condition and save the result
      primalChild[0] = pushCondition

      # Load the result and take the same path
      adjointChild.add(popCondition)

    let r = context.genNode(primalChild[^1], seed)
    primalChild[^1] = r.primal
    adjointChild.add(r.adjoint)

  return (primal, adjoint)

proc genBlock(context: Context; primal, seed: NimNode; head: NimNode = nil): Result =
  result = (newStmtList(), newStmtList())
  context.blocks.add(Block(head: head, primal: result.primal, adjoint: result.adjoint))
  try:
    let (primal, adjoint) = context.genNode(primal, seed)
    result.primal.add(primal)
    result.adjoint.add(adjoint)
    return result
  finally:
    discard context.blocks.pop()

proc genFor(context: Context; primal: NimNode; seed: NimNode): Result =

  let
    stack = context.stack
    counter = genSym(nskVar)
    (pushCounter, popCounter) = context.genPushPop(counter, bindSym"int")
    (it, itAdjoint) = context.genNode(primal[^2], nil)
    (body, bodyAdjoint) = context.genBlock(primal[^1], nil, primal)

  primal[^2] = it

  primal[^1] = quote do:
    `body`
    inc `counter`

  result.primal = quote do:
    var `counter`: int
    `primal`
    discard `pushCounter`

  result.adjoint = quote do:
    var count = `popCounter`
    while count > 0:
      dec count
      `bodyAdjoint`
      `itAdjoint`
    `itAdjoint`

proc genWhile(context: Context; primal: NimNode; seed: NimNode): Result =

  let
    stack = context.stack
    counter = genSym(nskVar)
    (pushCounter, popCounter) = context.genPushPop(counter, bindSym"int")
    (condition, conditionAdjoint) = context.genNode(primal[0], nil)
    (body, bodyAdjoint) = context.genBlock(primal[1], nil, primal)

  primal[0] = condition

  primal[1] = quote do:
    `body`
    inc `counter`

  result.primal = quote do:
    var `counter`: int
    `primal`
    discard `pushCounter`

  result.adjoint = quote do:
    var count = `popCounter`
    while count > 0:
      dec count
      `bodyAdjoint`
      `conditionAdjoint`
    `conditionAdjoint`

proc genBreak(context: Context; primal: NimNode; targetBlockIndex: int): Result =

  let stack = context.stack
  result.primal = quote do:
    `stack`.push(false) # The following block did not execute
    `primal` 

  result.adjoint = newStmtList()

  for i in countdown(context.blocks.high, targetBlockIndex):
    inc context.blocks[i].breakCount

proc genNode(context: Context; primal: NimNode; seed: NimNode): Result =

  case primal.kind:
    of nnkStmtList, nnkStmtListExpr:
      result.primal = newStmtList()
      result.adjoint = newStmtList()
      for child in primal: # TODO: reverse
        let x = context.genNode(child, seed)
        result.primal.add(x.primal)
        result.adjoint.insert(0, x.adjoint)

    of nnkLiterals: return (primal, newStmtList())

    of nnkCallKinds: return context.genCall(primal, seed)

    of nnkAsgn, nnkFastAsgn:
      expectKind(primal[0], nnkSym)
      for item in context.adjointSymbols:
        if item[0] == primal[0]:
          let adjointSym = item[1]

          let seed = quote do:
            let seed = `adjointSym`
            `adjointSym`.reset()
            seed

          (primal[1], result.adjoint) = context.genNode(primal[1], seed)
          result.primal = primal
          break

    of nnkIfStmt, nnkIfExpr: return context.genIf(primal, seed)

    of nnkWhileStmt: return context.genWhile(primal, seed)

    of nnkForStmt: return context.genFor(primal, seed)
    
    of nnkBlockStmt: return context.genBlock(primal[1], seed, primal)

    of nnkBreakStmt:
      if primal[0].kind != nnkSym:
        return context.genBreak(primal, context.blocks.high)
      else:
        for i in 0 .. context.blocks.high:
          let head = context.blocks[i].head
          if head.kind == nnkBlockStmt and head[0] == primal[0]:
            return context.genBreak(primal, i)

    of nnkContinueStmt:        
      for i in countdown(context.blocks.high, 0):
        if context.blocks[i].head.kind in { nnkForStmt, nnkWhileStmt }:
          return context.genBreak(primal, i)

    of nnkReturnStmt:
      return context.genBreak(primal, 0)

    of nnkVarSection, nnkLetSection:
      let adjointVarSecion = nnkVarSection.newTree()
      context.currentBlock.adjoint.insert(0, adjointVarSecion)
      result.adjoint = newStmtList()
      for child in primal: # TODO: reverse
        for i in countup(child.len - 3, 0):
          let
            sym = child[i]
            adjointSym = genSym(nskVar)

          # Create a variable declaration at the beginning of the
          # current of the block and register the symbol.
          context.adjointSymbols.add((sym, adjointSym))
          adjointVarSecion.add(newIdentDefs(adjointSym, sym.getType(), newEmptyNode()))

          # If there is an initializer, generate the adjoint.
          # The seed is the adjoint symbol to the original variable.
          # TODO: Share code with nnkAsgn
          if child[^1].kind != nnkEmpty:
            let (newPrimal, adjoint) = context.genNode(child[^1], adjointSym)
            child[^1] = newPrimal
            result.adjoint.add(adjoint)        

      result.primal = primal

    of nnkSym:
      if primal.getType().typeKind == ntyBool:
        return (primal, newStmtList())

      result.primal = primal

      var adjointSym: NimNode
      block findAdjointSym:
        for item in context.adjointSymbols:
          if item[0] == primal:
            adjointSym = item[1]
            break findAdjointSym

        # It's variable that was not declared in the transforms body
        adjointSym = genSym(nskVar)
        context.adjointSymbols.add((primal, adjointSym))

        let adjointType = primal.getType()
        context.blocks[0].primal.insert(0, quote do:
          var `adjointSym`: `adjointType`)

      result.adjoint = quote do: `adjointSym` += `seed`

    of nnkProcDef, nnkFuncDef, nnkMethodDef, nnkTypeSection, nnkConstSection: return (primal, newStmtList())

    else: error("Unhandled node kind: " & $primal.kind, primal)

#macro gradient*(body: typed): untyped =
  # let def = body.getImpl()
  # echo def.astGenRepr

  # let newBody = genNode(def.body)

  # return quote do:
  #   (proc() = `newBody`)
  
proc bar*(x: SomeFloat): SomeFloat = foo(x)

#let seed = gradient(bar)

macro gradient*(independent: typed; body: typed): untyped =
  #echo astGenRepr body
  if body.getType.typeKind == ntyVoid:
    error("Gradient expression must return a value", body)

  let
    stack = genSym(nskVar)
    returnSym = genSym(nskLabel)
    primalResultSym = genSym(nskVar)
    primalResultType = body.getType()
    adjointResultSym = genSym(nskVar)
    adjointResultType = independent.getType()

  let context = Context(
    returnSym: returnSym,
    resultSym: primalResultSym,
    stack: stack)

  context.adjointSymbols.add((independent, adjointResultSym))

  let (primal, adjoint) = context.genBlock(body, newLit(1.0))

  result = quote do:
    var
      `primalResultSym`: `primalResultType`
      `adjointResultSym`: `adjointResultType`
      `stack`: Stack
    block `returnSym`:
      discard `primal`
      `adjoint`
    `adjointResultSym`

  echo repr result

when isMainModule:
  var stack: Stack
  stack.push(1.int)
  echo stack.pop(int)

  block:
    var x: float = 1.0
    let d = gradient x:
      # var y = x
      # for i in 0 ..< 2:
      #   y = y + x
      # y
    
      var y = x
      var i = 0
      while i < 2:
        y = y + x
        # if true:
        #   break
        #y = y + x
        inc i
      y
    
      # let y = foo(sin(x))
      # let z = sin(y)
      # sin(z)
    
    
      # if true:
      #   foo(1.0)
      # else:
      #   foo(2.0)
    
    echo d

  import torch

  converter toTensor(x: float): Tensor = torch.tensor([x])

  proc adjointTanh*(x, originalResult, seed: Tensor): Tensor {.adjointOf: tanh.} =
    tanh_backward_impl(seed.expand(originalResult.sizes()), originalResult)

  proc `+=`(self: var Tensor; other: Tensor) =
    if not self.is_defined:
      self = other
    else:
      self = self + other#.sum_to()

  var
    x = torch.tensor([[0.1, 0.3], [-0.4, 0.2]])
    y = torch.tensor([[0.7, -0.5], [0.1, 0.1]])
    z = torch.tensor([[0.2, -0.4], [-0.5, -0.2]])

  let r = gradient x:
    ((x + y) * y).sin() + (z - x).tanh()

  echo r