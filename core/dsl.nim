import macros, tables, sequtils

var customTypes {.compileTime.} = initTable[string, tuple[inherit, procs, methods: NimNode]]()

# proc isValidTypeDef(name, typeName: string): bool {.compileTime.} =
#   let fooType = nnkTypeDef.newTree(
#     nnkPostfix.newTree(
#       newIdentNode("*"),
#       newIdentNode("Foo")
#     ),
#     newEmptyNode(),
#     nnkObjectTy.newTree(
#       newEmptyNode(),
#       newEmptyNode(),
#       nnkRecList.newTree(
#         nnkIdentDefs.newTree(
#           newIdentNode(name),
#           newIdentNode(typeName),
#           newEmptyNode()
#         )
#       )
#     )
#   )
#   template inner(x: untyped): untyped = x
#   let code = getAst(inner(fooType)).toStrLit
#   return compiles(code)

proc getNodeName(node: NimNode): NimNode =
  case node.kind
  of nnkPragmaExpr: # in the case of param {.private.}: float
    result = node[0] 
  of nnkIdent: # in the case of param: float
    result = node
  else:
    result = nil

proc getNodeType(node: NimNode): tuple[typeName, defaultValue: NimNode] =
  case node[0].kind
  of nnkAsgn: # in the case of param {.private.}: float
    # echo node[0].treeRepr
    result = (node[0][0], node[0][1])
  of nnkIdent: # in the case of param: float
    # echo node.treeRepr
    result = (node[0], nil)
  else:
    result = (nil, nil)

proc generateCustomCode(classDefName, className: string; body: NimNode): NimNode {.compileTime.} =
  let
    subresult = nnkStmtList.newTree()
    recList = nnkRecList.newTree()
    inherit = customTypes[classDefName].inherit
    procs = customTypes[classDefName].procs
    methods = customTypes[classDefName].methods
    selfNode = newIdentNode(className & "_self")

  var
    procOrMethodNames = newSeq[string]()
    initialization = newStmtList()
  
  for aproc in procs: procOrMethodNames.add($aproc[0])
  for amethod in methods: procOrMethodNames.add($amethod[0])

  for node in body:
    if node.kind != nnkCall:
      continue

    let name = getNodeName(node[0])
    
    # ignore procs or methods
    if procOrMethodNames.contains($name):
      continue
    
    let
      (paramTypeName, paramDefaultValue) = getNodeType(node[1])
      
    if paramTypeName != nil:
      recList.add(nnkIdentDefs.newTree(
        name,
        paramTypeName,
        newEmptyNode()
      ))

    if paramDefaultValue != nil:
      initialization.add(nnkAsgn.newTree(
        nnkDotExpr.newTree(
          selfNode,
          name
        ),
        paramDefaultValue
      ))
  
  # define our type
  subresult.add(nnkTypeSection.newTree(
    nnkTypeDef.newTree(
      nnkPostfix.newTree(
        newIdentNode("*"),
        newIdentNode(className)
      ),
      newEmptyNode(),
      nnkObjectTy.newTree(
        newEmptyNode(),
        inherit,
        recList
      )
    )
  ))

  # generate: {.this: MyEntity_self.}
  subresult.add(nnkPragma.newTree(
    nnkExprColonExpr.newTree(
      newIdentNode("this"),
      selfNode
    )
  ))

  return subresult

macro makeClass*(name, definition: untyped): untyped =
  result = nnkStmtList.newTree()
  let classDefName = $name
  var
    inherit = newEmptyNode()
    procs = newStmtList()
    methods = newStmtList()

  for node in definition:
    case node.kind
    of nnkCall:
      case $node[0]:
        of "super":
          doAssert(node[1].kind == nnkStmtList and node[1][0].kind == nnkIdent, "super needs to be followed by a type name")
          inherit = nnkOfInherit.newTree(node[1][0])
    of nnkProcDef:
      procs.add(node)
    of nnkMethodDef:
      methods.add(node)
    else:
      discard
  
  customTypes.add(classDefName, (inherit, procs, methods))

  # finally build the actual macro we use to declare our new classes
  result.add quote do:
    macro `name`*(subname, body: untyped): untyped =
      return generateCustomCode(`classDefName`, $subname, body)

when isMainModule:
  type
    MyBase = object of RootObj
      value: string

  method testMethod(b: MyBase) {.base.} = echo "Base"

  makeClass entity:
    super: MyBase
    proc start()
    proc run(state: int)
    proc stop()
    method testMethod(wow: ref int)

  dumpAstGen:
    type
      MyEntity* = object of MyBase
        param0: int
    
    type
      MyEntity = object
        param0: int

    var e: MyEntity
    e.params0 = 10

  entity MyEntity:
    # {.private.}:
      # param2: 10
    param0: 10
    param1 {.public.}: 10
    var0: float
    var1: float = 2.0

    start:
      discard
    
    run:
      discard
    
    stop:
      discard

    testMethod:
      echo "Derived"
      echo param0

  proc start(self: MyEntity) =
    const param0 = 10

  proc run(self: MyEntity, state: int) =
    const param0 = 10

  expandMacros:
    entity MyEntity2:
      param0: 10
      var0 {.public.}: 10
      var0: float
      var1: float = 2.0

      start:
        discard
      
      run:
        discard
      
      stop:
        discard