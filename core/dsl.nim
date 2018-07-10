import macros, tables, sequtils

var customTypes {.compileTime.} = initTable[string, tuple[inherit, procs, methods: NimNode]]()

proc getNodeName(node: NimNode): NimNode =
  case node[0].kind
  of nnkPragmaExpr: # in the case of param {.private.}: float
    result = node[0][0] 
  of nnkIdent: # in the case of param: float
    result = node[0]
  else:
    result = nil

proc getNodeType(node: NimNode): tuple[typeName, defaultValue: NimNode] =
  case node[1][0].kind
  of nnkAsgn: # in the case of param {.private.}: float
    result = (node[1][0][0], node[1][0][1])
  of nnkIdent: # in the case of param: float
    result = (node[1][0], nil)
  else:
    result = (nil, nil)

proc isPublic(node: NimNode): bool =
  result = false
  if node[0].kind == nnkPragmaExpr:
    # find, eg: var0 {.public, used, seri: off.}: float
    for subnode in node[0][1]:
      if subnode.kind == nnkIdent and $subnode == "public":
        result = true

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
    procOrMethodBodies = initTable[string, NimNode]()
    initialization = newStmtList()
    constants = newStmtList()
    publicConstants = newStmtList()
  
  for aproc in procs: procOrMethodNames.add($aproc[0])
  for amethod in methods: procOrMethodNames.add($amethod[0])
  
  # find variables and constants
  for node in body:
    if node.kind != nnkCall:
      continue

    let name = getNodeName(node)
    
    # ignore procs or methods
    if procOrMethodNames.contains($name):
      procOrMethodBodies.add($name, node[1])
      continue
    
    let
      (paramTypeName, paramDefaultValue) = getNodeType(node)
    
    # if we have type we qualify to be a variable
    if paramTypeName != nil:
      # generate record inside the type directly
      if isPublic(node):
        recList.add(nnkIdentDefs.newTree(
          nnkPostfix.newTree(
            newIdentNode("*"),
            name
          ),
          paramTypeName,
          newEmptyNode()
        ))
      else:
        recList.add(nnkIdentDefs.newTree(
          name,
          paramTypeName,
          newEmptyNode()
        ))

      # generate value initalization to include in our generated constructor
      if paramDefaultValue != nil:
        initialization.add(nnkAsgn.newTree(
          nnkDotExpr.newTree(
            selfNode,
            name
          ),
          paramDefaultValue
        ))
    else: # assume we are a constant to inject every proc/method
      if isPublic(node):
        # const myConst = 10
        constants.add(nnkConstSection.newTree(
          nnkConstDef.newTree(
            nnkPragmaExpr.newTree(
              name,
              nnkPragma.newTree(
                newIdentNode("used")
              )
            ),
            newEmptyNode(),
            node[1][0]
          )
        ))
        # func myConst*(_: typedesc[MyClass]): auto = 10
        # use: var x = MyClass.myConst
        publicConstants.add(nnkFuncDef.newTree(
          nnkPostfix.newTree(
            newIdentNode("*"),
            name
          ),
          newEmptyNode(),
          newEmptyNode(),
          nnkFormalParams.newTree(
            newIdentNode("auto"),
            nnkIdentDefs.newTree(
              newIdentNode("_"),
              nnkBracketExpr.newTree(
                newIdentNode("typedesc"),
                newIdentNode(className)
              ),
              newEmptyNode()
            )
          ),
          newEmptyNode(),
          newEmptyNode(),
          nnkStmtList.newTree(
            node[1][0]
          )
        ))
      else:
        # const myConst = 10
        constants.add(nnkConstSection.newTree(
          nnkConstDef.newTree(
            nnkPragmaExpr.newTree(
              name,
              nnkPragma.newTree(
                newIdentNode("used")
              )
            ),
            newEmptyNode(),
            node[1][0]
          )
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

  # generate init proc
  subresult.add(nnkProcDef.newTree(
    nnkPostfix.newTree(
      newIdentNode("*"),
      newIdentNode("init")
    ),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      newEmptyNode(),
      nnkIdentDefs.newTree(
        selfNode,
        nnkVarTy.newTree(
          newIdentNode(className)
        ),
        newEmptyNode()
      )
    ),
    newEmptyNode(),
    newEmptyNode(),
    nnkStmtList.newTree(
      initialization
    )
  ))

  # generate procs and methods
  for signature in procs:
    let procCopy = copyNimTree(signature)
    let constantsCopy = copyNimTree(constants)
    procCopy[0] = nnkPostfix.newTree(
      newIdentNode("*"),
      procCopy[0]
    )
    procCopy[3].insert(1, nnkIdentDefs.newTree(
      selfNode,
      nnkVarTy.newTree(
        newIdentNode(className)
      ),
      newEmptyNode()
    ))
    procCopy[6] = nnkStmtList.newTree(
      constantsCopy,
      procOrMethodBodies[$signature[0]]
    )
    subresult.add(procCopy)
  
  for signature in methods:
    if procOrMethodBodies.contains($signature[0]):
      let procCopy = copyNimTree(signature)
      let constantsCopy = copyNimTree(constants)
      procCopy[0] = nnkPostfix.newTree(
        newIdentNode("*"),
        procCopy[0]
      )
      procCopy[3].insert(1, nnkIdentDefs.newTree(
        selfNode,
        nnkVarTy.newTree(
          newIdentNode(className)
        ),
        newEmptyNode()
      ))
      procCopy[6] = nnkStmtList.newTree(
        constantsCopy,
        procOrMethodBodies[$signature[0]]
      )
      subresult.add(procCopy)

  # generate consts
  subresult.add(publicConstants)

  return subresult

macro archetype*(name, definition: untyped): untyped =
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
  
  doAssert(not customTypes.contains(classDefName), classDefName & " already declared!")
  customTypes.add(classDefName, (inherit, procs, methods))

  # finally build the actual macro we use to declare our new classes
  result.add quote do:
    macro `name`*(subname, body: untyped): untyped =
      return generateCustomCode(`classDefName`, $subname, body)

when isMainModule:
  import asyncdispatch

  type
    MyBase = object of RootObj
      value: string

  method testMethod(b: var MyBase; wow: ref int) {.base.} = echo "Base"

  archetype entity:
    super: MyBase
    proc start()
    proc run(state: int) {.async.}
    proc stop()
    method testMethod(wow: ref int)

  # dumpAstGen:
  #   type
  #     MyEntity* = object of MyBase
  #       param0: int
  #       param1*: float
    
  #   type
  #     MyEntity = object
  #       param0: int

  #   var e: MyEntity
  #   e.params0 = 10

  #   const myConst {.used.} = 10

  #   func myConst1*(_: typedesc[MyBase]): auto = 10

  #   proc start*(self: var MyEntity) =
  #     const param0 = 10
    
  #   var x = myConst1

  entity MyEntity:
    # {.private.}:
      # param2: 10
    param0: 10
    param1 {.public.}: 20
    var0 {.public, used, seri: off.}: float
    var1: float = 2.0

    start:
      var ten = param0
      var ten2 = param1
      var1 = (ten + ten2).float
    
    run:
      echo param0
      echo param1
      echo state
    
    stop:
      echo param0

    testMethod:
      echo "Derived"
      echo param0

  var x = MyEntity.param1
  var ent = new MyEntity
  ent[].init()
  ent[].start()
  asyncCheck ent[].run(x)
  ent[].testMethod(nil)
  
  echo ent.var1
  assert(ent.var1 == 30.0)

  entity MyEntity2:
    # {.private.}:
      # param2: 10
    param0: 10
    param1 {.public.}: 40
    var0 {.public, used, seri: off.}: float
    var1: float = 9.0

    start:
      discard
    
    run:
      echo param0
      echo param1
      echo state
    
    stop:
      echo param0

  var y = MyEntity2.param1
  var ent2 = new MyEntity2
  ent2[].init()
  ent2[].start()
  asyncCheck ent2[].run(x)
  ent2[].testMethod(nil)
  
  echo ent2.var1
  assert(ent2.var1 == 9.0)

  # expandMacros:
  #   entity MyEntity2:
  #     param0: 10
  #     param1 {.public.}: 10
  #     var0: float
  #     var1: float = 2.0

  #     start:
  #       discard
      
  #     run:
  #       discard
      
  #     stop:
  #       discard

  #     testMethod:
  #       echo "LOL"