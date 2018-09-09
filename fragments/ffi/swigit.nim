import macros, tables, strutils, ospaths

var module {.compileTime.}: string

static:
  module &= "%module nimexports\n\n"

proc addProcExportPragmas(procNode: var NimNode) {.compileTime.} =
  if procNode[4].kind == nnkEmpty:
    procNode[4] = nnkPragma.newTree()
  
  procNode[4].add(newIdentNode("exportc"))

proc swigProc(procNode: var NimNode) {.compileTime.} =
  discard

proc swigitNode(node: NimNode): NimNode {.compileTime.} =
  case node.kind
  of nnkProcDef:
    result = node
    addProcExportPragmas result
    swigProc result
  of nnkTypeSection:
    result = newStmtList()
    for subNode in node:
      result.add swigitNode(subNode)
  # of nnkTypeDef:
  #   echo node.treeRepr
  of nnkStmtList:
    result = newStmtList()
    for subNode in node:
      result.add swigitNode(subNode)
  else:
    raiseAssert("Node type cannot be swigged around yet " & $node.kind)
  
macro swigit*(node: untyped): untyped =
  result = swigitNode(node)

when isMainModule:
  proc myProc*() {.swigit.} =
    discard

  # swigit:
  #   type
  #     MyType = object
  #       x: int

