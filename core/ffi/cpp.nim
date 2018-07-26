{.experimental.}

import macros, tables, strutils

type
  CppProxy* {.nodecl.} = object
  CppObject* = concept type T
    T.isCppObject

when defined(js):
  type WasmPtr* = distinct int
else:
  # compiler utilities
  macro cppdefines*(defines: varargs[string]): untyped =
    result = nnkStmtList.newTree()
    
    for adefine in defines:
      var str: string
      when defined(windows) and defined(vcc):
        str = "/D" & $adefine
      else:
        str = "-D" & $adefine
        
      result.add nnkPragma.newTree(nnkExprColonExpr.newTree(newIdentNode("passC"), newLit(str)))
      
  macro cppincludes*(includes: varargs[string]): untyped =
    result = nnkStmtList.newTree()
    
    for incl in includes:
      var str: string
      when defined windows:
        let win_incl = ($incl).replace("/", "\\")
        when defined vcc:
          str = "/I" & win_incl
        else:
          str = "-I" & win_incl
      else:
        str = "-I" & $incl
      
      result.add nnkPragma.newTree(nnkExprColonExpr.newTree(newIdentNode("passC"), newLit(str)))
      
  macro cppfiles*(files: varargs[string]): untyped =
    result = nnkStmtList.newTree()
    
    for file in files:
      var str: string
      when defined windows:
        let win_incl = ($file).replace("/", "\\") 
        str = win_incl
      else:
        str = $file
      
      result.add nnkPragma.newTree(nnkExprColonExpr.newTree(newIdentNode("compile"), newLit(str)))
      
  macro cpplibpaths*(paths: varargs[string]): untyped =
    result = nnkStmtList.newTree()
    
    for path in paths:
      var str: string
      when defined windows:
        let win_path = ($path).replace("/", "\\")
        when defined vcc:
          str = "/LIBPATH:" & win_path
        else:
          str = "-L" & win_path
      else:
        str = "-L" & $path
      
      result.add nnkPragma.newTree(nnkExprColonExpr.newTree(newIdentNode("passL"), newLit(str)))
      
  macro cpplibs*(libs: varargs[string]): untyped =
    result = nnkStmtList.newTree()
    
    for lib in libs:
      var str: string
      when defined windows:
        let win_incl = ($lib).replace("/", "\\") 
        str = win_incl
      else:
        str = $lib
      
      result.add nnkPragma.newTree(nnkExprColonExpr.newTree(newIdentNode("passL"), newLit(str)))

var
  mangledNames {. compileTime .} = initTable[string, string]()
  nameCounter {. compileTime .} = 0

type CppGlobalType* = object
proc isCppObject*(T: typedesc[CppGlobalType]): bool = true
var global* {.nodecl.}: CppGlobalType
const CppGlobalName = "global"

const
  setImpl = "#[#] = #"
  getImpl = "#[#]"

template mangleCppName(name: string): string =
  inc nameCounter
  "mangledName" & $nameCounter

proc validCppName(name: string): bool =
  result = true
  const reservedWords = ["break", "case", "catch", "class", "const", "continue",
    "debugger", "default", "delete", "do", "else", "export", "extends",
    "finally", "for", "function", "if", "import", "in", "instanceof", "new",
    "return", "super", "switch", "this", "throw", "try", "typeof", "var",
    "void", "while", "with", "yield", "enum", "implements", "interface",
    "let", "package", "private", "protected", "public", "static", "await",
    "abstract", "boolean", "byte", "char", "double", "final", "float", "goto",
    "int", "long", "native", "short", "synchronized", "throws", "transient",
    "volatile", "null", "true", "false"]
  case name
  of reservedWords: return false
  else: discard
  if name[0] notin {'A'..'Z','a'..'z','_','$'}: return false
  for chr in name:
    if chr notin {'A'..'Z','a'..'z','_','$','0'..'9'}:
      return false

when not defined(js):
  {.emit:["""/*TYPESECTION*/
  #ifdef __cplusplus
  template<typename T>
  static inline void callCppPtrDestructor(T* instance) { instance->~T(); }
  #endif
    """].}

macro defineCppType*(name: untyped, importCppStr: string, headerStr: string = nil): untyped =
  result = nnkStmtList.newTree()

  result.add quote do:
    type `name`* {.header: "", importcpp: "", inheritable.} = object

  if headerStr != nil:
    # replace empty string with proper values
    result[0][0][0][1][0][1] = newStrLitNode($headerStr)
    result[0][0][0][1][1][1] = newStrLitNode($importCppStr)
  else:
    # remove header pragma
    result[0][0][0][1].del(0)
    # replace empty string with proper values
    result[0][0][0][1][0][1] = newStrLitNode($importCppStr)

  var converterName = newIdentNode("to" & $name)
  result.add quote do:
    converter `converterName`*(co: CppProxy): `name` {.used, importcpp:"(#)".}
    proc isCppObject*(T: typedesc[`name`]): bool = true

template cppOverride*(str: string) {.pragma, used.}
template cppCall*(str: string) {.pragma, used.}

# to be improved
macro defineCppSubType*(name: untyped, superType: typed, superCppStr: string, procs: untyped): untyped =
  result = nnkStmtList.newTree()

  var internalName = $name & "CppStruct"

  var typeDecl = quote do:
    type `name` {.importcpp:"".} = object of `superType`
      x: int
  typeDecl[0][0][1][0][1] = newStrLitNode($internalName)

  var procsToAdd = newSeq[NimNode]()

  var emitStmts = newSeq[NimNode]()

  # we need forward decl
  result.add quote do:
    {.emit:["struct ", `internalName`,";\n"].}

  emitStmts.add quote do:
    {.emit:["struct ", `internalName`, " : public ", `superCppStr`, " {\n"].}

  for node in procs:
    echo node.treeRepr
    case node.kind
    of nnkProcDef:  
      var procStr = node[4][0][1]
      var callStr = node[4][1][1]
      var procName = $name & $node[0]
      if node[3][0].kind == nnkEmpty: # void return
        emitStmts.add quote do:
          {.emit:[`procStr`, " override { return ", `procName`, "(this", `callStr`,"); }\n"].}
      else:
        emitStmts.add quote do:
          {.emit:[`procStr`, " override { ", `procName`, "(this", `callStr`, "); }\n"].}

      # mangle the name of the generated proc since we export c
      node[0] = newIdentNode(procName)
      # make sure we generate properly, idealy codegendecl is better?
      node.addPragma(newIdentNode("exportc"))

      # inject self
      node[3].insert(1, newIdentDefs(ident("self"), `name`))

      procsToAdd.add(node)
    of nnkCall:
      discard
    else:
      discard

  emitStmts.add quote do:
    {.emit:["\n};\n"].}

  result.add(typeDecl)

  for procToAdd in procsToAdd:
    result.add(procsToAdd)
  
  for emitStmt in emitStmts:
    result.add(emitStmt)

  var converterName = newIdentNode("to" & $name)
  result.add quote do:
    converter `converterName`*(co: CppProxy): `name` {.used, importcpp:"(#)".}
    proc isCppObject*(T: typedesc[`name`]): bool = true

# constructor call
proc cppinit*(T: typedesc[CppObject]): T {.importcpp:"'0(@)", varargs, constructor.}

# magic placement new constructor for ptrs
proc cppctor*[T: CppObject](x: ptr T): ptr T {.header:"new", importcpp: "(new (#) '*0(@))", varargs, nodecl, discardable.}

# magic placement new constructor for refs
proc cppctor*[T: CppObject](x: ref T): ref T {.header:"new", importcpp: "(new (#) '*0(@))", varargs, nodecl, discardable.}

# normal destructor for value types
proc cppdtor*[T: CppObject](x: T) {.importcpp:"#.~'1()".}

# magic placement new compatible destructor for ptrs
proc cppdtor*[T: CppObject](x: ptr T) {.importcpp:"callCppPtrDestructor(#)".}

# magic placement new compatible destructor for refs
proc cppdtor*[T](x: ref T) {.importcpp:"callCppPtrDestructor(#)".}

proc cppdelptr*[T: CppObject](x: ptr T) =
  x.cppdtor()
  dealloc(x)

proc cppdelref*[T: CppObject](x: ref T) =
  x.cppdtor()

# refs

template cppnewref*(myRef: ref CppObject): untyped =
  new(myRef, proc(self: type(myRef)) = self.cppdelref())
  myRef.cppctor()

# I could not find a way to avoid generating one of the following per each arg yet (so far varargs, typed, untyped didn't work)

template cppnewref*(myRef: ref CppObject, arg0: typed): untyped =
  new(myRef, proc(self: type(myRef)) = self.cppdelref())
  myRef.cppctor(arg0)

template cppnewref*(myRef: ref CppObject, arg0: typed, arg1: typed): untyped =
  new(myRef, proc(self: type(myRef)) = self.cppdelref())
  myRef.cppctor(arg0, arg1)

# ptr

template cppnewptr*(myPtr: ptr CppObject): untyped =
  myPtr = cast[type(myPtr)](alloc0(sizeof(type(myPtr[]))))
  myPtr.cppctor()

# I could not find a way to avoid generating one of the following per each arg yet (so far varargs, typed, untyped didn't work)

template cppnewptr*(myPtr: ptr CppObject, arg0: typed): untyped =
  myPtr = cast[type(myPtr)](alloc0(sizeof(type(myPtr[]))))
  myPtr.cppctor(arg0)

template cppnewptr*(myPtr: ptr CppObject, arg0, arg1: typed): untyped =
  myPtr = cast[type(myPtr)](alloc0(sizeof(type(myPtr[]))))
  myPtr.cppctor(arg0, arg1)

template cppnewptr*(myPtr: ptr CppObject, arg0, arg1, arg2: typed): untyped =
  myPtr = cast[type(myPtr)](alloc0(sizeof(type(myPtr[]))))
  myPtr.cppctor(arg0, arg1, arg2)

proc `+`  *(x, y: CppProxy): CppProxy {.importcpp:"(# + #)".}
proc `-`  *(x, y: CppProxy): CppProxy {.importcpp:"(# - #)".}
proc `*`  *(x, y: CppProxy): CppProxy {.importcpp:"(# * #)".}
proc `/`  *(x, y: CppProxy): CppProxy {.importcpp:"(# / #)".}
proc `%`  *(x, y: CppProxy): CppProxy {.importcpp:"(# % #)".}
proc `+=` *(x, y: CppProxy): CppProxy {.importcpp:"(# += #)", discardable.}
proc `-=` *(x, y: CppProxy): CppProxy {.importcpp:"(# -= #)", discardable.}
proc `*=` *(x, y: CppProxy): CppProxy {.importcpp:"(# *= #)", discardable.}
proc `/=` *(x, y: CppProxy): CppProxy {.importcpp:"(# /= #)", discardable.}
proc `%=` *(x, y: CppProxy): CppProxy {.importcpp:"(# %= #)", discardable.}
proc `++` *(x: CppProxy): CppProxy {.importcpp:"(++#)".}
proc `--` *(x: CppProxy): CppProxy {.importcpp:"(--#)".}
proc `>`  *(x, y: CppProxy): CppProxy {.importcpp:"(# > #)".}
proc `<`  *(x, y: CppProxy): CppProxy {.importcpp:"(# < #)".}
proc `>=` *(x, y: CppProxy): CppProxy {.importcpp:"(# >= #)".}
proc `<=` *(x, y: CppProxy): CppProxy {.importcpp:"(# <= #)".}
proc `<<` *(x, y: CppProxy): CppProxy {.importcpp:"(# << #)".}
proc `>>` *(x, y: CppProxy): CppProxy {.importcpp:"(# >> #)".}
proc `and`*(x, y: CppProxy): CppProxy {.importcpp:"(# && #)".}
proc `or` *(x, y: CppProxy): CppProxy {.importcpp:"(# || #)".}
proc `not`*(x: CppProxy): CppProxy {.importcpp:"(!#)".}
proc `in` *(x, y: CppProxy): CppProxy {.importcpp:"(# in #)".}

proc `[]`*(obj: CppProxy, field: auto): CppProxy {.importcpp:getImpl.}
  ## Return the value of a property of name `field` from a JsObject `obj`.

proc `[]=`*[T](obj: CppProxy, field: auto, val: T) {.importcpp:setImpl.}
  ## Set the value of a property of name `field` in a JsObject `obj` to `v`.

proc `[]`*(obj: CppObject, field: auto): CppProxy {.importcpp:getImpl.}
  ## Return the value of a property of name `field` from a JsObject `obj`.

proc `[]=`*[T](obj: CppObject, field: auto, val: T) {.importcpp:setImpl.}
  ## Set the value of a property of name `field` in a JsObject `obj` to `v`.

when defined(js):
  # Conversion to and from CppProxy
  proc to*(x: CppProxy, T: typedesc): T {. importcpp: "(#)" .}
    ## Converts a CppProxy `x` to type `T`.

  # Conversion to and from CppProxy
  proc to*[T](x: CppProxy): T {. importcpp: "(#)" .}
    ## Converts a CppProxy `x` to type `T`.
else:
  # Conversion to and from CppProxy
  proc to*(x: CppProxy, T: typedesc[void]): T {. importcpp: "(#)" .}
    ## Converts a CppProxy `x` to type `T`.

  # Conversion to and from CppProxy
  proc to*(x: CppProxy, T: typedesc): T {. importcpp: "('0)(#)" .}
    ## Converts a CppProxy `x` to type `T`.

  # Conversion to and from CppProxy
  proc to*[T](x: CppProxy): T {. importcpp: "('0)(#)" .}
    ## Converts a CppProxy `x` to type `T`.

proc toCpp*[T](val: T): CppProxy {. importcpp: "(#)" .}
  ## Converts a value of any type to type CppProxy

template toCpp*(s: string): CppProxy = cstring(s).toCpp

converter toByte*(co: CppProxy): int8 {.used, importcpp:"(#)".}
converter toUByte*(co: CppProxy): uint8 {.used, importcpp:"(#)".}

converter toShort*(co: CppProxy): int16 {.used, importcpp:"(#)".}
converter toUShort*(co: CppProxy): uint16 {.used, importcpp:"(#)".}

converter toInt*(co: CppProxy): int {.used, importcpp:"(#)".}
converter toUInt*(co: CppProxy): uint {.used, importcpp:"(#)".}

converter toLong*(co: CppProxy): int64 {.used, importcpp:"(#)".}
converter toULong*(co: CppProxy): uint64 {.used, importcpp:"(#)".}

converter toFloat*(co: CppProxy): float {.used, importcpp:"(#)".}
converter toFloat32*(co: CppProxy): float32 {.used, importcpp:"(#)".}

converter toDouble*(co: CppProxy): float64 {.used, importcpp:"(#)".}

converter toCString*(co: CppProxy): cstring {.used, importcpp:"(#)".}

when defined(js):
  converter toWasmPtr*(co: CppProxy): WasmPtr {.used, importcpp:"(#)".}

macro CppFromAst*(n: untyped): untyped =
  result = n
  if n.kind == nnkStmtList:
    result = newProc(procType = nnkDo, body = result)
  return quote: toCpp(`result`)

macro `.`*(obj: CppObject, field: untyped): CppProxy =
  ## Experimental dot accessor (get) for type JsObject.
  ## Returns the value of a property of name `field` from a JsObject `x`.
  ##
  ## Example:
  ##
  ## .. code-block:: nim
  ##
  ##  let obj = newJsObject()
  ##  obj.a = 20
  ##  console.log(obj.a) # puts 20 onto the console.
  if validCppName($field):
    if obj.len == 0 and $obj == CppGlobalName:
      let importString = "(" & $field & ")"
      result = quote do:
        proc helper(): CppProxy {.importcpp:`importString`, gensym.}
        helper()
    else:
      let importString = "#." & $field
      result = quote do:
        proc helper(o: CppObject): CppProxy {.importcpp:`importString`, gensym.}
        helper(`obj`)
  else:
    if not mangledNames.hasKey($field):
      mangledNames[$field] = $mangleCppName($field)
    
    let importString = "#." & mangledNames[$field]
    
    result = quote do:
      proc helper(o: CppObject): CppProxy {.importcpp:`importString`, gensym.}
      helper(`obj`)

macro `.=`*(obj: CppObject, field, value: untyped): untyped =
  ## Experimental dot accessor (set) for type JsObject.
  ## Sets the value of a property of name `field` in a JsObject `x` to `value`.
  if validCppName($field):
    if obj.len == 0 and $obj == CppGlobalName:
      let importString = $field & " = #"
      result = quote do:
        proc helper(v: auto) {.importcpp:`importString`, gensym.}
        helper(`value`.toCpp)
    else:
      let importString = "#." & $field & " = #"
      result = quote do:
        proc helper(o: CppObject, v: auto) {.importcpp:`importString`, gensym.}
        helper(`obj`, `value`.toCpp)
  else:
    if not mangledNames.hasKey($field):
      mangledNames[$field] = $mangleCppName($field)
    let importString = "#." & mangledNames[$field] & " = #"
    result = quote do:
      proc helper(o: CppObject, v: auto) {. importcpp: `importString`, gensym .}
      helper(`obj`, `value`.toCpp)

macro dynamicCppCall*(obj: CppObject, field: untyped, args: varargs[CppProxy, CppFromAst]): CppProxy =
  # Experimental "method call" operator for type CppProxy.
  # Takes the name of a method of the JavaScript object (`field`) and calls
  # it with `args` as arguments, returning a CppProxy (which may be discarded,
  # and may be `undefined`, if the method does not return anything,
  # so be careful when using this.)
  #
  # Example:
  #
  # .. code-block:: nim
  #
  #  # Let's get back to the console example:
  #  var console {. importc, nodecl .}: CppProxy
  #  let res = console.log("I return undefined!")
  #  console.log(res) # This prints undefined, as console.log always returns
  #                   # undefined. Thus one has to be careful, when using
  #                   # CppProxy calls. 
  var importString: string
  if obj.len == 0 and $obj == CppGlobalName:
    importString = $field & "(@)"
    
    result = quote:
      proc helper(): CppProxy {.importcpp:`importString`, gensym.}
      helper()
  else:
    if validCppName($field):
      when defined(js):
        importString = "#." & "_" & $field & "(@)"
      else:
        importString = "#." & $field & "(@)"
    else:
      if not mangledNames.hasKey($field): mangledNames[$field] = $mangleCppName($field)
      when defined(js):
        importString = "#." & "_" & mangledNames[$field] & "(@)"
      else:
        importString = "#." & mangledNames[$field] & "(@)"

    result = quote:
      proc helper(o: CppObject): CppProxy {.importcpp:`importString`, gensym.}
      helper(`obj`)
  
  for idx in 0 ..< args.len:
    let paramName = ident("param" & $idx)
    result[0][3].add newIdentDefs(paramName, ident("CppProxy"))
    result[1].add args[idx].copyNimTree

macro `.()`*(obj: CppObject, field: untyped, args: varargs[CppProxy, CppFromAst]): CppProxy = 
  # Experimental "method call" operator for type CppProxy.
  # Takes the name of a method of the JavaScript object (`field`) and calls
  # it with `args` as arguments, returning a CppProxy (which may be discarded,
  # and may be `undefined`, if the method does not return anything,
  # so be careful when using this.)
  #
  # Example:
  #
  # .. code-block:: nim
  #
  #  # Let's get back to the console example:
  #  var console {. importc, nodecl .}: CppProxy
  #  let res = console.log("I return undefined!")
  #  console.log(res) # This prints undefined, as console.log always returns
  #                   # undefined. Thus one has to be careful, when using
  #                   # CppProxy calls. 
  var importString: string
  if obj.len == 0 and $obj == CppGlobalName:
    importString = $field & "(@)"
    
    result = quote:
      proc helper(): CppProxy {.importcpp:`importString`, gensym.}
      helper()
  else:
    if validCppName($field):
      when defined(js):
        importString = "#." & "_" & $field & "(@)"
      else:
        importString = "#." & $field & "(@)"
    else:
      if not mangledNames.hasKey($field): mangledNames[$field] = $mangleCppName($field)
      when defined(js):
        importString = "#." & "_" & mangledNames[$field] & "(@)"
      else:
        importString = "#." & mangledNames[$field] & "(@)"

    result = quote:
      proc helper(o: CppObject): CppProxy {.importcpp:`importString`, gensym.}
      helper(`obj`)
  
  for idx in 0 ..< args.len:
    let paramName = ident("param" & $idx)
    result[0][3].add newIdentDefs(paramName, ident("CppProxy"))
    result[1].add args[idx].copyNimTree
    
# iterator utils
type CppIterator* {.importcpp: "'0::iterator".} [T] = object
proc itBegin [T] (cset: T): CppIterator[T] {.importcpp:"(#.begin())".}
proc itEnd [T] (cset: T): CppIterator[T] {.importcpp:"(#.end())".}
proc itPlusPlus [T] (csetIt: var CppIterator[T]): CppIterator[T] {.importcpp:"(++#)".}
proc itValue [T, R] (csetIt: var CppIterator[T]): R {.importcpp:"(*#)".}
proc itEqual [T] (csetIt: var CppIterator[T], csetIt2: var CppIterator[T]): bool {.importcpp:"(operator==(#, #))".}
iterator cppItems*[T, R](cset: var T): R =
  var it = cset.itBegin()
  var itend =  cset.itEnd()
  while not itEqual(it, itend):
    yield itValue[T, R](it)
    it = it.itPlusPlus

when isMainModule:
  {.emit:"#include <stdio.h>".}
  {.emit:"#include <string>".}
  
  cppdefines("MYDEFINE", "MYDEFINE2=10")
  cppincludes(".")
  cppfiles("MyClass.cpp")
  cpplibpaths(".")
  
  defineCppType(MyClass, "MyClass", "MyClass.hpp")
  defineCppType(MyClass2, "MyClass2", "MyClass.hpp")
  
  #[ to be improved
  defineCppSubType(MyOwnClass2, MyClass2, "MyClass2"):
    proc testVir(): cint {.cppOverride:"int testVir()", cppCall:"".} =
      return 22
    proc testVir2(i: cint): cint {.cppOverride:"int testVir2(int i)", cppCall:",i".} =
      return i + 22
    proc testVir3(i: cint) {.cppOverride:"void testVir3(int i)", cppCall:",i".} =
      echo i + 22
  ]#

  # expandMacros:
  # dumpAstGen:
  when true: 
    proc run() =
      var x = cppinit(MyClass, 1)
      var y = cast[ptr MyClass](alloc0(sizeof(MyClass)))
      var w: ref MyClass
      new(w, proc(self: ref MyClass) = self.cppdtor())
      var z = y.cppctor(1)
      var q = w.cppctor(1)
      
      var j: ref MyClass
      cppnewref(j, 1)
      j.number = 22
      echo $j.number.to(cint)
      
      var k: ptr MyClass 
      cppnewptr(k, 2)
      k.number = 55
      echo $k.number.to(cint)
      k.cppdelptr
      
      echo $global.globalNumber.to(cint)
      global.globalNumber = 102
      echo $global.globalNumber.to(cint)
      
      global.printf("Hello World\n".cstring).to(void)
      y.test3().to(void)
      y.test4(7, 8).to(void)

      echo $q.number.to(cint)
      y.number = 80
      y.numbers[0] = 23
      var n = (x.number + y.number + y.numbers[0]).to(cint)
      var nInt: int = x.number + y.number + y.numbers[0]
      echo $nInt
      echo $n
      echo $x.test(1).to(cdouble)
      echo $x.test(x.test2(2)).to(cdouble)
      echo $x.test2(3).to(cint)

      z.cppdtor()
      x.cppdtor()

      var x1 = cppinit(MyClass2, 1)
      echo $x1.test20(1).to(cint)

      var c1 = x1.class1.to(MyClass)
      c1.test3().to(void)
      x1.class1.test3().to(void)
      var myFloat: float32 = x1.myDouble
      echo $myFloat
      var myStr = "Hello Mars"
      x1.myCstring = myStr
      echo $x1.myCstring.to(cstring)
      # TODO check macros -> callsite macro

      x1.testVir3(11).to(void)

      # var subx1 = cppinit(MyOwnClass2)
      # echo $subx1.test20(1).to(cint)

      # subx1.testVir3(11).to(void)
      # subx1.testVir4(11).to(void)

    run()
