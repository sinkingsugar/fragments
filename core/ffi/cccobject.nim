{.experimental.}

import macros, tables

type
  CCCObject* {.nodecl.} = object
  CCCConcept* = concept type T
    T.isCCCConcept

var
  mangledNames {. compileTime .} = initTable[string, string]()
  nameCounter {. compileTime .} = 0

# var nullptr* {.nodecl, importcpp:"(nullptr)".}: object
type CCCGlobalType* = object
proc isCCCConcept*(T: typedesc[CCCGlobalType]): bool = true
var global* {.nodecl.}: CCCGlobalType
const CCCGlobalName = "global"

const
  setImpl = "#[#] = #"
  getImpl = "#[#]"

template mangleCCCName(name: string): string =
  inc nameCounter
  "mangledName" & $nameCounter

proc validCCCName(name: string): bool =
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

{.emit:["""/*TYPESECTION*/
#ifdef __cplusplus
template<typename T>
static inline void callCCCPtrDestructor(T* instance) { instance->~T(); }
#endif
  """].}

macro defineCCCType*(name: untyped, importCppStr: string, headerStr: string = nil): untyped =
  result = nnkStmtList.newTree()

  result.add quote do:
    type `name` {.header: "", importcpp: "".} = object

  if headerStr != nil:
    # replace empty string with proper values
    result[0][0][0][1][0][1] = newStrLitNode($headerStr)
    result[0][0][0][1][1][1] = newStrLitNode($importCppStr)
  else:
    # remove header pragma
    result[0][0][0][1].del(0)
    # replace empty string with proper values
    result[0][0][0][1][0][1] = newStrLitNode($importCppStr)

  result.add quote do:
    proc isCCCConcept*(T: typedesc[`name`]): bool = true

# constructor call
proc cppinit*(T: typedesc[CCCConcept]): T {.importcpp:"'0(@)", varargs, constructor.}

# magic placement new constructor for ptrs
proc cppctor*[T: CCCConcept](x: ptr T): ptr T {.header:"new", importcpp: "(new (#) '*0(@))", varargs, nodecl, discardable.}

# magic placement new constructor for refs
proc cppctor*[T: CCCConcept](x: ref T): ref T {.header:"new", importcpp: "(new (#) '*0(@))", varargs, nodecl, discardable.}

# normal destructor for value types
proc cppdtor*[T: CCCConcept](x: T) {.importcpp:"#.~'1()".}

# magic placement new compatible destructor for ptrs
proc cppdtor*[T: CCCConcept](x: ptr T) {.importcpp:"callCCCPtrDestructor(#)".}

# magic placement new compatible destructor for refs
proc cppdtor*[T](x: ref T) {.importcpp:"callCCCPtrDestructor(#)".}

proc cppdelptr*[T: CCCConcept](x: ptr T) =
  x.cppdtor()
  dealloc(x)

proc cppdelref*[T: CCCConcept](x: ref T) =
  x.cppdtor()

template cppnewref*(myRef: ref CCCConcept, args: typed): untyped =
  new(myRef, proc(self: type(myRef)) = self.cppdelref())
  myRef.cppctor(args)

template cppnewref*(myRef: ref CCCConcept): untyped =
  new(myRef, proc(self: type(myRef)) = self.cppdelref())
  myRef.cppctor()

template cppnewptr*(myPtr: ptr CCCConcept, args: typed): untyped =
  myPtr = cast[type(myPtr)](alloc0(sizeof(type(myPtr[]))))
  myPtr.cppctor(args)

template cppnewptr*(myPtr: ptr CCCConcept): untyped =
  myPtr = cast[type(myPtr)](alloc0(sizeof(type(myPtr[]))))
  myPtr.cppctor()

proc `+`  *(x, y: CCCObject): CCCObject {.importcpp:"(# + #)".}
proc `-`  *(x, y: CCCObject): CCCObject {.importcpp:"(# - #)".}
proc `*`  *(x, y: CCCObject): CCCObject {.importcpp:"(# * #)".}
proc `/`  *(x, y: CCCObject): CCCObject {.importcpp:"(# / #)".}
proc `%`  *(x, y: CCCObject): CCCObject {.importcpp:"(# % #)".}
proc `+=` *(x, y: CCCObject): CCCObject {.importcpp:"(# += #)", discardable.}
proc `-=` *(x, y: CCCObject): CCCObject {.importcpp:"(# -= #)", discardable.}
proc `*=` *(x, y: CCCObject): CCCObject {.importcpp:"(# *= #)", discardable.}
proc `/=` *(x, y: CCCObject): CCCObject {.importcpp:"(# /= #)", discardable.}
proc `%=` *(x, y: CCCObject): CCCObject {.importcpp:"(# %= #)", discardable.}
proc `++` *(x: CCCObject): CCCObject {.importcpp:"(++#)".}
proc `--` *(x: CCCObject): CCCObject {.importcpp:"(--#)".}
proc `>`  *(x, y: CCCObject): CCCObject {.importcpp:"(# > #)".}
proc `<`  *(x, y: CCCObject): CCCObject {.importcpp:"(# < #)".}
proc `>=` *(x, y: CCCObject): CCCObject {.importcpp:"(# >= #)".}
proc `<=` *(x, y: CCCObject): CCCObject {.importcpp:"(# <= #)".}
proc `and`*(x, y: CCCObject): CCCObject {.importcpp:"(# && #)".}
proc `or` *(x, y: CCCObject): CCCObject {.importcpp:"(# || #)".}
proc `not`*(x: CCCObject): CCCObject {.importcpp:"(!#)".}
proc `in` *(x, y: CCCObject): CCCObject {.importcpp:"(# in #)".}

proc `[]`*(obj: CCCObject, field: auto): CCCObject {.importcpp:getImpl.}
  ## Return the value of a property of name `field` from a JsObject `obj`.

proc `[]=`*[T](obj: CCCObject, field: auto, val: T) {.importcpp:setImpl.}
  ## Set the value of a property of name `field` in a JsObject `obj` to `v`.

proc `[]`*(obj: CCCConcept, field: auto): CCCObject {.importcpp:getImpl.}
  ## Return the value of a property of name `field` from a JsObject `obj`.

proc `[]=`*[T](obj: CCCConcept, field: auto, val: T) {.importcpp:setImpl.}
  ## Set the value of a property of name `field` in a JsObject `obj` to `v`.

# Conversion to and from CCCObject
proc to*(x: CCCObject, T: typedesc): T {. importcpp: "('0)(#)" .}
  ## Converts a CCCObject `x` to type `T`.

# Conversion to and from CCCObject
proc to*[T](x: CCCObject): T {. importcpp: "('0)(#)" .}
  ## Converts a CCCObject `x` to type `T`.

proc toCCC*[T](val: T): CCCObject {. importcpp: "(#)" .}
  ## Converts a value of any type to type CCCObject

template toCCC*(s: string): CCCObject = cstring(s).toCCC

macro cccFromAst*(n: untyped): untyped =
  result = n
  if n.kind == nnkStmtList:
    result = newProc(procType = nnkDo, body = result)
  return quote: toCCC(`result`)

macro `.`*(obj: CCCConcept, field: untyped): CCCObject =
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
  if validCCCName($field):
    if obj.len == 0 and $obj == CCCGlobalName:
      let importString = "(" & $field & ")"
      result = quote do:
        proc helper(): CCCObject {.importcpp:`importString`, gensym.}
        helper()
    else:
      let importString = "#." & $field
      result = quote do:
        proc helper(o: CCCConcept): CCCObject {.importcpp:`importString`, gensym.}
        helper(`obj`)
  
  else:
    if not mangledNames.hasKey($field):
      mangledNames[$field] = $mangleCCCName($field)
    
    let importString = "#." & mangledNames[$field]
    
    result = quote do:
      proc helper(o: CCCConcept): CCCObject {.importcpp:`importString`, gensym.}
      helper(`obj`)

macro `.=`*(obj: CCCConcept, field, value: untyped): untyped =
  ## Experimental dot accessor (set) for type JsObject.
  ## Sets the value of a property of name `field` in a JsObject `x` to `value`.
  if validCCCName($field):
    if obj.len == 0 and $obj == CCCGlobalName:
      let importString = $field & " = #"
      result = quote do:
        proc helper(v: auto) {.importcpp:`importString`, gensym.}
        helper(`value`)
    else:
      let importString = "#." & $field & " = #"
      result = quote do:
        proc helper(o: CCCConcept, v: auto) {.importcpp:`importString`, gensym.}
        helper(`obj`, `value`)

  else:
    if not mangledNames.hasKey($field):
      mangledNames[$field] = $mangleCCCName($field)
    let importString = "#." & mangledNames[$field] & " = #"
    result = quote do:
      proc helper(o: CCCConcept, v: auto) {. importcpp: `importString`, gensym .}
      helper(`obj`, `value`)

macro `.()`*(obj: CCCConcept, field: untyped, args: varargs[CCCObject, cccFromAst]): CCCObject =
  # Experimental "method call" operator for type CCCObject.
  # Takes the name of a method of the JavaScript object (`field`) and calls
  # it with `args` as arguments, returning a CCCObject (which may be discarded,
  # and may be `undefined`, if the method does not return anything,
  # so be careful when using this.)
  #
  # Example:
  #
  # .. code-block:: nim
  #
  #  # Let's get back to the console example:
  #  var console {. importc, nodecl .}: CCCObject
  #  let res = console.log("I return undefined!")
  #  console.log(res) # This prints undefined, as console.log always returns
  #                   # undefined. Thus one has to be careful, when using
  #                   # CCCObject calls.
  
  var importString: string
  if obj.len == 0 and $obj == CCCGlobalName:
    importString = $field & "(@)"
    
    result = quote:
      proc helper(): CCCObject {.importcpp:`importString`, gensym, discardable.}
      helper()
  
  else:
    if validCCCName($field):
      importString = "#." & $field & "(@)"

    else:
      if not mangledNames.hasKey($field):
        mangledNames[$field] = $mangleCCCName($field)
      importString = "#." & mangledNames[$field] & "(@)"

    result = quote:
      proc helper(o: CCCConcept): CCCObject {.importcpp:`importString`, gensym, discardable.}
      helper(`obj`)
  
  for idx in 0 ..< args.len:
    let paramName = newIdentNode(("param" & $idx).toNimIdent)
    result[0][3].add newIdentDefs(paramName, newIdentNode("CCCObject".toNimIdent))
    result[1].add args[idx].copyNimTree

when isMainModule:
  {.emit:"#include <stdio.h>".}
  
  defineCCCType(MyClass, "MyClass", "MyClass.hpp")
  defineCCCType(MyClass2, "MyClass2", "MyClass.hpp")

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
      echo $n
      echo $x.test(1).to(cdouble)
      echo $x.test(x.test2(2)).to(cdouble)
      echo $x.test2(3).to(cint)

      z.cppdtor()
      x.cppdtor()

      var x1 = cppinit(MyClass2, 1)
      echo $x1.test20(1).to(cint)
    
    run()
