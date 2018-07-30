{.experimental.}

import macros, tables, strutils

type
  CppProxy* {.nodecl.} = object
  CppObject* = concept type T
    T.isCppObject

when defined(js):
  type WasmPtr* = distinct int
else:
  # linux gprof utility define
  when defined(linux) and defined(gprof):
    {.passC: "-pg".}
    {.passL: "-pg".}

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

type CppGlobalType* = object
proc isCppObject*(T: typedesc[CppGlobalType]): bool = true
var global* {.nodecl.}: CppGlobalType
const CppGlobalName = "global"

const
  setImpl = "#[#] = #"
  getImpl = "#[#]"

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

macro dynamicCppGet*(obj: CppObject, field: untyped): CppProxy =
  ## Experimental dot accessor (get) for type JsObject.
  ## Returns the value of a property of name `field` from a CppObject `x`.
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

template `.`*(obj: CppObject, field: untyped): CppProxy =
  ## Experimental dot accessor (get) for type JsObject.
  ## Returns the value of a property of name `field` from a CppObject `x`.
  dynamicCppGet(obj, field)

macro dynamicCppSet*(obj: CppObject, field, value: untyped): untyped =
  ## Experimental dot accessor (set) for type JsObject.
  ## Sets the value of a property of name `field` in a CppObject `x` to `value`.
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

template `.=`*(obj: CppObject, field, value: untyped): untyped =
  ## Experimental dot accessor (set) for type JsObject.
  ## Sets the value of a property of name `field` in a CppObject `x` to `value`.
  dynamicCppSet(obj, field, value)

macro dynamicCppCall*(obj: CppObject, field: untyped, args: varargs[CppProxy, CppFromAst]): CppProxy =
  ## Experimental "method call" operator for type CppProxy.
  ## Takes the name of a method of the JavaScript object (`field`) and calls
  ## it with `args` as arguments, returning a CppProxy 
  ## return types have to be casted unless the type is known using `to(T)`, void returns need `to(void)`
  var importString: string
  if obj.len == 0 and $obj == CppGlobalName:
    importString = $field & "(@)"
    
    result = quote:
      proc helper(): CppProxy {.importcpp:`importString`, gensym.}
      helper()
  else:
    when defined(js):
      importString = "#." & "_" & $field & "(@)"
    else:
      importString = "#." & $field & "(@)"
    
    result = quote:
      proc helper(o: CppObject): CppProxy {.importcpp:`importString`, gensym.}
      helper(`obj`)
  
  for idx in 0 ..< args.len:
    let paramName = ident("param" & $idx)
    result[0][3].add newIdentDefs(paramName, ident("CppProxy"))
    result[1].add args[idx].copyNimTree

template `.()`*(obj: CppObject, field: untyped, args: varargs[CppProxy, CppFromAst]): CppProxy =
  ## Experimental "method call" operator for type CppProxy.
  ## Takes the name of a method of the JavaScript object (`field`) and calls
  ## it with `args` as arguments, returning a CppProxy 
  ## return types have to be casted unless the type is known using `to(T)`, void returns need `to(void)`
  dynamicCppCall(obj, field, args)
    
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

    run()
