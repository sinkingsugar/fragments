import macros, strutils

import jsffi, async, cpp
export jsffi, async, cpp

{. experimental .}

{.emit:"const sleep = msec => new Promise(resolve => setTimeout(resolve, msec));".}

type
  JsConsole* = distinct JsObject

template `.()`*(obj: JsConsole, field: untyped, args: varargs[JsObject, jsFromAst]): JsObject =
  `.()`(obj.JsObject, field, args)
  
# import the document object and the console
var window* {.importc.}: JsObject
var document* {.importc, nodecl.}: JsObject
var console* {.importc, nodecl.}: JsConsole
var this* {.importc, nodecl.} : JsObject
var process* {.importc, nodecl.}: JsObject
var JSON* {.importc, nodecl.}: JsObject
var jsGlobal* {.importcpp:"global".}: JsObject
var localStorage* {.importc.}: JsObject
var sessionStorage* {.importc.}: JsObject
var undefined* {.importc.}: JsObject

# Js converters
converter toByte*(co: JsObject): int8 {.used, importcpp:"(#)".}
converter toUByte*(co: JsObject): uint8 {.used, importcpp:"(#)".}
converter toShort*(co: JsObject): int16 {.used, importcpp:"(#)".}
converter toUShort*(co: JsObject): uint16 {.used, importcpp:"(#)".}
converter toInt*(co: JsObject): int {.used, importcpp:"(#)".}
converter toUInt*(co: JsObject): uint {.used, importcpp:"(#)".}
converter toLong*(co: JsObject): int64 {.used, importcpp:"(#)".}
converter toULong*(co: JsObject): uint64 {.used, importcpp:"(#)".}
converter toFloat*(co: JsObject): float {.used, importcpp:"(#)".}
converter toFloat32*(co: JsObject): float32 {.used, importcpp:"(#)".}
converter toDouble*(co: JsObject): float64 {.used, importcpp:"(#)".}
converter toCString*(co: JsObject): cstring {.used, importcpp:"(#)".}

# utilities
proc require*(h: cstring): JsObject {.importcpp:"require(#)".}
func instanceOf*(current, parent: JsObject): bool {.importcpp:"(# instanceof #)".}
proc jsParseFloat*(o: JsObject): JsObject {.importcpp:"parseFloat(#)".}
proc jsParseFloat*(o: cstring): JsObject {.importcpp:"parseFloat(#)".}
proc jsParseInt*(o: JsObject): JsObject {.importcpp:"parseInt(#)".}
proc jsParseInt*(o: cstring): JsObject {.importcpp:"parseInt(#)".}
proc parseDate*(o: JsObject): JsObject {.importcpp: "new Date(#)".}
proc parseDate*(o: cstring): JsObject {.importcpp: "new Date(#)".}
proc re*(exprex: cstring): JsObject {.importcpp:"new RegExp(#)".}
proc re*(exprex: cstring, postfix: cstring): JsObject {.importcpp:"new RegExp(#, #)".}
proc now*(): JsObject {.importcpp:"new Date()".}
proc newBuffer*(obj: JsObject): JsObject {.importcpp:"Buffer.from(#)".}
proc slice*(self: JsObject; begin, endExclusive: int): JsObject {.importcpp: "#.slice(@)".}

proc `[]`*(self: JsObject, bounds: Slice[int]): JsObject =
  self.slice(bounds.a, bounds.b + 1)

proc mdbFind*(db: JsObject, query: JsObject): JsObject {.importcpp:"#.find(#)".}
proc revision*(doc: JsObject): cstring {.importcpp:"#._rev".}
proc newPromise*(promiseProc: JsObject): JsObject {.importcpp:"new Promise(#)".}
proc sleep*(ms: int): Future[void] {.importc, nodecl.}
template asyncCheck*(exp: untyped): untyped = discard exp
template waitFor*(exp: untyped): untyped = (await exp)
proc discardableAny*[T](self: T): T {.discardable.} = return self

proc invoke*(self: JsObject): JsObject {.importcpp:"#()".}

# this is defined in jssys, careful as might change in the future as it's interal for now basically
var lastJSError {.importc.}: JsObject
func isJsException*(exClass: JsObject): bool = instanceOf(lastJSError, exClass)