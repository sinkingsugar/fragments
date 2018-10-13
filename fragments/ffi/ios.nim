# collection of utilities to deal with iOS

import osproc, strformat, macros

const platforms = [
  "iphoneos",
  "iphonesimulator",
  "appletvos",
  "appletvsimulator",
  "watchos",
  "watchsimulator"
]

when defined bitcode:
  {.passC: "-fembed-bitcode".}
  {.passL: "-fembed-bitcode".}

when defined objcArc:
  {.passC: "-fobjc-abi-version=2 -fobjc-arc".}
else:
  {.passC: "-fobjc-abi-version=2 -fno-objc-arc".}

const
  xcodeMinVersion* {.strdefine.} = "10.0"
  xcodePlatform* {.strdefine.} = "iphoneos"

static:
  doAssert xcodePlatform in platforms

when xcodePlatform == "iphoneos" or xcodePlatform == "iphonesimulator":
  const xcodeMinVersionFull = "-m" & xcodePlatform & "-version-min=" & xcodeMinVersion
elif xcodePlatform == "appletvos":
  const xcodeMinVersionFull = "-mtvos-version-min=" & xcodeMinVersion
elif xcodePlatform == "appletvsimulator":
  const xcodeMinVersionFull = "-mtvos-simulator-version-min=" & xcodeMinVersion
elif xcodePlatform == "watchos":
  const xcodeMinVersionFull = "-mwatchos-version-min=" & xcodeMinVersion
elif xcodePlatform == "watchsimulator":
  const xcodeMinVersionFull = "-mwatchos-simulator-version-min=" & xcodeMinVersion

const
  sysrootStr = gorge fmt"xcodebuild -version -sdk {xcodePlatform} Path"
  sysrootFull = "-isysroot " & sysrootStr

{.passC: sysrootFull.}
{.passC: xcodeMinVersionFull.}
{.passC: "-arch arm64".} # TODO

macro xcodeFrameworks*(defines: varargs[string]): untyped =
  result = nnkStmtList.newTree()
  
  for adefine in defines:
    var str: string
    str = "-framework " & $adefine     
    result.add nnkPragma.newTree(nnkExprColonExpr.newTree(newIdentNode("passC"), newLit(str)))

when isMainModule and defined(osx):
  xcodeFrameworks("Foundation")
