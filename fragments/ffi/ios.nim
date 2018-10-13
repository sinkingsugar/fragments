# collection of utilities to deal with iOS
import osproc, strformat, macros

const architectures = [
  "armv7",
  "armv7s",
  "arm64",
  "i386,",
  "x86_64",
  "armv7k"
]

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
  {.passL: "-fobjc-abi-version=2 -fobjc-arc".}
else:
  {.passC: "-fobjc-abi-version=2 -fno-objc-arc".}
  {.passL: "-fobjc-abi-version=2 -fno-objc-arc".}

when defined release:
  {.passC: "-fvisibility=hidden".}
  {.passL: "-fvisibility=hidden".}

const
  xcodeMinVersion* {.strdefine.} = "10.0"
  xcodePlatform* {.strdefine.} = "iphonesimulator"
  xcodeArch* {.strdefine.} = "x86_64"

static:
  doAssert xcodePlatform in platforms
  doAssert xcodeArch in architectures

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
  archFull = "-arch " & xcodeArch

{.passC: sysrootFull.}
{.passL: sysrootFull.}
{.passC: xcodeMinVersionFull.}
{.passL: xcodeMinVersionFull.}
{.passC: archFull.}
{.passL: archFull.}

macro xcodeFrameworks*(defines: varargs[string]): untyped =
  result = nnkStmtList.newTree()
  
  for adefine in defines:
    var str: string
    str = "-framework " & $adefine
    result.add nnkPragma.newTree(nnkExprColonExpr.newTree(newIdentNode("passC"), newLit(str)))

when isMainModule and defined(osx):
  xcodeFrameworks("Foundation")

  echo "Hello world"
