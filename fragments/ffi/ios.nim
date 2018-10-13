# collection of utilities to deal with iOS

import osproc, strformat

const architectures = [
  ""
]

const platforms = [
  "iphoneos",
  "iphonesimulator",
  "appletvos",
  "appletvsimulator",
  "watchos",
  "watchsimulator"
]

proc getSysroot*(platform: string): string {.compileTime.} =
  doAssert platform in platforms
  let
    (output, error) = gorgeEx fmt"xcodebuild -version -sdk {platform} Path"
  doAssert error == 0, "Failed to find sysroot"
  return output

proc getSdkVersion*(platform: string): string {.compileTime.} =
  doAssert platform in platforms
  let
    (output, error) = gorgeEx fmt"xcodebuild -version -sdk {platform} SDKVersion"
  doAssert error == 0, "Failed to find SdkVersion"
  return output

when defined bitcode:
  {.passC: "-fembed-bitcode".}
  {.passL: "-fembed-bitcode".}

when defined objcArc:
  {.passC: "-fobjc-abi-version=2 -fobjc-arc".}
else:
  {.passC: "-fobjc-abi-version=2 -fno-objc-arc".}

const xcodeMinVersion {.strdefine.} = ""
const xcodePlatform {.strdefine.} = ""

when xcodePlatform == "iphoneos" or xcodePlatform == "iphonesimulator":
  const xcodeMinVersionFull = "-m" & xcodePlatform & "-version-min=" & xcodeMinVersion
elif xcodePlatform == "appletvos":
  const tvosMinVerSwitch = "-mtvos-version-min=" & xcodeMinVersion
elif xcodePlatform == "appletvsimulator":
  const tvosMinVerSwitch = "-mtvos-simulator-version-min=" & xcodeMinVersion
elif xcodePlatform == "watchos":
  const tvosMinVerSwitch = "-mwatchos-version-min=" & xcodeMinVersion
elif xcodePlatform == "watchsimulator":
  const tvosMinVerSwitch = "-mwatchos-simulator-version-min=" & xcodeMinVersion

when isMainModule and defined(osx):
  static:
    echo getSysroot("iphoneos")
    echo getSdkVersion("iphoneos")