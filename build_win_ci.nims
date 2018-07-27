import strutils

var 
  varScript: string

let
  programFiles = getEnv("ProgramFiles(x86)")
  vsFlavors = [
    "BuildTools",
    "Professional",
    "Enterprise",
    "Community"
  ]

for flavor in vsFlavors:
  if existsFile(programFiles & "\\Microsoft Visual Studio\\2017\\" & flavor & "\\Common7\\Tools\\VsDevCmd.bat"):
    varScript = programFiles & "\\Microsoft Visual Studio\\2017\\" & flavor & "\\Common7\\Tools\\VsDevCmd.bat"
    break
  elif existsFile("C:\\" & flavor & "\\Common7\\Tools\\VsDevCmd.bat"):
    varScript = "C:\\" & flavor & "\\Common7\\Tools\\VsDevCmd.bat"
    break

doAssert(varScript != nil)

let 
  buildScript = """
nim cpp --cc:vcc --cincludes:. -r fragments/ffi/cpp.nim
nim c --cc:vcc -r fragments/dsl.nim
nim c --cc:vcc --threads:on -r fragments/threading/threading_primitives.nim
nim c --cc:vcc --threads:on -r fragments/threading/atomics.nim
""" % [thisDir()]

writeFile("build.bat", buildScript)

exec("cmd.exe /C \"\"" & varScript & "\" -arch=amd64\" && build.bat && del build.bat")
