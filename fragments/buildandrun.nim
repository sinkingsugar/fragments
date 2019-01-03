import macros, os, strutils

macro buildAndRun*(body: untyped): tuple[output: string; exitCode: int] =
  var temp = getEnv("TMP")
  
  if temp == "":
    temp = getEnv("TEMP")
  if temp == "":
    temp = getEnv("TEMPDIR")
  if temp == "":
    temp = "/tmp"

  # check will also be in the nim global cache.. we need to think if we want to use some unique

  let
    code = body.repr
    checkFile = temp & "/check.nim"
    outputFile = temp & "/output.exe"
  
  writeFile(checkFile, code)

  when defined vcc:
    let (text, errorCode) = gorgeEx("nim cpp --cc:vcc -f -o:" & outputFile & " " & checkFile)
  elif defined clang:
    let (text, errorCode) = gorgeEx("nim cpp --cc:clang -f -o:" & outputFile & " " & checkFile)
  else:
    let (text, errorCode) = gorgeEx("nim cpp -f -o:" & outputFile & " " & checkFile)

  if errorCode != 0:
    return (text, errorCode)

  let res = gorge(outputFile)

  when defined windows:
    discard gorge "del " & checkFile
    discard gorge "del " & outputFile
  else:
    discard gorge "rm " & checkFile
    discard gorge "rm " & outputFile

  return (res, 0)

when isMainModule:
  let helloWorld {.compiletime.} = buildAndRun:
    echo "Hello World"
  
  static:
    assert helloWorld.output.contains("Hello World")