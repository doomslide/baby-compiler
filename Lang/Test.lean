import Lean
import Lean.Environment
import Lean.Elab.Frontend
import Std.Data.HashMap

import Lang.AST
import Lang.Monad
import Lang.Driver
import Lang.Util

import Init.System.IO -- For EIO.toIO'

open Lang
open Lang.Monad
open System
open IO.FS
open Lean (Environment)
open IO

structure CodegenTestCase where
  name : String
  deriving Repr

/--
Normalizes LLVM IR to make variable names canonical for alpha-invariant testing.
-/
def normalizeLLVMVariables (code : String) : String := Id.run do
  let originalLines := code.splitOn "\n"
  let mut varMap : Std.HashMap String String := Std.HashMap.emptyWithCapacity 0
  let mut nextParamIdx := 0
  let mut nextTempIdx := 0

  -- Pass 1: Populate varMap with mappings for parameters and temporary variables
  for line in originalLines do
    let trimmedLine := line.trim
    -- Parameter identification
    if trimmedLine.startsWith "define" then
      let openParenPos := trimmedLine.posOf '('
      let closeParenPos := trimmedLine.posOf ')'
      if openParenPos != trimmedLine.endPos && closeParenPos != trimmedLine.endPos && openParenPos < closeParenPos then
        let paramsPart := trimmedLine.extract (openParenPos + String.Pos.mk 1) closeParenPos
        let paramDefs := paramsPart.splitOn "," |>.map String.trim
        for paramDef in paramDefs do
          let parts := paramDef.splitOn " "
          let optParamName := parts.find? (fun s => s.startsWith "%" && !(s.endsWith ":"))
          if let some paramName := optParamName then
            unless varMap.contains paramName do
              let newName := s!"%param{nextParamIdx}"
              varMap := varMap.insert paramName newName
              nextParamIdx := nextParamIdx + 1

    let potentialDef := trimmedLine
    if potentialDef.startsWith "%" then
      match potentialDef.splitOn " = " with
      | [defVarCandidate, _] =>
        let defVar := defVarCandidate.trim
        if defVar.startsWith "%" && !(defVar.contains ' ') && !(defVar.endsWith ":") then
          unless varMap.contains defVar do
            let newName := s!"%temp{nextTempIdx}"
            varMap := varMap.insert defVar newName
            nextTempIdx := nextTempIdx + 1
      | _ => ()

  -- Pass 2: Apply transformations using the populated varMap
  let mut finalNormalizedLines : Array String := #[]
  for line in originalLines do
    let mut currentLine := line
    let sortedKeys := varMap.keysArray.qsort (fun a b => a.length > b.length)
    for oldName in sortedKeys do
      if let some newName := varMap[oldName]? then
        currentLine := currentLine.replace oldName newName
    finalNormalizedLines := finalNormalizedLines.push currentLine

  "\n".intercalate finalNormalizedLines.toList

def normalizeOutputCode (code : String) : String :=
  let varNormalizedCode := normalizeLLVMVariables code
  "\n".intercalate <| (varNormalizedCode.splitOn "\n").map String.trim |>.filter (fun s => ¬s.isEmpty)

def discoverTests (sourcesDir : FilePath) (expectedDir : FilePath) : IO (Array CodegenTestCase) := do
  let mut testCases := #[]
  unless ← sourcesDir.pathExists do
    IO.eprintln s!"Warning: Sources directory '{sourcesDir}' does not exist."
    return #[]
  unless ← expectedDir.pathExists do
    IO.eprintln s!"Warning: Expected LLVM directory '{expectedDir}' does not exist."
    return #[]

  for entry in ← sourcesDir.readDir do
    let meta ← FilePath.metadata entry.path
    if meta.type == FileType.file then
      let fileName := entry.fileName
      if fileName.endsWith ".lang" then
        let testName := fileName.dropRight ".lang".length
        let expectedLLVMPath := expectedDir / (testName ++ ".ll")
        if ← expectedLLVMPath.pathExists then
          testCases := testCases.push { name := testName }
        else
          IO.eprintln s!"Warning: Found source file '{entry.path}' but no corresponding expected LLVM file '{expectedLLVMPath}'."
  return testCases

def runTest (env : Environment) (testCase : CodegenTestCase) : IO Bool := do
  IO.println s!"Running test: {testCase.name}"
  let srcPath := FilePath.mk "examples/Sources" / (testCase.name ++ ".lang")
  let expectedPath := FilePath.mk "examples/ExpectedLLVM" / (testCase.name ++ ".ll")
  let src ← readFile srcPath
  let expectedLLVM ← readFile expectedPath

  let eCombinedResult : IO (Except Lang.Monad.LangError String) :=
    EIO.toIO' (Lang.Driver.compile env src)

  match ← eCombinedResult with
  | Except.error err =>
    IO.println s!"  FAIL: {testCase.name} - LangM Error:"
    IO.eprintln (langErrorToIOError err)
    return false
  | Except.ok generatedLLVMCode =>
    let genNorm := normalizeOutputCode generatedLLVMCode
    let expNorm := normalizeOutputCode expectedLLVM
    if genNorm == expNorm then
      IO.println s!"  PASS: {testCase.name}"
      return true
    else
      IO.println s!"  FAIL: {testCase.name}"
      IO.println "  Expected (normalized LLVM):"
      IO.println expNorm
      IO.println "  Got (normalized LLVM):"
      IO.println genNorm
      IO.println "--- Original Expected LLVM ---"
      IO.println expectedLLVM
      IO.println "--- Original Generated LLVM ---"
      IO.println generatedLLVMCode
      return false

unsafe def main (_args : List String) : IO UInt32 := do
  Lean.initSearchPath (← Lean.findSysroot)

  let env ← Lang.Util.createLangEnvironment
  let sourcesPath := FilePath.mk "examples/Sources"
  let expectedLLVMPath := FilePath.mk "examples/ExpectedLLVM"
  let discoveredTests ← discoverTests sourcesPath expectedLLVMPath

  if discoveredTests.isEmpty then
    IO.println "Warning: No test cases found or matched. Ensure '*.lang' files in 'examples/Sources' have corresponding '*.ll' files in 'examples/ExpectedLLVM'."
    IO.println "No tests run. Exiting."
    return 1

  let results ← discoveredTests.mapM (runTest env)
  let numTests := discoveredTests.size
  let numPassed := results.filter (· == true) |>.size

  if numPassed == numTests then
    IO.println s!"All {numTests} codegen tests passed!"
    return 0
  else
    IO.println s!"{numPassed}/{numTests} codegen tests passed. Some tests failed."
    return 1
