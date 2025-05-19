-- Main.lean
import Lean
import Init.System.IO

import Lang.Monad
import Lang.Driver
import Lang.Util

open Lean
open Lang.Monad

/-- CLI entry point. -/
unsafe def main (args : List String) : IO UInt32 := do
  if args.length ≠ 2 then
    IO.println "Usage: compiler <input-file> <output-file>"
    return 1

  let inputFile  := System.FilePath.mk args[0]!
  let outputFile := System.FilePath.mk args[1]!

  try
    let input ← IO.FS.readFile inputFile
    let env ← Lang.Util.createLangEnvironment
    let llvmIr ← Lang.Monad.toIO (Lang.Driver.compile env input)
    IO.FS.writeFile outputFile llvmIr
    return 0
  catch e =>
    IO.eprintln (toString e)
    return 1

-- Lazy hack around lake... :/
@[export lc_main]
unsafe def lcMain (args: List String): IO UInt32 := main args
