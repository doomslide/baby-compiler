import Lean
import Lean.Util.Path
import Init.System.IO
import Lean.Elab.Frontend

namespace Lang.Util

/-- Initialises a Lean `Environment` that knows about `Lang.Syntax` (our custom grammar). -/
unsafe def createLangEnvironment : IO Lean.Environment := do
  Lean.enableInitializersExecution -- this is *essential* for loading the syntax extension and i have absolutely no idea why...
  Lean.initSearchPath (← Lean.findSysroot) [(← IO.currentDir) / ".lake" / "build" / "lib" / "lean"] -- it should be `.lake` but just in case.

  let inputCtx := Lean.Parser.mkInputContext "import Lang.Syntax\n#eval 0\n" "<env_init>"
  let (header, parserState, messages) ← Lean.Parser.parseHeader inputCtx
  let (envAfterHeader, messagesAfterHeader) ← Lean.Elab.processHeader header {} messages inputCtx
  let initialCommandState := Lean.Elab.Command.mkState envAfterHeader messagesAfterHeader {}
  let frontendState ← Lean.Elab.IO.processCommands inputCtx parserState initialCommandState

  if frontendState.commandState.messages.hasErrors then
    frontendState.commandState.messages.forM fun msg => msg.toString >>= IO.eprintln
    throw <| IO.userError "Lang.Util.createLangEnvironment: Failed to initialize environment due to errors in dummy input processing."

  return frontendState.commandState.env

end Lang.Util
