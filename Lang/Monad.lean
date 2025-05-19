import Lean
import Lang.LCNF
import Std.Data.HashMap

open Lang.LCNF
open Lean

namespace Lang.Monad

inductive LangError where
  | syntaxError (msg : String)
  | typeError (msg : String)
  | nameError (msg : String)
  | loweringError (msg : String)
  | codegenError (msg : String)
  | optimizationError (msg : String)
  | internalError (msg : String)
  | generic (msg : String)
deriving Repr

def langErrorToIOError : LangError → IO.Error
  | LangError.syntaxError msg => IO.userError s!"Syntax Error: {msg}"
  | LangError.typeError msg => IO.userError s!"Type Error: {msg}"
  | LangError.nameError msg => IO.userError s!"Name Error: {msg}"
  | LangError.loweringError msg => IO.userError s!"Lowering Error: {msg}"
  | LangError.codegenError msg => IO.userError s!"Code Generation Error: {msg}"
  | LangError.optimizationError msg => IO.userError s!"Optimization Error: {msg}"
  | LangError.internalError msg => IO.userError s!"Internal Compiler Error: {msg}"
  | LangError.generic msg => IO.userError s!"Compilation Error: {msg}"

def toIO {α : Type} (x : EIO LangError α) : IO α :=
  EIO.toIO langErrorToIOError x

structure LangGlobalOptions where
  dummyOption : Bool := true -- Placeholder option
  deriving Inhabited, Nonempty

structure FVarInfo where
  uniqueName       : String -- LLVM-compatible SSA name (e.g., "%{hash}")
  /-- Aliased value (`LCNF.Arg`) if `FVarId` is constant/alias. Used by optimizer. -/
  valueAlias       : Option LCNF.Arg := none
  deriving Inhabited

/-- Bidirectional mapping: `FVarId` <=> unique string name (`FVarInfo`). -/
structure FVarNameDb where
  infoById   : Std.HashMap Lean.FVarId FVarInfo := Std.HashMap.emptyWithCapacity 0
  idByName   : Std.HashMap String Lean.FVarId    := Std.HashMap.emptyWithCapacity 0
  deriving Inhabited

/-- Lang compiler state. -/
structure LangState where
  fvarDb           : FVarNameDb := default
  nameGen          : NameGenerator := default
  deriving Inhabited


abbrev LangM := ReaderT LangGlobalOptions (StateT LangState (EIO LangError))

def throwSyntaxError (msg : String) : LangM α :=
  throw (LangError.syntaxError msg)

def throwTypeError (msg : String) : LangM α :=
  throw (LangError.typeError msg)

def throwNameError (msg : String) : LangM α :=
  throw (LangError.nameError msg)

def throwLoweringError (msg : String) : LangM α :=
  throw (LangError.loweringError msg)

def throwCodegenError (msg : String) : LangM α :=
  throw (LangError.codegenError msg)

def throwOptimizationError (msg : String) : LangM α :=
  throw (LangError.optimizationError msg)

def throwInternalError (msg : String) : LangM α :=
  throw (LangError.internalError msg)

def throwLangError (msg : String) : LangM α :=
  throw (LangError.generic msg)

instance : MonadNameGenerator LangM where
  getNGen := do pure (← get).nameGen
  setNGen ngen := modify fun s => { s with nameGen := ngen }

def runLangM (options: LangGlobalOptions) (x : LangM α)
    : EIO LangError (α × LangState) :=
  let initialMutableState : LangState := {
    fvarDb := default,
    nameGen := default
  }
  (ReaderT.run x options).run initialMutableState

def runLangM' (options: LangGlobalOptions) (x : LangM α)
    : EIO LangError α :=
  Prod.fst <$> runLangM options x

def getFVarIdForUniqueVar (uniqueNameKey : String) : LangM Lean.FVarId := do
  let s ← get
  match s.fvarDb.idByName.get? uniqueNameKey with
  | some fvarId => pure fvarId
  | none => throwInternalError s!"FVarId for unique variable '{uniqueNameKey}' not found in fvarNameDb.idByName. Known names: {s.fvarDb.idByName.toList.map fun x => x.1}"



end Lang.Monad
