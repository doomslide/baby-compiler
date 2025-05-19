import Lean
import Lang.AST
import Std.Data.HashMap

namespace Lang.LCNF
open Lean
open Lang.AST

/-- Generates an LLVM SSA variable name from a `Lean.FVarId` using a hash of its internal name (e.g., `%{12345}`). -/
def llvmName (id : Lean.FVarId) : String :=
  let h := id.name.hash % 100000
  s!"%{h}"

structure Param where
  varId     : Lean.FVarId
  binderName : Lean.Name
  type       : LangTypeExpr
  deriving Repr

inductive Arg where
  | fvar (fvarId : Lean.FVarId)
  | lit (val : Nat)
  deriving Repr

inductive LetValue where
  | lit (val : Nat)
  | var (fvarId : Lean.FVarId)
  | prim (opName : Lean.Name) (args : Array Arg)
  deriving Repr

structure LetDecl where
  varId     : Lean.FVarId
  binderName : Lean.Name
  type       : LangTypeExpr
  value      : LetValue
  deriving Repr

inductive Code where
  | let_ (decl : LetDecl) (k : Code) : Code
  | return_ (retVal : Arg) : Code
  deriving Repr


structure FunDef where
  name    : Lean.Name
  params  : Array Param
  body    : Code
  deriving Repr

end Lang.LCNF
