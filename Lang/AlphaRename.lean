import Lean
import Lang.AST
import Lang.Monad
import Lang.LCNF
import Std.Data.HashMap

namespace Lang.AlphaRename

open Lean Lang.Monad Lang.LCNF

/-- Alpha-renames `AST.Expr` using `origNameToUniqueNameStrMap`. -/
partial def alphaRenameExpr (expr : Lang.AST.Expr) (origNameToUniqueNameStrMap : Std.HashMap String String) : Lang.Monad.LangM Lang.AST.Expr := do
  match expr with
  | Lang.AST.Expr.lit n => pure (Lang.AST.Expr.lit n)
  | Lang.AST.Expr.var name => -- 'name' is the original string identifier here
    match origNameToUniqueNameStrMap.get? name with
    | some newUniqueAstName => pure (Lang.AST.Expr.var newUniqueAstName)
    | none => Lang.Monad.throwNameError s!"Alpha rename: Undefined variable '{name}' or variable used out of scope."
  | Lang.AST.Expr.add lhs rhs => do
    let newLhs ← alphaRenameExpr lhs origNameToUniqueNameStrMap
    let newRhs ← alphaRenameExpr rhs origNameToUniqueNameStrMap
    -- The FVarId for the result of 'add' will be generated during Lowering.
    pure (Lang.AST.Expr.add newLhs newRhs)

/-- Alpha-renames `AST.FunDef`. Updates parameter names to unique `FVarId`-based strings and body variables. Populates `fvarDb`. -/
def alphaRenameFunDef (funDef : Lang.AST.FunDef) : Lang.Monad.LangM Lang.AST.FunDef := do
  let mut newAstParams : Array (String × Lang.AST.LangTypeExpr) := #[]
  let mut origNameToUniqueNameStrMap : Std.HashMap String String := Std.HashMap.emptyWithCapacity funDef.params.length

  for (pOrigName, pType) in funDef.params do
    if origNameToUniqueNameStrMap.contains pOrigName then
      Lang.Monad.throwNameError s!"Alpha rename: Duplicate parameter name '{pOrigName}' in function definition '{funDef.name}'."

    let fvarId ← Lean.mkFreshFVarId
    let newUniqueName := LCNF.llvmName fvarId

    let fvarInfoEntry : FVarInfo := { uniqueName := newUniqueName }
    modify fun s => { s with fvarDb :=
      { infoById := s.fvarDb.infoById.insert fvarId fvarInfoEntry,
        idByName := s.fvarDb.idByName.insert newUniqueName fvarId
      }
    }

    newAstParams := newAstParams.push (newUniqueName, pType)
    origNameToUniqueNameStrMap := origNameToUniqueNameStrMap.insert pOrigName newUniqueName

  let newBody ← alphaRenameExpr funDef.body origNameToUniqueNameStrMap

  let renamedFunDef : Lang.AST.FunDef := { funDef with
    name   := funDef.name,
    params := newAstParams.toList,
    body   := newBody
  }
  pure renamedFunDef

end Lang.AlphaRename
