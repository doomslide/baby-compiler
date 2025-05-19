import Lean
import Lang.AST
import Lang.LCNF
import Lang.Monad
import Lang.AlphaRename
import Lang.Prim
import Std.Data.HashMap


open Lean (Name)
open Lang.AST (Expr FunDef)
open Lang.LCNF
open Lang.Monad

namespace Lang.Lower

open Lean Lang.Monad Lang.LCNF Lang.AST Lang.Prim

/-- Lowers `AST.Expr` to LCNF (`LetDecl` list and final `Arg`). Resolves/creates `FVarId`s. -/
def lowerExpr (expr : Lang.AST.Expr) : LangM (List Lang.LCNF.LetDecl × Lang.LCNF.Arg) :=
  match expr with
  | Lang.AST.Expr.lit n =>
    return ([], Lang.LCNF.Arg.lit n)
  | Lang.AST.Expr.var name => do
    let fvarId ← getFVarIdForUniqueVar name
    return ([], Lang.LCNF.Arg.fvar fvarId)
  | Lang.AST.Expr.add lhs rhs => do
    let (lhsDecls, lhsArg) ← lowerExpr lhs
    let (rhsDecls, rhsArg) ← lowerExpr rhs
    let resultType := Lang.AST.LangTypeExpr.int64
    let resultVarId ← Lean.mkFreshFVarId

    -- Re-use the deterministic LLVM name everywhere
    let resultLlvmNameStr := LCNF.llvmName resultVarId
    let resultUniqueNameStr := resultLlvmNameStr
    -- For the binder we strip the leading '%' so it stays a valid Lean identifier.
    let resultBinderName := Lean.Name.mkSimple (resultLlvmNameStr.drop 1)

    let fvarInfoEntry : FVarInfo := { uniqueName := resultUniqueNameStr }
    modify fun s => { s with fvarDb :=
      { infoById := s.fvarDb.infoById.insert resultVarId fvarInfoEntry,
        idByName := s.fvarDb.idByName.insert resultUniqueNameStr resultVarId
      }
    }

    let addPrimKeyName : Name := `lang_add
    match Lang.Prim.PrimInfo.table.find? addPrimKeyName with
    | none => throwLoweringError s!"Lowering: Primitive operation '{addPrimKeyName}' not defined in PrimInfo."
    | some addInfo =>
      let addArgs := #[lhsArg, rhsArg]
      let addDecl : Lang.LCNF.LetDecl := {
        varId      := resultVarId,
        binderName := resultBinderName,
        type       := resultType,
        value      := Lang.LCNF.LetValue.prim addInfo.leanName addArgs
      }
      let allDecls := lhsDecls ++ rhsDecls ++ [addDecl]
      return (allDecls, Lang.LCNF.Arg.fvar resultVarId)

/-- Lowers `AST.FunDef` to `LCNF.FunDef`. Alpha-renames, creates LCNF params, lowers body. -/
def lowerFunDef (funDef : Lang.AST.FunDef) : LangM Lang.LCNF.FunDef := do
  let alphaRenamedFunDefAst ← Lang.AlphaRename.alphaRenameFunDef funDef
  let mut lcnfParams : Array Lang.LCNF.Param := #[]
  -- FVarIds are retrieved via getFVarIdForUniqueVar, using the unique names from alphaRenamedFunDefAst.
  for (pNameUniqueStr, pTypeExpr) in alphaRenamedFunDefAst.params do
    let pFVarId ← getFVarIdForUniqueVar pNameUniqueStr
    lcnfParams := lcnfParams.push {
      varId      := pFVarId,
      binderName := Lean.Name.mkSimple (if pNameUniqueStr.startsWith "%" then pNameUniqueStr.drop 1 else pNameUniqueStr),
      type       := pTypeExpr
    }

  let (bodyDecls, finalBodyArg) ← lowerExpr alphaRenamedFunDefAst.body
  let lcnfBodyCode := bodyDecls.foldr (fun decl k => Lang.LCNF.Code.let_ decl k) (Lang.LCNF.Code.return_ finalBodyArg)

  pure {
    name   := Lean.Name.mkSimple alphaRenamedFunDefAst.name,
    params := lcnfParams,
    body   := lcnfBodyCode
  }

end Lang.Lower
