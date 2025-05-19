import Lean
import Lang.Monad
import Lang.LCNF
import Lang.Prim
import Std.Data.HashMap
import Std.Data.HashSet

-- TODO: pass manager?

namespace Lang.Optimize

open Lean Lang.Monad Lang.LCNF Lang.Prim Std

-- A map to store known constant Nat values for FVarIds during the pass
abbrev ConstMap := Std.HashMap Lean.FVarId Nat

/-- Constant folding on `LCNF.Code`. Propagates `ConstMap`, updates `LetDecl` values and `FVarInfo.valueAlias`. Returns `(LCNF.Code × ConstMap)`. -/
partial def foldConstantsInCode (code : LCNF.Code) (consts : ConstMap) : Monad.LangM (LCNF.Code × ConstMap) := do
  match code with
  | LCNF.Code.let_ decl c =>
    match decl.value with
    | LCNF.LetValue.lit n =>
      -- Record literal value for alias.
      modify fun s =>
        match s.fvarDb.infoById.get? decl.varId with
        | some info =>
          let newInfo : FVarInfo := { info with valueAlias := some (LCNF.Arg.lit n) }
          { s with fvarDb := { s.fvarDb with infoById := s.fvarDb.infoById.insert decl.varId newInfo } }
        | none => s -- should be unreachable
      let newConsts := consts.insert decl.varId n
      let (c', finalConsts) ← foldConstantsInCode c newConsts
      pure (LCNF.Code.let_ decl c', finalConsts)

    | LCNF.LetValue.var fvId =>
      -- Record alias for another variable.
      modify fun s =>
        match s.fvarDb.infoById.get? decl.varId with
        | some info =>
          let newInfo : FVarInfo := { info with valueAlias := some (LCNF.Arg.fvar fvId) }
          { s with fvarDb := { s.fvarDb with infoById := s.fvarDb.infoById.insert decl.varId newInfo } }
        | none => s -- should be unreachable

      let newConsts :=
        match consts.get? fvId with
        | some val => consts.insert decl.varId val
        | none     => consts
      let (c', finalConsts) ← foldConstantsInCode c newConsts
      pure (LCNF.Code.let_ decl c', finalConsts)

    | LCNF.LetValue.prim opName args =>
      match PrimInfo.table.find? opName with
      | some primInfo =>
        match primInfo.foldingRule with
        | some rule =>
          let concreteArgs : List LCNF.Arg := args.toList.map fun arg =>
            match arg with
            | LCNF.Arg.fvar argFvId => match consts.get? argFvId with
                                    | some knownNat => LCNF.Arg.lit knownNat
                                    | none => arg
            | LCNF.Arg.lit _ => arg

          match rule concreteArgs with
          | some (LCNF.Arg.lit foldedVal) =>
            let foldedDecl : LCNF.LetDecl := { decl with value := LCNF.LetValue.lit foldedVal }
            modify fun s =>
              match s.fvarDb.infoById.get? decl.varId with
              | some info =>
                let newInfo : FVarInfo := { info with valueAlias := some (LCNF.Arg.lit foldedVal) }
                { s with fvarDb := { s.fvarDb with infoById := s.fvarDb.infoById.insert decl.varId newInfo } }
              | none => s -- should be unreachable
            let newConsts := consts.insert decl.varId foldedVal
            let (c', finalConsts) ← foldConstantsInCode c newConsts
            pure (LCNF.Code.let_ foldedDecl c', finalConsts)
          | _ =>
            let (c', finalConsts) ← foldConstantsInCode c consts
            pure (LCNF.Code.let_ decl c', finalConsts)
        | none =>
          let (c', finalConsts) ← foldConstantsInCode c consts
          pure (LCNF.Code.let_ decl c', finalConsts)
      | none =>
        Lang.Monad.throwOptimizationError s!"Unknown primitive operation '{opName}' encountered during constant folding."

  | LCNF.Code.return_ retVal =>
    let resolvedRetVal :=
      match retVal with
      | LCNF.Arg.fvar fvId => match consts.get? fvId with
                              | some n => LCNF.Arg.lit n
                              | none   => retVal
      | _ => retVal
    pure (LCNF.Code.return_ resolvedRetVal, consts)

def getArgFreeVars (arg : Arg) : HashSet FVarId :=
  match arg with
  | Arg.fvar fvarId => (HashSet.emptyWithCapacity 0).insert fvarId
  | Arg.lit _       => HashSet.emptyWithCapacity 0

def getLetValueFreeVars (value : LetValue) : HashSet FVarId :=
  match value with
  | LetValue.lit _         => HashSet.emptyWithCapacity 0
  | LetValue.var fvarId    => (HashSet.emptyWithCapacity 0).insert fvarId
  | LetValue.prim _ args =>
    args.foldl (fun acc arg => acc.union (getArgFreeVars arg)) (HashSet.emptyWithCapacity 0)

partial def dceCodeRecursive (code : Code) : LangM (Code × HashSet FVarId) := do
  match code with
  | Code.return_ retVal =>
    -- For a return statement, the live variables are simply those free in its argument.
    let liveVars := getArgFreeVars retVal
    pure (Code.return_ retVal, liveVars)

  | Code.let_ decl c =>
    -- Recursively process the rest of the code (c) first.
    let (cOpt, liveInC) ← dceCodeRecursive c
    let liveInDeclValue := getLetValueFreeVars decl.value

    -- A declaration is live if its varId is used in the subsequent optimized code (cOpt).
    if liveInC.contains decl.varId then
      -- Declaration is live. Keep it.
      -- The new code is this declaration prepended to the optimized c (cOpt).
      let newCode := Code.let_ decl cOpt
      -- Variables that are alive before this 'let' are:
      --   1. Those live in cOpt, excluding the one defined by this 'let' (decl.varId).
      --   2. Those live in the value of the current declaration.
      let liveHere := (liveInC.erase decl.varId).union liveInDeclValue
      pure (newCode, liveHere)
    else
      -- Declaration is dead. Remove it.
      -- The new code is just the optimized c (cOpt).
      -- Variables that are alive before this (removed) 'let' are:
      --   1. Those alive in cOpt.
      --   2. Those alive in the value of the (now removed) declaration.
      --      (These are needed if they weren't already covered by liveInK,
      --       or they might become dead code further up).
      let liveHere := liveInC.union liveInDeclValue
      pure (cOpt, liveHere)

def dceFunBody (code : Code) : LangM Code := do
  let (newCode, _liveVarsAtStart) ← dceCodeRecursive code
  pure newCode -- we don't care about the live variables at the start of the function body only the actual code

def dceFunDef (funDef : FunDef) : LangM FunDef := do
  let newBody ← dceFunBody funDef.body
  pure { funDef with body := newBody }

def optimizeFunDef (funDef : Lang.LCNF.FunDef) : Monad.LangM Lang.LCNF.FunDef := do
  let (foldedBody, _) ← foldConstantsInCode funDef.body (Std.HashMap.emptyWithCapacity 0)
  let funDefAfterFolding := { funDef with body := foldedBody }
  let funDefAfterDCE ← dceFunDef funDefAfterFolding
  pure funDefAfterDCE

end Lang.Optimize
