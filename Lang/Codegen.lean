import Lean
import Lang.LCNF
import Lang.Monad
import Lang.Prim
import Std.Data.HashMap
import Lang.AST

namespace Lang.Codegen

open Lang.LCNF Lang.Monad Lean Std Lang.Prim Lang.AST

/-- Gets LLVM SSA value name for `FVarId` from `fvarDb.infoById[fvarId].uniqueName`. -/
def getLLVMSsaValueForFVar (fvarId : FVarId) : LangM String := do
  match (← get).fvarDb.infoById.get? fvarId with
  | some info => pure info.uniqueName
  | none => throwCodegenError s!"No FVarInfo found for FVarId '{fvarId.name}' in fvarDb.infoById."

partial def codegenArg (arg : LCNF.Arg) (visited : Std.HashSet FVarId := Std.HashSet.emptyWithCapacity 0) : LangM String := do
  match arg with
  | LCNF.Arg.fvar fvarId =>
    if visited.contains fvarId then
      throwCodegenError s!"Circular alias detected for FVarId '{fvarId.name}' when resolving argument."
    let s ← get
    match s.fvarDb.infoById.get? fvarId with
    | some info =>
      match info.valueAlias with
      | some aliasArg => codegenArg aliasArg (visited.insert fvarId) -- Recursively resolve alias
      | none => pure info.uniqueName -- No alias, use its own unique SSA name
    | none => throwCodegenError s!"No FVarInfo found for FVarId '{fvarId.name}' in fvarDb.infoById when resolving argument."
  | LCNF.Arg.lit val => pure (toString val)

def lcnfTypeToLLVMType (typeInstance : LangTypeExpr) : LangM String := do
  match typeInstance with
  | LangTypeExpr.int64 => pure "i64"


partial def codegenCode (code : LCNF.Code) (currentSeenNames : Std.HashSet String) : LangM (Array String × Std.HashSet String) := do
  match code with
  | LCNF.Code.let_ decl k =>
      let llvmTypeStr ← lcnfTypeToLLVMType decl.type
      let mut instrs : Array String := #[]
      let mut newSeenNames := currentSeenNames

      -- All let decls define a new variable `decl.varId` whose LLVM name must be unique.
      let declSsaName ← getLLVMSsaValueForFVar decl.varId
      if newSeenNames.contains declSsaName then
        throwCodegenError s!"Duplicate SSA name '{declSsaName}' generated for FVarId '{decl.varId.name}' in a let-binding."
      else
        newSeenNames := newSeenNames.insert declSsaName

      match decl.value with
      | LCNF.LetValue.lit _ =>
          -- No instruction for a literal
          pure ()
      | LCNF.LetValue.var _ =>
          -- No instruction for a variable alias
          pure ()
      | LCNF.LetValue.prim opName args =>
          match PrimInfo.table.find? opName with
          | none => throwCodegenError s!"Unsupported primitive '{opName}'"
          | some primInfo =>
              let argSsaValueNames ← args.mapM codegenArg

              if argSsaValueNames.size != primInfo.arity then
                throwCodegenError s!"Primitive '{opName}' expects {primInfo.arity} operands, got {argSsaValueNames.size}"

              let resultSsaName := declSsaName

              if primInfo.arity == 2 then
                instrs := instrs.push s!"{resultSsaName} = {primInfo.llvmOpcode} {llvmTypeStr} {argSsaValueNames[0]!}, {argSsaValueNames[1]!}"
              else
                throwCodegenError s!"Primitive '{opName}' with arity {primInfo.arity} not yet fully supported in SSA instruction generation template."

      let (restInstrs, finalSeenNamesFromK) ← codegenCode k newSeenNames
      pure (instrs ++ restInstrs, finalSeenNamesFromK)

  | LCNF.Code.return_ retVal =>
      let llvmTypeStr := "i64"
      let retSsaValueName ← codegenArg retVal
      pure (#[s!"ret {llvmTypeStr} {retSsaValueName}"], currentSeenNames)

def codegenFunDef (funDef : LCNF.FunDef) : LangM String := do
  let fnName := funDef.name.toString
  let mut paramSignatureParts : List String := []
  let mut seenNames : Std.HashSet String := Std.HashSet.emptyWithCapacity 0

  for param in funDef.params do
    let paramLLVMType ← lcnfTypeToLLVMType param.type
    let llvmParamSsaName ← getLLVMSsaValueForFVar param.varId

    if seenNames.contains llvmParamSsaName then
      throwCodegenError s!"Duplicate SSA name '{llvmParamSsaName}' generated for parameter FVarId '{param.varId.name}'."
    else
      seenNames := seenNames.insert llvmParamSsaName

    paramSignatureParts := s!"{paramLLVMType} {llvmParamSsaName}" :: paramSignatureParts

  let paramsString := String.join (paramSignatureParts.reverse.intersperse ", ")
  let retTypeStr := "i64"

  let mut funcIrLines : Array String := #[]
  funcIrLines := funcIrLines.push s!"define {retTypeStr} @{fnName}({paramsString})"
  funcIrLines := funcIrLines.push "{"

  let (bodyInstructions, _finalSeenNames) ← codegenCode funDef.body seenNames
  funcIrLines := funcIrLines ++ bodyInstructions

  funcIrLines := funcIrLines.push "}"
  pure (String.intercalate "\n" funcIrLines.toList)

def emitLLVM (funDefs : List LCNF.FunDef) : LangM String := do
  -- The `fvarDb` (populated by AlphaRename/Lowering) provides `FVarId`-to-LLVM-name mappings and is read-only in this pass.
  let mut moduleIrString := ""
  -- For more complete LLVM IR, one might add target datalayout and triple here.
  for funDef in funDefs do
    let funIr ← codegenFunDef funDef
    moduleIrString := moduleIrString ++ "\n" ++ funIr -- Add a newline for separation

  pure moduleIrString

end Lang.Codegen
