import Lean
import Lang.AST
import Lang.Monad
import Std.Data.HashMap

namespace Lang.Checker

open Lang.AST Lang.Monad Std

-- Type Environment: Maps variable name (String) to LangTypeExpr.
abbrev TypeEnv := HashMap String LangTypeExpr

/-- Infers type of an `AST.Expr` given `TypeEnv`. Returns `LangTypeExpr` or `LangError`. -/
partial def infer (env : TypeEnv) (e : AST.Expr) : LangM LangTypeExpr := do
  match e with
  | AST.Expr.lit _ =>
    -- All literals are currently int64
    pure LangTypeExpr.int64

  | AST.Expr.var name =>
    match env.get? name with
    | some typeExpr => pure typeExpr
    | none => throwNameError s!"Undefined variable '{name}'"

  | AST.Expr.add lhs rhs =>
    let lhsType ← infer env lhs
    let rhsType ← infer env rhs
    if lhsType == LangTypeExpr.int64 && rhsType == LangTypeExpr.int64 then
      pure LangTypeExpr.int64
    else
      throwTypeError s!"'+ ' operator expects two int64 operands, got '{lhsType}' and '{rhsType}'"

/-- Type-checks `AST.FunDef`. Infers body type from parameter types. Returns `LangM Unit` or `LangError`. -/
def checkFunDef (funDef : AST.FunDef) : LangM Unit := do
  let mut initialEnv : TypeEnv := HashMap.emptyWithCapacity 0
  for (paramName, paramType) in funDef.params do
    if initialEnv.contains paramName then
      throwNameError s!"Duplicate parameter name '{paramName}' in function '{funDef.name}'"
    initialEnv := initialEnv.insert paramName paramType

  let bodyType ← infer initialEnv funDef.body
  if bodyType != funDef.returnType then
    throwTypeError s!"Type mismatch in function '{funDef.name}'. Body has type '{bodyType}' but declared return type is '{funDef.returnType}'."
  pure ()

end Lang.Checker
