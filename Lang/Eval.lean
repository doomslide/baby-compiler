import Lang.AST
import Lang.Monad
import Std.Data.HashMap

open Lang.AST Lang.Monad

/-- Evaluation environment: variable name (String) to value (Nat). -/
abbrev Env := Std.HashMap String Nat

/-- AST big-step interpreter. Returns `Except LangError Nat`. -/
def eval (env : Env) : Expr → Except LangError Nat
| Expr.lit n      => pure n
| Expr.var name   =>
  match env.get? name with
  | some val => pure val
  | none     => throw (LangError.generic s!"Eval: Undefined variable '{name}'")
| Expr.add a b    => do
    let valA ← eval env a
    let valB ← eval env b
    pure (valA + valB)
