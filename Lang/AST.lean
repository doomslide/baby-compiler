import Lean

namespace Lang.AST

inductive LangTypeExpr where
  | int64
  deriving Repr, BEq, Inhabited, Nonempty

instance : ToString LangTypeExpr where
  toString typeExpr :=
    match typeExpr with
    | LangTypeExpr.int64 => "int64"

inductive Expr where
  | lit (n : Nat)
  | var (name : String)
  | add (lhs : Expr) (rhs : Expr)
  deriving Repr, BEq, Inhabited, Nonempty

structure FunDef where
  name : String
  params : List (String × LangTypeExpr)
  returnType : LangTypeExpr
  body : Expr
  deriving Repr, BEq, Inhabited, Nonempty

inductive Decl where
  | funDef (def_ : FunDef)
  deriving Repr, BEq, Inhabited, Nonempty

end Lang.AST
