import Lean
import Lean.Data.NameMap
import Lang.LCNF -- Required for LCNF.Arg

open Lean -- Open Lean namespace for direct access to Name, NameMap etc.

namespace Lang.Prim

/-- Primitive operation metadata: `leanName`, `arity`, `llvmOpcode`, optional `foldingRule` (`List LCNF.Arg → Option LCNF.Arg`). -/
structure PrimInfo where
  leanName    : Name
  arity       : Nat
  llvmOpcode  : String
  foldingRule : Option (List LCNF.Arg → Option LCNF.Arg)

namespace PrimInfo

/-- Constant folding for `lang_add`: `lit n + lit m -> lit (n+m)`. -/
def foldAdd (args : List LCNF.Arg) : Option LCNF.Arg :=
  if h : args.length = 2 then
    match args[0], args[1] with
    | LCNF.Arg.lit val1, LCNF.Arg.lit val2 =>
      some (LCNF.Arg.lit (val1 + val2))
    | _, _ => none
  else
    none

/-- Map from primitive `Name` to `PrimInfo`. Defines recognized primitives. -/
def table : NameMap PrimInfo :=
  mkNameMap PrimInfo
  |>.insert `lang_add {
    leanName    := `lang_add,
    arity       := 2,
    llvmOpcode  := "add",
    foldingRule := some foldAdd
  }

end PrimInfo

end Lang.Prim
