import Lean
open Lean (Environment)


namespace Lang.Syntax

declare_syntax_cat lang_expr
syntax (name := lang_num) num : lang_expr
syntax (name := lang_ident) ident : lang_expr
syntax (name := lang_paren) "(" lang_expr ")" : lang_expr
syntax (name := lang_add) lang_expr "+" lang_expr : lang_expr

-- A single, typed parameter e.g. `(x : Int64)`
declare_syntax_cat single_typed_param
syntax (name := singleTypedParamRule) "(" ident ":" ident ")" : single_typed_param

declare_syntax_cat lang_decl
-- Return type is *mandatory*
syntax (name := lang_function_def) "def" ident single_typed_param* ":" ident ":=" lang_expr : lang_decl

def parse (env : Environment) (input : String) : Except String Lean.Syntax :=
  Lean.Parser.runParserCategory env `lang_decl input

end Lang.Syntax
