import Lean
import Lang.Syntax
import Lang.AST
import Lang.Monad

open Lean
open Lang.Monad
open Lang.AST

namespace Lang.Parser

def parseTypeIdent (typeName : Name) : Monad.LangM LangTypeExpr :=
  if typeName == `Int64 then
    pure LangTypeExpr.int64
  else
    Monad.throwSyntaxError (s!"Unsupported type identifier: {typeName}")

/-- Parses `param_list` syntax (a sequence of `single_typed_param` nodes) to a flat list of (name, type) pairs. -/
def parseParamsList (paramStxArray : Array Syntax) : Monad.LangM (List (String × LangTypeExpr)) := do
  let paramPairs ← paramStxArray.foldlM (fun accParams eachParamNode => do
    if eachParamNode.isOfKind `Lang.Syntax.singleTypedParamRule then
      let pnameNode := eachParamNode.getArg 1
      let ptypeNode := eachParamNode.getArg 3

      if pnameNode.isIdent && ptypeNode.isIdent then
        let typeExpr ← parseTypeIdent ptypeNode.getId
        pure ((pnameNode.getId.toString, typeExpr) :: accParams)
      else
        Monad.throwSyntaxError (s!"Malformed single_typed_param: expected (ident : ident), got ({pnameNode} : {ptypeNode}) in {eachParamNode}")
    else
      Monad.throwSyntaxError (s!"Internal parser error: Expected singleTypedParamRule, got {eachParamNode.getKind}")
  ) []
  pure paramPairs.reverse

partial def parseExpr (syntaxNode : Syntax) : Monad.LangM AST.Expr := do
  let kind := syntaxNode.getKind
  if kind == ``Lang.Syntax.lang_add then
    let args := syntaxNode.getArgs
    if args.size == 3 then -- Expected: [lhs_expr, plus_token, rhs_expr]
      let eval_lhs ← parseExpr (args[0]!)
      let eval_rhs ← parseExpr (args[2]!)
      return AST.Expr.add eval_lhs eval_rhs
    else
      Monad.throwSyntaxError (s!"Malformed lang_add node (arity {args.size}): {syntaxNode}")
  else if kind == ``Lang.Syntax.lang_paren then
    let args := syntaxNode.getArgs
    if args.size == 3 then -- Expected: [open_paren_token, actual_expr, close_paren_token]
      parseExpr (args[1]!)
    else
      Monad.throwSyntaxError (s!"Malformed lang_paren node (arity {args.size}): {syntaxNode}")
  else if kind == ``Lang.Syntax.lang_num then
    let args := syntaxNode.getArgs
    if args.size == 1 then
      let numNode := args[0]!
      match numNode.isNatLit? with
      | some val => pure (AST.Expr.lit val)
      | none => Monad.throwSyntaxError (s!"Numeric literal is not a Nat: {numNode} in {syntaxNode}")
    else
      Monad.throwSyntaxError (s!"Malformed lang_num node (arity {args.size}, expected 1): {syntaxNode}")
  else if kind == ``Lang.Syntax.lang_ident then
    let args := syntaxNode.getArgs
    if args.size == 1 && args[0]!.isIdent then -- Expected: [ident_atom]
      pure (AST.Expr.var (args[0]!).getId.toString)
    else
      Monad.throwSyntaxError (s!"Malformed lang_ident node (child not ident or arity {args.size}): {syntaxNode}")
  else
    Monad.throwSyntaxError (s!"Invalid expression _syntax (unknown kind {kind}): {syntaxNode}")

def parse (syntaxNode : Syntax) : Monad.LangM AST.Decl :=
  let decl_kind := syntaxNode.getKind
  if decl_kind == ``Lang.Syntax.lang_function_def then
    match syntaxNode with
    | `(Lang.Syntax.lang_function_def| def $name:ident $[$paramStxArray:single_typed_param]* : $retTypeIdent:ident := $body) => do
        let bodyExpr ← parseExpr body
        let paramPairs ← parseParamsList paramStxArray
        let returnTypeExpr ← parseTypeIdent retTypeIdent.getId

        let funDef : AST.FunDef := {
          name       := name.getId.toString,
          params     := paramPairs,
          returnType := returnTypeExpr,
          body       := bodyExpr
        }
        return AST.Decl.funDef funDef
    | _ => Monad.throwSyntaxError (s!"Internal error: Mismatched stx for lang_function_def: {syntaxNode}")
  else
    Monad.throwSyntaxError (s!"Invalid declaration syntax (unknown kind {decl_kind}): {syntaxNode}")

end Lang.Parser
