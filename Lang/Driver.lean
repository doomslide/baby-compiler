import Lean
import Lang.AST
import Lang.Monad
import Lang.Syntax
import Lang.Parser
import Lang.Checker
import Lang.AlphaRename
import Lang.Lower
import Lang.Optimize
import Lang.Codegen

open Lean Lang.AST Lang.LCNF Lang.Monad Lang.Optimize

namespace Lang.Driver

/-- Core compilation function -/
@[inline]
def compile (env : Environment) (src : String) : EIO LangError String := do
  match Lang.Syntax.parse env src with
  | Except.error err => throw (LangError.syntaxError err)
  | Except.ok stx =>
    let options : LangGlobalOptions := {}
    let langMComputation : LangM String := do
      let astDecl ← Lang.Parser.parse stx
      let funAst ← match astDecl with
                    | Lang.AST.Decl.funDef f => pure f
      discard <| Lang.Checker.checkFunDef funAst
      let alphaRenamedFunAst ← Lang.AlphaRename.alphaRenameFunDef funAst
      let lcnfFunDef ← Lang.Lower.lowerFunDef alphaRenamedFunAst
      let optimizedLcnfFunDef ← Lang.Optimize.optimizeFunDef lcnfFunDef
      Lang.Codegen.codegenFunDef optimizedLcnfFunDef
    Lang.Monad.runLangM' options langMComputation

end Lang.Driver
