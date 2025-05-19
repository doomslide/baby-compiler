import Lake
open Lake DSL

package compiler {

}

lean_lib Lang {
  roots := #[`Lang.Syntax, `Lang.Parser, `Lang.AST, `Lang.Monad, `Lang.Checker, `Lang.AlphaRename, `Lang.Lower, `Lang.Optimize, `Lang.Codegen, `Lang.Eval, `Lang.Prim, `Lang.LCNF, `Lang.Test, `Lang.Driver, `Lang.Util]
}

@[default_target]
lean_exe compiler {
  root := `Main
  supportInterpreter := true
}

@[test_driver]
lean_exe test {
  root := `Lang.Test
  supportInterpreter := true
}
