# Baby Compiler

It's a baby compiler.

## Overview

Source Code
-> Parser (using Lean `declare_syntax_cat` and `Lean.Parser`)
-> AST
-> Type Checker
-> Alpha Renamer (using Lean `FVarId`)
-> LCNF Converter (AST to LCNF)
-> Optimizer (Constant Folding, DCE)
-> LLVM IR Generator
-> LLVM IR

## Syntax

-   **Types:** `Int64`.
-   **Expressions:**
    -   Integer literals (e.g., `123`).
    -   Variables (e.g., `x`).
    -   Addition: `expr1 + expr2`.
    -   Parenthesized expressions: `(expr)`.
-   **Function Definitions:**
    ```lean
    def function_name (param1 : Int64) (param2 : Int64) : Int64 := body_expression
    ```
    -   Parameters and return type are `Int64`.
    -   Body is a single expression.

**Example Function:**
```lean
def my_add (a : Int64) (b : Int64) : Int64 := (a + b) + (a + 3)
```

If successful, this will compile to LLVM IR similar to:
```llvm
define i64 @my_add(i64 %param0, i64 %param1) {
entry:
  %temp0 = add i64 %param1, 5
  %temp1 = add i64 %param0, %temp0
  ret i64 %temp1
}
```
(Note: The raw compiler output uses hash-based names for SSA variables, e.g., `%{12345}`. The example above shows the structurally equivalent form after normalization by the test suite, which ensures consistent comparisons.)
A success message will be printed to the console:
```
Compiling file: my_func.lang...
Successfully compiled my_func.lang and wrote LLVM IR to my_func.ll
```

## Project Structure

*   `README.md`: This document.
*   `lakefile.lean`: Lake build system configuration.
*   `lean-toolchain`: Lean version specification.
*   `Main.lean`: Command-line entry point for the compiler.
*   `Lang/`: Core compiler logic and utilities:
    *   `AST.lean`: Abstract Syntax Tree definitions.
    *   `Syntax.lean`: Concrete syntax definitions used by the parser.
    *   `Parser.lean`: Text/`Lean.Syntax` to AST conversion.
    *   `Checker.lean`: Semantic (type) checking.
    *   `AlphaRename.lean`: Unique variable name management.
    *   `LCNF.lean`: Low-Level Compiler Normal Form (IR) definitions.
    *   `Lower.lean`: AST to LCNF conversion.
    *   `Monad.lean`: `LangM`, `LangState`, `LangError` definitions.
    *   `Prim.lean`: Primitive operation definitions (`PrimInfo`).
    *   `Optimize.lean`: Optimization pass implementations (Constant Folding, DCE).
    *   `Codegen.lean`: LCNF to LLVM IR generation.
    *   `Driver.lean`: Core compilation pipeline logic.
    *   `Util.lean`: Shared utility functions (e.g., environment setup).
    *   `Test.lean`: Test runner for codegen tests.
    *   `Eval.lean`: A simple direct evaluator/interpreter for the AST (e.g., for testing/reference).
*   `examples/`: Example programs and expected outputs:
    *   `Sources/`: `.lang` source files.
    *   `ExpectedLLVM/`: `.ll` files with expected LLVM IR.
*   `.gitignore`: Git ignore file for Lean projects.
*   `lake-manifest.json`: Dependency version tracking.

## Building the Project

1.  **Install Lean 4:** See [https://leanprover.github.io/lean4/setup](https://leanprover.github.io/lean4/setup).
2.  **Clone Repository.**
3.  **Build:**
    ```bash
    lake build
    ```
    (This builds the default `compiler` executable and the `Lang` library).


## Command-Line Interface

Executable: `compiler` (typically found at `./.lake/build/bin/compiler` after building).

**Syntax:**
```bash
./.lake/build/bin/compiler <input_file.lang> <output_file.ll>
```

*   `<input_file.lang>`: Path to the source file containing a single function definition in the Baby Compiler language.
*   `<output_file.ll>`: Path where the generated LLVM IR will be saved.

### Examples

Here are some practical examples of using the compiler:

1. **Basic Addition Function:**
   ```bash
   # Create a source file
   echo "def add (x : Int64) (y : Int64) : Int64 := x + y" > add.lang
   
   # Compile it
   ./.lake/build/bin/compiler add.lang add.ll
   
   # View the generated LLVM IR
   cat add.ll
   ```

2. **Function with Constant Folding:**
   ```bash
   # Create a source file with constants that will be folded
   echo "def compute (x : Int64) : Int64 := (10 + 20) + x" > compute.lang
   
   # Compile it
   ./.lake/build/bin/compiler compute.lang compute.ll
   
   # View the output - notice that "10 + 20" is optimized to "30"
   cat compute.ll
   ```

The compiler provides clear error messages for various issues:

```bash
# Syntax error example (using = instead of :=)
echo "def bad (x : Int64) : Int64 = x + 1" > bad.lang
./.lake/build/bin/compiler bad.lang bad.ll
# Output: Syntax Error: <input>:1:28: expected ':=' 

# Undefined variable example (caught by checker)
echo "def use_undef (p : Int64) : Int64 := p + zzz" > use_undef.lang
./.lake/build/bin/compiler use_undef.lang use_undef.ll
# Output: Name Error: Undefined variable 'zzz'

# Duplicate parameter example
echo "def sum (x : Int64) (x : Int64) : Int64 := x + 1" > sum.lang
./.lake/build/bin/compiler sum.lang sum.ll
# Output: Name Error: Duplicate parameter name 'x' in function 'sum'.
```

You can also compile the example programs from the test suite:
```bash
# Use an example from the test suite
./.lake/build/bin/compiler examples/Sources/ex_program_func_add.lang ex_program_func_add_output.ll

# You can examine the generated LLVM IR
cat ex_program_func_add_output.ll
```

Expected output (`ex_program_func_add_output.ll` will contain something like):
```llvm
define i64 @f(i64 %{hash_param_0}, i64 %{hash_param_1}) 
{
  %{hash_temp_0} = add i64 %{hash_param_0}, %{hash_param_1}
  ret i64 %{hash_temp_0}
}
```
(Note: The raw compiler output uses hash-based names for SSA variables, e.g., `%{12345}`. The example above uses `%{hash_...}` as a placeholder for these. The test suite normalizes these to a consistent `%paramN` and `%tempN` format to compare.)

Note: The compiler executable might need to be run from the project root directory for it to correctly locate dynamically linked library components if not installed system-wide. (More broadly, if Lake is being mean to you, empty sympathy is the only thing I can offer.)

### Running Tests

The tests compile `.lang` files from `examples/Sources` and compare the generated LLVM IR against expected `.ll` files in `examples/ExpectedLLVM`.

To run the tests:
```bash
lake test
```
You should see output similar to:
```
Running test: test_dce_after_fold
  PASS: test_dce_after_fold
Running test: test_const_fold
  PASS: test_const_fold
Running test: ex_program_func_add
  PASS: ex_program_func_add
All 3 codegen tests passed!
```