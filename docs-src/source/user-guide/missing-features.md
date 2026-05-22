# Language Features Missing from the Language Reference

## High Priority

- [x] **Modules and Imports** — `import ModuleName`, `import extern ModuleName`, module structure, file naming conventions.

- [x] **Floating-Point Types and Operations** — Types `float`, `double`; literal `pi`; floating-point literals (e.g. `3.14`); conversions `wordToFloat`, `wordToDouble`; predicates `isNaN`, `isInfinite`, `isDenormalized`, `isNegativeZero`.

- [x] **The `@` (Quiet) Operator** — Suppresses the semantic value of a parser. Runs it for its effect on the stream but discards the result. Syntax: `@ expr`.

- [x] **Explicit `struct` Type Declarations** — `def MyStruct = struct` with named/typed fields, as opposed to the implicit struct types created by named fields in a sequence.

- [x] **External Declarations (`extern`)** — The `external.rst` file is a placeholder TODO. The `extern` keyword semantics are unexplained.

- [x] **`rangeUp` and `rangeDown`** — Built-in range generation. Variants: `rangeUp end`, `rangeUp start end`, `rangeUp start end step` (and similarly for `rangeDown`).

## Medium Priority

- [x] **The `!=` (Not Equal) Operator** — Present in the AST as `NotEq` but never mentioned in the docs (only `==` is documented).

- [x] **Computed/Local Fields in Structs** — `@name = expr` or `let name = expr` inside a struct block creates a local binding that doesn't appear in the output type.

- [x] **Octal Literals (`0o...`)** — The lexer supports them but the docs only mention decimal, hex, and binary.

- [NO] **`concat` as a Built-in** — Flattens nested arrays (`[[a]] -> [a]`). Used in examples but never documented as a built-in.

- [NO] **`Many?` and `Optional?` (Backtracking Variants)** — Allow backtracking after success, unlike `Many`/`Optional` which commit. Only briefly alluded to in the `many` loop section.

- [NO] **`COMMIT` in Struct Fields** — A commit point within a sequence block, separate from the `commit` statement documented in control structures.

## Lower Priority

- [x] **Pattern Matching on Numeric and String Literals** — `case` docs show union/boolean/maybe patterns but not matching on numbers or byte strings directly.

- [x] **Standard Library (`Daedalus.ddl`)** — Many useful combinators not referenced: `Guard`, `GuardMsg`, `When`, `Default`, `Only`, `Count`, `LookAhead`, `Skip`, `Chunk`, `Bytes`, `UInt16/32/64`, `SInt16/32/64`, `Float`, `Double`, `HalfFloat`, `min`, `max`.
