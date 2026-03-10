---
name: daedalus-ddl
description: Use when writing, reading, or debugging Daedalus (.ddl) format specifications. Use when generating parsers for binary or text data formats. Use when working with data-dependent parsing, bitdata, or stream manipulation in Daedalus. Use when writing Dex (.dex) export specifications to translate Daedalus parser output into application types (C++).
---

# Daedalus Data Description Language

Daedalus (DaeDaLus) is a DSL for specifying parsers with data-dependent parsing. Given a `.ddl` spec, the `daedalus` tool interprets it or compiles to Haskell/C++.

Docs: https://galoisinc.github.io/daedalus/

## When to Use

- Writing parser specs for binary/text formats (PDF, PNG, MIDI, etc.)
- Reading or modifying `.ddl` files
- Debugging parse failures in Daedalus specs
- Generating C++ or Haskell parsers from format descriptions

## Key Concepts

**Three declaration types** by naming convention:
- Uppercase = parser (`def Main`), lowercase = semantic value (`def addDigit`), `$`-prefix = character class (`def $digit`)

**Parsers** consume input and may fail. Key primitives: `UInt8`, `$[set]`, `Match`, `END`, `^ expr` (pure), `Fail`.

**Sequencing**: `block` for sequential parsing. `let` = local variable, bare assignment = struct field. `$$ =` sets explicit return value.

**Alternatives**: `<|` / `First` (biased, try left first), `|` / `Choose` (unbiased). Named alternatives create tagged unions.

**Repetition**: `Many P` (zero+, maximal), `Many (1..) P` (one+), `Many n P` (exactly n). Warning: `Many` is greedy.

**Types**: `uint N`, `sint N`, `int`, `bool`, `maybe T`, `[T]` (array), `[K -> V]` (map), `stream`.

**Coercions**: `as` (compile-time), `as!` (lossy), `as?` (runtime parser check).

**Control flow**: `if/then/else`, guards (`expr is true/just/tag`), `case`, `for` loops, `map`, `commit`.

**Bitdata**: Bit-level field definitions, applied via `as?` coercion on parsed bytes.

**Streams**: `GetStream`/`SetStream` for save/restore, `Take`/`Drop` for substreams.

See `lang-reference.md` in this directory for complete syntax with examples.

## Common Mistakes

- **`Many` consuming too much**: maximal parsing. `{ Many $['A']; $['A'] }` always fails.
- **Forgetting `let`**: Without `let`, assignment creates a struct field, not a local variable.
- **Naming**: `def foo` = value, `def Foo` = parser. Wrong case = confusing type errors.
- **`as` vs `as?`**: Use `as?` for potentially lossy conversions at runtime.
- **Validation**: Prefer lightweight guards during parse; defer complex checks to post-parse.
- **Non-ASCII in source**: Em-dashes, curly quotes, etc. crash the toolchain (`Enum.toEnum` error). Use only ASCII.
- **`_`-prefix parameters**: Not supported. Use full names even for unused parameters.
- **Nested blocks in parens**: `Many n (block x = P; y = Q)` fails to parse. Extract to a named def instead.
- **Character class definitions**: Use bare expression: `def $foo = 0x00 .. 0x3C`. NOT `def $foo = $[0x00 .. 0x3C]`. The `$[...]` wrapper is only for *using* a class as a parser.
- **External declarations break interpreter**: Adding bodiless declarations (for C++ primitives) makes `daedalus run` fail on the *entire* spec, even if the declarations are never called. Factor into separate modules.

## Modules

Module name must match filename (`import Foo` requires `Foo.ddl`). Daedalus resolves imports from the directory of the importing file and from `--path` directories. Use modules to factor specs into shared base + variant top-level files (e.g., interpreter vs compiled versions).

## External Primitives (Compiled Parsers)

Bodiless declarations signal C++ implementations linked at compile time:

```
def MyDecode (body : stream) : stream    -- no = body
```

The generated C++ expects functions named `parser_MyDecode` with this signature:
```cpp
bool parser_MyDecode(DDL::ParserState<DDL::Input>& pstate,
                     DDL::Input* result, DDL::Input* out_input,
                     DDL::Input input, DDL::Input body);
```

Return `true` on success (set `*result` and `*out_input`), `false` on failure (free `input`). Wrap DDL parameters in `DDL::Owned()` for RAII. Create result streams with `DDL::Input("name", data, DDL::Size(len))`. See `daedalus/formats/pdf/new/c++/pdfcos/src/primitives.cpp` for examples.

## Dex (Daedalus Export DSL)

Dex generates glue code to translate Daedalus parser output into application types (currently C++). A `.dex` spec declares how each Daedalus type maps to a target-language type, and the `dex` tool generates `.h` + `.cpp` files.

**Key ideas**: import Daedalus parsers, define type aliases, write exporter functions (struct/union/iteration/polymorphic). The standard `CPP` module provides default exporters for common types (`uint8_t`..`uint64_t`, `string`, `vector<T>`, `optional<T>`, `map<K,V>`).

See `dex-reference.md` in this directory for the complete Dex language reference.

## Running

```bash
# --path is a GLOBAL flag (before subcommand, not after)
daedalus run spec.ddl --input file
daedalus --path=dir run spec.ddl --input file
daedalus --path=dir compile-c++ spec.ddl --out-dir=dir --entry=Main --file-root=name
daedalus compile-hs spec.ddl --out-dir=dir

# Dex: generate C++ exporter from .dex spec
dex my-spec.dex
dex my-spec.dex --dex-path=<dex-lib-dir> --ddl-path=<ddl-dir> --output=exporter.cpp
```
