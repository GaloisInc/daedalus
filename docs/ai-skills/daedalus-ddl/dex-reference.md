# Dex Language Reference

Dex (Daedalus Export DSL) generates glue code to translate Daedalus parser output into application-specific data representations. Given a `.dex` spec, the `dex` tool generates target-language files (currently C++ `.h` + `.cpp`).

## Command Line

```bash
dex my-spec.dex
dex my-spec.dex --dex-path=DIR --ddl-path=DIR --output=FILE
```

| Flag | Purpose |
|------|---------|
| `--ddl-path=DIR` | Search path for Daedalus `.ddl` specs |
| `--dex-path=DIR` | Search path for Dex `.dex` modules (e.g., standard library) |
| `--output=FILE` | Output filename (derives `.h` and `.cpp` from it) |

## Top-Level Declarations

A `.dex` file has five declaration types:

### 1. Import -- Bring in Daedalus types

```dex
import json(JSON_value_strict)
```

Specifies which Daedalus module and parsers to work with. Only main parsers need listing; dependencies are inferred automatically.

### 2. Using -- Import Dex modules

```dex
using CPP
```

Brings all definitions from a Dex module into scope. Refer to definitions as `NAME` or qualified as `MODULE::NAME`. The special `CPP` module provides standard C++ type mappings.

### 3. Extern -- Inject target-language code

```dex
// Added to .h file (typically #includes)
extern ->
  #include <vector>
  #include <string>

// Added to .cpp file (helper functions)
extern def ->
  uint64_t exp10(uint32_t exp) {
    uint64_t res = 1;
    while (exp != 0) { res *= 10; --exp; }
    return res;
  }
```

Code blocks in `extern` declarations cannot contain escapes.

### 4. Type -- Declare target-language type aliases

```dex
type JSON -> nlohmann::basic_json<>
type optional<T> -> std::optional<T>
type vector<T> -> std::vector<$T>
type map<K,V> -> std::unordered_map<$K,$V>
```

Defines how to refer to target-language types in the rest of the spec. Type parameters can be referenced via `$` escapes.

### 5. Exporter Definitions -- Map Daedalus types to target types

```dex
[default] def NAME<TYPE_PARAMS,FUN_PARAMS>(NAME: DAEDALUS_TYPE): EXTERN_TYPE
  DEFINITION
```

The `default` keyword marks this as the automatic exporter for the Daedalus type (only for definitions without `FUN_PARAMS`).

## External Code Blocks

`->` begins a code block in the target language. The block includes all text indented at or beyond the first non-whitespace character after `->`.

```dex
->
  This is part of the block.
  This too.
    And this (more indented is fine).
This is NOT part of the block (less indented).
```

### Escapes

| Syntax | Meaning |
|--------|---------|
| `$identifier` | Single identifier escape to Dex |
| `$(expression)` | Complex expression escape to Dex |
| `$$` | Literal `$` in target code |

```dex
->
  CustomPoint($(pt.x), $(pt.y))
  A literal $$dollar sign.
```

When an escape refers to a Daedalus value without specifying an exporter, Dex uses the `default` exporter for that type.

## Exporter Definition Styles

### Struct Export

For Daedalus record/struct types. The definition is a code block with field access via escapes.

```dex
def as_point(pt: Point): CustomAppPoint ->
  CustomAppPoint($(pt.x), $(pt.y))
```

### Discriminated Union (Case) Export

For Daedalus tagged union types. Uses `case` to handle each constructor.

```dex
def mb_to_u8(x: maybe (uint 8)): uint8_t =
  case x of
    nothing -> 0
    just v  -> $v

def exportJSON(x: JSON_value): JSON =
  case x of
    Null      -> nlohmann::json(nullptr)
    Bool b    -> nlohmann::json($b)
    Number n  = exportNumber(n)
    String s  -> nlohmann::json($(as_string(s)))
    Array xs  -> nlohmann::json($(as_vector<exportJSON>(xs)))
    Object xs -> nlohmann::json($(as_map<as_string,exportJSON>(xs)))
```

Note: `->` introduces a code block for the case arm; `=` delegates to another exporter.

### Iteration Export

For arrays and maps. Has three parts: `init`, `for`, `return`.

```dex
def as_string (xs: [uint 8]): string =
  init        -> std::string result;
  for x in xs -> result.push_back($(as_char(x)));
  return      -> result

def as_vector<A, T, element: A => T>(xs: [A]): vector<T> =
  init -> std::vector<$T> result;
  for x in xs -> result.push_back($(element(x)));
  return -> result

// Maps iterate over key-value pairs
def as_map<K,V,X,Y, expK: K => X, expV: V => Y> (mp: [K:V]): map<X,Y> =
  init -> std::unordered_map<$X,$Y> result;
  for k,v in mp -> result.insert({$(expK(k)), $(expV(v))});
  return -> result
```

### Extern Export

`= extern` declares an exporter whose C++ implementation is provided elsewhere (typically in the `extern def` block). Dex generates a declaration in the header but no definition in the `.cpp` — you supply it.

```dex
default def as_uint8(x: uint 8): uint8_t = extern
default def as_int64(x: sint 64): int64_t = extern
```

The CPP module's primitives use pre-defined C++ functions in the DDL runtime (`ddl/dex.h`). But `= extern` works for **any** exporter and is the primary escape hatch when dex can't express an export directly (see Known Limitations below):

```dex
// Declare in dex — dex generates the header declaration
def exportRecord(x: Record): AppRecord = extern

// Provide the implementation in extern def, inside the matching namespace
extern def ->
  namespace mymodule {
  Record exportRecord(User::Record x) {
    // ... hand-written C++ using borrow_*() accessors ...
  }
  }
```

**Important**: `extern def` code is emitted outside the module namespace (see Generated File Structure). When implementing `= extern` exporters, you must wrap the implementation in the module's namespace yourself.

## Polymorphic Exporters

Type parameters (`<A,T>`) and function/exporter parameters (`f: A => T`) allow generic exporters.

```dex
def as_optional<A,T,f: A => T>(x: maybe A): optional<T> =
  case x of
    nothing -> std::optional<$T>()
    just a  -> std::optional($(f(a)))
```

Usage at call site -- supply exporter as type argument:

```dex
$(as_vector<exportJSON>(xs))
$(as_map<as_string,exportJSON>(xs))
$(as_optional<myExporter>(val))
```

## Standard Library: CPP Module

Located at `dex/lib/CPP.dex`. Provides default exporters for common Daedalus-to-C++ conversions.

### Primitive Types

| Daedalus Type | C++ Type | Exporter | Default? |
|---------------|----------|----------|----------|
| `bool` | `bool` | `as_bool` | yes |
| `uint 8` | `uint8_t` | `as_uint8` | yes |
| `uint 16` | `uint16_t` | `as_uint16` | yes |
| `uint 32` | `uint32_t` | `as_uint32` | yes |
| `uint 64` | `uint64_t` | `as_uint64` | yes |
| `sint 8` | `int8_t` | `as_int8` | yes |
| `sint 16` | `int16_t` | `as_int16` | yes |
| `sint 32` | `int32_t` | `as_int32` | yes |
| `sint 64` | `int64_t` | `as_int64` | yes |
| `uint 8` | `char` | `as_char` | no |

### Collection Types

| Daedalus Type | C++ Type | Exporter | Params |
|---------------|----------|----------|--------|
| `maybe A` | `std::optional<T>` | `as_optional` | `<A,T,f: A => T>` |
| `[A]` | `std::vector<T>` | `as_vector` | `<A,T,element: A => T>` |
| `[uint 8]` | `std::string` | `as_string` | (none) |
| `[K:V]` | `std::unordered_map<X,Y>` | `as_map` | `<K,V,X,Y,expK: K => X, expV: V => Y>` |

## Lexical Elements

**Keywords**: `case`, `def`, `default`, `extern`, `for`, `import`, `in`, `init`, `of`, `return`, `type`, `using`

**Operators**: `->`, `=>`, `=`, `.`, `,`, `::`, `:`, `$`, `<`, `>`

**Comments**: `// single line`

## Generated File Structure

Dex generates a `.h` and `.cpp` with this layout:

**Header (.h)**:
1. `#pragma once` + DDL runtime includes
2. `extern ->` blocks (raw, no namespace) — your `#include`s for parser header, app types
3. Exporter declarations wrapped in `namespace <module> { ... }`

**Implementation (.cpp)**:
1. `extern def ->` blocks (raw, **outside** any namespace) — your helper functions
2. Exporter definitions wrapped in `namespace <module> { ... }`

The module namespace is derived from the `.dex` filename (e.g., `rosbag.dex` → `namespace rosbag`). CPP module definitions go in `namespace CPP`.

**Key consequence**: `extern def` code is emitted before and outside the namespace-wrapped definitions. For `= extern` implementations, you must manually wrap them in the correct namespace. However, since `extern def` comes after the `#include` of the generated header (which you place at the top of the extern def block), all dex-generated declarations are visible to your helper code.

## Practical Gotchas

### Case-bound variables must be exported

In case arms, every bound variable **must** appear inside a `$()` escape. Referencing the variable in raw C++ text does not count — dex's checker tracks exports statically.

```dex
// WRONG: c used in raw C++, dex reports "Variable 'c' was never exported"
chunk c -> MyType{ helper(c) }

// OK: c exported via $() escape
chunk c -> MyType{ $(exportChunk(c)) }

// OK: delegation form (= instead of ->)
chunk c = exportChunk(c)
```

If you can't export a case variable (e.g., due to type ambiguity), use `= extern` for the entire exporter and implement it in C++.

### DDL type names in dex

- DDL types are referenced by their **DDL name**, not the C++ generated name. Dex resolves them from the imported DDL module.
- **Parameterized types** must include their parameters: `MessageDataRecord S`, not just `MessageDataRecord`. Use a dex type parameter (`<S>`) if the actual type can't be named.
- **DDL field names** are used as-is in dex escapes (`x.size`), even if the generated C++ accessor has a different name (`borrow_sizze()`). DDL's C++ backend uses `z` as an escape character, so every literal `z` in identifiers is doubled to `zz` (e.g., `size` → `sizze`, `analyze` → `analyzzze`).
- **`stream` type** cannot currently be named in dex signatures (checker bug). Access stream fields via raw C++ (`x.borrow_data()`) or use a type parameter as a proxy.

### Converting DDL streams to byte vectors

DDL `stream` fields are `DDL::Input` in C++. To get raw bytes, use `borrowBytes()` which returns `std::string_view`:

```cpp
std::vector<uint8_t> input_to_bytes(DDL::Input inp) {
  auto sv = inp.borrowBytes();
  return std::vector<uint8_t>(
    reinterpret_cast<const uint8_t*>(sv.data()),
    reinterpret_cast<const uint8_t*>(sv.data()) + sv.size()
  );
}
```

Place this in `extern def` and call it from struct export code blocks.

## Known Limitations (as of 2026-02)

These are bugs in the dex tool. Use `= extern` with C++ implementations to work around all three.

1. **`stream` type arity mismatch**: `Check.hs` registers `stream` as needing 1 type argument, but `TStream` in daedalus-core takes 0. Any exporter with `stream` in its type signature fails.

2. **Anonymous union name ambiguity**: When a DDL struct contains an anonymous union, both get the same `tnameText`. Dex can't disambiguate — referencing either by name produces "Ambiguous name". The C++ generated name (e.g., `ChunkRecord3`) is not accessible from dex.

3. **Non-boxed union case codegen**: `genCase` in `ExportCPP.hs` emits `refCount()`/`del()` calls for all user-defined unions, but DDL union types only derive `HasRefs` (not `IsBoxed`), so those methods don't exist. Case exporters on DDL union types fail to compile.

## Build Pipeline Integration

Typical Makefile flow for a format with Dex export:

```makefile
DDL=path/to/rts-c
DEX=path/to/dex/lib

# 1. Generate parser from DDL
parser/main_parser.h parser/main_parser.o: spec.ddl
	daedalus compile-c++ $< --out-dir=parser --entry=Main
	clang++ -std=c++20 -I $(DDL) -c parser/main_parser.cpp -o parser/main_parser.o

# 2. Generate exporter from Dex
parser/exporter.o: my-spec.dex parser/main_parser.h
	dex my-spec.dex --dex-path="$(DEX)" --output=parser/exporter.cpp
	clang++ -std=c++20 -I $(DDL) -I. -c parser/exporter.cpp -o parser/exporter.o

# 3. Link application
app: app.cpp parser/main_parser.o parser/exporter.o
	clang++ -std=c++20 -I $(DDL) -I. app.cpp parser/main_parser.o parser/exporter.o -o $@
```

## Complete Example: JSON Exporter

```dex
import json(JSON_value_strict)

using CPP

extern ->
  #include <cmath>
  #include "main_parser.h"
  #include "json.hpp"

extern def ->
  #include <parser/exporter.h>

  uint64_t exp10(uint32_t exp) {
    uint64_t res = 1;
    while (exp != 0) { res *= 10; --exp; }
    return res;
  }

type JSON -> nlohmann::basic_json<>

def exportJSON(x: JSON_value): JSON =
  case x of
    Null      -> nlohmann::json(nullptr)
    Bool b    -> nlohmann::json($b)
    Number n  = exportNumber(n)
    String s  -> nlohmann::json($(as_string(s)))
    Array xs  -> nlohmann::json($(as_vector<exportJSON>(xs)))
    Object xs -> nlohmann::json($(as_map<as_string,exportJSON>(xs)))

def exportNumber(x: JSON_number): JSON ->
  [=] () mutable {
    int64_t w = $(x.whole);
    int32_t e = $(x.exp);
    if (e < 0) return nlohmann::json(w * std::pow(10,e));
    auto pos_e = exp10(static_cast<uint32_t>(e));
    if (w < 0) return nlohmann::json(w * static_cast<int64_t>(pos_e));
    return nlohmann::json(static_cast<uint64_t>(w) * pos_e);
  }()
```
