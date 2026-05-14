# Daedalus Language Reference

## Declarations

```
def ParserName = ...          -- parser (uppercase)
def valueName = ...           -- semantic value (lowercase)
def $className = ...          -- character class ($-prefix)

def Example n P =             -- parameterized: n=value, P=parser
  if n > 0 then P else UInt8

import ModuleName             -- import another .ddl file

-- single line comment
{- multi-line comment (nestable) -}
```

## Primitive Parsers

| Parser | Effect |
|--------|--------|
| `UInt8` | Parse one byte, return `uint 8` |
| `$[ set ]` | Parse one byte matching set |
| `Match "str"` | Parse exact byte sequence |
| `Match [0x00, 0x01]` | Parse exact bytes |
| `END` | Succeed only if input exhausted |
| `^ expr` | Return value, consume nothing (pure) |
| `Fail "msg"` | Always fail with message |
| `Accept` | Succeed, consume nothing |

## Sequencing

```
-- Block: returns last result (or $$ if set)
def P =
  block
    Match "AB"
    $$ = UInt8
    Match "CD"

-- Braces alternative
def P = { Match "AB"; $$ = UInt8; Match "CD" }

-- Array sequence: returns array of all results (must be same type)
def Q = [ $['A']; $['B']; $['C'] ]

-- Structure: named fields become record type
def RGB =
  block
    red   = UInt8
    green = UInt8
    blue  = UInt8

-- Local variable (NOT a field): use let
def Digit =
  block
    let d = $['0' .. '9']
    ^ d - '0'
```

**Desugaring rules:**

| Sugar | Expands to |
|-------|-----------|
| `{ $$ = P; Q }` | `{ let x = P; Q; ^ x }` |
| `[ P; Q ]` | `{ let x0 = P; let x1 = Q; ^ [x0, x1] }` |
| `{ x = P; y = Q }` | `{ let x = P; let y = Q; ^ { x = x, y = y } }` |

## Alternatives

```
-- Biased choice: try left first, backtrack on failure
def P = $['A'] <| $['B']

-- Biased with layout
def P =
  First
    Match "option1"
    Match "option2"

-- Unbiased choice (ambiguous, both may succeed)
def P = $['A'] | $['B']

-- Unbiased with layout
def P =
  Choose
    Match "option1"
    Match "option2"

-- Tagged unions (named alternatives)
def GoodOrBad =
  First
    good = $['G']
    bad  = $['B']

-- Explicit union construction
^ {| good = x |}

-- Explicit union type declaration
def MyTag =
  union
    tagA: uint 8
    tagB: [uint 8]
```

## Repetition

```
Many P              -- zero or more (maximal!)
Many (1..) P        -- one or more
Many n P            -- exactly n times
Many (i..j) P       -- between i and j times
Many (..j) P        -- at most j times
```

**Warning**: `Many` is maximal. `{ Many $['7']; $['7'] }` always fails.

## Types and Values

| Type | Values | Notes |
|------|--------|-------|
| `bool` | `true`, `false` | `!`, `&&`, `\|\|` (short-circuit) |
| `uint N` | N-bit unsigned | `uint 8`, `uint 16`, `uint 64` |
| `sint N` | N-bit signed | |
| `int` | Arbitrary integer | |
| `maybe T` | `just x`, `nothing` | `Optional P` returns `maybe` |
| `[T]` | Array | `[]`, `[1,2,3]`, `"hello"` |
| `[K -> V]` | Association map | `empty`, `insert`, `lookup` |
| `stream` | Input stream | `GetStream`, `SetStream` |

## Operators

```
-- Arithmetic
+ - * / %

-- Comparison
== < <= > >=

-- Bit shift (amount is uint 64)
<< >>

-- Bitwise
~              -- complement
.^.            -- xor
.|.            -- or
.&.            -- and
#              -- bit append: uint N # uint M -> uint (N+M)
<#             -- truncating append (keeps N least-significant bits)
```

## Type Coercions

```
e as  T    -- compile-time check, lossless only
e as! T    -- always succeeds, may lose info
e as? T    -- runtime check, fails (backtracks) if lossy
```

## Control Flow

```
-- If-then-else
if cond then P else Q

-- Guards (consume no input, fail if not matched)
expr is true
expr is false
expr is just           -- unwraps maybe, returns inner value
expr is nothing
expr is tagName        -- match union variant

-- Case (no backtracking on match; unmatched = fail)
case expr of
  good x -> ^ x
  bad    -> Fail "bad"
  _      -> Fail "other"

-- For loop with accumulator
for (acc = 0; elem in array) (acc + elem)
for (acc = 0; idx, elem in array) (acc + idx * elem)

-- Map (transform each element, returns same-size collection)
map (x in xs) (x + 1)
map (i, x in xs) (i + x)

-- Many loop with state (repeat until body fails)
many (s = 0) (10 * s + Digit)

-- Commit (prevent backtracking past this point)
First
  { $['A']; commit; $['B'] }
  { $['A']; $['C'] }
```

## Array Builders

```
builder              -- empty builder
emit b x             -- append value
emitArray b xs       -- append array
emitBuilder b1 b2    -- append builder
build b              -- finalize to array

def BuildString =
  block
    let b = builder
    let b = emit b 'H'
    let b = emitArray b "ello"
    ^ build b
```

## Map Operations

```
empty                -- empty map
insert k v m         -- insert/replace key
Insert k v m         -- parser: fails if key already exists
lookup k m           -- returns maybe
Lookup k m           -- parser: fails if key missing
```

## Bitdata

Bit-level field definitions, applied via coercion on parsed bytes:

```
bitdata MyFlags where
  MyFlags = { flag1 : uint 1, flag2 : uint 1, reserved : uint 6 }

bitdata Color where
  red   = 0x00 : uint 8
  green = 0x01
  blue  = 0x02

-- Usage: parse byte then coerce
def ParseFlags =
  block
    let raw = UInt8
    raw as? MyFlags
```

## Stream Manipulation

```
GetStream            -- save current stream position
SetStream s          -- restore stream position
Take n s             -- first n bytes (fails if insufficient)
Drop n s             -- skip n bytes
Offset               -- current byte offset (uint 64)
arrayStream data     -- convert byte array to stream

-- Parse same input twice
def ParseTwice P1 P2 =
  block
    let cur = GetStream
    p1result = P1
    SetStream cur
    p2result = P2

-- Parse fixed-size chunk
def Chunk n P =
  block
    let cur  = GetStream
    let this = Take n cur
    let next = Drop n cur
    SetStream this
    $$ = P
    SetStream next
```

## Character Classes

```
def $digit   = '0' .. '9'                     -- range
def $hex     = $digit | 'a'..'f' | 'A'..'F'   -- union
def $noquote = $any - '"'                      -- difference
$[ $digit ]                                    -- use class as parser
$[ 'A' | 'B' | 'C' ]                          -- inline set
$[ 0x00 .. 0x1F ]                              -- byte range
```

## Standard Library (`import Daedalus`)

```
-- Endian-aware integer parsers
BEUInt16  BEUInt32  BEUInt64       -- big-endian unsigned
LEUInt16  LEUInt32  LEUInt64       -- little-endian unsigned
BESInt16  BESInt32  BESInt64       -- big-endian signed
LESInt16  LESInt32  LESInt64       -- little-endian signed

-- Floating point
BEFloat   BEDouble   BEHalfFloat
LEFloat   LEDouble   LEHalfFloat

-- Stream / chunk utilities
Bytes n            -- consume n bytes, return as stream (not byte array)
Chunk n P          -- parse P over exactly the next n bytes (substream)
WithStream s P     -- run parser P on stream s (restores original stream after)
arrayStream arr    -- convert [uint 8] byte array to a stream
Only P             -- succeed only if P consumes ALL remaining input

-- Utilities
Guard b            -- succeed if b is true (fail otherwise)
Skip n             -- consume n bytes, discard
LookAhead P        -- run P without consuming input
```

## Implicit Parameters

Parameters prefixed with `?` are propagated automatically through the call chain:

```
def ParseWord = UInt16        -- uses ?bigEndian implicitly
```

## Modules and Imports

```
import Daedalus        -- standard library (resolved via --path)
import MyModule        -- imports MyModule.ddl from same directory or --path

-- Module name MUST match filename (MyModule -> MyModule.ddl)
-- Imported names are available unqualified
-- Use modules to factor shared definitions from variant top-level files
```

## External Primitives

Bodiless declarations (no `=` body) signal C++ implementations linked at compile time.
Only usable with `daedalus compile-c++`, NOT `daedalus run`.

```
-- DDL declaration (no body)
def MyDecode (compressed : stream) : stream

-- Use like any other parser
def Decompress s = WithStream (MyDecode s) (Many Record)
```

**Critical**: Adding external declarations to a spec makes `daedalus run` fail on the
*entire* spec, even if the external is never called. Factor into separate modules:
- `Base.ddl` -- shared definitions
- `spec.ddl` -- interpreter version (no externals)
- `spec-compiled.ddl` -- compiled version (with externals)

## Common Patterns

```
-- Token combinator (skip trailing whitespace)
def Token P =
  block
    $$ = P
    Many WS

-- Natural number from digit sequence
def Natural =
  block
    let ds = Many (1..) Digit
    ^ for (val = 0; d in ds) (addDigit val d)

def Digit =
  block
    let d = $['0' .. '9']
    ^ d - '0'

def addDigit val d = 10 * val + (d as uint 64)

-- Length-prefixed data
def LengthPrefixed P =
  block
    let n = BEUInt32
    Chunk n P

-- Magic number validation
def Header =
  block
    Match "MAGIC"
    let version = UInt8
    version == 1 is true
```

## Binary Format Patterns

```
-- Header field map: parse name=value pairs into a map for random access
def $notEq = 0x00 .. 0x3C | 0x3E .. 0xFF  -- any byte except '='

def HeaderField = block
  let len = LEUInt32 as uint 64
  Chunk len
    block
      name  = Many $[$notEq]
      Match "="
      value = Many UInt8

def ParseHeaderFields = block
  let fields = Many HeaderField
  ^ for (m = (empty : [[uint 8] -> [uint 8]]); f in fields)
      (insert f.name f.value m)

-- Look up a required field and parse its value bytes with parser P
def Field P name m =
  WithStream (arrayStream (Lookup name m)) (Only P)

-- Op-code dispatched records: parse header + data, dispatch on op field
def Record = block
  let headerLen  = LEUInt32 as uint 64
  let headerMap  = Chunk headerLen ParseHeaderFields
  let dataLen    = LEUInt32 as uint 64
  let dataStream = Bytes dataLen          -- capture data as stream
  let op         = Field UInt8 "op" headerMap
  case op of
    0x01 -> {| typeA = ParseTypeA headerMap dataStream |}
    0x02 -> {| typeB = ParseTypeB headerMap dataStream |}
    _    -> Fail "unknown op code"

-- Counted entries: parse exactly N items from a substream
def ParseEntries m dataStream = block
  count   = Field LEUInt32 "count" m
  entries = WithStream dataStream (Many (count as uint 64) Entry)
```
