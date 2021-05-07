A Daedalus Code Generator
=========================


Languages / ASTs
-----------------

1. **`TC`**: typechecked Daedalus (input)

2. **`Core`**: 1st order language with state and non-determinism

3. **`VM`**: a pure (i.e., SSA-like) CFG language

4. **`C++`**: the target language



A Flavor of `Core`
------------------

* Has statements and expressions (`Grammar` and `Expr`)
  - Effects only done by statements
  - Expressions are pure, but potentilally partial

* Effects:
  - Maniuplate input (get, set)
  - Failure
  - Choice operators: `<|` and `|`


The `VM` Language
-----------------

* A collection of *basic blocks*

* A basic block has:
  1. parameters
  2. A sequence of statements (no control)
  3. A terminal statement (control)

* Supports a limited form of coroutines



Passes
------

1. Specialize: `TC` &rarr; `TC`
   - Remove higher-order constructs and polymorphism

2. Desugar: `TC` &rarr; `Core`
   - Reduces number of special cases

3. Compile: `Core` &rarr; `VM`
   - Explicit implementation of choice and failure

4. Memory: `VM` &rarr; `VM`
   - Ownership analysis and MM instructions

5. Generate: `VM` &rarr; `C++`


Runtime System
--------------

* A `C++` support library for running Daedalus parsers

* Implements Daedalus types
  - numbers, big-integers, arrays, maps, streams
  - assocaited operations

* Implements global parser state
  - input manipulation
  - call stack
  - coroutine support



