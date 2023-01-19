
Talos implementation
====================

The Talos tool synthesises valid documents from DDL grammars.  The tool has the following phases:

1. Parsing the input file and converting to Talos-flavoured Core;
2. Analysing the Core to decompose the grammar into slices;
3. Traversing the Core to synthesize a document, calling into slice model generators when the start of a slice is encountered;

The implementatino is discussed below

Text to Core
------------

The majority of the translation from the text file into Core is
implemented as part of Daedalus, leaving us with a [Core module](../daedalus-core/src/Daedalus/Core/Decl.hs).

Talos [performs](src/Talos.hs) a number of standard and specific
Core to Core passes ([Talos.Passes]), namely

* Fail propagation (making cases partial)
* Polymorphic type monomorphisation
* Constant folding
* Naming arguments to functions;
* Naming the argument to string matches (which is also the result of the match)

These passes ensure that the analysis phase has a name for the
arguments to a function (as the argument to a function may be the
start of a slice, so it needs a name), and that we can safely ignore
string matches when slicing (as a string match always returns its
argument on success, so we just use the argument directly).

Slicing
-------

Briefly, the [Talos.Analysis.Exported][] module contains the data
structures used by the synthesis phase of Talos, using definitions
from [Talos.Analysis.Slice][] The below notes on the implementation of
slicing is not required to use the results.

The slicing phase lives under [Talos.Analysis][].  The aim of the
slicing phase is to decompose the input grammar into a collection of
independent slices (defined in [Talos.Analysis.Slice][], along with
sliced expressions in [Talos.Analysis.SLExpr][]).  A slice is, in
essence, a grammar where some nodes have been replaced by holes.
Slices are independent if each choice in the input grammar (i.e., byte
values and grammar alternatives) occurs in exactly one slice (or no
slice if the choice cannot impact the feasibility of synthesis).
Slices are also closed, in the sense that all (parts of) variables
used in the grammar are defined in the grammar.

The slicing process has two phases, firstly a fixpoint is constructed;
and secondly, the fixpoint is _exported_ [Talos.Analysis.Exported][].
The export process simplifies the representation of slices from that
used in the fixpoint construction, including replacing holes in
expressions by default values (which should never actually be
examined), and forgetting which abstract environment (see below) was
used (to avoid carrying around the class in the remainder of Talos).

The slicing algorithm is abstract in the way in which the grammar is
analyzed; the abstraction is in
[Talos.Analysis.AbsEnv][].  Briefly, an
_abstract environment_ relates variables with _abstract predicates_,
and has a precondition operation which calculates the
abstract environment required for an expression to satisfy an abstract predicate.  

The two main abstract environments are the whole-variable environment
([Talos.Analysis.VarAbsEnv][]) 
and the field-sensitive abstract environment 
([Talos.Analysis.FieldAbsEnv][]).  
The whole-variable abstract predicate carries no information (we want
the whole variable), and the abstract environment is equivalent to a
set of variables --- the abstract precondition operation then just
calculates the set of free variables.

The field-sensitive abstract predicate projects a subset of the fields
in a struct, including nested fields for fields with structure type.
The abstract precondition operation then calculates which fields of
the free variables are used in an expression.

<!-- Slicing is per-function backwards analysis; initially, each function -->
<!-- is sliced ignoring the result.  Predicates are introduced when a -->
<!-- failing construct (i.e., a case) is reached:  -->

The slicing algorithm uses the following auxiliary modules
  * [Talos.Analysis.Eqv][]: notions of equivalence used to determine if a fixpoint has been reached;
  * [Talos.Analysis.Merge][]: merging is used to combine slices when they are discovered to be non-independent (i.e., rely on the same choice);
  * [Talos.Analysis.Domain][]: the domain over which the slicing algorithm operates, containing the in-progress slices for the current grammar function;
  * [Talos.Analysis.Fixpoint][]: a naive algorithm for constructing the fixpoint for the slicing algorithm
  * [Talos.Analysis.Monad][]: the monad containing the state for slicing, mostly this builds on Talos.Analysis.Fixpoint.
  
Non-dependent Document Generation
---------------------------------

The outer synthesis loop is in [Talos.Synthesis][].  


Slice Model Generation
----------------------

Symbolic Execution
------------------

[Talos.SymExec.SolverT]: src/Talos/SymExec/SolverT.hs
[Talos.SymExec.StdLib]: src/Talos/SymExec/StdLib.hs
[Talos.SymExec.ModelParser]: src/Talos/SymExec/ModelParser.hs
[Talos.SymExec.Expr]: src/Talos/SymExec/Expr.hs
[Talos.SymExec.Funs]: src/Talos/SymExec/Funs.hs
[Talos.SymExec.SemiExpr]: src/Talos/SymExec/SemiExpr.hs
[Talos.SymExec.SemiValue]: src/Talos/SymExec/SemiValue.hs
[Talos.SymExec.Type]: src/Talos/SymExec/Type.hs
[Talos.SymExec.Path]: src/Talos/SymExec/Path.hs
[Talos.Analysis]: src/Talos/Analysis.hs
[Talos.Analysis.AbsEnv]: src/Talos/Analysis/AbsEnv.hs
[Talos.Analysis.VarAbsEnv]: src/Talos/Analysis/VarAbsEnv.hs
[Talos.Analysis.FieldAbsEnv]: src/Talos/Analysis/FieldAbsEnv.hs
[Talos.Analysis.Eqv]: src/Talos/Analysis/Eqv.hs
[Talos.Analysis.Merge]: src/Talos/Analysis/Merge.hs
[Talos.Analysis.Domain]: src/Talos/Analysis/Domain.hs
[Talos.Analysis.Fixpoint]: src/Talos/Analysis/Fixpoint.hs
[Talos.Analysis.Monad]: src/Talos/Analysis/Monad.hs
[Talos.Analysis.Slice]: src/Talos/Analysis/Slice.hs
[Talos.Analysis.SLExpr]: src/Talos/Analysis/SLExpr.hs
[Talos.Analysis.Exported]: src/Talos/Analysis/Exported.hs
[Talos.Strategy]: src/Talos/Strategy.hs
[Talos.Strategy.Monad]: src/Talos/Strategy/Monad.hs
[Talos.Strategy.DFST]: src/Talos/Strategy/DFST.hs
[Talos.Strategy.SearchTree]: src/Talos/Strategy/SearchTree.hs
[Talos.Strategy.SearchT]: src/Talos/Strategy/SearchT.hs
[Talos.Strategy.SymbolicM]: src/Talos/Strategy/SymbolicM.hs
[Talos.Strategy.PathSymbolicM]: src/Talos/Strategy/PathSymbolicM.hs
[Talos.Strategy.MuxValue]: src/Talos/Strategy/MuxValue.hs
[Talos.Strategy.BTRand]: src/Talos/Strategy/BTRand.hs
[Talos.Strategy.Symbolic]: src/Talos/Strategy/Symbolic.hs
[Talos.Strategy.PathSymbolic]: src/Talos/Strategy/PathSymbolic.hs
[Talos.Strategy.PathCondition]: src/Talos/Strategy/PathCondition.hs
[Talos.Strategy.MemoSearch]: src/Talos/Strategy/MemoSearch.hs
[Talos.Strategy.OptParser]: src/Talos/Strategy/OptParser.hs
[Talos.Synthesis]: src/Talos/Synthesis.hs
[Talos.Passes]: src/Talos/Passes.hs



 

