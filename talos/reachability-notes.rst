Reachability Analysis for Talos 
-------------------------------


Objective 
=========

Suppose we have a simple Daedalus program such as ``reachtest.ddl``, shown
below: 

.. code-block:: daedalus

  Main = { 
    @a = UInt8 as int; 
    @b = Choose1 { 
      { a < 42 is true; P1 a }
      { P2 a }
    }; 
    b < 50 is true; 
  }

  P1 (i : int) = { @j = Uint8 as int; ^ i + j; -- #MYANCHOR }
  P2 (i : int) = { ^ i + 8; }

We want to be able to select a particular syntactic construct, e.g. the anchor
``MYANCHOR``. Suppose we call Talos as follows: 

.. code-block: bash 

  cabal exe talos -- ./tests/reachtest1.ddl --target "MYANCHOR" 

We want Talos to construct an input document that can be parsed in a way that
reaches the anchor. 

Note that parsing in Daedalus is nondeterministic, which means that a document
may not actually reach the anchor if the parser chooses another control path. To
solve this, we would need to modify the original DDL or control the Daedalus
runtime which seems beyond the scope of this initial investigation. 


Notes 
=====

One question is whether we should over- or under-approximates the original DDL
program: 

* Over-approximation would mean *increasing the likelyhood* that a particular 
  anchor would occur, but avoiding any transformations that would eliminate a
  possible document that could reach the relevant path. This means that Talos
  might not generate a relevant document. The limit case of over-approximation
  is the original program. 

* Under-approximation would mean eliminating possibilities from the program while 
  ensuring that at least one document can be generated that includes the selected
  anchor. The limit case of under-approximation is a parser that only matches one 
  document. 

We can also do both at the same time, so long as we preserve the property that
some document can be generated that satisfies the property.  

One good thing about Daedalus is that there are only two branching constructs:
case and choice. So if we control these constructs, we can direct the search
wherever we want. 


Ideas
=====

A really dumb way of doing what we want is to thread through a control variable 
which is assigned only when the correct program point is reached. E.g. for a small 
program we might have the following: 

.. code-block: daedalus 

  Main = { 
    @a = Choose1 {
      {$$ = UInt8 as int; Guard $$ > 7; #ANCHOR}
      {$$ = UInt8 as int; Many 2 UInt8; }
    }
  }

We might transform the program as follows: 

.. code-block: daedalus 

  Main = { 
    (@a, @_control) = Choose1 {
      {@_var = UInt8 as int; Guard _var > 7; ^(_var, true)}
      {@_var = UInt8 as int; Many 2 UInt8; ^(_var, false)}
    }; 
    _control is true; 
  }

If we had deeper nesting or function calls we could thread this through. This is 
somewhere that having global state would have been helpful though :) 

This is obviously semantically correct and precise, but has a couple of
disadvantages for synthesis. (1) This won't result in good performance for
synthesis, becuase the solver doesn't know that one of the branches is dead. (2)
by entangling more things, this might make the construction of independent
slices more difficult, which will make the search space less tractable. 

On the other hand, this might be a starting point for an analysis that tries to
eliminate dead code. I.e. you start with an instrumented program that uses a
control variable to prune the space, and then apply a separate dead code tool
that eliminates branches if they can be shown to be unreachable. 

A straightforward alternative way to do this is to build a literal program slice
that includes the syntactic point we care about. If the program doesn't include
iteration or recursive function calls then there's an easy way to do this: we
just walk up the AST and prune any choice points or case statements that don't
include the current path. 

For the program above, we can build the program slice as follows by commenting 
out the alternate choice and the function P2: 

.. code-block:: daedalus

  Main = { 
    @a = UInt8 as int; 
    @b = Choose1 { 
      { a < 42 is true; P1 a }
      -- { P2 a }
    }; 
    b < 50 is true; 
  }

  P1 (i : int) = { @j = Uint8 as int; ^ i + j; -- #MYANCHOR }
  -- P2 (i : int) = { ^ i + 8; }

This gets a bit more tricky if we have different call sites for functions. In
this case we would have to specialise the functions to call-sites so that we can
prune choices appropriately. 

Another tricky case comes in situations where the anchor is in a function. We want 
to reach the anchor down *some* path but not necessarily every path---in fact, to do so 
might be impossible. For example: 

.. code-block: daedalus 

  Main = { 
    @x = 8;
    @y = 6;  
    lessThanSeven x; 
    lessThanSeven y; 
  }

  lessThanSeven (i : int) = { 
    Choose1 { 
      { guard i < 7; ^true #ANCHOR }; 
      { guard i >= 7; ^false };  
    }
  }

This function will execute to completion and hit the anchor. But if we mandate
that only the anchor path can be taken, then we won't be able to execute the
program. One answer here is to specialise the function to the callsite but in
general this shows that taking a particular path is a complex property that
might not be something we can determine statically. 

This also presents problems if we run a backwards analysis which tries to 
hit the *first* anchor it finds. 


Reusing Existing Analyses? 
==========================

The current analysis builds slices which represent the entanglement properties
of particular variables---either internal variables, parameters, or return
values. It doesn't slice by control flow, so each slice includes all the
control-flow that is involved in the current program element. This is necessary
because slices may have to be merged, ie. overlayed if two relevant variables
are needed to generate a particular path.

The Analysis module builds these slices working backwards by tracking
dependencies for different variables. The slices are built in a modular way,
with each slice representing the behaviour of a particular function. The slices
can be composed for use in an inter procedural generation. 

One idea: build the set of slices using the current mechanism, and then stitch
them together in a way that generates the correct document. Is it true that
separate slices are independent from a control-flow perspective, i.e. can we
just synthesize an answer for a particular slice that contains the anchor and
then synthesize the rest of the document independently? (This relates to the
question of whether it's possible to parallelise synthesis of independent
slices). 

A simpler idea is to modify the analysis mechanism to mark particular choices
as favored. The algorithm to do this is pretty simple: 

* When encountering an anchor, mark the path as favored. 
* Back propagate the favored property when moving through the program. 
* If you're at a choice point where one path is favored, eliminate the 
  other choices

Then modify the search process to only use favored paths when reaching a choice
node. 

