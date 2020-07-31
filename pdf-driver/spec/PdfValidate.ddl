import PdfDecl
import PdfValue

{-
Primitives for Validating References
====================================

The object graph in a PDF graph may contain cycles.  For example, a node
in a tree points to its children, and the children point back to the parent.
To avoid getting stuck in such cycles during validaiton, we ensure that
each node is validated at most once for each given property name.

This is implemented using the two primitves below.
-}

-- Check if the given reference has been (or is being) validated for
-- the given property name.
def IsValidated (obj : int) (gen : int) (ty : [uint 8]) : bool

-- Mark the given reference as validated for the given type name.
def StartValidating (obj : int) (gen : int) (ty : [uint 8]) : {}



--------------------------------------------------------------------------------

-- Check that a value matches the property named `ty` and described by `P`.
-- References are transparently dereferenced.
def CheckValue ty P (v : Value) =
  try (
       { @r = v is ref; commit; CheckRef ty P r }
    <| P v
  )

-- Check that the this reference points to a value, which satisfy property
-- named `ty`, described by `P.
def CheckRef ty P (r : Ref) =
  try (Default {} {
    @done = IsValidated r.gen r.obj ty;
    done is false;
    commit;
    StartValidating r.gen r.obj ty;
    @v = ResolveValRef r;
    P v;
  })


def CheckInteger (v : Value) : int =
  try { @n = v is number; commit; n.exp == 0; ^ n.num }
  -- XXX: +ve exponents are OK, as are -ve if number is big enough

-- XXX: more checking
def CheckDate (v : Value) : [uint 8] =
  { v is string }

-- XXX: more checking
def CheckText (v : Value) : [uint 8] =
  { v is string }

-- XXX: more checking?
def CheckASCII (v : Value) : [uint 8] =
  { v is string }

-- XXX
def CheckRectangle (v : Value) : {} =
  {}

-- XXX 
def EqNumber (x : Number) (y : Number) : {} =
  { x.num == y.num; x.exp == y.exp }



