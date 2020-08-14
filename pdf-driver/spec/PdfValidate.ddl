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
def CheckValue ty P (v : Value) = try {
     { @r = v is ref; commit; CheckRef ty P r }
  <| P v
}

-- Check that the this reference points to a value, which satisfy property
-- named `ty`, described by `P.
def CheckRef ty P (r : Ref) = try {
  Default {} {
    @done = IsValidated r.gen r.obj ty;
    done is false;
    commit;
    StartValidating r.gen r.obj ty;
    @v = ResolveValRef r;
    P v;
  }
}


def Any v       = {}
def Is x y      = Guard (x == y)
def AtLeast x y = Guard (x <= y)


def PdfName P (v : Value) = {
  @x = v is name;
  P x;
}

def PdfType d expect = PdfName (Is expect) (Lookup "Type" d)

def PdfIndirect ty P (v : Value) = {
  @r = v is ref;
  CheckRef ty P r;
}



{- Match integer values that satisfy the given predicate.
You may use `Integer Any` if any integer would do.

Note that this only matches values that were written without a decimal
point in the PDF.  The reason for that is we represent
`1.00` as `{ num = 100, exp = -2 }` see `PDFValue` for details. -}
def PdfInteger (v : Value) : int = {
  @n = v is number;
  Guard (n.exp == 0);
  ^ n.num;
} <| Fail "Value not an integer."

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
def EqNumber (x : Number) (y : Number) : {} =
  Guard (x.num == y.num && x.exp == y.exp)



