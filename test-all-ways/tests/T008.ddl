def Main =
  case UInt8 of
    'F' -> ThrowF 4 255
    'T' -> TailThrowF 4 255
    'N' -> Normal
    


def Normal : uint 8 =
  block
    SetStream (arrayStream [40])
    let depth = UInt8
    END

    let counted = countDown depth
    (counted == depth) is true
    (countTail depth 0 == depth) is true
    (isEven depth) is true
    (isOdd (depth + 1)) is true

    -- Exercise a non-tail parser call within a mutually recursive group.
    -- Both functions are called from outside the group, so both need wrappers.
    SetStream (arrayStream "((((x))))")
    let nestedF = NestF 4
    (nestedF == 'x') is true
    END

    SetStream (arrayStream "((((x))))")
    let nestedG = NestG 4
    (nestedG == 'x') is true
    END

    -- Exercise parser failure through a continuation frame.  The input saved
    -- by the success and failure continuations is the same VM variable.
    SetStream (arrayStream "((y))")
    let nestedFailure = Optional (NestF 2)
    nestedFailure is nothing

    -- Exercise a tail call which leaves a recursive group.
    SetStream (arrayStream "z")
    let exitSuccess = ExitF 4
    (exitSuccess == 'z') is true
    END

    SetStream (arrayStream "y")
    let exitFailure = Optional (ExitF 4)
    exitFailure is nothing

    counted


-- Non-tail pure recursion.
def countDown (n : uint 8) =
  if n == 0
    then 0
    else 1 + countDown (n - 1)

-- Direct self tail recursion.  This should use the same recursive-group
-- worker as a group containing multiple mutually recursive functions.
def countTail (n : uint 8) (acc : uint 8) =
  if n == 0
    then acc
    else countTail (n - 1) (acc + 1)


-- Tail-recursive mutual pure functions.  Main calls both members, so both
-- need ordinary wrappers.
def isEven (n : uint 8) =
  if n == 0 then true else isOdd (n - 1)

def isOdd (n : uint 8) =
  if n == 0 then false else isEven (n - 1)


-- NestF makes a non-tail call to NestG because it must still parse the closing
-- parenthesis.  NestG tail-calls NestF.
def NestF (n : uint 8) : uint 8 =
  if n == 0
    then $['x']
    else block
           $['(']
           $$ = NestG (n - 1)
           $[')']

def NestG (n : uint 8) : uint 8 =
  NestF n


-- ThrowF has continuation frames on the explicit stack when the exception
-- occurs.  ThrowG is an internal-only entry to the group.
def ThrowF (n : uint 8) (x : uint 8) : uint 8 =
  if n == 0
    then x + 1
    else ThrowG (n - 1) x + 1

def ThrowG (n : uint 8) (x : uint 8) : uint 8 =
  ThrowF n x


-- This group has no non-tail calls, so its frame type contains only the root
-- result frame.
def TailThrowF (n : uint 8) (x : uint 8) : uint 8 =
  if n == 0
    then x + 1
    else TailThrowG (n - 1) x

def TailThrowG (n : uint 8) (x : uint 8) : uint 8 =
  TailThrowF n x


-- ExitF and ExitG recurse mutually, then tail-call a parser outside their SCC.
def ExitF (n : uint 8) : uint 8 =
  if n == 0 then ExitLeaf else ExitG (n - 1)

def ExitG (n : uint 8) : uint 8 =
  ExitF n

def ExitLeaf = $['z']
