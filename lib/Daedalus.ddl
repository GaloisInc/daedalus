--------------------------------------------------------------------------------
-- Parsing Basic Types

def joinWords a b = if ?bigEndian then a # b else b # a

def UInt16        = joinWords UInt8 UInt8
def UInt32        = joinWords UInt16 UInt16
def UInt64        = joinWords UInt32 UInt32
def Float         = wordToFloat UInt32
def Double        = wordToDouble UInt64

def BE16          = block let ?bigEndian = true; UInt16
def BE32          = block let ?bigEndian = true; UInt32
def BE64          = block let ?bigEndian = true; UInt64
def BEFloat       = block let ?bigEndian = true; Float
def BEDouble      = block let ?bigEndian = true; Double

def LE16          = block let ?bigEndian = false; UInt16
def LE32          = block let ?bigEndian = false; UInt32
def LE64          = block let ?bigEndian = false; UInt64
def LEFloat       = block let ?bigEndian = false; Float
def LEDouble      = block let ?bigEndian = false; Double



--------------------------------------------------------------------------------

-- Succeed if the condition holds or fail otherwise.
def Guard b       = b is true

-- Succeed if `P` consumes all input.
def Only P        = block $$ = P; END


--------------------------------------------------------------------------------
-- Stream Manipulation

-- Set the stream to the `n`-th byte of `s`.
def SetStreamAt n s = SetStream (Drop n s)

-- Advance the current stream by `n` bytes.
def Skip n          = SetStreamAt n GetStream

-- Parse the following `n` bytes using `P`.
-- Note that `P` does not have to consume all of the bytes,
-- but we still advance by `n`.  Use `Only` if you want to ensure
-- that `P` consumes the whole input.
def Chunk n P =
  block
    let s = GetStream
    SetStream (Take n s)
    $$ = P
    SetStreamAt n s

-- Get a chunk of unprocessed bytes.
def Bytes n = Chunk n GetStream

-- Parse using `P` but reset the stream to the current position on success.
def LookAhead P =
  block
    let s = GetStream
    $$ = P
    SetStream s






