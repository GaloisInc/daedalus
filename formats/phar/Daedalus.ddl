--------------------------------------------------------------------------------
-- Parsing Basic Types

def joinWords a b = if ?bigEndian then a # b else b # a

def UInt16        = joinWords UInt8 UInt8
def UInt32        = joinWords UInt16 UInt16
def UInt64        = joinWords UInt32 UInt32

def SInt16        = UInt16 as! sint 16
def SInt32        = UInt32 as! sint 32
def SInt64        = UInt64 as! sint 64

def HalfFloat     = wordToHalfFloat UInt16
def Float         = wordToFloat UInt32
def Double        = wordToDouble UInt64

def BEUInt16      = block let ?bigEndian = true; UInt16
def BEUInt32      = block let ?bigEndian = true; UInt32
def BEUInt64      = block let ?bigEndian = true; UInt64

def BESInt16      = block let ?bigEndian = true; SInt16
def BESInt32      = block let ?bigEndian = true; SInt32
def BESInt64      = block let ?bigEndian = true; SInt64

def BEHalfFloat   = block let ?bigEndian = true; HalfFloat
def BEFloat       = block let ?bigEndian = true; Float
def BEDouble      = block let ?bigEndian = true; Double

def LEUInt16      = block let ?bigEndian = false; UInt16
def LEUInt32      = block let ?bigEndian = false; UInt32
def LEUInt64      = block let ?bigEndian = false; UInt64

def LESInt16      = block let ?bigEndian = false; SInt16
def LESInt32      = block let ?bigEndian = false; SInt32
def LESInt64      = block let ?bigEndian = false; SInt64

def LEHalfFloat   = block let ?bigEndian = false; HalfFloat
def LEFloat       = block let ?bigEndian = false; Float
def LEDouble      = block let ?bigEndian = false; Double

def WS = $[0 | 9 | 12 | 32 | '\n' | '\r']

-- See: https://fgiesen.wordpress.com/2012/03/28/half-to-float-done-quic/
-- for an explanation of what's going on here.
def wordToHalfFloat (w : uint 16) =
  block
    let sign = w >> 15 as! uint 1
    let expo = w >> 10 as! uint 5
    let mant = w       as! uint 10
    if expo == 0
      then
        block
          let magic = 126 << 23 : uint 32
          let num   = wordToFloat (magic + (0 # mant)) - wordToFloat magic
          if sign == 1 then -num else num
      else
        block
         let newExp =
                if expo == 0x1F then 0xFF else 127 - 15 + (0 # expo) : uint 8
         wordToFloat (sign # newExp # mant # 0)

--------------------------------------------------------------------------------

-- Succeed if the condition holds or fail otherwise.
def Guard b       = b is true
def GuardMsg p s  = if p then Accept
                         else Fail (concat ["Guard failed: ", s])


-- Parse with `P` and return result `x`
def When P x      = block P; x

-- Parse with `P` but return `x` if `P` fails.
def Default x P   = P <| x



-- Succeed if `P` consumes all input.
def Only P        = P


-- Evaluate a sequence of digits in the given base
def numBase base ds = for (val = 0; d in ds) (val * base + d)

--------------------------------------------------------------------------------

-- Compute the smaller value
def min x y = if x < y then x else y

-- Compute the larger value
def max x y = if x < y then y else x

-- Skip some occurances of the given parser, and return how many
-- were skipped.
def Count P : uint 64 = many (count = 0) { P; count + 1 }

-- Match a P, followed by some Qs
def ManyStart P Q = build (many (buf = emit builder P) (emit buf Q))

-- Match some Ps up to (and including) a Q
def UpTo P Q =
  First
    Q
    build (emitArray (emit builder P) (UpTo P Q))
