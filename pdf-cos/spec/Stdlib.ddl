import Pair
import Sum

def Unit = ^{}

def Void = Choose { }

def FoldMany P acc = Choose1 {
  { @acc0 = P acc;
    FoldMany P acc0
  };
  ^ acc
}

def Const P x = P

def boolXor b0 b1 = if b0 then !b1 else b1

def inc n = n + 1
def dec n = n - 1
def max m n = if m > n then m else n
def min m n = if m < n then m else n
def bitIsSet8 (n : uint 8) bs = (n .&. (1 << bs)) != 0
def bitIsSet32 (n : uint 32) bs = (n .&. (1 << bs)) != 0
def setBit bs (n : uint 32) = n .|. (1 << bs)

def numBase base ds       = for (val = 0; d in ds) (val * base + d)
def bytesNum (bs : [ uint 8 ]) : uint 64 =
  for (val = 0 : uint 64; b in bs) 8 * val + (b as uint 64)

def octalTriple a b c = numBase 8 [ a, b, c ]

def Only P                = { $$ = P; END }
def When P x              = { P; ^ x }
def Guard p               = p is true
def Holds P = When P true

def append x y = concat [ x, y ]

def cons x xs = append [ x ] xs

def snoc x xs = append xs [ x ]

def condJust p x = if p then just x else nothing

def OptionalIf P N = case Optional P of {
  just x -> just x
; nothing -> When N nothing
}

def optionToArray x = case x of
  just y -> [ y ]
  nothing -> [ ]

def optionsToArray (xs : [ maybe a ]) : [ a ] = concat
  (map (x in xs) optionToArray x)

-- bounded sequences of bytes:
def OrEatByte P = OptionalIf P UInt8

def Bytes1 (b : uint 8) = {
  only = b
}

def Bytes1P = Bytes1 UInt8

def Bytes2 (high : uint 8) (others : Bytes1) = {
  second = high
; rest1 = others
}

def Bytes2All (high : uint 8) (low : uint 8) = Bytes2 high (Bytes1 low)

def Bytes2P = Bytes2 UInt8 Bytes1P

def Bytes3 (pthird : uint 8) (others : Bytes2) = {
  third = pthird;
  rest2 = others;
}

def Bytes3P = Bytes3 UInt8 Bytes2P

def Bytes4 (pfourth : uint 8) (others : Bytes3) = {
  fourth = pfourth;
  rest3 = others;
}

def Bytes4All (b0 : uint 8) (b1 : uint 8) (b2 : uint 8) (b3 : uint 8) = 
  Bytes4 b0 (Bytes3 b1 (Bytes2 b2 (Bytes1 b3)))

def Bytes4P = Bytes4 UInt8 Bytes3P

def Bytes5 (b0 : uint 8) (others : Bytes4) = {
  byte5 = b0;
  rest4 = others;
}

def Bytes5P = Bytes5 UInt8 Bytes4P

def Bytes6 (b0 : uint 8) (others : Bytes5) = {
  byte6 = b0;
  rest5 = others;
}

def Bytes6P = Bytes6 UInt8 Bytes5P

def Bytes6All (b0 : uint 8) (b1 : uint 8)
  (b2 : uint 8) (b3 : uint 8)
  (b4 : uint 8) (b5 : uint 8) = Bytes6 b0 (Bytes5 b1 (Bytes4All b2 b3 b4 b5))

-- functions for serializing bounded structures into arrays
def bndBytes1 bs1 = [ bs1.only ]

def bndBytes2 bs2 = cons bs2.second (bndBytes1 bs2.rest1)

def bndBytes3 bs3 = cons bs3.third (bndBytes2 bs3.rest2)

def bndBytes4 bs4 = cons bs4.fourth (bndBytes3 bs4.rest3)

def Default x P = P <| ^ x

def GenMany P acc0 =
  { @acc1 = P acc0;
    GenMany P acc1
  } <|
  ^acc0

def ManyWithStateRec P q (res : [ a ]) : Pair = Default (Pair q res) {
  @eff = P q;
  ManyWithStateRec P eff.fst (append res (optionToArray eff.snd))
}

def ManyWithState P q0 = ManyWithStateRec P q0 [ ]

def WithStream s P = {
  @cur = GetStream;
  SetStream s;
  $$ = P;
  SetStream cur;
}

def sepLists2 ls : Pair = {
  fst = optionsToArray (map (x in ls) getLeft x);
  snd = optionsToArray (map (x in ls) getRight x);
}

def Lists2 P0 P1 = {
  @xs = Many (Sum P0 P1);
  sepLists2 xs
}

def lenLists2 ls =
  (length ls.fst) +
  (length ls.snd)

--------------------------------------------------------------------------------
-- White Space (Section 7.2)

def $lf                   = 10
def $cr                   = 13
def $space                = 32
def $simpleWS             = 0 | 9 | 12 | 32

def SimpleEOL             = { $cr; $lf } | $lf
def EOL                   = SimpleEOL <| $cr
def GenComment start = {
  Match (append "%" start);
  Many (Match1 (! ($lf | $cr)));
  EOL }
def Comment               = GenComment ""
def JustWhite             = $simpleWS | EOL

def AnyWS                 = $simpleWS | Comment | EOL

