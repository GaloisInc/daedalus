import Daedalus

-- | Extract a UTF-8 encoded code point
def UTF8 : uint 32 =
  case $any as? UTF8_Lead of
    U1 x -> x.value as uint 32
    U2 x ->
      block
        $$ = UTF8_next x.value as uint 32
        0x80 <= $$ is true
    U3 x ->
      block
        $$ = UTF8_next (UTF8_next x.value) as uint 32
        0x800 <= $$ && !(0xD800 <= $$ && $$ <= 0xDFFF) is true
    U4 x ->
      block
        $$ = UTF8_next (UTF8_next (UTF8_next x.value)) as uint 32
        (0x1_0000 <= $$ && $$ <= 0x10_FFFF) is true

-- | Emit a UTF-8 encoded code point
def EmitUTF8 b (x : uint 32) =
  if (0xD800 <= x && x <= 0xDFF) || x > 0x10_FFFF
    then Fail "EmitUTF8: Invalid code point"
    else

      First

        UTF8_emitLead b {| U1 = { value = x as? ?l1 } |}

        block
          let b1 = UTF8_emitLead b {| U2 = { value = x >> 6 as? ?l2 } |}
          UTF8_emitFollow b1 0 x

        block
          let b1  = UTF8_emitLead   b {| U3 = { value = x >> 12 as? ?l3 } |}
          let b2  = UTF8_emitFollow b1 1 x
          UTF8_emitFollow b2 0 x

        block
          let b1 = UTF8_emitLead b {| U4 = { value = x >> 18 as? ?l4 } |}
          let b2 = UTF8_emitFollow b1 12 x
          let b3 = UTF8_emitFollow b2 6 x
          UTF8_emitFollow b3 0 x



--------------------------------------------------------------------------------
-- Helpers

bitdata UTF8_Lead where
  U1 = { 0b0;     value : uint 7 }
  U2 = { 0b110;   value : uint 5 }
  U3 = { 0b1110;  value : uint 4 }
  U4 = { 0b11110; value : uint 3 }

bitdata UTF8_Follow where
  0b10
  value : uint 6

-- Parse following byte
def UTF8_next x = x # ($any as? UTF8_Follow).value

-- Emit leading byte
def UTF8_emitLead b (x : UTF8_Lead) = emit b (x as uint 8)

-- Emit following byte
def UTF8_emitFollow b n (x : uint 32) =
  emit b (({ value = x >> n as! ?f } : UTF8_Follow) as uint 8)



--------------------------------------------------------------------------------
-- Tesets


def Test =
  block
    let bytes = "\208\183\208\180\209\128\208\176\208\178\208\181\208\185"
    let text  = [ 1079, 1076, 1088, 1072, 1074, 1077, 1081 ]
    TestParses (Many UTF8) bytes text
    TestParses (build (for (b = builder; y in text) (EmitUTF8 b y))) "" bytes
    TestFails UTF8 [ 0b1110_0000, 0 ]


