import Daedalus
import Debug
import Common
import Message01
import Message200

def Main =
    block
        Message

def Message =
    block
        let start         = GetStream
        let startOffset   = Offset
        iddVersion        = IDDVersion
        msgInstanceID     = BEUInt32
        msgType           = BEUInt32
        msgLength         = NumericParserWithRange BEUInt32 1 514
        streamID          = BEUInt32
        messageProperties = BEUInt32 as? MessageProperties
        message           = MessageData msgType (msgLength as uint 64)
        let cstream       = Take (Offset - startOffset) start
        checksum          = Checksum cstream
        

-- Parse the IDD Version string
-- def IDDVersion = Chunk 10 NTString
def IDDVersion = Chunk 10 { $$ = Match "12" ; Match [0x0] }

-- Parse a null-terminated string
def NTString =
  block
    let res =
         many (s = { buf = builder, done = false })
           block
             s.done is false
             let c = UInt8
             case c of
               0 -> { buf = s.buf; done = true }
               _ -> { buf = emit s.buf c; done = false }
    res.done is true <| Fail "Malformed cstring"
    build res.buf

-- Parse message data as per the specified type in the wrapper
def MessageData ( type : uint 32 ) (len : uint 64 ) =
    case type of
        1   -> {| message01 = Chunk len (Only Message01) |}
        200 -> {| message200 = Chunk len (Only Message200) |}

-- Read the checksum and verify that the computed checksum over the stream
-- is same as what we read.
def Checksum cstream =
    block
        -- Read embedded checksum
        let embeddedChecksum = BEUInt32
        
        -- Checksums are computed by summing the bytes of the stream
        -- Compute and check
        let calculatedChecksum = calculateChecksum cstream
        GuardMsg ( embeddedChecksum ==  calculatedChecksum ) "Checksum failed to match"
        
        -- Return the checksum
        ^ embeddedChecksum


-- Compute the checksum over a stream of bytes
-- TODO: Verify that truncation behavior is as expected
def calculateChecksum s =
    block
        let byteArray = bytesOfStream s
        for (val = 0 : uint 32; v in byteArray)
          val + (v as uint 32)

bitdata AckOrNot where
    noAck = 0x0 : uint 1
    doAck = 0x1 : uint 1

bitdata MessageProperties where
    MessageProperties = { ack : AckOrNot, reserved : uint 31 }