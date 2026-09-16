import Daedalus
import PdfValue


--------------------------------------------------------------------------------
-- ASCIIHexDecode filter

def ASCIIHexDecode (body : stream) : stream =
  WithStream body
    block
      let result =
        many (output = builder)
          (emit output ASCIIHexByte)
      let result = emit result ASCIIHexLast <| result
      Many JustWhite
      Match ">"
      arrayStream (build result)

-- ISO 32000-2:2017 section 7.4.2 specifies that all PDF white-space
-- characters shall be ignored within ASCII hexadecimal data.
def ASCIIHexDigit =
  block
    Many JustWhite
    HexDigit

def ASCIIHexByte = numBase 16 (Many 2 ASCIIHexDigit) as! uint 8
def ASCIIHexLast = 16 * ASCIIHexDigit as! uint 8


--------------------------------------------------------------------------------
-- ASCII85Decode filter

def ASCII85Decode (body : stream) : stream =
  WithStream body
    block
      let result =
        many (state = ASCII85State builder 0 0 false)
          (ASCII85Step state)
      if result.done
        then arrayStream (build result.output)
        else Fail "ASCII85 stream is missing its end-of-data marker"

-- ISO 32000-2:2017 section 7.4.3 specifies the ASCII base-85 encoding.
-- PDF white-space characters may occur between digits and shall be ignored.
def ASCII85Step state =
  if state.done
    then Fail "" -- stop
    else
      block
        Many JustWhite
        let char = UInt8
        case char of
          'z' ->
            block
              state.count == 0 is true
              let output = ASCII85Emit state.output (0 : uint 32) 4
              ASCII85State output 0 0 false
          '~' ->
            block
              Match ">"
              ASCII85Finish state
          _ ->
            block
              (char >= '!' && char <= 'u') is true
              let value = state.value * 85 + (char - '!' as uint 64)
              let count = state.count + 1
              if count == 5
                then
                  block
                    let output = ASCII85Emit state.output (value as! uint 32) 4
                    ASCII85State output 0 0 false
                else
                  ASCII85State state.output value count false

def ASCII85Finish state =
  if state.count == 0
    then ASCII85State state.output 0 0 true
    else
      block
        (state.count >= 2 && state.count <= 4) is true
        let value =
          for (value = state.value; i in rangeUp (5 - state.count))
            value * 85 + 84
        let output =
          ASCII85Emit state.output (value as! uint 32) (state.count - 1)
        ASCII85State output 0 0 true

def ASCII85State output (value : uint 64) (count : uint 8) done =
  { output = output, value = value, count = count, done = done }

def ASCII85Emit output (value : uint 32) (count : uint 8) =
  for (output = output; byte in rangeUp count)
    emit output ((value >> (24 - 8 * (byte as uint 64))) as! uint 8)
