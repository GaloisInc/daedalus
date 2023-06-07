-- Version 1.1 Reference: https://bsonspec.org/spec.html

import Daedalus
import utf8

def BSON_document =
  block
    let len = LEUInt32 as ?auto
    len >= 5 is true    -- 4 for the length, 1 for the 0
    $$ = Chunk (len - 5) (Many BSON_element)
    $[0]

def BSON_element =
  block
    let tag = UInt8
    name  = BSON_cstring
    value = BSON_value tag

def BSON_value tag =
  case tag of
    0x01 -> {| Double         = LEDouble |}
    0x02 -> {| String         = BSON_string |}
    0x03 -> {| Document       = BSON_document |}
    0x04 -> {| Array          = BSON_document |}
    0x05 -> {| Binary         = BSON_binary |}
    0x06 -> {| Undefined |}
    0x07 -> {| ObjectId       = Many 12 UInt8 |}
    0x08 -> {| Bool           = case UInt8 of
                                  0x00 -> false
                                  0x01 -> true
            |}
    0x09 -> {| Date            = LESInt64 |}
    0x0A -> {| Null |}
    0x0B -> {| RegEx          = BSON_regular_expression |}
    0x0C -> {| DBPointer      = BSON_DBPointer |}
    0x0D -> {| JavaScript     = BSON_string |}
    0x0E -> {| Symbol         = BSON_string |}
    0x0F -> {| JavaScriptWS   = BSON_code_w_s |}
    0x10 -> {| Int32          = LESInt32 |}
    0x11 -> {| TimeStamp      = LEUInt64 |}
    0x12 -> {| Int64          = LESInt64 |}
    0x13 -> {| Decimal128     = Many 16 UInt8 |} -- Unchecked
    0xFF -> {| MinKey |}
    0x7F -> {| MaxKey |}
    _    -> Fail "Malformed tag"

def BSON_regular_expression =
  block
    pattern = BSON_cstring
    options = BSON_cstring

def BSON_DBPointer =
  block
    field1 = BSON_string
    field2 = Many 12 UInt8

def BSON_string =
  block
    let len = LEUInt32 as ?auto
    len > 0 is true   -- 0 terminator
    $$ = Chunk (len - 1) (Only (Many UTF8))
    $[0]

def BSON_cstring =
  block
    let res =
         many (s = { buf = builder, done = false })
           block
             s.done is false
             let c = UTF8
             case c of
               0 -> { buf = s.buf; done = true }
               _ -> { buf = emit s.buf c; done = false }
    res.done is true <| Fail "Malformed cstring"
    build res.buf

def BSON_binary =
  BSON_chunk_prefix
    block
      tag  = UInt8 as? BSON_binary_subtype
      data = GetStream

bitdata BSON_binary_subtype where
  Generic         = 0x00 : uint 8
  Function        = 0x01
  Binary_old      = 0x02
  UUID_old        = 0x03
  UUID            = 0x04
  MD5             = 0x05
  EncryptedBSON   = 0x06
  CompressedBSON  = 0x07
  UserDefined     = 0x80

def BSON_code_w_s =
  BSON_chunk
    block
      field1 = BSON_string
      field2 = BSON_document

def BSON_chunk P =
  BSON_chunk_prefix (Only P)

def BSON_chunk_prefix P =
  Chunk (LEUInt32 as ?auto) P


