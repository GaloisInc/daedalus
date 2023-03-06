-- HTTP 2 frame parser
--
-- Unsupported features:
-- * Protocol extensions (https://www.rfc-editor.org/rfc/rfc9113#section-5.5)

-- Frame header:
-- https://www.rfc-editor.org/rfc/rfc9113#name-frame-format
def HTTP2_frame =
  block
    -- Length (24 bits)
    len = UInt24

    -- Type (8 bits) and flags (8 bits)
    type = Frame_Type

    -- Reserved (1 bit)
    -- Stream Identifier (31 bits)
    --
    -- If we parse the next 32 bits as a unit, then the top bit is the
    -- Reserved bit which we ignore here.
    --
    -- https://www.rfc-editor.org/rfc/rfc9113#section-4.1-4.8.1
    let packed_ident = UInt32
    stream_identifier = packed_ident as! uint 31

    body = HTTP2_frame_body len type

def Data_Frame_Body_s =
  struct
    body: [uint 8]
    padding: uint 8

def Goaway_Frame_Body_s =
  struct
    last_stream_id: uint 31
    error_code: Error_Code
    debug_data: [uint 8]

def HTTP2_frame_body_u =
  union
    Data_Frame_Body: Data_Frame_Body_s
    Ping_Frame_Body: [uint 8]
    Goaway_Frame_Body: Goaway_Frame_Body_s

def HTTP2_frame_body (len: uint 24) (ty: Frame_Type): HTTP2_frame_body_u =
  case ty of
    F_DATA info ->
      block
        -- Optional padding field: if the field is present in the flags,
        -- we consume it now.
        let padding_amt = case info.flags of
                            Flags fs -> if fs.padded == 1
                                          then UInt8
                                          else ^ 0

        -- The data frame payload is the total frame length (len) minus
        -- the padding field byte (if any) minus the padding bytes
        -- themselves (if the padding field was present).
        let padding_byte = case info.flags of
                             Flags fs -> if fs.padded == 1
                                           then 1
                                           else 0
        let data_len = (len as uint 64) - (padding_amt as uint 64) - padding_byte

        -- Read the data frame body.
        let body = Many (data_len as! uint 64) $any
        $$ = {| Data_Frame_Body = { body = body, padding = padding_amt } |}

        -- Now consume and discard the padding bytes.
        Many (padding_amt as uint 64) $any

    F_PING info ->
      block
        -- Note that we ignore the length here, as specified. It is up
        -- to the application to respond with an error if the length is
        -- not 8.
        --
        -- https://www.rfc-editor.org/rfc/rfc9113#section-6.7-9
        let opaque_data = Many 8 $any
        ^ {| Ping_Frame_Body = opaque_data |}

    F_GOAWAY ->
      block
        -- https://www.rfc-editor.org/rfc/rfc9113#name-goaway-frame-format
        -- Reserved (1),
        -- Last-Stream-ID (31),
        -- Error Code (32),
        -- Additional Debug Data (..),
        let packed_last_stream_id = UInt32
        let last_stream_id = packed_last_stream_id as! uint 31
        let error_code = Error_Code
        -- Subtract stream ID and error code bytes from frame length to
        -- get debug data length
        let debug_data_len = len - 8
        let debug_data = Many (debug_data_len as uint 64) $any
        ^ {| Goaway_Frame_Body = { last_stream_id = last_stream_id,
                                   error_code = error_code,
                                   debug_data = debug_data } |}

-- Data frame flags:
-- Unused Flags (4)
-- PADDED Flag (1)
-- Unused Flags (2)
-- END_STREAM Flag (1)
--
-- https://www.rfc-editor.org/rfc/rfc9113#name-data
bitdata Data_Frame_Flags where
  Flags = { unused1: uint 4,
            padded: uint 1,
            unused2: uint 2,
            end_stream: uint 1
          }

-- Ping frame flags:
-- Unused Flags (7)
-- ACK Flag (1)
--
-- https://www.rfc-editor.org/rfc/rfc9113#name-ping
bitdata Ping_Frame_Flags where
  Flags = { unused: uint 7,
            ack: uint 1
          }

-- Frame types
-- https://www.rfc-editor.org/rfc/rfc9113#name-frame-definitions
def Frame_Type =
  First
    F_DATA = block
      -- https://www.rfc-editor.org/rfc/rfc9113#name-data
      -- Frame type: data
      $[0x00]
      -- Flags:
      flags = UInt8 as? Data_Frame_Flags

    -- F_HEADERS = $[0x01]
    -- F_PRIORITY = $[0x02]
    -- F_RST_STREAM = $[0x03]
    -- F_SETTINGS = $[0x04]
    -- F_PUSH_PROMISE = $[0x05]

    F_PING = block
      -- https://www.rfc-editor.org/rfc/rfc9113#name-ping
      -- Frame type: ping
      $[0x06]
      -- Flags:
      flags = UInt8 as? Ping_Frame_Flags

    F_GOAWAY = block
      -- https://www.rfc-editor.org/rfc/rfc9113#name-goaway
      -- Frame type: goaway
      $[0x07]
      -- Flags: no flags specified by the GOAWAY frame
      @UInt8

    -- F_WINDOW_UPDATE = $[0x08]
    -- F_CONTINUATION = $[0x09]

-- GOAWAY / RST_STREAM error codes
-- https://www.rfc-editor.org/rfc/rfc9113#name-error-codes
def Error_Code =
  First
    NO_ERROR            = @Match [0x0, 0x0, 0x0, 0x00]
    PROTOCOL_ERROR      = @Match [0x0, 0x0, 0x0, 0x01]
    INTERNAL_ERROR      = @Match [0x0, 0x0, 0x0, 0x02]
    FLOW_CONTROL_ERROR  = @Match [0x0, 0x0, 0x0, 0x03]
    SETTINGS_TIMEOUT    = @Match [0x0, 0x0, 0x0, 0x04]
    STREAM_CLOSED       = @Match [0x0, 0x0, 0x0, 0x05]
    FRAME_SIZE_ERROR    = @Match [0x0, 0x0, 0x0, 0x06]
    REFUSED_STREAM      = @Match [0x0, 0x0, 0x0, 0x07]
    CANCEL              = @Match [0x0, 0x0, 0x0, 0x08]
    COMPRESSION_ERROR   = @Match [0x0, 0x0, 0x0, 0x09]
    CONNECT_ERROR       = @Match [0x0, 0x0, 0x0, 0x0a]
    ENHANCE_YOUR_CALM   = @Match [0x0, 0x0, 0x0, 0x0b]
    INADEQUATE_SECURITY = @Match [0x0, 0x0, 0x0, 0x0c]
    HTTP_1_1_REQUIRED   = @Match [0x0, 0x0, 0x0, 0x0d]

    -- This fall-through case is specified as a possibility that should
    -- trigger no special behavior. We have this case here to be
    -- permissive and indicate clearly that the parsed error code is not
    -- defined.
    --
    -- https://www.rfc-editor.org/rfc/rfc9113#section-7-5
    UNKNOWN_ERROR = Many 4 $any

-- DATA frame flags
-- https://www.rfc-editor.org/rfc/rfc9113#section-6.1-6

-- HEADERS frame flags
-- https://www.rfc-editor.org/rfc/rfc9113#name-headers

-- Defined settings identifiers
-- https://www.rfc-editor.org/rfc/rfc9113#name-defined-settings
def Settings_Identifier =
  First
    SETTINGS_HEADER_TABLE_SIZE      = $[0x01] as uint 16
    SETTINGS_ENABLE_PUSH            = $[0x02] as uint 16
    SETTINGS_MAX_CONCURRENT_STREAMS = $[0x03] as uint 16
    SETTINGS_INITIAL_WINDOW_SIZE    = $[0x04] as uint 16
    SETTINGS_MAX_FRAME_SIZE         = $[0x05] as uint 16
    SETTINGS_MAX_HEADER_LIST_SIZE   = $[0x06] as uint 16

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

def UInt24: uint 24 =
  block
    let b0 = UInt8
    let b1 = UInt8
    let b2 = UInt8
    ^ (b0 # b1 # b2)

def UInt32: uint 32 =
  block
    let b0 = UInt8
    let b1 = UInt8
    let b2 = UInt8
    let b3 = UInt8
    ^ (b0 # b1 # b2 # b3)