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

def Rst_Stream_Frame_Body_s =
  struct
    error_code: Error_Code

def Priority_Frame_Body_s =
  struct
    exclusive: uint 1
    stream_dependency: uint 31
    weight: uint 8

def Window_Update_Frame_Body_s =
  struct
    window_size_increment: uint 31

def Continuation_Frame_Body_s =
  struct
    field_block_fragment: [uint 8]

def Push_Promise_Frame_Body_s =
  struct
    field_block_fragment: [uint 8]
    padding: uint 8
    promised_stream_id: uint 31

def HTTP2_frame_body_u =
  union
    Data_Frame_Body: Data_Frame_Body_s
    Ping_Frame_Body: [uint 8]
    Rst_Stream_Frame_Body: Rst_Stream_Frame_Body_s
    Priority_Frame_Body: Priority_Frame_Body_s
    Goaway_Frame_Body: Goaway_Frame_Body_s
    Window_Update_Frame_Body: Window_Update_Frame_Body_s
    Settings_Frame_Body: [Setting_s]
    Continuation_Frame_Body: Continuation_Frame_Body_s
    Push_Promise_Frame_Body: Push_Promise_Frame_Body_s

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

    F_PUSH_PROMISE info ->
      block
        -- https://www.rfc-editor.org/rfc/rfc9113#name-push_promise-frame-format
        --
        -- Optional padding field: if the field is present in the flags,
        -- we consume it now.
        let padding_amt = case info.flags of
                            Flags fs -> if fs.padded == 1
                                          then UInt8
                                          else ^ 0

        -- Next, parse:
        -- Reserved (1),
        -- Promised Stream ID (31),
        let packed_stream_id = UInt32
        let promised_stream_id = packed_stream_id as! uint 31

        -- The field block fragment size is the total frame length (len)
        -- minus the padding field byte (if any) minus the promised
        -- stream ID (4 bytes) and padding bytes themselves (if the
        -- padding field was present).
        let padding_byte = case info.flags of
                             Flags fs -> if fs.padded == 1
                                           then 1
                                           else 0
        let field_block_len = (len as uint 64) - padding_byte - 4 - (padding_amt as uint 64)

        -- Read the field block fragment.
        let frag = Many field_block_len $any
        $$ = {| Push_Promise_Frame_Body = { field_block_fragment = frag,
                                            padding = padding_amt,
                                            promised_stream_id = promised_stream_id } |}

        -- Now consume and discard the padding bytes.
        Many (padding_amt as uint 64) $any

    F_SETTINGS ->
      block
        -- In a settings frame, each of the (zero or more) settings is
        -- made of a two-byte identifier followed by a four-byte value.
        -- We require that the frame body length be exactly a multiple
        -- of this size.
        len % 6 == 0 is true

        let num_settings = (len as uint 64) / 6
        let rawSettings = Many num_settings Setting
        let settings = catMaybes rawSettings
        ^ {| Settings_Frame_Body = settings |}

    F_RST_STREAM ->
      block
        -- https://www.rfc-editor.org/rfc/rfc9113#name-rst_stream-frame-format
        -- Error Code (32)
        --
        -- Note that as with F_PING, we do not check the length here
        -- since that is an application concern. We only guard against
        -- it being too small to avoid parsing bytes ambiguously.
        --
        -- https://www.rfc-editor.org/rfc/rfc9113#section-6.4-8
        len >= 4 is true
        let error_code = Error_Code
        ^ {| Rst_Stream_Frame_Body = { error_code = error_code } |}

    F_PRIORITY ->
      block
        -- https://www.rfc-editor.org/rfc/rfc9113#name-priority-frame-format
        -- Exclusive (1),
        -- Stream Dependency (31),
        -- Weight (8),
        --
        -- Note that as with F_PING, we do not check the length here
        -- since that is an application concern. We only guard against
        -- it being too small to avoid parsing bytes ambiguously.
        len >= 5 is true

        let info = UInt32 as? Priority_info
        let weight = $any
        case info of
          Info dat ->
            ^ {| Priority_Frame_Body = { exclusive = dat.exclusive,
                                         stream_dependency = dat.stream_dependency,
                                         weight = weight } |}

    F_CONTINUATION ->
      block
        -- https://www.rfc-editor.org/rfc/rfc9113#name-continuation-frame-format
        let frag = Many (len as uint 64) $any
        ^ {| Continuation_Frame_Body = { field_block_fragment = frag } |}

    F_WINDOW_UPDATE ->
      block
        -- https://www.rfc-editor.org/rfc/rfc9113#name-window_update-frame-format
        -- Reserved (1),
        -- Window size increment (31),
        --
        -- Note that as with F_PING, we do not check the length here
        -- since that is an application concern. We only guard against
        -- it being too small to avoid parsing bytes ambiguously.
        len >= 4 is true

        let packed_increment = UInt32
        let increment = packed_increment as! uint 31

        ^ {| Window_Update_Frame_Body = { window_size_increment = increment } |}

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

-- Setting parses a setting if the setting's identifier is known. In
-- that case 'just' is returned. Otherwise 'nothing' is returned. This
-- silent dropping of unrecognized settings, rather than an explicit
-- capture for the application to inspect, is done because the spec says
-- we MUST ignore settings that we don't know about:
--
-- https://www.rfc-editor.org/rfc/rfc9113#section-6.5.2-3
def Setting = Maybe Setting_s

def Setting_s =
  block
    identifier = Settings_Identifier
    value = UInt32

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

bitdata Priority_info where
  Info = { exclusive: uint 1,
           stream_dependency: uint 31
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

-- Settings frame flags:
-- Unused Flags (7)
-- ACK Flag (1)
--
-- https://www.rfc-editor.org/rfc/rfc9113#name-settings-format
bitdata Settings_Frame_Flags where
  Flags = { unused: uint 7,
            ack: uint 1
          }

-- Continuation frame flags
-- Unused Flags (5),
-- END_HEADERS Flag (1),
-- Unused Flags (2),
bitdata Continuation_Frame_Flags where
  Flags = { unused1: uint 5,
            end_headers: uint 1,
            unused2: uint 2
          }

-- Push promise frame flags
--
-- Unused Flags (4),
-- PADDED Flag (1),
-- END_HEADERS Flag (1),
-- Unused Flags (2),
--
-- https://www.rfc-editor.org/rfc/rfc9113#name-push_promise-frame-format
bitdata Push_Promise_Frame_Flags where
  Flags = { unused1: uint 4,
            padded: uint 1,
            end_headers: uint 1,
            unused2: uint 2
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

    F_PRIORITY = block
      -- https://www.rfc-editor.org/rfc/rfc9113#name-priority
      -- Frame type: PRIORITY
      $[0x02]
      -- Flags: no flags specified by the PRIORITY frame
      @UInt8

    F_RST_STREAM = block
      -- https://www.rfc-editor.org/rfc/rfc9113#name-rst_stream
      -- Frame type: RST_STREAM
      $[0x03]
      -- Flags: no flags specified by the RST_STREAM frame
      @UInt8

    F_SETTINGS = block
      -- https://www.rfc-editor.org/rfc/rfc9113#name-settings-format
      -- Frame type: settings
      $[0x04]
      -- Flags:
      flags = UInt8 as? Settings_Frame_Flags

    F_PUSH_PROMISE = block
      -- https://www.rfc-editor.org/rfc/rfc9113#name-push_promise
      -- Frame type: push promise
      $[0x05]
      -- Flags:
      flags = UInt8 as? Push_Promise_Frame_Flags

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

    F_WINDOW_UPDATE = block
      -- https://www.rfc-editor.org/rfc/rfc9113#name-window_update
      -- Frame type: window update
      $[0x08]
      -- Flags: no flags specified by the window update frame
      @UInt8

    F_CONTINUATION = block
      -- https://www.rfc-editor.org/rfc/rfc9113#name-continuation
      -- Frame type: continuation
      $[0x09]
      -- Flags: continuation frame flags
      flags = UInt8 as? Continuation_Frame_Flags

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

-- HEADERS frame flags
-- https://www.rfc-editor.org/rfc/rfc9113#name-headers

-- Defined settings identifiers
-- https://www.rfc-editor.org/rfc/rfc9113#name-defined-settings
def Settings_Identifier =
  First
    SETTINGS_HEADER_TABLE_SIZE      = @Match [0x0, 0x01]
    SETTINGS_ENABLE_PUSH            = @Match [0x0, 0x02]
    SETTINGS_MAX_CONCURRENT_STREAMS = @Match [0x0, 0x03]
    SETTINGS_INITIAL_WINDOW_SIZE    = @Match [0x0, 0x04]
    SETTINGS_MAX_FRAME_SIZE         = @Match [0x0, 0x05]
    SETTINGS_MAX_HEADER_LIST_SIZE   = @Match [0x0, 0x06]

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

-- Given a list of 'maybe' values, return a list of only the values
-- found in 'just'.
def catMaybes (values: [maybe ?a]): [?a] =
  block
    let b = for (b = builder; mv in values)
      case mv of
        just v -> emit b v
        nothing -> b
    build b

-- Given a parser, return just if the parser succeeded, or nothing if
-- not.
def Maybe P =
  First
    block
      let value = P
      ^ just value
    ^ nothing
