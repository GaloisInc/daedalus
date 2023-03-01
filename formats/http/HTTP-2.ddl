-- HTTP 2 frame parser
--
-- Unsupported features:
-- * Protocol extensions (https://www.rfc-editor.org/rfc/rfc9113#section-5.5)

def HTTP2_frame =
  block
    header = Frame_Header
    body = Many (header.len as! uint 64) $any

-- Frame header:
-- https://www.rfc-editor.org/rfc/rfc9113#name-frame-format
def Frame_Header =
  block
    -- Length (24 bits)
    len = UInt24

    -- Type (8 bits)
    type = Frame_Type

    -- Flags (8 bits)
    -- TODO: parse the flags along with the frame type so they can be in
    -- the same structure?
    flags = UInt8

    -- Reserved (1 bit)
    -- Stream Identifier (31 bits)
    --
    -- If we parse the next 32 bits as a unit, then the top bit is the
    -- Reserved bit which we ignore here.
    --
    -- https://www.rfc-editor.org/rfc/rfc9113#section-4.1-4.8.1
    let packed_ident = UInt32
    ident = packed_ident as! uint 31

-- Frame types
-- https://www.rfc-editor.org/rfc/rfc9113#name-frame-definitions
def Frame_Type =
  First
    F_DATA          = $[0x00]
    F_HEADERS       = $[0x01]
    F_PRIORITY      = $[0x02]
    F_RST_STREAM    = $[0x03]
    F_SETTINGS      = $[0x04]
    F_PUSH_PROMISE  = $[0x05]
    F_PING          = $[0x06]
    F_GOAWAY        = $[0x07]
    F_WINDOW_UPDATE = $[0x08]
    F_CONTINUATION  = $[0x09]

-- GOAWAY / RST_STREAM error codes
-- https://www.rfc-editor.org/rfc/rfc9113#name-error-codes

-- DATA frame flags
-- https://www.rfc-editor.org/rfc/rfc9113#section-6.1-6

-- HEADERS frame flags
-- https://www.rfc-editor.org/rfc/rfc9113#name-headers

-- Defined settings identifiers
-- https://www.rfc-editor.org/rfc/rfc9113#name-defined-settings
def SETTINGS_HEADER_TABLE_SIZE      = $[0x01] as uint 16
def SETTINGS_ENABLE_PUSH            = $[0x02] as uint 16
def SETTINGS_MAX_CONCURRENT_STREAMS = $[0x03] as uint 16
def SETTINGS_INITIAL_WINDOW_SIZE    = $[0x04] as uint 16
def SETTINGS_MAX_FRAME_SIZE         = $[0x05] as uint 16
def SETTINGS_MAX_HEADER_LIST_SIZE   = $[0x06] as uint 16

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
