
def Main =
  block
    header = Header
    tracks = Many header.track_num Track

-- A tagged block of midi data
def Chunk name Body =
  block
    ExactBlock 4 (Match name)
    ExactBlock (BE32 as uint 64) Body


--------------------------------------------------------------------------------
-- MIDI Header

def Header =
  Chunk "MThd"
    block
      format    = BE16 as? MidiFormat
      track_num = BE16 as uint 64
      time_unit = BE16 as MidiTimeUnit

bitdata MidiFormat where
  single_track = 0 : uint 16
  multi_track  = 1
  multi_song   = 2

bitdata MidiTimeUnit where
  quarter_len = { 0b0; value : uint 15 }
  smtpe       = { 0b1; value : sint 15 }
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- A MIDI Track
def Track = Chunk "MTrk" (Many (Delta Event))

-- Events spaces by time
def Delta E = block
  after = VarQ
  event = E

-- Different types of events
def Event =
  Choose1
    voiceMessage = VoiceMessages
    modeMessage  = ModeMessages
    sysEx        = SysEx
    meta         = Meta


--------------------------------------------------------------------------------
-- Voice Message Events

def VoiceMessages =
  case UInt8 as? VoiceMessagesHeader of
    VoiceMessagesHeader header ->
      block
        channel = header.channel
        message = VoiceMessage header.tag
        extra   = Many (Delta (VoiceMessage header.tag))

bitdata VoiceMessagesHeader where
  VoiceMessagesHeader = { tag : VoiceMessageTag, channel : uint 4 }

bitdata VoiceMessageTag where
  note_off          = 0x8 : uint 4
  note_on           = 0x9
  aftertouch        = 0xA
  controller_change = 0xB
  program_change    = 0xC
  channel_pressure  = 0xD
  pitch_bend        = 0xE


def VoiceMessage (tag : VoiceMessageTag) =
  case tag of
    note_off          -> {| note_off          = NoteEvent |}
    note_on           -> {| note_on           = NoteEvent |}
    aftertouch        -> {| aftertouch        = NoteEvent |}
    controller_change -> {| controller_change = ControllerChange |}
    program_change    -> {| program_change    = UInt7 |}
    channel_pressure  -> {| channel_pressure  = UInt7 |}
    pitch_bend        -> {| pitch_bend =
                              block
                                let lsb = UInt7
                                let msb = UInt7
                                msb # lsb
                           |}



def NoteEvent = block
  key      = UInt7
  velocity = UInt7

def ControllerChange = block
  controller = UInt7
  Guard (controller <= 0x77)
  value      = UInt7



--------------------------------------------------------------------------------
-- Mode Message Events

def ModeMessages =
  case UInt8 as? ModeMessagesHeader of
    ModeMessagesHeader header ->
      block
        channel = header.channel
        message = ModeMessage
        extra   = Many (Delta ModeMessage)

bitdata ModeMessagesHeader where
  ModeMessagesHeader = { 0xB : uint 4, channel : uint 4 }

def ModeMessage =
  Choose1
    all_sound_off     = @Match [ 0x78; 0x00 ]
    reset_controllers = @Match [ 0x79; 0x00 ]
    local_control_off = @Match [ 0x7A; 0x00 ]
    local_control_on  = @Match [ 0x7A; 0x7F ]
    all_notes_off     = @Match [ 0x7B; 0x00 ]
    omni_off          = @Match [ 0x7C; 0x00 ]
    omni_on           = @Match [ 0x7D; 0x00 ]
    mono_on           = block
                          Match1 0x7E
                          $$ = UInt8
                          Guard ($$ <= 0x10)
    poly_on           = @Match [ 0x7F; 0x00 ]
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- SysEx Events

def SysEx =
  Choose1

    add_f0 =
      block
        Match1 0xF0
        Block VarQ (Many UInt8)

    as_is =
      block
        Match1 0xF7
        Block VarQ (Many UInt8)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Metdata events

def Meta =
  block
    Match1 0xFF
    let tag = UInt8
    Guard (tag <= 0x7F);
    ExactBlock VarQ
      case tag of
        0x00 -> {| sequence       = BE16 |}
        0x01 -> {| text           = Many UInt8 |}
        0x02 -> {| copyright      = Many UInt8 |}
        0x03 -> {| name           = Many UInt8 |}
        0x04 -> {| instrument     = Many UInt8 |}
        0x05 -> {| lyrics         = Many UInt8 |}
        0x06 -> {| marker         = Many UInt8 |}
        0x07 -> {| cue            = Many UInt8 |}
        0x20 -> {| channel        = UInt8 |}
        0x2F -> {| end_track      = END |}
        0x51 -> {| tempo          = BE24 |}
        0x54 -> {| smtpe_offset   = SMTPEOffset |}
        0x58 -> {| time_sig       = TimeSig |}
        0x59 -> {| key_sig        = KeySig |}
        0x7F -> {| seq_specifiec  = MetaSeqSpecific |}
        _    -> {| unknown        = MetaUnknown tag |}

def MetaSeqSpecific =
  block
    manufacturer =
      Choose1
        block
          Match1 0
          BE16
        UInt8 as uint 16
    data = Many UInt8

def MetaUnknown tag =
  block
    tag = tag : uint 8
    data = Many UInt8

bitdata Accidentals where
  flats   = { 0b1, flats : uint 7 }
  sharps  = { 0b0, sharps : uint 7 }

bitdata Mode where
  minor = 0 : uint 8
  major = 1 : uint 8

def KeySig =
  block
    key   = UInt8 as Accidentals
    mode  = UInt8 as? Mode



def SMTPEOffset =
  block
    hh = UInt8
    mm = UInt8
    ss = UInt8
    fr = UInt8
    ff = UInt8

def TimeSig =
  block
    nn = UInt8
    dd = UInt8
    cc = UInt8
    bb = UInt8
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Utilities


-- Variable length numbers
bitdata VarQChunk where
  More = { 0b1; value : uint 7 }
  Last = { 0b0; value : uint 7 }

def VarQBuilder (result : int) =
  case UInt8 as VarQChunk of
    More x -> VarQBuilder (result <# x.value)
    Last x -> result <# x.value

-- The MIDI format restricts tihs to 4 bytes, but we allow a bit more
def VarQ = VarQBuilder 0 as? uint 64




def BE16        = UInt8 # UInt8
def BE24        = BE16  # UInt8
def BE32        = BE16  # BE16
def UInt7       = UInt8 as? uint 7

def Block n P =
  block
    let cur = GetStream
    SetStream (Take n cur)
    $$ = P
    SetStream (Drop n cur)

def Only P =
  block
    $$ = P
    END

def ExactBlock n P = Block n (Only P)

def Guard p = p is true
