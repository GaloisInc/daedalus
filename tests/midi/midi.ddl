
def Main =
  { header = Header;
    @n     = header.track_num as int;
    tracks = Many n Track
  }



def VarQ : int =
  { @lead = Many { @b = UInt8; getBit 7 b == 1; ^ b as! uint 7 };
    @last = UInt7;
    ^ (for (v = 0; l in lead) v <# l) <# last
  }


def Chunk Ty P =
  { Block 4 (Only Ty);
    @len = BE32;
    Block (len as int) (Only P)
  }



def Header =
  Chunk "MThd"
  { format = Choose { single_track = TAG16 0;
                      multi_track  = TAG16 1;
                      multi_song   = TAG16 2;
                    };
    track_num = BE16;
    time_unit =
      { @w   = BE16;
        @tag = ^ getBit 15 w;
        Choose { quarter_len = { tag == 0; ^ w as! uint 15 };
                 smtpe       = { tag == 1; ^ w as! sint 15 };
               }
      }
  }




def Track = Chunk "MTrk" (Many (Delta Event))

def Delta E = { after = VarQ; event = E }

def Event =
  Choose {
    voiceMessage = VoiceMessages;
    modeMessage  = ModeMessages;
    sysEx        = SysEx;
    meta         = Meta;
  }

def VoiceMessages =
  { @status  = UInt8;
    @tag     = ^ status >> 4 as! uint 4;
    channel  = ^ status as! uint 4;
    message  = VoiceMessage tag;
    extra    = Many (Delta (VoiceMessage tag))
  }

def VoiceMessage (tag : uint 4) =
  Choose {
    note_off          = { tag == 0x8; key = UInt7; velocity = UInt7; };
    note_on           = { tag == 0x9; key = UInt7; velocity = UInt7; };
    aftertouch        = { tag == 0xA; key = UInt7; pressure = UInt7; };
    controller_change = { tag == 0xB;
                          controller = UInt7; controller <= 0x77;
                          value      = UInt7; };
    program_change    = { tag == 0xC; $$ = UInt7 };
    channel_pressure  = { tag == 0xD; $$ = UInt7 };
    pitch_bend        = { tag == 0xE; @lsb = UInt7; @msb = UInt7; ^ msb # lsb };
  }

def ModeMessages =
  { @status = UInt8;
    @tag    = ^ status >> 4 as! uint 4;
    tag == 0xB;
    channel = ^ status as! uint 4;
    messages = ModeMessage;
    extra    = Many (Delta ModeMessage);
  }

def ModeMessage =
  Choose {
    all_sound_off     = { 0x78; 0x00 };
    reset_controllers = { 0x79; 0x00 };
    local_control     = { 0x7A; Choose { off = @0x00; on = @0x7F } };
    all_notes_off     = { 0x7B; 0x00 };
    omni_off          = { 0x7C; 0x00 };
    omni_on           = { 0x7D; 0x00 };
    mono_on           = { 0x7E; $$ = UInt8; $$ <= 0x10; };
    poly_on           = { 0x7F; 0x00 };
  }


def SysEx =
  Choose {
    add_f0 = { 0xF0; @len = VarQ; Block len GetStream };
    as_is  = { 0xF7; @len = VarQ; Block len GetStream };
  }


def Meta =
  { 0xFF;
    @type = UInt8; type <= 0x7F;
    @len  = VarQ;
    Block len
      Choose1 {
        sequence     = { type == 0x00; Only BE16 };
        text         = { type == 0x01; GetStream };
        copyright    = { type == 0x02; GetStream };
        name         = { type == 0x03; GetStream };
        instrument   = { type == 0x04; GetStream };
        lyrics       = { type == 0x05; GetStream };
        marker       = { type == 0x06; GetStream };
        cue          = { type == 0x07; GetStream };
        channel      = { type == 0x20; Only UInt8 };
        end_track    = { type == 0x2F; END };
        tempo        = { type == 0x51; Only BE24 };
        smtpe_offset = { type == 0x54;
                         hh = UInt8; mm = UInt8; ss = UInt8; fr = UInt8;
                         ff = UInt8; END };
        time_sig     = { type == 0x58;
                         nn = UInt8; dd = UInt8; cc = UInt8;
                         bb = UInt8;
                         END
                       };
        key_sig      = { type == 0x59;
                         key  = UInt8; -- -ve: no. of flats
                                       -- +ve: no. of sharps
                         mode = Choose { major = 0; minor = 1 };
                         END
                       };
        seq_specifiec = { type == 0x7F;
                          manufacturer = { 0; BE16 } <|
                                         { @b = UInt8; ^ b as uint 16 };
                          data = GetStream
                        };
        unknown = { type = ^ type; data = GetStream }
      }

  }


--------------------------------------------------------------------------------
def BE16        = { @b1 = UInt8; @b2 = UInt8; ^ b1 # b2 }
def BE24        = { @w1 = BE16;  @b2 = UInt8; ^ w1 # b2 }
def BE32        = { @w1 = BE16;  @w2 = BE16;  ^ w1 # w2 }
def TAG16 n     = { @b  = BE16; b == n }
def getBit n b  = b >> n as! uint 1


def UInt7       = { @b = UInt8; b as uint 7 }

def Block n P =
  { @cur = GetStream;
    @this = Take n cur;
    SetStream this;
    $$ = P;
    @next = Drop n cur;
    SetStream next;
  }

def Only P = { $$ = P; END }


