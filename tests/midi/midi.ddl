
def Main =
  { header = Header;
    @n     = header.track_num as uint 64;
    tracks = Many n Track
  }



def VarQ : int =
  { @lead = Many { @b = UInt8; Guard (getBit 7 b == 1); b as! uint 7 };
    @last = UInt7;
    ^ (for (v = 0; l in lead) v <# l) <# last
  }


def Chunk Ty P =
  { Block 4 (Only Ty);
    Block (BE32 as uint 64) (Only P)
  }



def Header =
  Chunk (Match "MThd")
  { format = Choose { single_track = TAG16 0;
                      multi_track  = TAG16 1;
                      multi_song   = TAG16 2;
                    };
    track_num = BE16;
    time_unit =
      { @w   = BE16;
        @tag = getBit 15 w;
        Choose { quarter_len = { Guard (tag == 0); ^ w as! uint 15 };
                 smtpe       = { Guard (tag == 1); ^ w as! sint 15 };
               }
      }
  }




def Track = Chunk (Match "MTrk") (Many (Delta Event))

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
    note_off          = { Guard (tag == 0x8); key = UInt7; velocity = UInt7; };
    note_on           = { Guard (tag == 0x9); key = UInt7; velocity = UInt7; };
    aftertouch        = { Guard (tag == 0xA); key = UInt7; pressure = UInt7; };
    controller_change = { Guard (tag == 0xB);
                          controller = UInt7; Guard (controller <= 0x77);
                          value      = UInt7; };
    program_change    = { Guard (tag == 0xC); $$ = UInt7 };
    channel_pressure  = { Guard (tag == 0xD); $$ = UInt7 };
    pitch_bend        = { Guard (tag == 0xE);
                          @lsb = UInt7; @msb = UInt7; ^ msb # lsb };
  }

def ModeMessages =
  { @status = UInt8;
    @tag    = ^ status >> 4 as! uint 4;
    Guard (tag == 0xB);
    channel = ^ status as! uint 4;
    messages = ModeMessage;
    extra    = Many (Delta ModeMessage);
  }

def ModeMessage =
  Choose {
    all_sound_off     = @Match [ 0x78; 0x00 ];
    reset_controllers = @Match [ 0x79; 0x00 ];
    local_control_off = @Match [ 0x7A; 0x00 ];
    local_control_on  = @Match [ 0x7A; 0x7f ];
    all_notes_off     = @Match [ 0x7B; 0x00 ];
    omni_off          = @Match [ 0x7C; 0x00 ];
    omni_on           = @Match [ 0x7D; 0x00 ];
    mono_on           = { Match1 0x7E; $$ = UInt8; Guard ($$ <= 0x10); };
    poly_on           = @Match [ 0x7F; 0x00 ];
  }


def SysEx =
  Choose {
    add_f0 = { Match1 0xF0; @len = VarQ as uint 64; Block len GetStream };
    as_is  = { Match1 0xF7; @len = VarQ as uint 64; Block len GetStream };
  }


def Meta =
  { Match1 0xFF;
    @type = UInt8; Guard (type <= 0x7F);
    @len  = VarQ as uint 64;
    Block len
      Choose1 {
        sequence     = { Guard (type == 0x00); Only BE16 };
        text         = { Guard (type == 0x01); GetStream };
        copyright    = { Guard (type == 0x02); GetStream };
        name         = { Guard (type == 0x03); GetStream };
        instrument   = { Guard (type == 0x04); GetStream };
        lyrics       = { Guard (type == 0x05); GetStream };
        marker       = { Guard (type == 0x06); GetStream };
        cue          = { Guard (type == 0x07); GetStream };
        channel      = { Guard (type == 0x20); Only UInt8 };
        end_track    = { Guard (type == 0x2F); END };
        tempo        = { Guard (type == 0x51); Only BE24 };
        smtpe_offset = { Guard (type == 0x54);
                         hh = UInt8; mm = UInt8; ss = UInt8; fr = UInt8;
                         ff = UInt8; END };
        time_sig     = { Guard (type == 0x58);
                         nn = UInt8; dd = UInt8; cc = UInt8;
                         bb = UInt8;
                         END
                       };
        key_sig      = { Guard (type == 0x59);
                         key  = UInt8; -- -ve: no. of flats
                                       -- +ve: no. of sharps
                         mode = Choose { major = Match1 0; minor = Match1 1 };
                         END
                       };
        seq_specifiec = { Guard (type == 0x7F);
                          manufacturer = { Match1 0; BE16 } <|
                                         { @b = UInt8; ^ b as uint 16 };
                          data = GetStream
                        };
        unknown = { type = ^ type; data = GetStream }
      }

  }


--------------------------------------------------------------------------------
def BE16        = UInt8 # UInt8
def BE24        = BE16  # UInt8
def BE32        = BE16  # BE16
def TAG16 n     = Guard (BE16 == n)
def getBit n b  = b >> n as! uint 1


def UInt7       = UInt8 as uint 7

def Block n P =
  { @cur = GetStream;
    SetStream (Take n cur);
    $$ = P;
    SetStream (Drop n cur);
  }

def Only P = { $$ = P; END }

def Guard p = p is true
