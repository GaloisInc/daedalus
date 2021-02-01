-- Testing stream operations with some static data-dependency
-- should be same output as D008.ddl

def Chunk n P =  {
  @cur  = GetStream;
  @this = Take n cur;
  @next = Drop n cur;
  SetStream this;
  $$ = P;
  SetStream next;
}

def PadWSpaces n P = 
  Chunk n {$$ = P; Many (Match1 ' '); END}

def Main =
  { @a = Choose
    { x = PadWSpaces 3 { Match "ab"}
    ; y = PadWSpaces 3 { Match "ac"}
    }
  ; END
  }
