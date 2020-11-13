-- simple example testing the * of LL(*)

-- Testing stream operations

def Chunk P =  {
  @cur  = GetStream;
  @this = Take 3 cur;
  @next = Drop 3 cur;
  SetStream this;
  $$ = P;
  SetStream next;
}

def PadWSpaces P = 
  Chunk {$$ = P; Many (Match1 ' '); END}

def Main =
  { @a = Choose
    { x = PadWSpaces { Match "ab"}
    ; y = PadWSpaces { Match "ac"}
    }
  ; END
  }
