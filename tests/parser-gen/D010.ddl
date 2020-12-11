-- testing input synchronization during lockstep move

def Chunk n P =  {
  @cur  = GetStream;
  @this = Take n cur;
  @next = Drop n cur;
  SetStream this;
  $$ = P;
  SetStream next;
}

def CanDisambiguate =
  Choose
  { x = Chunk 2 { Match "ab" }
  ; y = Chunk 2 { Match "ac" }
  }

def ShouldAbortWithIncompatibleInput =
 Choose
 { x = { Chunk 1 { Match "a" }
       ; Match "b"
       }
 ; y = Chunk 2 { Match "ac" }
 }

def Main =
  { @a = CanDisambiguate
  ; @b = ShouldAbortWithIncompatibleInput
  ; END
  }
