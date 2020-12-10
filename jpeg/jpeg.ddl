def Main = { 
  Start; 
  JFIF; 
  EXIF; 
  $$ = Many Segment; 
  END; 
} 

def Start : {} = { 
  Match [0xff, 0xd8]; 
  ^ {}; 
}

def JFIF : [uint 8] = { 
  Match [0xff, 0xe0]; 
  Many { 
    @i = UInt8; 
    i == 0xff is false; 
    ^ i; 
  } 
} 

def EXIF : [uint 8] = { 
  Match [0xff, 0xe1]; 
  Many { 
    @i = UInt8; 
    i == 0xff is false; 
    ^ i; 
  } 
} 

def Segment : [uint 8] = { 
  Match1 0xff;
  UInt8;  
  Many { 
    @i = UInt8; 
    i == 0xff is false; 
    ^ i; 
  } 
} 
