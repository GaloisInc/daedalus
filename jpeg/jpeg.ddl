def Main = { 
  SOI; 
  JFIF; 
  EXIF; 
  $$ = Many Segment; 
  EOI; 
} 

-- Start of image 
def SOI : {} = { 
  Match [0xff, 0xd8]; 
  ^ {}; 
}

def EOI = { 
  Match [0xff, 0xd9]; 
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
  @tag = { Match1 0xff; UInt8 }; 
  tag == 0xd9 is false; 
  case tag is {
    0xdb -> SegmentBody; 
    _    -> SegmentBody;
  } 
}

def SegmentBody = Many { 
    @i = UInt8; 
    i == 0xff is false; 
    ^ i; 
  } 
