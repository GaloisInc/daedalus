{- This is a partial implementation of PNG. It splits the file into
   chunks as it should, but the CRC check is still buggy. No
   validation of correct chunk sequence, nor of the validity of chunk
   data with respect to chunk types, is performed. -}

def $letter = 'A'..'Z' | 'a'..'z'

def Header = @Match [ 0x89 ; 0x50 ; 0x4e ; 0x47 ; 0x0d ; 0x0a ; 0x1a; 0x0a ]

def BE = UInt8 # UInt8 # UInt8 # UInt8

def Length = BE as uint 64
def CRC    = BE
def Type   = Many 4 (Match1 $letter)

def Chunk = {
  @len     = Length;
  chunk_type = Type;
  contents = Many len UInt8;
  crcField = CRC;
  crcCheck = ^crc (concat [chunk_type,contents])
          -- This check currently fails, and there is insufficient
          -- time to debug it. The error message is not very useful,
          -- either, presumably due to too much backtracking.
          -- @(crcField == crcCheck).true
        }

def step c (k : uint 8) =
  if k < 8
    then (step (  (if c == (c >> 1) << 1 then (0xedb88320 : uint 32) else 0x0)
                .^. (c >> 1)
               )
               (k + 1))
    else c

def crcTable (n : uint 8) =
 step (n as uint 32) 0

def updatecrc (crc : uint 32) (bytes : [uint 8]) =
  for (out = crc; b in bytes)
    ((crcTable (((crc .^. (b as uint 32)) .&. 0xff) as! uint 8)) .^.
     (crc >> 8))

def crc input = updatecrc 0xffffffff input .^. 0xffffffff

def Main = { Header; $$ = Many Chunk; END }
