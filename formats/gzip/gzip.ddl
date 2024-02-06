-- Parser for the gzip file compression format.

-- References:
-- https://www.rfc-editor.org/rfc/rfc1952#section-8
-- https://formats.kaitai.io/gzip/


--  ------------------------------------------------------------------------
-- Section of the rfc1952 (linked above) copied here for easy reference.
-- -------------------------------------------------------------------------

--      In the diagrams below, a box like this:
--
--         +---+
--         |   | <-- the vertical bars might be missing
--         +---+
--
--      represents one byte; a box like this:
--
--         +==============+
--         |              |
--         +==============+
--
--      represents a variable number of bytes.
--
-- The Gzip format is rather simple and should be **sequentially**
--  parsed as follows:
--
--         +---+---+---+---+---+---+---+---+---+---+
--         |ID1|ID2|CM |FLG|     MTIME     |XFL|OS | (more-->)
--         +---+---+---+---+---+---+---+---+---+---+
--
--      (if FLG.FEXTRA set)
--
--         +---+---+=================================+
--         | XLEN  |...XLEN bytes of "extra field"...| (more-->)
--         +---+---+=================================+
--
--      (if FLG.FNAME set)
--
--         +=========================================+
--         |...original file name, zero-terminated...| (more-->)
--         +=========================================+
--
--      (if FLG.FCOMMENT set)
--
--         +===================================+
--         |...file comment, zero-terminated...| (more-->)
--         +===================================+
--
--      (if FLG.FHCRC set)
--
--         +---+---+
--         | CRC16 |
--         +---+---+
--
--         +=======================+
--         |...compressed blocks...| (more-->)
--         +=======================+
--
--           0   1   2   3   4   5   6   7
--         +---+---+---+---+---+---+---+---+
--         |     CRC32     |     ISIZE     |
--         +---+---+---+---+---+---+---+---+

import Daedalus

-- Main parser
def Main =
  block
    Match [0x1f, 0x8b] -- Magic number for GZIP
    compression_method = UInt8
    flag = UInt8 as? Flags
    mtime = LEUInt32 -- modofication time in Unix format (seconds since 00:00:00 GMT, Jan.  1, 1970.)
    xfl = UInt8 -- extra flags
    os = GetOs -- get the OS/File System type
    flag_data = FlagData flag -- parse the data depending on flags that are set
    compressed_data = Many? UInt8 -- ? is a hack; is there a way to specify parse till end but - 64?
    crc32 = LEUInt32 -- CRC-32 algorithm used in the ISO 3309
    isize = LEUInt32 -- Input Size: This contains the size of the original (uncompressed) input data modulo 2^32
    END

bitdata Flags where
  reserved3: uint 1           -- bit7 
  reserved2: uint 1           -- bit6   
  reserved1: uint 1           -- bit5   
  fcomment: uint 1            -- bit4   
  fname: uint 1               -- bit3   
  fextra: uint 1              -- bit2   
  fhcrc: uint 1               -- bit1   
  ftext: uint 1               -- bit0   

def FlagData flag = 
  block

    -- reserved bytes should always be 0x00
    flag.reserved1 == 0 is true
    flag.reserved2 == 0 is true
    flag.reserved3 == 0 is true

    extra = if flag.fextra == 1
      then just FEXTRA
      else nothing

    name = if flag.fname == 1
      then just ZeroTerminatedBytes
      else nothing

    comment = if flag.fcomment == 1
      then just ZeroTerminatedBytes
      else nothing

    hcrc = if flag.fhcrc == 1
      then just FHRC -- crc16
      else nothing
    

def FHRC =
  block
    crc16 = LEUInt16

def FEXTRA =
  block
    xlen = LEUInt16 as uint 64
    --extra_field = Many xlen UInt8 -- Conservative parse which does not parse Subfields
    data = Many Subfield -- Untested. Will this always succeed?

def Subfield =
  block
    si1 = UInt8
    si2 = UInt8
    len = LEUInt16 as uint 64
    data = Many len UInt8 -- fix this
    

def ZeroTerminatedBytes = 
  block
    $$ = Many $[0x01 .. 0xFF] --$[1 .. 255]: parse non-zero bytes
    $[0x00]

-- Returns file system string
-- It is called OS to mantain coherence with the original standard that also calls it OS
def GetOs =
  block
  os_id = UInt8
  os_string = case os_id of
    0 ->  "FAT filesystem (MS-DOS, OS/2, NT/Win32)"
    1 ->  "Amiga"
    2 ->  "VMS (or OpenVMS)"
    3 ->  "Unix"
    4 ->  "VM/CMS"
    5 ->  "Atari TOS"
    6 ->  "HPFS filesystem (OS/2, NT)"
    7 ->  "Macintosh"
    8 ->  "Z-System"
    9 ->  "CP/M"
    10 ->  "TOPS-20"
    11 ->  "NTFS filesystem (NT)"
    12 ->  "QDOS"
    13 ->  "Acorn RISCOS"
    255 -> "unknown"
    _ ->  ""
