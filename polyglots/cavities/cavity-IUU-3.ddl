import common
import Daedalus

-- A simplified approximation of the Doom Patch WAD file format.

def Main = WAD

def WAD =
  block
    let start = GetStream
    Magic
    Many (1..) (EntryAndBlock start)

def Magic = TokenM "PWAD"

def TOCEntry =
  block
    TokenM "TOC-"
    lump = Token Natural
    len = Token Natural

def Block =
  block
    TokenM "BLOCK"
    Token Natural

def EntryAndBlock start =
  block
    let entry = TOCEntry
    let nextTOCEntry = GetStream
    SetStream (GetRegion start entry.lump entry.len)
    $$ = Block
    SetStream nextTOCEntry

def GetRegion (head: stream) (offset: uint 64) (len: uint 64) =
  Take len (Drop offset head) : stream
