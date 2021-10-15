def joinWords a b = if ?bigEndian then a # b else b # a

def UInt16        = joinWords UInt8 UInt8
def UInt32        = joinWords UInt16 UInt16
def UInt64        = joinWords UInt32 UInt32

def BE16          = block let ?bigEndian = true; UInt16
def BE32          = block let ?bigEndian = true; UInt32
def BE64          = block let ?bigEndian = true; UInt64

def LE16          = block let ?bigEndian = false; UInt16
def LE32          = block let ?bigEndian = false; UInt32
def LE64          = block let ?bigEndian = false; UInt64


