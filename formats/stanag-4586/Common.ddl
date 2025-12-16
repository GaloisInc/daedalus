import Daedalus

def IDNumber = 
    First
        broadcastId = When ( Match [0xFF, 0xFF, 0xFF, 0xFF] ) {}
        nullId      = When ( Match [0xFF, 0x0, 0x0, 0x0] ) {}
        logicalUAId = $[0x0] # UInt8 # UInt8 # UInt8 
        idnumber    = BEUInt32


def StationNumber =
    block
        case BEUInt32 of
            0x00000000 -> {| uaPlatform |}
            0x00000001 -> {| station = (1 : uint 8) |}
            0x00000002 -> {| station = 2 |}
            0x00000004 -> {| station = 3 |}
            0x00000008 -> {| station = 4 |}
            0x00000010 -> {| station = 5 |}
            0x00000020 -> {| station = 6 |}
            0x00000040 -> {| station = 7 |}
            0x00000080 -> {| station = 8 |}
            0x00000100 -> {| station = 9 |}
            0x00000200 -> {| station = 10 |}
            0x00000400 -> {| station = 11 |}
            0x00000800 -> {| station = 12 |}
            0x00001000 -> {| station = 13 |}
            0x00002000 -> {| station = 14 |}
            0x00004000 -> {| station = 15 |}
            0x00008000 -> {| station = 16 |}
            0x00010000 -> {| station = 17 |}
            0x00020000 -> {| station = 18 |}
            0x00040000 -> {| station = 19 |}
            0x00080000 -> {| station = 20 |}
            0x00100000 -> {| station = 21 |}
            0x00200000 -> {| station = 22 |}
            0x00400000 -> {| station = 23 |}
            0x00800000 -> {| station = 24 |}
            0x01000000 -> {| station = 25 |}
            0x02000000 -> {| station = 26 |}
            0x04000000 -> {| station = 27 |}
            0x08000000 -> {| station = 28 |}
            0x10000000 -> {| station = 29 |}
            0x20000000 -> {| station = 30 |}
            0x40000000 -> {| station = 31 |}
            0x80000000 -> {| station = 32 |}

-- Parse with a numeric Parser and ensure that it is
-- within a specfied range
def NumericParserWithRange Parser min max =
    block
        let v = Parser
        GuardMsg ( v >= min && v <= max ) "Value not in range"
        ^ v