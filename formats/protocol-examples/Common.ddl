-- One type of packet
def PACKET_ONE =
  block
    Line (Match "PacketOne")
    x = Line (Many (1..) $['0' .. '9'])
    y = Line (Many (1..) $['0' .. '9'])

def Line P =
  block
    $$ = P
    $['\n']

-- Another type of packet
def PACKET_TWO =
  block
    Line (Match "PacketTwo")
    someBytes = Many UInt8

