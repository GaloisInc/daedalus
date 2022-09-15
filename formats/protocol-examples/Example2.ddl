import Common

{- Parse 2 PACKET_ONE, followed by a PACKET_TWO.
Data is parsed from *dfferent* data sources, provided by an external primitve.

The result is a `struct` with 2 fields:
  * The first field is an array of PACKET_ONE packets
  * The second field has a single PACKET_TWO packlet.
-}
def Protocol =
  block
    one = Many 2 (Packet PACKET_ONE)
    two = Packet PACKET_TWO


{- A conveinience function---it first gets the bytes for the packet using
an external primitve, then runs the given parser. -}
def Packet P =
  block
    GetPacketBytes
    P


{- This is an *external primitive*.  It allows Daedalus users to extend
Daedalus with custom behavior.   In this case we use it to modify the parser's
input to set bytes for the next packet. -}
def GetPacketBytes : {}


