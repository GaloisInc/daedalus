import Common

{- Parse 2 PACKET_ONE, followed by a PACKET_TWO.
All data is parsed from a *single* data source (array).

The result is a `struct` with 2 fields:
  * The first field is an array of PACKET_ONE packets
  * The second field has a single PACKET_TWO packlet.
-}
def Main =
  block
    one = Many 2 PACKET_ONE
    two = PACKET_TWO

