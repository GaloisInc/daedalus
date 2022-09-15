import Common

{- Parse a single packet.
Returns a different type of a packet depending on the count.
The result is a *tagged union*:
  * if the tag is `one` then we have a PACKET_ONE packet
  * if the tag is `two` then we have a PACKET_TWO packet
-}
def Packet (count : uint 64) =
  if count < 3
    then {| one = PACKET_ONE |}
    else {| two = PACKET_TWO |}



