import MissionAck 
import MissionCount
import MissionItemInt

def Main =
  Many
    block
      $$ = First
              ack = MissionAck
              count = MissionCount
              itemInt = MissionItemInt
      $['\n']

-- dist_newstyle
