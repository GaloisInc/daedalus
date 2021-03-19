import MissionAck 
import MissionCount
import MissionItemInt

def Main = {
  $$ = Many {
    $$ = Choose1 {
      ack = MissionAck;
      count = MissionCount;
      itemInt = MissionItemInt;
    };
    '\n'
  };
  -- END
}

-- dist_newstyle
