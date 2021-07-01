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
    Match1 '\n'
  };
  -- END
}

-- dist_newstyle
