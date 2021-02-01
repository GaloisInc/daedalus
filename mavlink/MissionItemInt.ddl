import MavNumerics
import MavEnums

-- MISSION_ITEM_INT (#73)
def MissionItemInt = {
  target_system = UInt16;
  target_component = UInt16;
  seq = UInt16;
  frame = MavFrame;
  @command = MavCmd;
  current = Bool;
  autocontinue = UInt16;
  cmdParams = MavCmdParams command;
  mission_type = MavMissionType;
}
