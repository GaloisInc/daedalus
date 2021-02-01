import MavNumerics
import MavEnums

def MissionItemInt = {
  target_system = Uint8;
  target_component = Uint8;
  seq = Uint16;
  frame = MavFrame;
  command = MavCmd;
  current = MavBool;
  autocontinue = Uint8;
  param1 = Float;
  param2 = Float;
  param3 = Float;
  param4 = Float;
  x = Int32;
  y = Int32;
  z = Float;
  mission_type = MavMissionType;
}
