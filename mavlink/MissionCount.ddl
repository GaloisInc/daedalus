import MavNumerics
import MavEnums

-- MISSION_COUNT (#44)
def MissionCount = { 
  target_system = UInt8; 
  target_component = UInt8; 
  count = UInt16; 
  mission_type = MavMissionType; 
}