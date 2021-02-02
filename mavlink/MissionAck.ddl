import MavNumerics
import MavEnums 

-- MISSION_ACK (#47)
def MissionAck = { 
  target_system = UInt8; 
  target_component = UInt8; 
  type = MavMissionResult; 
  mission_type = MavMissionType; 
}