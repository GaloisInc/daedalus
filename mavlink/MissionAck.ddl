import MavNumerics
import MavEnums 

-- MISSION_ACK (#47)
def MissionAck = { 
  target_system = UInt8;          -- System ID 
  target_component = UInt8;       -- Component ID 
  type = MavMissionResult;        -- Mission result 
  mission_type = MavMissionType;  -- Mission type 
}