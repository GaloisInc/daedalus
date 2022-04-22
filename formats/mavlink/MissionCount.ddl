import MavNumerics
import MavEnums

-- MISSION_COUNT (#44)
def MissionCount = { 
  target_system = UInt8;          -- System ID 
  target_component = UInt8;       -- Component ID 
  count = UInt16;                 -- Number of mission items 
                                  --  in the sequence
  mission_type = MavMissionType;  -- Mission type 
}

-- uncomment to add entry point for compilation
-- def Main = MissionCount
