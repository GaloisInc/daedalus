import MavNumerics
import MavEnums

-- MISSION_ITEM_INT (#73)
def MissionItemInt = {
  -- four-byte fields:
  @params = CmdParams;

  -- two-byte fields:
  seq = UInt16;                     -- Waypoint ID (sequence number) 
  command = MavCmd params;          -- The scheduled action for the waypoint 
  autocontinue = UInt16;            -- Autocontinue to next waypoint 

  -- one-byte fields:
  target_system = UInt8;            -- System ID 
  target_component = UInt8;         -- Component ID 
  frame = MavFrame;                 -- The coordinate system of the waypoint 
  current = Bool;                   -- false:0, true:1
  mission_type = MavMissionType;    -- Mission type 
}

-- uncomment to add entry point for compilation
def Main = MissionItemInt
