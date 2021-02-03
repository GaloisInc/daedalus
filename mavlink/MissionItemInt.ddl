import MavNumerics
import MavEnums

-- MISSION_ITEM_INT (#73)
def MissionItemInt = {
  target_system = UInt8;            -- System ID 
  target_component = UInt8;         -- Component ID 
  seq = UInt16;                     -- Waypoint ID (sequence number) 
  frame = MavFrame;                 -- The coordinate system of the waypoint 
  @command = MavCmd;                -- The scheduled action for the waypoint 
  current = Bool;                   -- false:0, true:1
  autocontinue = UInt16;            -- Autocontinue to next waypoint 
  cmdParams = MavCmdParams command; -- See MAV_CMD enum 
  mission_type = MavMissionType;    -- Mission type 
}
