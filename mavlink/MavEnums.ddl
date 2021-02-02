import MavNumerics

def MavFrame = Choose1 {
  mavFrameGlobal = @Match1 0;
  mavFrameLocalNed = @Match1 1;
  mavFrameMission = @Match1 2;
  mavFrameGlobalRelativeAlt = @Match1 3;
  mavFrameLocalEnu = @Match1 4;
  mavFrameGlobalInt = @Match1 5;
  mavFrameGlobalRelativeAltInt = @Match1 6;
  mavFrameLocalOffsetNed = @Match1 7;
  mavFrameBodyNed = @Match1 8;
  mavFrameBodyOffsetNed = @Match1 9;
  mavFrameGlobalTerrainAlt = @Match1 10;
  mavFrameGlobalTerrainAltInt = @Match1 11;
  mavFrameBodyFrd = @Match1 12;
  mavFrameReserved13 = @Match1 13;
  mavFrameReserved14 = @Match1 14;
  mavFrameReserved14 = @Match1 15;
  mavFrameReserved14 = @Match1 16;
  mavFrameReserved14 = @Match1 17;
  mavFrameReserved14 = @Match1 18;
  mavFrameReserved14 = @Match1 19;
  mavFrameLocalFrd = @Match1 20;
  mavFrameLocalFlu = @Match1 21;
}

-- MAVLink Commands (MAV_CMD)
-- TODO: fix to always parse two bytes
def MavCmd = Choose1 {
  mavCmdNavWaypoint = @Match1 16;
  mavCmdNavLoiterUnlim = @Match1 17;
  mavCmdNavLoiterTurns = @Match1 18;
  mavCmdNavLoiterTime = @Match1 19;
  mavCmdNavReturnToLaunch = @Match1 20;
  -- mavCmdSome can be further refined into cases for more precise value checking:
  mavCmdSome = UInt16;
}

-- CmdParams: parameters of a command, uninterpreted
def CmdParams = {
  param1 = Float is float;
  param2 = Float;
  param3 = Float;
  param4 = Float;
  x = Int32;
  y = Int32;
  z = Float;
} 

-- MAV_CMD_NAV_WAYPOINT (16)
def CmdNavWaypoint = Fail "not defined"

-- MAV_CMD_NAV_LOITER_UNLIM (17)
def CmdNavLoiterUnlim = Fail "not defined"

-- MAV_CMD_NAV_LOITER_TURNS (18)
def CmdNavLoiterTurns = Fail "not defined"

-- MAV_CMD_NAV_LOITER_TIME (19)
def CmdNavLoiterTime = Fail "not defined"

-- MAV_CMD_NAV_RETURN_TO_LAUNCH (20)
def CmdNavReturnToLaunch = Fail "not defined"

-- definition sketch, using case:
def MavCmdParams cmd = 
  case cmd is {
    mavCmdNavWaypoint -> CmdNavWaypoint ;
    mavCmdNavLoiterUnlim -> CmdNavLoiterUnlim ;
    mavCmdNavLoiterTurns -> CmdNavLoiterTurns ;
    mavCmdNavLoiterTime -> CmdNavLoiterTime ;
    mavCmdNavReturnToLaunch -> CmdNavReturnToLaunch ;
    mavCmdSome opcode -> {
      opc = ^opcode;
      ps = CmdParams;
    }
  }

def MavMissionType = Choose1 {
  mavMissionTypeMission = @Match1 0;
  mavMissionTypeFence = @Match1 1;
  mavMissionTypeRally = @Match1 2;
  mavMissionTypeAll = @Match1 255;
}

def MavMissionResult = Choose1 { 
  mavMissionAccepted = @Match1 0; 
  mavMissionError = @Match1 1; 
  mavMissionUnsupportedFrame = @Match1 2; 
  mavMissionUnsupported = @Match1 3;
  mavMissionNoSpace = @Match1 4; 
  mavMissionInvalid = @Match1 5; 
  mavMissionInvalidParam1 = @Match1 6;
  mavMissionInvalidParam2 = @Match1 7;
  mavMissionInvalidParam3 = @Match1 8;
  mavMissionInvalidParam4 = @Match1 9;
  mavMissionInvalidParam5X = @Match1 10;
  mavMissionInvalidParam6Y = @Match1 11;
  mavMissionInvalidParam7 = @Match1 12;
  mavMissionInvalidSequence = @Match1 13; 
  mavMissionDenied = @Match1 14; 
  mavMissionOperationCancelled = @Match1 15;   
}