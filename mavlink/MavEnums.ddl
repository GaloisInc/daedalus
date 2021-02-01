import MavNumerics

def MavFrame = Choose1 {
  mavFrameGlobal = @0;
  mavFrameLocalNed = @1;
  mavFrameMission = @2;
  mavFrameGlobalRelativeAlt = @3;
  mavFrameLocalEnu = @4;
  mavFrameGlobalInt = @5;
  mavFrameGlobalRelativeAltInt = @6;
  mavFrameLocalOffsetNed = @7;
  mavFrameBodyNed = @8;
  mavFrameBodyOffsetNed = @9;
  mavFrameGlobalTerrainAlt = @10;
  mavFrameGlobalTerrainAltInt = @11;
  mavFrameBodyFrd = @12;
  mavFrameReserved13 = @13;
  mavFrameReserved14 = @14;
  mavFrameReserved14 = @15;
  mavFrameReserved14 = @16;
  mavFrameReserved14 = @17;
  mavFrameReserved14 = @18;
  mavFrameReserved14 = @19;
  mavFrameLocalFrd = @20;
  mavFrameLocalFlu = @21;
}

-- MAVLink Commands (MAV_CMD)
-- TODO: fix to always parse two bytes
def MavCmd = Choose1 {
  cmdNavWaypoint = @16;
  cmdNavLoiterUnlim = @17;
  cmdNavLoiterTurns = @18;
  cmdNavLoiterTime = @19;
  cmdNavReturnToLaunch = @20;
  -- cmdSome can be further refined into cases for more precise value checking:
  cmdSome = UInt16;
}

-- CmdParams: parameters of a command, uninterpreted
def CmdParams = {
  param1 = Float;
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
def MavCmdParams cmd = case cmd is {
  cmdNavWaypoint -> CmdNavWaypoint ;
  cmdNavLoiterUnlim -> CmdNavLoiterUnlim ;
  cmdNavLoiterTurns -> CmdNavLoiterTurns ;
  cmdNavLoiterTime -> CmdNavLoiterTime ;
  cmdNavReturnToLaunch -> CmdNavReturnToLaunch ;
  cmdSome opcode -> {
    opc = ^opcode;
    ps = CmdParams;
  }
}

def MavMissionType = Choose1 {
  mission = @0;
  fence = @1;
  rally = @2;
  all = @255;
}

