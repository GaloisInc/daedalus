import MavNumerics

def MavFrame = First
  mavFrameGlobal = @$[0]
  mavFrameLocalNed = @$[1]
  mavFrameMission = @$[2]
  mavFrameGlobalRelativeAlt = @$[3]
  mavFrameLocalEnu = @$[4]
  mavFrameGlobalInt = @$[5]
  mavFrameGlobalRelativeAltInt = @$[6]
  mavFrameLocalOffsetNed = @$[7]
  mavFrameBodyNed = @$[8]
  mavFrameBodyOffsetNed = @$[9]
  mavFrameGlobalTerrainAlt = @$[10]
  mavFrameGlobalTerrainAltInt = @$[11]
  mavFrameBodyFrd = @$[12]
  mavFrameReserved13 = @$[13]
  mavFrameReserved14 = @$[14]
  mavFrameReserved14 = @$[15]
  mavFrameReserved14 = @$[16]
  mavFrameReserved14 = @$[17]
  mavFrameReserved14 = @$[18]
  mavFrameReserved14 = @$[19]
  mavFrameLocalFrd = @$[20]
  mavFrameLocalFlu = @$[21]

-- CmdParams: parameters of a command
def CmdParams = {
  param1 = Float;
  param2 = Float;
  param3 = Float;
  param4 = Float;
  x = Int32;
  y = Int32;
  z = Float;
} 

def Position params = {
  latitude = ^params.x;
  longitude = ^params.y;
  altitude = ^params.z;
}

-- PrecisionLandMode f: the precision land mode represented by float f:
def PrecisionLandMode (f : Float) = First
  precisionLandModeDisabled = f is zero
  precisionLandModeOpportunistic = {
    @n = f is number;
    n.sign is pos;
    Guard (n.exponent == 0);
    Guard (n.mantissa == 1)
    }
  precisionLandModeRequired = {
    @n = f is number;
    n.sign is pos;
    Guard (n.exponent == 0);
    Guard (n.mantissa == 2)
    }


-- MAVLink Commands (MAV_CMD)
def MavCmd params = {
  @cmd = UInt16;
  First
    mavCmdNavWaypoint = { 
      Guard (cmd == 16); -- MAV_CMD_NAV_WAYPOINT (16)

      hold = NonNegFloat params.param1;
      acceptRadius = NonNegFloat params.param2;
      passRadius = ^params.param3;
      yaw = ^params.param4;

      position = Position params;
      }

    mavCmdNavLoiterUnlim = {
      Guard (cmd == 17); -- MAV_CMD_LOITER_UNLIM (17)

      -- param 1: Empty
      -- param 2: Empty
      radius = ^params.param3;
      yaw = ^params.param4;

      position = Position params;
      }

    mavCmdNavLoiterTurns = {
      Guard (cmd == 18); -- MAV_CMD_NAV_LOITER_TURNS(18)

      turns = NonNegFloat params.param1;
      headingRequired = InclusiveFractional params.param2;
      radius = ^params.param3;
      xtrackLoc = ^params.param4;

      position = Position params;
      }
  
    mavCmdNavLoiterTime = {
      Guard (cmd == 19); -- MAV_CMD_NAV_LOITER_TIME(19)

      time = NonNegFloat params.param1;
      headingRequired = InclusiveFractional params.param2;
      radius = ^params.param3;
      xtrackLocation = ^params.param4;

      position = Position params;
      }

    -- MAV_CMD_NAV_RETURN_TO_LAUNCH(20):
    mavCmdNavReturnToLaunch = Guard (cmd == 20);

    mavCmdNavLand = {
      Guard (cmd == 21); -- MAV_CMD_NAV_LAND(21)

      abortAlt = ^params.param1;
      landMode = PrecisionLandMode params.param2; 
      -- param 3: Empty
      yawAngle = ^params.param4;

      position = Position params;
      }

    mavCmdNavTakeoff = {
      Guard (cmd == 22); -- MAV_CMD_NAV_TAKEOFF(22)

      pitch = ^params.param1;
      -- param 2: Empty
      -- param 3: Empty
      yaw = ^params.param4;

      position = Position params
      }

    -- TODO: refine this clause to define deeper command and parameter
    -- validation for commands >= 23
    mavCmdSome = ^params;
}

def MavMissionType = First
  mavMissionTypeMission = @$[0]
  mavMissionTypeFence = @$[1]
  mavMissionTypeRally = @$[2]
  mavMissionTypeAll = @$[255]

def MavMissionResult = First
  mavMissionAccepted = @$[0]
  mavMissionError = @$[1]
  mavMissionUnsupportedFrame = @$[2]
  mavMissionUnsupported = @$[3]
  mavMissionNoSpace = @$[4]
  mavMissionInvalid = @$[5]
  mavMissionInvalidParam1 = @$[6]
  mavMissionInvalidParam2 = @$[7]
  mavMissionInvalidParam3 = @$[8]
  mavMissionInvalidParam4 = @$[9]
  mavMissionInvalidParam5X = @$[10]
  mavMissionInvalidParam6Y = @$[11]
  mavMissionInvalidParam7 = @$[12]
  mavMissionInvalidSequence = @$[13]
  mavMissionDenied = @$[14]
  mavMissionOperationCancelled = @$[15]
