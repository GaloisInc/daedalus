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
def PrecisionLandMode (f : Float) = Choose1 {
  precisionLandModeDisabled = f is zero;
  precisionLandModeOpportunistic = {
    @n = f is number;
    n.sign is pos;
    Guard (n.exponent == 0);
    Guard (n.mantissa == 1)
  };
  precisionLandModeRequired = {
    @n = f is number;
    n.sign is pos;
    Guard (n.exponent == 0);
    Guard (n.mantissa == 2)
  };

}

-- MAVLink Commands (MAV_CMD)
def MavCmd params = {
  @cmd = UInt16;
  Choose1 {
    mavCmdNavWaypoint = { 
      Guard (cmd == 16); -- MAV_CMD_NAV_WAYPOINT (16)

      hold = NonNegFloat params.param1;
      acceptRadius = NonNegFloat params.param2;
      passRadius = ^params.param3;
      yaw = ^params.param4;

      position = Position params;
    };

    mavCmdNavLoiterUnlim = {
      Guard (cmd == 17); -- MAV_CMD_LOITER_UNLIM (17)

      -- param 1: Empty
      -- param 2: Empty
      radius = ^params.param3;
      yaw = ^params.param4;

      position = Position params;
    };

    mavCmdNavLoiterTurns = {
      Guard (cmd == 18); -- MAV_CMD_NAV_LOITER_TURNS(18)

      turns = NonNegFloat params.param1;
      headingRequired = InclusiveFractional params.param2;
      radius = ^params.param3;
      xtrackLoc = ^params.param4;

      position = Position params;
    };
  
    mavCmdNavLoiterTime = {
      Guard (cmd == 19); -- MAV_CMD_NAV_LOITER_TIME(19)

      time = NonNegFloat params.param1;
      headingRequired = InclusiveFractional params.param2;
      radius = ^params.param3;
      xtrackLocation = ^params.param4;

      position = Position params;
    };

    -- MAV_CMD_NAV_RETURN_TO_LAUNCH(20):
    mavCmdNavReturnToLaunch = Guard (cmd == 20);

    mavCmdNavLand = {
      Guard (cmd == 21); -- MAV_CMD_NAV_LAND(21)

      abortAlt = ^params.param1;
      landMode = PrecisionLandMode params.param2; 
      -- param 3: Empty
      yawAngle = ^params.param4;

      position = Position params;
    };

    mavCmdNavTakeoff = {
      Guard (cmd == 22); -- MAV_CMD_NAV_TAKEOFF(22)

      pitch = ^params.param1;
      -- param 2: Empty
      -- param 3: Empty
      yaw = ^params.param4;

      position = Position params
    };

    -- TODO: refine this clause to define deeper command and parameter
    -- validation for commands >= 23
    mavCmdSome = ^params;
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
