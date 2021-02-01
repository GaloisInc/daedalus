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

def MavCmd = Choose1 {
  cmdNavWaypoint = @16;
  cmdNavLoiterUnlim = @17;
  cmdNavLoiterTurns = @18;
  cmdNavLoiterTime = @19;
  cmdNavReturnToLaunch = @20;
  -- cmdSome can be further refined into cases for more precise value checking:
  cmdSome = Uint16;
}

def MavMissionType = Choose1 {
  mission = @0;
  fence = @1;
  rally = @2;
  all = @255;
}

