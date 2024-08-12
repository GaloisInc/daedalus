import Daedalus
import Common

def Message01 =
    block
        timestamp             = BEDouble
        vehicleId             = IDNumber
        cucsId                = IDNumber
        vsmId                 = IDNumberWithBroadcastCheck vehicleId
        dataLinkId            = IDNumber
        vehicleType           = BEUInt16
        vehicleSubtype        = BEUInt16
        requestedHandoverLOI  = UInt8
        controlledStation     = StationNumber
        controlledStationMode = ControlledStationModeWithBroadcastCheck vehicleId
        waitForVehicleDataLinkTransitionCoordinationMessage = WaitForVehicleDataLinkTransitionCoordinationMessage

-- A wrapper around IDNumber parsing that does additional validation
-- In particular, this is a data-dependent parser that takes a previously
-- parsed IDNumber as argument. If it is of type broadcastId, the newly parsed
-- IDNumber must also of the same type. If the input is not a broadcastId,
-- it reverts to being a simple IDNumber parser
def IDNumberWithBroadcastCheck (vehicleId: IDNumber) =
    block
        case vehicleId of
            broadcastId -> 
                block
                    let vsmid = IDNumber
                    vsmid is broadcastId
                    ^ vsmid
            _ -> IDNumber

def ControlledStationMode =
    block
        case $[0..2] of
            0x0 -> {| relinquishOrHandoffControl |}
            0x1 -> {| requestControl |}
            0x2 -> {| overrideControl |}

-- A wrapper around ControlledStationMode parsing that does additional validation
-- In particular, this is a data-dependent parser that takes a previously
-- parsed IDNumber as argument. If it is of type broadcastId, the newly parsed
-- ControlledStationMode must be set to 0 (i.e. relinquishOrHandoffControl).
-- If the input is not a broadcastId, it reverts to being a simple ControlledStationMode
-- parser
-- NOTE: This sort of validation may perhaps be better done at the application layer
-- but Daedalus does allow you to do this.
def ControlledStationModeWithBroadcastCheck (vehicleId: IDNumber) =
    block
        case vehicleId of
            broadcastId -> 
                block
                    let mode = ControlledStationMode
                    mode is relinquishOrHandoffControl
                    ^ mode
            _ -> ControlledStationMode

def WaitForVehicleDataLinkTransitionCoordinationMessage =
    block
        case $[0..1] of
            0x0 -> {| dontWait |}
            0x1 -> {| wait |}
