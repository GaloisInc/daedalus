import Daedalus
import Common

def Message200 =
    block
        timestamp                   = BEDouble
        vehicleId                   = IDNumber
        cucsId                      = IDNumber
        stationNumber               = StationNumberExcludingUAPlatform
        setCentrelineAzimuthAngle   = RadiansFloat (-pi) pi
        setCentrelineElevationAngle = RadiansFloat (-pi) pi
        setZoom                     = Zoom
        setHorizontalFieldOfView    = RadiansFloat 0 (2 * pi)
        setVerticalFieldOfView      = RadiansFloat 0 (2 * pi)
        horizontalSlewRate          = RadiansPerSec (-2 * pi) (2 * pi)
        verticalSlewRate            = RadiansPerSec (-2 * pi) (2 * pi) 
        latitude                    = RadiansDouble (-pi / 2) (pi / 2)
        longitude                   = RadiansDouble (-pi / 2) (pi / 2)
        altitude                    = Altitude
        altitudeType                = AltitudeType
        focus                       = Focus
        focusType                   = FocusType
        

-- A wrapper around StationNumber parser that excludes
-- uaPlatform as this message does not support it
def StationNumberExcludingUAPlatform =
    block
        let val = StationNumber
        case val of 
            uaPlatform -> Fail "Station Number cannot be 0x0 [UA Platform]"
            _          -> ^ val
    
-- Parse radians expressed as 32-bit floats and ensure it is within a range    
def RadiansFloat min max =
    block
        let val = BEFloat
        val >= min && val <= max
        ^ val

-- Parse radians expressed as 64-bit doubles and ensure it is within a range    
def RadiansDouble min max =
    block
        let val = BEDouble
        val >= min && val <= max
        ^ val

-- Parse Altitude with validation
def Altitude =
    block
        let val = BEFloat
        val >= -1000 && val <= 100000
        ^ val

-- Parse values of kind radians / sec with validation
def RadiansPerSec min max =
    block
        let val = BEFloat
        val >= min && val <= max
        ^ val

def Zoom =
    block
        case $[0..3] of
            0x0 -> {| useSetFieldOfView |}
            0x1 -> {| stopZoom |}
            0x2 -> {| zoomIn |}
            0x3 -> {| zoomOut |}

def AltitudeType =
    block
        case $[0..3] of
            0x0 -> {| pressureAltitude |}
            0x1 -> {| baroAltitude |}
            0x2 -> {| agl |}
            0x3 -> {| wgs84 |}

def Focus =
    block
        case $[0..2] of
            0x0 -> {| noChange |}
            0x1 -> {| focusCloser |}
            0x2 -> {| focusFarther |}
            
def FocusType =
    block
        case $[0..1] of
            0x0 -> {| auto |}
            0x1 -> {| manual |}
