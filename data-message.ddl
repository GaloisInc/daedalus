-- Definition of DDS messages, based on the DDSI-RTPS spec.
def Octet = Uint8

-- Sec. 8.3.3 The Overall sStrucutre of an RTPS Message: the overall
-- structure of an RTPS Message consists of a fixed-size leading RTPS
-- Header followed by a variable number of RTPS Submessage
-- parts. Fig. 8.8:
def Message = {
  header = Header;
  submessages = Many Submessage;
}

-- Sec. 8.3.3.1 Header Structure. Table 8.14:
def Header = {
  @protocol = Protocol;
  @version = ProtocolVersion;
  vendorId = VendorId;
  guidPrefix = GuidPrefix;
}

-- Sec. 8.3.3.1.1 protocol: value is set to PROTOCOL_RTPS
def Protocol = ProtocolRTPS

-- Sec. 8.3.3.1.2 version:
def ProtocolVersion = Version24

def VendorId = Many 2 Octet

def ProtocolRTPS = "RTPS" -- ?

def Version24 = "24"

def GuidPrefix = Many 12 Octet

def Submessage = Fail "Submessage: not defined"

def Main = Message 
