-- Definition of DDS messages, based on the DDSI-RTPS spec.
import Stdlib

-- TODO: account for endianness parameters
def Octet = UInt8

def OctetArray2 = Many 2 Octet

def OctetArray3 = Many 3 Octet

def UShort = {
  @highByte = UInt8;
  @lowByte = UInt8;
  ^(highByte # lowByte)
}

def ULong = {
  @highUShort = UShort;
  @lowUShort = UShort;
  ^(highUShort # lowUShort)
}

-- SubmessageFlag: a Boolean value
def SubmessageFlag = {
  @v = Octet;
  Choose1 {
    { Guard (v == 1);
      ^true
    };
    { Guard (v == 0);
      ^false
    };
  }
}

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

def Submessage = {
  subHeader = SubmessageHeader;
  elt = Chunk
    (subHeader.submessageLength as int)
    (SubmessageElement subHeader.flags);
}

-- Sec 8.3.3.2 Submessage structure: Table 8.15:
def SubmessageHeader = {
  submessageId = SubmessageId;

  flags = SubmessageFlags submessageId;
  submessageLength = UShort; -- TODO: validate this (Sec. 8.3.3.2.3)
}

-- Sec 8.3.3.2.1 SubmessageId: a SubmessageKind (Sec 9.4.5.1.1)
def SubmessageId = Choose1{
  pad = @Match1 0x01;
  acknack = @Match1 0x06;
  heartbeat = @Match1 0x07;
  gap = @Match1 0x08;
  infoTs = @Match1 0x09;
  infoSrc = @Match1 0x0c;
  infoReplyIP4 = @Match1 0x0d;
  infoDst = @Match1 0x0e;
  infoReply = @Match1 0x0f;
  nackFrag = @Match1 0x12;
  heartbeatFrag = @Match1 0x13;
  data = @Match1 0x015;
  dataFrag = @Match1 0x16;
}

def NthBit n fs = {
  ^((fs .&. (1 << n)) == 1);
}

-- SubmessageFlags:
def SubmessageFlags subId = {
  @flagBits = UInt8;
  Choose1 {
    dataFlags = {
      subId is data;
      endiannessFlag = NthBit 0 flagBits;
      inlineQosFlag = NthBit 1 flagBits;
      dataFlag = NthBit 2 flagBits;
      keyFlag = NthBit 3 flagBits;
      nonStandardPayloadFlag = NthBit 4 flagBits;
    };
    dataFragFlags = {
      subId is dataFrag;
      endiannessFlag = NthBit 0 flagBits;
      inlineQosFlag = NthBit 1 flagBits;
      nonStandardPayloadFlag = NthBit 2 flagBits;
      keyFlag = NthBit 3 flagBits;
    };
    -- submessages that don't contain data are not supported
  }
}

def SubmessageElement flags = Choose1 {
  dataElt = {
    dFlags = flags is dataFlags;
    readerId = EntityId;
    writerId = EntityId;

    writerSN = SequenceNumber;
    Guard (writerSN > 0);

    inlineQos = Choose1 {
      hasQos = {
        Guard (dFlags.inlineQosFlag);
        ParameterList;
      };
      noQos = Guard (dFlags.inlineQosFlag == false);
    };
    serializedPayload = Choose1 {
      hasPayload = {
        (Guard (dFlags.dataFlag) | Guard (dFlags.keyFlag));
        SerializedPayload;
      };
      noPayload = {
        Guard (dFlags.dataFlag == false);
        Guard (dFlags.keyFlag == false);
      };
    };
  };
  dataFragElt = {
    fragFlags = flags is dataFragFlags;
    readerId = EntityId;
    writerId = EntityId;

    writerSN = SequenceNumber;
    Guard (writerSN > 0);

    fragmentStartingNum = FragmentNumber;
    Guard (0 < fragmentStartingNum);
    
    fragmentsInSubmessage = UShort;
    Guard (fragmentStartingNum <= (fragmentsInSubmessage as uint 32));

    dataSize = ULong;

    fragmentSize = UShort;
    Guard (fragmentSize as uint 32 < dataSize); 
    Guard (fragmentSize <= 64000);

    inlineQos = Choose1 {
      hasQos = {
        Guard fragFlags.inlineQosFlag;
        ParameterList;
      };
      noQos = Guard (fragFlags.inlineQosFlag == false);
    };

    serializedPayload = Chunk
      ((fragmentsInSubmessage * fragmentSize) as int)
      (Many
        ((fragmentsInSubmessage as uint 32 - fragmentStartingNum) as int)
        SerializedPayload);
  }
}

def EntityId = {
  entityKey = OctetArray3;
  entityKind = Octet;
}

-- Sec 9.3.2 Mapping of the Types that Appear Within Submessages...
def SequenceNumber = {
  @high = ULong;
  @low = ULong;
  ^(high # low);
}

-- Sec 9.4.2.11 ParameterList:

-- Sec 9.6.2.2.2 ParameterID values: Table 9.13:
def ParameterIdT = Choose1 {
    pidPad = @Match [0x00, 0x00];
    pidSentinel = @Match [0x00, 0x01];
    pidUserData = @Match [0x00, 0x2c]; 
    pidTopicName = @Match [0x00, 0x05];
    pidTypeName = @Match [0x00, 0x07];
    pidGroupData = @Match [0x00, 0x2d];
    pidTopicData = @Match [0x00, 0x2e];
    pidDurability = @Match [0x00, 0x1d];
    pidDurabilityService = @Match [0x00, 0x1e];
    pidDeadline = @Match [0x00, 0x23];
    pidLatencyBudget = @Match [0x00, 0x27];
    pidLiveliness = @Match [0x00, 0x1b];
    pidReliability = @Match [0x00, 0x1a];
    pidLifespan = @Match [0x00, 0x2b];
    pidDestinationOrder = @Match [0x00, 0x25];
    pidHistory = @Match [0x00, 0x40];
    pidResourceLimits = @Match [0x00, 0x41];
    pidOwnership = @Match [0x00, 0x1f];
    pidOwnershipStrength = @Match [0x00, 0x06];
    pidPresentation = @Match [0x00, 0x21];
    pidPartition = @Match [0x00, 0x29];
    pidTimeBasedFilter = @Match [0x00, 0x04];
    pidTransportPriority = @Match [0x00, 0x49];
    pidDomainId = @Match [0x00, 0x0f];
    pidDomainTag = @Match [0x40, 0x14];
    pidProtocolVersion = @Match [0x00, 0x15];
    pidVendorid = @Match [0x00, 0x16];
    pidUnicastLocator = @Match [0x00, 0x2f];
    pidMulticastLocator = @Match [0x00, 0x30];
    pidDefaultUnicastLocator = @Match [0x00, 0x31];
    pidDefaultMulticastLocator = @Match [0x00, 0x48];
    pidMetatrafficUnicastLocator = @Match [0x00, 0x32];
    pidMetatrafficMulticastLocator = @Match [0x00, 0x33];
    pidExpectsInlineQos = @Match [0x00, 0x43];
    pidParticipantManualLivelinessCount = @Match [0x00, 0x34];
    pidParticipantLeaseDuration = @Match [0x00, 0x02];
    pidContentFilterProperty = @Match [0x00, 0x35];
    pidParticipantGuid = @Match [0x00, 0x50];
    pidGroupGuid = @Match [0x00, 0x52];
    pidBuiltinEndpointSet = @Match [0x00, 0x58];
    pidBuiltinEndpointQos = @Match [0x00, 0x77];
    pidPropertyList = @Match [0x00, 0x59];
    pidTypeMaxSizeSerialized = @Match [0x00, 0x60];
    pidEntityName = @Match [0x00, 0x62];
    pidEndpointGuid = @Match [0x00, 0x5a];
}

def Parameter = {
  parameterId = ParameterIdT;
  @len = UShort;
  value = Choose1 {
    sentinel = {
      parameterId is pidSentinel;
    };
    other = {
      Guard (len % 4 == 0);
      Many (len as int) Octet;
    };
  };
}

def ParameterList = Many Parameter

-- Sec 10 Serialized Payload Representation:
def SerializedPayload = {
  payloadHeader = SerializedPayloadHeader;
  data = PayloadData;
}

def SerializedPayloadHeader = {
  representationIdentifier = Choose1 {
    -- Table 10.3:
    userDefinedTopicData = Choose1 {
      cdrBe = @Match [0x00, 0x00];
      cdrLe = @Match [0x00, 0x01];
      plCdrBE = @Match [0x00, 0x02];
      plCdrLE = @Match [0x00, 0x03];
      cdr2Be = @Match [0x00, 0x10];
      cdr2LE = @Match [0x00, 0x11];
      plCdr2Be = @Match [0x00, 0x12];
      plCdr2LE = @Match [0x00, 0x03];
      dCdrBe = @Match [0x00, 0x14];
      dCdrLe = @Match [0x00, 0x15];
      xml = @Match [0x00, 0x04];
    };
  };
  representationOptions = OctetArray2;
}

-- Sec 9.3.2
def FragmentNumber = ULong

-- TODO: import payload definition here
def PayloadData = Fail "import definition"

-- parse messages
def Main = Message 

