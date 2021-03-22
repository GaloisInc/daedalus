-- Definition of DDS messages, based on the DDSI-RTPS spec.
import Stdlib

-- TODO: account for endianness parameters
def Octet = UInt8

def OctetArray2 = Many 2 Octet

def OctetArray3 = Many 3 Octet

def UShort = {
  @highByte = UInt8;
  @lowByte = UInt8;
  ^(highByte as (uint 16) << 8 + (lowByte as uint 16))
}

def ULong = {
  @highUShort = UShort;
  @lowUShort = UShort;
  ^((highUShort << 16) + lowUShort)
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
  submessageId is data; -- only parsing data packets

  flags = SubmessageFlags submessageId;
  submessageLength = UShort; -- TODO: validate this (Sec. 8.3.3.2.3)
}

-- Sec 8.3.3.2.1 SubmessageId: a SubmessageKind (Sec 9.4.5.1.1)
def SubmessageId = Choose1{
  pad = Match1 0x01;
  acknack = Match1 0x06;
  heartbeat = Match1 0x07;
  gap = Match1 0x08;
  infoTs = Match1 0x09;
  infoSrc = Match1 0x0c;
  infoReplyIP4 = Match1 0x0d;
  infoDst = Match1 0x0e;
  infoReply = Match1 0x0f;
  nackFrag = Match1 0x12;
  heartbeatFrag = Match1 0x13;
  data = Match1 0x015;
  dataFrag = Match1 0x16;
}

-- TODO: validate submessage ID
def SubmessageFlags subId = Choose1 {
  dataFlags = {
    endiannessFlag = SubmessageFlag;
    inlineQosFlag = SubmessageFlag;
    dataFlag = SubmessageFlag;
    keyFlag = SubmessageFlag;
    nonStandardPayloadFlag = SubmessageFlag;
    SubmessageFlag; -- TODO: determine if these actually occur
    SubmessageFlag;
    SubmessageFlag;
  };
  dataFragFlags = {
    endiannessFlag = SubmessageFlag;
    inlineQosFlag = SubmessageFlag;
    nonStandardPayloadFlag = SubmessageFlag;
    keyFlag = SubmessageFlag;
    SubmessageFlag; -- TODO: determine if these actually occur
    SubmessageFlag;
    SubmessageFlag;
    SubmessageFlag;
  };
  -- submessages that don't contain data are not supported
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
    Guard (fragmentStartingNum <= fragmentsInSubmessage);

    dataSize = ULong;

    fragmentSize = UShort;
    Guard (fragmentSize < dataSize); 
    Guard (fragmentSize <= 64000);

    inlineQos = Choose1 {
      hasQos = {
        Guard fragFlags.inlineQosFlag;
        ParameterList;
      };
      noQos = Guard (fragFlags.inlineQosFlag == false);
    };

    -- TODO: parse using Chunk
    serializedPayload = Chunk
      ((fragmentsInSubmessage * fragmentSize) as int)
      (Many
        ((fragmentsInSubmessage - fragmentStartingNum) as int)
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
  ^((high << 32) + low);
}

-- Sec 9.4.2.11 ParameterList:

-- Sec 9.6.2.2.2 ParameterID values: Table 9.13:
def ParameterIdT = Choose1 {
    pidPad = {
      Match1 0x00; 
      Match1 0x00; 
    };
    pidSentinel = {
      Match1 0x00; 
      Match1 0x01; 
    };
    pidUserData = {
      Match1 0x00; 
      Match1 0x2c; 
    };
    pidTopicName = {
      Match1 0x00;
      Match1 0x05;
    };
    pidTypeName = {
      Match1 0x00;
      Match1 0x07;
    };
    pidGroupData = {
      Match1 0x00;
      Match1 0x2d;
    };
    pidTopicData = {
      Match1 0x00;
      Match1 0x2e;
    };
    pidDurability = {
      Match1 0x00;
      Match1 0x1d;
    };
    pidDurabilityService = {
      Match1 0x00;
      Match1 0x1e;
    };
    pidDeadline = {
      Match1 0x00;
      Match1 0x23;
    };
    pidLatencyBudget = {
      Match1 0x00;
      Match1 0x27;
    };
    pidLiveliness = {
      Match1 0x00;
      Match1 0x1b;
    };
    pidReliability = {
      Match1 0x00;
      Match1 0x1a;
    };
    pidLifespan = {
      Match1 0x00;
      Match1 0x2b;
    };
    pidDestinationOrder = {
      Match1 0x00;
      Match1 0x25;
    };
    pidHistory = {
      Match1 0x00;
      Match1 0x40;
    };
    pidResourceLimits = {
      Match1 0x00;
      Match1 0x41;
    };
    pidOwnership = {
      Match1 0x00;
      Match1 0x1f;
    };
    pidOwnershipStrength = {
      Match1 0x00;
      Match1 0x06;
    };
    pidPresentation = {
      Match1 0x00;
      Match1 0x21;
    };
    pidPartition = {
      Match1 0x00;
      Match1 0x29;
    };
    pidTimeBasedFilter = {
      Match1 0x00;
      Match1 0x04;
    };
    pidTransportPriority = {
      Match1 0x00;
      Match1 0x49;
    };
    pidDomainId = {
      Match1 0x00;
      Match1 0x0f;
    };
    pidDomainTag = {
      Match1 0x40;
      Match1 0x14;
    };
    pidProtocolVersion = {
      Match1 0x00;
      Match1 0x15;
    };
    pidVendorid = {
      Match1 0x00;
      Match1 0x16;
    };
    pidUnicastLocator = {
      Match1 0x00;
      Match1 0x2f;
    };
    pidMulticastLocator = {
      Match1 0x00;
      Match1 0x30;
    };
    pidDefaultUnicastLocator = {
      Match1 0x00;
      Match1 0x31;
    };
    pidDefaultMulticastLocator = {
      Match1 0x00;
      Match1 0x48;
    };
    pidMetatrafficUnicastLocator = {
      Match1 0x00;
      Match1 0x32;
    };
    pidMetatrafficMulticastLocator = {
      Match1 0x00;
      Match1 0x33;
    };
    pidExpectsInlineQos = {
      Match1 0x00;
      Match1 0x43;
    };
    pidParticipantManualLivelinessCount = {
      Match1 0x00;
      Match1 0x34;
    };
    pidParticipantLeaseDuration = {
      Match1 0x00;
      Match1 0x02;
    };
    pidContentFilterProperty = {
      Match1 0x00;
      Match1 0x35;
    };
    pidParticipantGuid = {
      Match1 0x00;
      Match1 0x50;
    };
    pidGroupGuid = {
      Match1 0x00;
      Match1 0x52;
    };
    pidBuiltinEndpointSet = {
      Match1 0x00;
      Match1 0x58;
    };
    pidBuiltinEndpointQos = {
      Match1 0x00;
      Match1 0x77;
    };
    pidPropertyList = {
      Match1 0x00;
      Match1 0x59;
    };
    pidTypeMaxSizeSerialized = {
      Match1 0x00;
      Match1 0x60;
    };
    pidEntityName = {
      Match1 0x00;
      Match1 0x62;
    };
    pidEndpointGuid = {
      Match1 0x00;
      Match1 0x5a;
    };
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
      cdrBe = {
        Match1 0x00;
        Match1 0x00;
      };
      cdrLe = {
        Match1 0x00;
        Match1 0x01;
      };
      plCdrBE = {
        Match1 0x00;
        Match1 0x02;
      };
      plCdrLE = {
        Match1 0x00;
        Match1 0x03;
      };
      cdr2Be = {
        Match1 0x00;
        Match1 0x10;
      };
      cdr2LE = {
        Match1 0x00;
        Match1 0x11;
      };
      plCdr2Be = {
        Match1 0x00;
        Match1 0x12;
      };
      plCdr2LE = {
        Match1 0x00;
        Match1 0x03;
      };
      dCdrBe = {
        Match1 0x00;
        Match1 0x14;
      };
      dCdrLe = {
        Match1 0x00;
        Match1 0x15;
      };
      xml = {
        Match1 0x00;
        Match1 0x04;
      };
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

