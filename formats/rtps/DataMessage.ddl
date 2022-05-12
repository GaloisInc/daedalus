-- Definition of DDS messages, based on the DDSI-RTPS spec.
import Stdlib

-- TODO: account for endianness encoding modes
def Octet = UInt8

def OctetArray2 = Many 2 Octet

def OctetArray3 = Many 3 Octet

-- Big Endian parsers:
def EndUShort littleEnd = End16 littleEnd
def EndULong littleEnd = End32 littleEnd
def EndLong littleEnd = EndSign32 littleEnd

-- Sec. 8.3.3 The Overall strucutre of an RTPS Message: the overall
-- structure of an RTPS Message consists of a fixed-size leading RTPS
-- Header followed by a variable number of RTPS Submessage
-- parts. Fig. 8.8:
-- DBG:
def Message PayloadData = {
-- def Message = {
  header = Header;
-- DBG:
  submessages = Many (Submessage PayloadData);
--  subm0 = Submessage PayloadData;
--  subm0 = Submessage;
  END
}

-- Sec. 8.3.3.1 Header Structure. Table 8.14:
def Header = {
  protocol = Protocol;
  version = ProtocolVersion;
  vendorId = VendorId;
  guidPrefix = GuidPrefix;
}

-- Sec. 8.3.3.1.1 protocol: value is set to PROTOCOL_RTPS
def Protocol = ProtocolRTPS

-- Sec. 8.3.3.1.2 version:
def ProtocolVersion = {
  major = Octet;
  mintor = Octet;
}

def VendorId = Many 2 Octet

def ProtocolRTPS = Match "RTPS" 

def GuidPrefix = Many 12 Octet

def Submessage PayloadData = {
  subHeader = SubmessageHeader;
  elt = First
          { Guard (subHeader.submessageLength > 0);
            Chunk
              (subHeader.submessageLength as uint 64)
              (SubmessageElement PayloadData subHeader.flags);
            }
          { Guard (subHeader.submessageLength == 0);
            $$ = SubmessageElement PayloadData subHeader.flags;
            Many Octet; -- TODO: this is maybe too sloppy
            }
}

-- Sec 8.3.3.2 Submessage structure: Table 8.15:
def SubmessageHeader = {
  submessageId = SubmessageId;
  flags = SubmessageFlags submessageId;
  submessageLength = EndUShort flags.endiannessFlag;
}

-- Sec 8.3.3.2.1 SubmessageId: a SubmessageKind (Sec 9.4.5.1.1)
def SubmessageId = First
  pad           = @$[0x01]
  acknack       = @$[0x06]
  heartbeat     = @$[0x07]
  gap           = @$[0x08]
  infoTs        = @$[0x09]
  infoSrc       = @$[0x0c]
  infoReplyIP4  = @$[0x0d]
  infoDst       = @$[0x0e]
  infoReply     = @$[0x0f]
  nackFrag      = @$[0x12]
  heartbeatFrag = @$[0x13]
  data          = @$[0x15]
  dataFrag      = @$[0x16]

def NthBit n fs = {
  ^((fs .&. (1 << n)) != 0);
}

-- SubmessageFlags:

def SubmessageFlags (subId: SubmessageId) =
  block
    let flagBits = UInt8
    endiannessFlag = NthBit 0 flagBits
    subFlags =
      First

        ackNackFlags =
          block
            subId is acknack
            finalFlag = NthBit 1 flagBits

        dataFlags =
          block
            subId is data
            inlineQosFlag = NthBit 1 flagBits
            dataFlag = NthBit 2 flagBits
            keyFlag = NthBit 3 flagBits
            Guard (!(dataFlag && keyFlag)) -- invalid combo

            nonStandardPayloadFlag = NthBit 4 flagBits

        dataFragFlags =
          block
            subId is dataFrag
            inlineQosFlag = NthBit 1 flagBits
            keyFlag = NthBit 2 flagBits
            nonStandardPayloadFlag = NthBit 3 flagBits

        gapFlags =
          block
            subId is gap
            groupInfoFlag = NthBit 1 flagBits

        heartBeatFlags =
          block
            subId is heartbeat
            finalFlag = NthBit 1 flagBits
            livelinessFlag = NthBit 2 flagBits
            groupInfoFlag = NthBit 3 flagBits

        heartBeatFragFlags =
          subId is heartbeatFrag

        infoDstFlags =
          subId is infoDst

        infoReplyFlags =
          block
            subId is infoReply
            multicastFlag = NthBit 1 flagBits

        infoSourceFlags =
          subId is infoSrc

        infoTimestampFlags =
          block
            subId is infoTs
            invalidateFlag = NthBit 1 flagBits

        padFlags =
          subId is pad

        nackFragFlags =
          subId is acknack

        infoReplyIP4Flags =
          block
            subId is infoReplyIP4
            multicastFlag = NthBit 1 flagBits


def QosParams littleEnd f = if f then ParameterList littleEnd else []


-- refactor id and flag parsing into this
def SubmessageElement PayloadData (flags: SubmessageFlags) =
  First

    ackNackElt = {
      @ackNackFlags = flags.subFlags is ackNackFlags;
      readerId = EntityId;
      writerId = EntityId;
      readerSNState = SequenceNumberSet flags.endiannessFlag;
      count = Count flags.endiannessFlag;
      }

    dataElt = {
      @dFlags = flags.subFlags is dataFlags;
      Match [0x00, 0x00]; -- extra flags for future compatibility
      octetsToInlineQos = EndUShort flags.endiannessFlag; 

      rdWrs = Chunk (octetsToInlineQos as uint 64)
      {
        readerId = EntityId;
        writerId = EntityId;
        writerSN = SequenceNumber flags.endiannessFlag;
        Guard (writerSN > 0);
      };

      inlineQos = QosParams flags.endiannessFlag dFlags.inlineQosFlag;
      serializedPayload = First
        hasData = {
          Guard dFlags.dataFlag;
          SerializedPayload PayloadData inlineQos;
          }
        hasKey = Guard dFlags.keyFlag;
        noPayload = Guard (!(dFlags.dataFlag || dFlags.keyFlag));
      }

    dataFragElt = {
      @fragFlags = flags.subFlags is dataFragFlags;
      commit;
      Match [0x00, 0x00]; -- extraFlags
      octetsToInlineQos = EndUShort flags.endiannessFlag;

      Chunk (octetsToInlineQos as uint 64) {
      };

      readerId = EntityId;
      writerId = EntityId;

      writerSN = SequenceNumber flags.endiannessFlag;
      Guard (writerSN > 0);

      fragmentStartingNum = FragmentNumber flags.endiannessFlag;
      Guard (0 < fragmentStartingNum);
      
      fragmentsInSubmessage = EndUShort flags.endiannessFlag;
      Guard (fragmentStartingNum <= (fragmentsInSubmessage as uint 32));

      fragmentSize = EndUShort flags.endiannessFlag;
      Guard (fragmentSize <= 64000);

      sampleSize = EndULong flags.endiannessFlag;
      Guard (fragmentSize as uint 32 < sampleSize); 

      inlineQos = QosParams flags.endiannessFlag fragFlags.inlineQosFlag;

      serializedPayload = First
        hasData = {
          Guard (!(fragFlags.keyFlag));
          Chunk
            ((fragmentsInSubmessage * fragmentSize) as uint 64)
            (Many
              ((fragmentsInSubmessage as uint 32 -
                fragmentStartingNum) as uint 64)
              (SerializedPayload PayloadData inlineQos));
          }
        hasKey = Guard fragFlags.keyFlag;
      }

    gapElt = {
      @gapFlags0 = flags.subFlags is gapFlags;
      commit;
      readerId = EntityId;
      writerId = EntityId;

      gapStart = SequenceNumber flags.endiannessFlag;
      Guard (gapStart > 0);

      gapList = SequenceNumberSet flags.endiannessFlag;

      -- WARNING: layout of these is not defined at PSM level. This is a
      -- guess.
      First
        hasGpInfo = {
          Guard (gapFlags0.groupInfoFlag);

          gapStartGSN = SequenceNumber flags.endiannessFlag;
          Guard (gapStartGSN > 0);

          gapEndGSN = SequenceNumber flags.endiannessFlag;
          Guard (gapEndGSN >= gapStartGSN - 1);
          }
        noGpInfo = Guard (!(gapFlags0.groupInfoFlag));


      }

    heartBeatElt = {
      @hbFlags = flags.subFlags is heartBeatFlags;
      commit;
      readerId = EntityId;
      writerId = EntityId;

      firstSN = SequenceNumber flags.endiannessFlag;
      Guard (firstSN > 0);

      lastSN = SequenceNumber flags.endiannessFlag;
      Guard (lastSN >= firstSN - 1);
      count = Count flags.endiannessFlag;

      -- WARNING: layout of these is not defined at PSM level. This is a
      -- guess, based on Table 8.38.
      First
        hasGpInfo = {
          Guard (hbFlags.groupInfoFlag);

          currentGSN = SequenceNumber flags.endiannessFlag;

          firstGSN = SequenceNumber flags.endiannessFlag;
          Guard (firstGSN > 0);
          Guard (currentGSN >= firstGSN);

          lastGSN = SequenceNumber flags.endiannessFlag;
          Guard (lastGSN >= firstGSN - 1);
          Guard (currentGSN >= lastGSN);

          writerSet = GroupDigest;
          secureWriterSet = GroupDigest;
          }
        noGpInfo = Guard (!(hbFlags.groupInfoFlag));
      
      }

    heartBeatFragElt = {
      @hbFragFlags = flags.subFlags is heartBeatFragFlags;
      commit;
      readerId = EntityId;
      writerId = EntityId;

      writerSN = SequenceNumber flags.endiannessFlag;
      Guard (writerSN > 0);
      
      lastFragmentNum = FragmentNumber flags.endiannessFlag;
      Guard (lastFragmentNum > 0);

      GuidPrefix;
      }

    infoDstElt = {
      @infoDstFlags0 = flags.subFlags is infoDstFlags;
      commit;
      GuidPrefix;
      }

    timestampElt = {
      @tsFlags0 = flags.subFlags is infoTimestampFlags;
      First
        hasTs = {
          Guard (!(tsFlags0.invalidateFlag));
          Time flags.endiannessFlag;
          }
        noTs = Guard (tsFlags0.invalidateFlag);
      }

    padElt = {
      @padFlags = flags.subFlags is padFlags;
      }

    nackFragElt = {
      @nackFlags = flags.subFlags is nackFragFlags;
      readerId = EntityId;
      writerId = EntityId;
      writerSN = SequenceNumber flags.endiannessFlag;
      fragmentNumberState = FragmentNumberSet flags.endiannessFlag;
      count = Count flags.endiannessFlag;
      }

    infoReplyIP4Elt = {
      @replyIP4Flags0 = flags.subFlags is infoReplyIP4Flags;
      unicastLocator = LocatorUDPv4 flags.endiannessFlag;
      multicastLocator = LocatorUDPv4 flags.endiannessFlag;
      }

def EntityId = {
  entityKey = OctetArray3;
  entityKind = Octet;
}

def HighBit32 = (1 : uint 32) << 31

-- Sec 9.3.2 Mapping of the Types that Appear Within Submessages...
def SequenceNumber littleEnd = {
  @high = EndULong littleEnd;
  @low = EndULong littleEnd;
  (high .&. ~HighBit32) # low -
  (high .&. HighBit32 as uint 64) << 56 as int
}

def SequenceNumberSet littleEnd = {
  bitmapBase = SequenceNumber littleEnd;
  Guard (bitmapBase >= 1);
  
  numBits = EndULong littleEnd;
  Many ((numBits + 31)/32 as uint 64) (EndLong littleEnd);
}

def GroupDigest = Many 4 Octet

-- Sec 9.4.2.11 ParameterList:

-- Sec 9.6.2.2.2 ParameterID values: Table 9.13:
def ParameterIdT littleEnd = {
  @p = EndUShort littleEnd;
  First
    pidPad = Guard (p == 0x0000)
    pidSentinel = Guard (p == 0x0001)
    pidUserData = Guard (p == 0x002c)
    pidTopicName = Guard (p == 0x0005)
    pidTypeName = Guard (p == 0x0007)
    pidGroupData = Guard (p == 0x002d)
    pidTopicData = Guard (p == 0x002e)
    pidDurability = Guard (p == 0x001d)
    pidDurabilityService = Guard (p == 0x001e)
    pidDeadline = Guard (p == 0x0023)
    pidLatencyBudget = Guard (p == 0x0027)
    pidLiveliness = Guard (p == 0x001b)
    pidReliability = Guard (p == 0x001a)
    pidLifespan = Guard (p == 0x002b)
    pidDestinationOrder = Guard (p == 0x0025)
    pidHistory = Guard (p == 0x0040)
    pidResourceLimits = Guard (p == 0x0041)
    pidOwnership = Guard (p == 0x001f)
    pidOwnershipStrength = Guard (p == 0x0006)
    pidPresentation = Guard (p == 0x0021)
    pidPartition = Guard (p == 0x0029)
    pidTimeBasedFilter = Guard (p == 0x0004)
    pidTransportPriority = Guard (p == 0x0049)
    pidDomainId = Guard (p == 0x000f)
    pidDomainTag = Guard (p == 0x4014)
    pidProtocolVersion = Guard (p == 0x0015)
    pidVendorId = Guard (p == 0x0016)
    pidUnicastLocator = Guard (p == 0x002f)
    pidMulticastLocator = Guard (p == 0x0030)
    pidDefaultUnicastLocator = Guard (p == 0x0031)
    pidDefaultMulticastLocator = Guard (p == 0x0048)
    pidMetatrafficUnicastLocator = Guard (p == 0x0032)
    pidMetatrafficMulticastLocator = Guard (p == 0x0033)
    pidExpectsInlineQos = Guard (p == 0x0043)
    pidParticipantManualLivelinessCount = Guard (p == 0x0034)
    pidParticipantLeaseDuration = Guard (p == 0x0002)
    pidContentFilterProperty = Guard (p == 0x0035)
    pidParticipantGuid = Guard (p == 0x0050)
    pidGroupGuid = Guard (p == 0x0052)
    pidBuiltinEndpointSet = Guard (p == 0x0058)
    pidBuiltinEndpointQos = Guard (p == 0x0077)
    pidPropertyList = Guard (p == 0x0059)
    pidTypeMaxSizeSerialized = Guard (p == 0x0060)
    pidEntityName = Guard (p == 0x0062)
    pidEndpointGuid = Guard (p == 0x005a)

    -- Table 9.15:
    pidContentFilterInfo = Guard (p == 0x0055)
    pidCoherentSet = Guard (p == 0x0056)
    pidDirectedWrite = Guard (p == 0x0057)
    pidOriginalWriterInfo = Guard (p == 0x0061)
    pidGroupCoherentSet = Guard (p == 0x0063)
    pidGroupSeqNum = @Guard (p == 0x0064)
    pidWriterGroupInfo = Guard (p == 0x0065)
    pidSecureWriterGroupInfo = Guard (p == 0x0066)
    pidKeyHash = Guard (p == 0x0070)
    pidStatusInfo = Guard (p == 0x0071)
 
}

-- Table 9.13:
def ParameterIdValues littleEnd (pid: ParameterIdT) =
  case pid of

    pidPad -> {| padVal = Many Octet |}
    -- sentinel: not allowed

    pidUserData -> {| userDataVal = UserDataQosPolicy |}

    pidTopicName -> {| topicNameVal = String256 littleEnd |}

    pidTypeName -> {| typeNameVal = String256 littleEnd |}

    pidGroupData -> {| groupDataVal = GroupDataQosPolicy |}

    pidTopicData -> {| topicDataVal = TopicDataQosPolicy |}

    pidDurability -> {| durabilityVal = DurabilityQosPolicy |}

    pidDurabilityService ->
      {| durabilityServiceVal = DurabilityServiceQosPolicy |}

    pidDeadline -> {| deadlineVal = DeadlineQosPolicy |}

    pidLatencyBudget -> {| latencyBudgetVal = LatencyBudgetQosPolicy |}

    pidLiveliness -> {| livelinessVal = LivenessQosPolicy |}

    pidReliability -> {| reliabilityVal = ReliabilityQosPolicy |}

    pidLifespan -> {| lifespanVal = LifespanQosPolicy |}

    pidDestinationOrder -> {| destinationOrderVal = DestinationOrderQosPolicy |}

    pidHistory -> {| historyVal = HistoryQosPolicy |}

    pidResourceLimits -> {| resourceLimitsVal = ResourceLimitsQosPolicy |}

    pidOwnership -> {| ownershipVal = OwnershipQosPolicy |}

    pidOwnershipStrength ->
      {| ownershipStrengthVal = OwnershipStrengthQosPolicy |}

    pidPresentation -> {| presentationVal = PresentationQosPolicy |}

    pidPartition -> {| partitionVal = PartitionQosPolicy |}

    pidTimeBasedFilter -> {| timeBasedFilterVal = TimeBasedFilterQosPolicy |}

    pidTransportPriority ->
      {| transportPriorityVal = TransportPriorityQosPolicy |}

    pidDomainId -> {| domainIdVal = DomainId littleEnd |}

    pidDomainTag -> {| domainTagVal = String256 littleEnd |}

    pidProtocolVersion -> {| protocolVersionVal = ProtocolVersion |}

    pidVendorId -> {| vendorIdVal = VendorId |}

    pidUnicastLocator -> {| unicastLocatorVal = Locator littleEnd |}

    pidMulticastLocator -> {| multicastLocatorVal = Locator littleEnd |}

    pidDefaultUnicastLocator -> {| defaultUnicastLocatorVal = Locator littleEnd |}

    pidDefaultMulticastLocator ->
      {| multicastDefaultLocatorVal = Locator littleEnd |}

    pidMetatrafficUnicastLocator ->
      {| metatrafficUnicastLocatorVal = Locator littleEnd |}

    pidMetatrafficMulticastLocator ->
      {| metatrafficMulticastLocatorVal = Locator littleEnd |}

    pidExpectsInlineQos ->
      {| expectsInlineQos = Boolean |}

    pidParticipantManualLivelinessCount ->
      {| participantManualLivelinessCountVal = Count littleEnd |}

    pidParticipantLeaseDuration ->
      {| participantLeaseDurationVal = Duration littleEnd |}

    pidContentFilterProperty ->
      {| contentFilterPropertyVal = ContentFilterProperty littleEnd |}

    pidParticipantGuid -> {| participantGUIDVal = GUIDT |}

    pidGroupGuid -> {| groupGUIDVal = GUIDT |}

    pidBuiltinEndpointSet -> {| builtinEndpointSetVal = BuiltinEndpointSetT |}

    pidBuiltinEndpointQos -> {| buildtinEndpointQosVal = BuiltinEndpointQosT |}

    pidPropertyList -> {| propertyListVal = Many PropertyT |}

    pidTypeMaxSizeSerialized ->
      {| typeMaxSizeSerializedVal = EndULong littleEnd |}

    pidEntityName -> {| entityNameVal = EntityName |}

    pidEndpointGuid -> {| endpointGuidVal = GUIDT |}

    pidContentFilterInfo ->
      {| contentFilterInfoVal = ContentFilterInfo littleEnd |}

    pidCoherentSet -> {| coherentSetVal = SequenceNumber littleEnd |}

    pidDirectedWrite -> {| directedWriteVal = GUIDT |}

    pidOriginalWriterInfo ->
      {| orignalWriterInfoVal = OriginalWriterInfo littleEnd |}

    pidGroupCoherentSet ->
      {| groupCoherentSetVal = SequenceNumber littleEnd |}

    pidGroupSeqNum -> {| groupSeqNumVal = SequenceNumber littleEnd |}

    pidWriterGroupInfo -> {| writerGroupInfoVal = WriterGroupInfo |}

    pidSecureWriterGroupInfo -> {| secureWriterGroupInfoVal = WriterGroupInfo |}

    pidKeyHash -> {| keyHashVal = KeyHash |}

    pidStatusInfo -> {| statusInfoVal = StatusInfo |}

def ContentFilterInfo littleEnd = {
  numBitmaps = EndULong littleEnd;
  bitmaps = Many (numBitmaps as uint 64) (EndLong littleEnd);
  numSigs = EndULong littleEnd;
  signatures = Many (numSigs as uint 64) (FilterSignature littleEnd);
}

def OriginalWriterInfo littleEnd = {
  originalWriterGUID = GUIDT;
  originalWriterSN = SequenceNumber littleEnd;
}

def WriterGroupInfo = {
  writerSet = GroupDigest;
}

def FilterSignature littleEnd = Many 4 (EndLong littleEnd)

def KeyHash = Many 16 Octet

def StatusInfo = Many 4 Octet

-- relaxed definition. Refine as needed.
def UserDataQosPolicy = Many Octet

def BndString littleEnd n = {
  @len = EndULong littleEnd ;
  Guard (len <= n);
  $$ = Many (len as uint 64) Octet;
  $[0x00]
}

def String256 littleEnd = BndString littleEnd 256

-- relaxed definition. Refine as needed.
def GroupDataQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def TopicDataQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def DurabilityQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def DurabilityServiceQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def DeadlineQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def LatencyBudgetQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def LivenessQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def ReliabilityQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def LifespanQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def DestinationOrderQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def HistoryQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def ResourceLimitsQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def OwnershipQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def OwnershipStrengthQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def PresentationQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def PartitionQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def TimeBasedFilterQosPolicy = Many Octet

-- relaxed definition. Refine as needed.
def TransportPriorityQosPolicy = Many Octet

def ContentFilter = Many Octet

-- relaxed definition. Refine as needed.
def DomainId littleEnd = EndULong littleEnd

def Locator littleEnd = {
  kind = EndLong littleEnd;
  port = EndULong littleEnd;
  address = Many 16 Octet;
}

def LocatorList littleEnd = {
  numLocators = EndULong littleEnd;
  Many (numLocators as uint 64) (Locator littleEnd);
}

def LocatorUDPv4 littleEnd = {
  address = EndULong littleEnd;
  port = EndULong littleEnd;
}

def Count littleEnd = EndLong littleEnd

def Duration littleEnd = {
  seconds = EndLong littleEnd;
  fraction = EndULong littleEnd;
}

def ContentFilterProperty littleEnd = {
  contentFilteredTopicName = String256 littleEnd;
  relatedTopicName = String256 littleEnd;
  filterClassName = String256 littleEnd;
  filterExpression = String; -- littleEnd;
   -- XXX: String does not take param, so this was broken not, sure if it
    -- should be String256 instead...
  expressionParameters = Many String;
}

def Time littleEnd = {
  seconds = EndULong littleEnd;
  fraction = EndULong littleEnd;
}

def GUIDT = {
  guidPrefix = GuidPrefix;
  entityId = EntityId;
}

-- relaxed definition. Refine as needed.
def BuiltinEndpointSetT = Many Octet

-- relaxed definition. Refine as needed.
def BuiltinEndpointQosT = Many Octet

-- relaxed definition. Refine as needed.
def PropertyT = Many Octet

def EntityName = String

def Parameter littleEnd = {
  parameterId = ParameterIdT littleEnd;
  -- TODO: clean this up
  Guard (parameterId != {| pidSentinel = {} |});

  len = EndUShort littleEnd;
  Guard (len % 4 == 0);

  val = Chunk (len as uint 64) (ParameterIdValues littleEnd parameterId);
}

def Sentinel littleEnd = {
  @pId = ParameterIdT littleEnd;
  pId is pidSentinel;
  EndUShort littleEnd;
}

def ParameterList littleEnd = {
  $$ = Many (Parameter littleEnd);
  Sentinel littleEnd;
}

-- Sec 10 Serialized Payload Representation:
def SerializedPayload PayloadData qos = {
  payloadHeader = SerializedPayloadHeader;
  data = PayloadData qos;
}

def SerializedPayloadHeader =
  block

    representationIdentifier =
      -- There is only one alternative here... ???
      First
        userDefinedTopicData =
          First
            cdrBe     = @Match [0x00, 0x00]
            cdrLe     = @Match [0x00, 0x01]
            plCdrBE   = @Match [0x00, 0x02]
            plCdrLE   = @Match [0x00, 0x03]
            cdr2Be    = @Match [0x00, 0x10]
            cdr2LE    = @Match [0x00, 0x11]
            plCdr2Be  = @Match [0x00, 0x12]
            plCdr2LE  = @Match [0x00, 0x03]
            dCdrBe    = @Match [0x00, 0x14]
            dCdrLe    = @Match [0x00, 0x15]
            xml       = @Match [0x00, 0x04]

    representationOptions = OctetArray2;

-- Sec 9.3.2:
def FragmentNumber littleEnd = EndULong littleEnd

-- Sec 9.4.2.8:
def FragmentNumberSet littleEnd = {
  bitmapBase = FragmentNumber littleEnd;
  numBits = EndULong littleEnd;
  bitmap = Many ((numBits + 31)/32 as uint 64)
    (EndLong littleEnd);
}
