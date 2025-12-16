-- A partial specification for the JP2 file format.

-- The specification covers the complete super structure of JP2 files
-- (i.e. the boxes and marker segment structures), but only "interprets" and
-- validates a subset thereof. Unparsed boxes and segments are read and their
-- data collected as raw bytes.

-- NOTE: By default, the raw data from JPEG 2000 tiles are stored and exposed
-- as arrays of bytes, which is likely what is needed for a practical parser generated
-- from this specification. However, if we are using the daedalus interpreter to
-- work with JP2 files, this may cause unnecessary clutter. Turning the implicit
-- parameter `?collectRawTileData` below to `false` will avoid exposing this data
-- and produce more readable output on the terminal.

import Daedalus
import Common


def Main =
    block
        let ?collectRawTileData = true
        JP2

-- The format consists of a series of boxes. The first two must be a
-- signature box followed by a file type box.
def JP2 =
    block
        ValidSignatureBox
        fileType = FileTypeBox
        boxes = Many JP2Box

-- The signature box contents are fixed
def ValidSignatureBox =
    block
        let len     = Match [0x0, 0x0, 0x0, 0xC]     -- Length should be 12
        let type    = Match ['j', 'P', 0x20, 0x20]   -- Fixed block type
        let content = Match [0xD, 0xA, 0x87, 0xA]    -- <CR><LF>0x87<LF>
        ^ true

-- The file type box
def FileTypeBox = BoxOfType "ftyp" FileTypeBoxContent

def FileTypeBoxContent =
    block
        brand         = FourByteString
        minorVersion  = BEUInt32         -- Should be zero, but apparently you are supposed to continue parsing anyway
        compatibility = Many FourByteString
        -- Verify that compatibility list contains at least one: entry of the form 'jp2\040'
        let index     = indexOf ['j', 'p', '2', 0x20] compatibility
        Guard ( index != -1 )

-- Parse any of the possible boxes
-- In addition to a defined set of boxes, this will also parse unknown
-- boxes, by simply collecting all the bytes that belong to the box without
-- attempting to interpret the contents
def JP2Box =
    First
        jp2header   = JP2HeaderBox
        codestream  = ContiguousCodestreamBox
        uuidInfoBox = UUIDInfoBox
        unknownBox = Box (Many UInt8)

-- The JP2 Header box
def JP2HeaderBox = BoxOfType "jp2h" JP2HeaderBoxContent

-- The contents of the header box should being with an image header
-- and can contain other sub boxes
def JP2HeaderBoxContent =
    block
        imageHeader = ImageHeaderBox
        boxes       = Many JP2HeaderSubBox

-- Parse any of the possible boxes
-- Once again we support unknown boxes here
def JP2HeaderSubBox =
    block
        First
            imageHeader        = ImageHeaderBox  -- This is allowed, but is expected to be ignored
            colorSpecification = ColorSpecificationBox
            unknownBox         = Box (Many UInt8) -- For boxes we don't know of, just store the bytes

-- The image header box
def ImageHeaderBox = BoxOfType "ihdr" ImageHeaderBoxContent

def ImageHeaderBoxContent =
    block
        height             = BEUInt32
        width              = BEUInt32
        numberOfComponents = BEUInt16
        bitsPerComponent   = UInt8        -- Note: Informs other boxes
        compressionType    = $[0x7]       -- Fixed value for compression type
        colorspaceKnown    = BooleanFlag
        ipRights           = BooleanFlag

-- The color specification box
def ColorSpecificationBox = BoxOfType "colr" ColorSpecificationBoxContent

def ColorSpecificationBoxContent =
    block
        method     = ColorSpaceSpecificationType
        precdence  = UInt8  -- Should be zero, but parsers are expected to ignore it
        approx     = UInt8  -- Should be zero, but parsers are expected to ignore it
        colorspace = ColorSpaceSpecification method

-- Type of the color specification. Can be a pre-specified one or a restricted ICC profile
def ColorSpaceSpecificationType =
    block
        case UInt8 of
            0x1 -> {| enumeratedColorSpace |}
            0x2 -> {| restrictedICCProfile |}
            _   -> {| unknown |}

-- Read the actual color space depending on the type
def ColorSpaceSpecification (method : ColorSpaceSpecificationType) =
    block
        case method of
            enumeratedColorSpace -> {| enumeratedColorSpace = EnumeratedColorSpace |}
            restrictedICCProfile -> {| iccProfile = ICCProfile |}
            _                    -> {| unknownColorSpaceData = Many UInt8 |}

-- Determine which of the pre-specified color spaces has been specified.
def EnumeratedColorSpace =
    block
        let val = BEUInt32
        case val of
            16 -> {| sRGB |}
            17 -> {| greyscale |}
            18 -> {| sYCC |}
            _  -> {| unknown |}

-- TODO: Real ICC profile ?
def ICCProfile = Many UInt8

def UUIDInfoBox = BoxOfType "uinf" UUIDInfoBoxContents

def UUIDInfoBoxContents =
    block
        uuidList     = UUIDListBox
        dataEntryURL = DataEntryURLBox

def UUIDListBox = BoxOfType "ulst" UUIDListBoxContent

def UUIDListBoxContent =
    block
        count = BEUInt16 as? uint 64
        uuids = Many count UUID
        END

def UUID = Many 16 UInt8

def DataEntryURLBox = BoxOfType ['u', 'r', 'l', 0x20] DataEntryURLBoxContent

def DataEntryURLBoxContent =
    block
        version = $[0x0]
        flag    = { Match [0x0, 0x0, 0x0] ; ^ 0 : uint 32 }
        local   = UTF8NTString
        -- After that we can still have bytes, just ignore it
        Many UInt8

-- The stream box containing the data stream that makes up the image
def ContiguousCodestreamBox = BoxOfType "jp2c" ContiguousCodestreamBoxContent

def ContiguousCodestreamBoxContent =
    block
        SOC
        mainHeaderMarkerSegments = UntilMarker sotId MainHeaderMarkerSegment
        tileParts = Many TilePart
        EOC

def MainHeaderMarkerSegment =
    First
        sizSegment = SIZSegment
        codSegment = CODSegment
        qcdSegment = QCDSegment
        comSegment = COMSegment
        unknownSegment = MarkerSegment (Many UInt8)


def sotId = 0x90

def sodId = 0x93

def eocId = 0xD9

def SOC = Marker 0x4F

def SOT = Marker sotId

def SOD = Marker sodId

def EOC = Marker eocId

-- The SIZ marker segment that describes image and tile sizes
def SIZSegment = MarkerSegmentOfType 0x51  SIZParameters

def SIZParameters =
    block
        rSiz = BEUInt16  -- TODO: Capabilities, can be interpreted better, but complex
        xSiz = NumericParserWithRange BEUInt32 1 0xFFFFFFFF
        ySiz = NumericParserWithRange BEUInt32 1 0xFFFFFFFF
        xoSiz = NumericParserWithRange BEUInt32 0 0xFFFFFFFE
        yoSiz = NumericParserWithRange BEUInt32 0 0xFFFFFFFE
        xtSiz = NumericParserWithRange BEUInt32 1 0xFFFFFFFF
        ytSiz = NumericParserWithRange BEUInt32 1 0xFFFFFFFF
        xtoSiz = NumericParserWithRange BEUInt32 0 0xFFFFFFFE
        ytoSiz = NumericParserWithRange BEUInt32 0 0xFFFFFFFE
        cSiz = NumericParserWithRange BEUInt16 1 16384
        componentInfo = Many (1..) ComponentBlock

def ComponentBlock =
    block
        sSiz = SSiz
        yrSiz = UInt8
        xrSiz = UInt8

def SSiz =
    block
        let data = UInt8 as? SSizData
        case data of
            SSizData d ->
                case d.signed of
                    0x1 -> {| signed = d.bitDepth + 1 |}
                    0x0 -> {| unsigned = d.bitDepth + 1 |}

bitdata SSizData where
  SSizData = { signed : uint 1, bitDepth : uint 7 }

-- Default coding style marker segment
def CODSegment = MarkerSegmentOfType 0x52 CODSegmentParameters

def CODSegmentParameters =
    block
        sCod = SCod
        sgCod = SGCod
        spCod = SPCod sCod

def SCod =
    block
        let data = UInt8 as? SCodData
        case data of
            SCodData d ->
                block
                    entropyCoder = EntropyCoder d.entropyCoder
                    sopMarker = toBoolean d.sopMarker
                    ephMarker = toBoolean d.ephMarker

bitdata SCodData where
  SCodData = { reserved : uint 5
             , ephMarker : uint 1
             , sopMarker : uint 1
             , entropyCoder : uint 1
             }

def SGCod =
    block
        progressionOrder = ProgressionOrder
        numberOfLayers   = BEUInt16
        multipleComponentTransformation = MultipleComponentTransformation

def EntropyCoder (v : uint 1) =
    if (v > 0)
        then {| specifiedEntropy |}
        else {| fixedEntropy |}

def ProgressionOrder =
    block
        case UInt8 of
            0x0 -> {| layerResolutionLevelComponentPositionProgression |}
            0x1 -> {| resolutionLevelLayerComponentPositionProgression |}
            0x2 -> {| resolutionLevelPositionComponentLayerProgression |}
            0x3 -> {| positionComponentResolutionLevelLayerProgression |}
            0x4 -> {| componentPositionResolutionLevelLayerProgression |}
            _   -> {| reserved |}

def MultipleComponentTransformation =
    block
        case UInt8 of
            0x0 -> {| noMultipleComponentTransformation |}
            0x1 -> {| predefinedComponentTransformation |}


def SPCod sCod =
    block
        numberOfDecompositionLevels = UInt8
        codeBlockWidthExponent = CodeBlockExponent
        codeBlockHeightExponent = CodeBlockExponent
        Guard ( codeBlockWidthExponent + codeBlockHeightExponent <= 12 )
        codeBlockStyle = UInt8 as? CodeBlockStyle
        transformation = Transformation
        precinctSizes = PrecinctSizes sCod.entropyCoder

def CodeBlockExponent =
    block
        let val = UInt8
        Guard ( val >= 0 && val <= 8 )
        ^ (val + 2)

bitdata CodeBlockStyle where
  CodeBlockStyle = { unused : uint 2
                   , segmentationSymbolsUsed: uint 1
                   , predictableTermination: uint 1
                   , verticallyCausalContext: uint 1
                   , terminationOnEachCodingPass: uint 1
                   , resetContextProbabilitiesOnBoundaries: uint 1
                   , selectiveArithmeticCodingByPass: uint 1
                   }

def Transformation =
    block
        case UInt8 of
            0x0 -> {| nineSevenIrreversibleFilter |}
            0x1 -> {| fiveThreeReversibleFilter |}


def PrecinctSizes ( entropyCoder : EntropyCoder ) : [PrecinctSizeXandY] =
    block
        case entropyCoder of
            fixedEntropy -> []
            specifiedEntropy -> Many PrecinctSizeXandY


def PrecinctSizeXandY =
    block
        let data = UInt8 as? PrecinctSizeXandYBits
        case data of
            PrecinctSizeXandYBits d ->
                block
                    ppX = d.ppX as uint 8
                    ppY = d.ppY as uint 8

bitdata PrecinctSizeXandYBits where
    PrecinctSizeXandYBits = { ppX : uint 4, ppY : uint 4 }

-- Quantization defaults marker segment
def QCDSegment = MarkerSegmentOfType 0x5C QCDParameters

def QCDParameters =
    block
        sqcd  = Sqcd
        spqcd = SPqcd sqcd

def Sqcd =
    block
        let data = UInt8 as? SqcdBits
        case data of
            SqcdBits d ->
                block
                    guardBits = d.guardBits as uint 8
                    quantizationStyle =
                        block
                            case d.quantizationStyle of
                                0x0 -> {| noQuantization |}
                                0x1 -> {| scalarDerived |}
                                0x2 -> {| scalarExpounded |}

bitdata SqcdBits where
    SqcdBits = { guardBits : uint 3, quantizationStyle : uint 5 }

def SPqcd (sqcd : Sqcd) =
    block
        case sqcd.quantizationStyle of
            noQuantization   -> {| reversibleStepSize = UInt8 |}   -- Question: Should we strip the last 3 bits ? Not quite sure
            scalarDerived    -> {| quantizationValue = SPqcdQuantizationValue |}
            scalarExpounded  -> {| quantizationValue = SPqcdQuantizationValue |}

def SPqcdQuantizationValue =
    block
        let data = BEUInt16 as? SPqcdQuantizationValueBits
        case data of
            SPqcdQuantizationValueBits d ->
                block
                    mantissa = d.mantissa as uint 16
                    exponent = d.exponent as uint 16

bitdata SPqcdQuantizationValueBits where
    SPqcdQuantizationValueBits = { exponent : uint 5, mantissa : uint 11 }

-- Comment segment
def COMSegment = MarkerSegmentOfType 0x64 COMParameters

def COMParameters =
    block
        registrationValue =
            block
                case BEUInt16 of
                    0x0 -> {| binary |}
                    0x1 -> {| latin |}
                    _   -> {| reserved |}
        comment = Many UInt8

-- A tile part
-- It starts with a `start of tile` marker segment, followed by headers a
-- `start of data` marker and then a byte stream which lasts until
-- the start of the next tile part or the end of the coding stream
-- TODO: Differentiate between first tile part header and the rest; there are
--   more marker segments possible in the first tile part than the rest
def TilePart =
    block
        -- To validate lengths, capture the offset before we start parsing
        let start = Offset

        -- Parse the initial part of the tile
        sotSegment = SOTSegment
        headers = UntilMarker sodId TilePartHeaderSegment
        SOD

        -- To validate lengths, once again capture the offset
        let beforeData = Offset  -- And this offset

        -- Read data as raw bytes until we hit an end condition
        -- We only expose the raw tile data as raw bytes based on an implicit
        -- parameters
        let data = Many ByteUnlessTileEnd
        dataLength = length data
        rawData = if ?collectRawTileData then data else []

        -- Verify that the expected length of the data is same what we expect
        let expectedSize = sotSegment.parameters.pSot as uint 64 - (beforeData - start)
        Guard ( dataLength == expectedSize )


def TilePartHeaderSegment =
    First
        comSegment     = COMSegment
        unknownSegment = MarkerSegment (Many UInt8)

-- The `start of tile` (SOT) segment
def SOTSegment = MarkerSegmentOfType sotId SOTParameters

def SOTParameters =
    block
        iSot = BEUInt16
        pSot = BEUInt32
        Guard ( pSot == 0 || pSot >= 14 )
        tpSot = NumericParserWithRange UInt8 0 254
        tnSot = TNSot

def TNSot =
    block
        let val = UInt8
        case val of
            0x0 -> {| undefined |}
            _   -> {| tileParts = val |}


-- Read bytes until we hit either a SOT marker or a EOC marker
-- (i.e. the next tile part or the end of the code stream)
def ByteUnlessTileEnd =
    block
        let bytes = LookAhead { first = UInt8 ; second = UInt8 }
        case bytes.first of
            0xFF ->
                block
                    (bytes.second == sotId || bytes.second == eocId) is false
                    UInt8
            _ -> UInt8
