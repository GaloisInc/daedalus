import Daedalus

-- Utilities

def Length =
def Crc =

def FLAG =

def NullChar    =
def NonNullChar =

def OMany (omin:maybe (uint 64)) (omax:maybe (uint 64)) P =

def NTString (omin:maybe (uint 64)) (omax:maybe (uint 64)) =

def RGB =

def UTCTime =

-- Chunks / PNG

bitdata ChunkType where
  -- Your solution here

def ChunkData sig (type : ChunkType) =

def PLTEChunkData =

def IDATChunkData =

def TRNSChunkData sig =

def TRNSData0 =

def TRNSData2 =

def TRNSData3 =

def CHRMChunkData =

def GAMAChunkData =

def ICCPChunkData =

def SBITChunkData sig =

def SBITData0 =

def SBITData2or3 =

def SBITData4 =

def SBITData6 =

def SRGBChunkData =

def TEXTChunkData =

def ZTXTChunkData =

def ITXTChunkData =

def BKGDChunkData sig =

def BKGDData0or4 =

def BKGDData2or6 =

def BKGDData3 =

def HISTChunkData =

def PHYSChunkData =

def SPLTChunkData =

def SPLTSample (depth : uint 8) =

def SPLTSample8 =

def SPLTSample16 =

def TIMEChunkData =

def PNGChunk sig =

def IHDRChunk =

def IENDChunk =

def PNGHeader =

def Main =
