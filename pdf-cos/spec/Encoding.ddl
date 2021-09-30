-- Encoding: encoding dictionary
import Maybe
import Map
import Stdlib

import GenPdfValue
import Glyph
import PdfValue
import PdfDecl

import StdEncoding
import MacEncoding
import WinEncoding
-- import MacExpertEncoding TODO: define

-- Character encodings (Sec. 9.6.5)

-- Names of pre-defined encodings:
def PredefEncodingName = Choose1 {
  macRoman = @(NameToken "MacRomanEncoding");
  macExpert = @(NameToken "MacExpertEncoding");
  winAnsi = @(NameToken "WinAnsiEncoding");
}

-- PredefEncoding: the encodings for each special encoding
def PredefEncoding (encNm : PredefEncodingName) =
  case encNm of {
    macRoman -> MacEncoding
  ; macExpert -> MacEncoding -- TODO: use MacExpert
  ; winAnsi -> WinEncoding
  }

def partialEncoding (pTy : bool)
  (pBaseEnc : maybe PredefEncodingName)
  (pDiffs : maybe [ uint 8 -> glyph ]) = {
  type = pTy;
  baseEncoding = pBaseEnc;
  differences = pDiffs;
}

def initPartialEncoding = partialEncoding
  false
  nothing
  nothing

def EncAddType (enc : partialEncoding) = partialEncoding
  (Holds (DirectOrRef (NameToken "Encoding")))
  enc.baseEncoding
  enc.differences

def AddBaseEncoding (enc : partialEncoding) = partialEncoding
  enc.type
  (just (DirectOrRef (Token (GenName PredefEncodingName))))
  enc.differences

def Differences = Between "[" "]" {
  @es = Many {
    code = Token UNatural as! uint 8;
    glyphs = Many (glyph (Token Name))
  };
  @codeDiffs = map (ent in es) (
    for (entDict = empty; glyph in ent.glyphs) 
      (Insert (ent.code + ((mapLength entDict) as! uint 8)) glyph entDict)
  );
  for (d = empty : [ uint 8 -> glyph ]; codeDict in codeDiffs)
    (MapUnion d codeDict)
  -- TODO: refine to require maps to be disjoint
}

-- parse difference arrays
def AddDifferences (enc : partialEncoding) = partialEncoding
  enc.type
  enc.baseEncoding
  (just (DirectOrRef Differences))

def ExtendPartialEncoding (k : [ uint 8 ]) (enc : partialEncoding) =
  if k == "Type" then {
    enc.type is false;
    just (EncAddType enc)
  }
  else if k == "BaseEncoding" then {
    enc.baseEncoding is nothing;
    just (AddBaseEncoding enc)
  }
  else if k == "Differences" then {
    enc.differences is nothing;
    just (AddDifferences enc)
  }
  else nothing

def Encoding (enc : partialEncoding) = {
  baseEncoding = enc.baseEncoding;
  differences = maybeDefault empty enc.differences;
}

def StubEncoding = Encoding (partialEncoding false nothing nothing)

def EncodingP = GenPdfDict1
  initPartialEncoding
  ExtendPartialEncoding
  Encoding
