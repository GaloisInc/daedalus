--------------------------------------------------------------------------------
-- Futher validation of basic fields
--------------------------------------------------------------------------------

import Stdlib
import ISOCodes
import PdfValue

--------------------------------------------------------------------------------
-- Section 7.9.3: text sterams

-- Parsers for validating text objects:
def Byte = 0 .. 255

-- U+001B: the Unicode escape character
def U001B = { 0 ; 27 }

def HighSurrogate = {
  0xd8 .. 0xdb ; 
  Byte
}

def LowSurrogate = {
  0xdc .. 0xdf ; 
  Byte
}

-- BmpByte: a non-surrogate byte
def BmpByte = 0x00 .. 0xd7 | 0xe0 .. 0xff

def UniChar = Choose1 {
  escape = {
    U001B ;

    -- an ISO language code:
    lang = LanguageCode ;

    -- an optional ISO country code:
    ctry = Optional CountryCode ;

    U001B 
  } ;

  supplementary = {
    big = HighSurrogate ;
    small = LowSurrogate
  } ;

  twoByte = {
    big = BmpByte ;
    small = BmpByte
  } ;
}

def UFEFF = { 254 ; 255 }

-- TextObj: parses a text object
def TextObj = {
  Choose1 {
    -- validate as unicode
    isUnicode = {
      UFEFF ; -- magic number from Sec. 7.9.2.2
      Many UniChar 
    } ;
  
    -- validate as PDFDocEncoding (arbitrary bytes)
    isPdfDoc = Many Byte ;
  } ;

  END
}

--------------------------------------------------------------------------------
-- Section 7.9.4: dates

def BoundedTwoDigits lb ub = {
  @digs = Many 2 Digit ;
  $$ = ^ numBase 10 digs ;
  lb <= $$ ; $$ <= ub
}

def Month = BoundedTwoDigits 1 12

def Minute = BoundedTwoDigits 0 60

def Hour = BoundedTwoDigits 0 60

def Second = BoundedTwoDigits 0 60

def Day = BoundedTwoDigits 1 31

def Date = {
    "D:" ;
    year = Many 4 Digit ;
    month = Month ;
    day = Day ;
    hour = Hour ;
    minute = Minute ;
    second = Second ;
    relLocalTime = Choose {
      plus = @'+' ;
      minus = @'-' ;
      bigZ = @'Z' ;
    } ;
    utOffsetHours = Hour ;
    '\'' ;
    utOffsetMins = Minute ;
}


