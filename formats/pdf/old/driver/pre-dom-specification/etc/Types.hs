{-# LANGUAGE EmptyDataDecls, TypeOperators, LambdaCase #-}
module Types where

import           Data.Map(Map)
import           Utils

---- hidden types ------------------------------------------------------------

type ByteString = String -- FIXME: update
type P a = IO a -- P is monadic parser, let's just use IO for the moment

---- Types definid in main, so ... -------------------------------------------

type Update = (XRefRaw, TrailerDict)
type XRefEntry = Free :+: (Offset :+: Type2Ref)
type SEEK = Offset -> P () -- type of seek

---- basic types -------------------------------------------------------------
data PdfValue = V_Ref ObjNum GenNum
              | V_Int Int
              | V_Dict Dict
              | V_String ByteString
              -- ...

type Name   = String            -- dictionary keys
type Dict   = [(Name,PdfValue)] -- dups may exist in NCBUR PDFs?
              
-- | The types of PDF Values
data PdfType = T_Int | T_Dict [(Name,PdfType)]
             -- ...
               
type ObjNum = Int
type GenNum = Int
type ObjId  = (ObjNum,GenNum) -- object identifier

type Offset = Int    -- offset from start of "PDF file" (not nec. file offset)
type Len    = Int    -- used as a length


type Version = (Int,Int)

---- types for xref and lower level ------------------------------------------


data Free = Free  -- currently we are ignoring free objects

type XRefRaw       = [SubSectionRaw]
  -- Raw = have not parsed individual Xref entries
type SubSectionRaw = (ObjId,Len,Offset)
                      -- the xref entries are not parsed yet
                      -- the Offset is the file offset of first xref entry
                      -- we assume valid PDF, with length(xrefEntry) = 20

-- Alternative when we allow 19-21 byte xref entries:
--   TODO

---- types for DOM creation --------------------------------------------------

data TopLevelDef' a = TLD_Value  PdfValue
                    | TLD_Stream Dict a -- ^ multiple possibilities for
                                        --   'state' of the Stream
                    | TLD_ObjStm ObjStm -- ^ Stream with "Type ObjStm" is
                                        --   treated specially

data ObjStm = ObjStm [ByteString]
  -- pre-processed ObjStm, can access source of objects efficiently, objects
  -- not parsed.

type TopLevelDef_UnDecStm = TopLevelDef' Offset  -- Undecoded Streams
type TopLevelDef          = TopLevelDef' ByteString -- Streams decoded

type DOM = Map ObjId TopLevelDef

-- | Type2Ref refers to a "compressed object" inside an ObjStm.
--   These only occur in 1.5+ PDFs that use Xref Streams.
data Type2Ref =
  Type2Ref
    { t2_objstm :: ObjNum -- ^ indexes an ObjStm in the DOM (gen == 0)
    , t2_offset :: Int    -- ^ the offset of object in the above ObjStm
    }

---- Dictionary/Misc ---------------------------------------------------------

type TrailerDict = Dict


