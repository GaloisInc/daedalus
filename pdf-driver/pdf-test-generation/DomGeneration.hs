{-| 

   DESIDERATA:

   - You want data types that reflect the internal data structures that allow
     for both good and bad PDFs.
-}

module DomGeneration where

import Data.Word

-- pkg bytestring:
import qualified Data.ByteString as BS

-- local
import Utils

---- lib ---------------------------------------------------------------------

type BS = BS.ByteString  -- bytestream

---- basic PDF types ---------------------------------------------------------

data Value = V_Name String
           | V_Int  Int
           | V_Dict Dict
           | V_String String
           | V_Null
           | V_Array [Value]
           | V_Indirect ObjRef
           | V_Bool Bool
           | V_Raw String     -- render this using 'id'
           deriving (Eq,Ord,Read,Show)
   -- FIXME: hmmm, use GADTs to encode type?
   --  - con:: lose simplicity, read/show, ...

type Header = BS

-- FIXME: dead code
mkStreamObject :: Dict -> Filter -> BS -> StreamObj
mkStreamObject dict f bs = StreamObj (("Length", V_Int l):dict)
                                     f
                                     bs
                           where
                           l = BS.length bs

-- | PDF_DOM - PDF at the DOM level, no choices about xrefs, compressed objects, _
type PDF_DOM = ( BS   -- the first two lines only
               , BS   -- extra 'cavity/etc' lines
               , [TopDecl]
               , Dict
               )

type OID    = Int   -- 10 digits, object identifier (without generation)
type Gen    = Int   -- 5 digits
type ObjRef = (OID, Gen)
type Dict   = [(String, Value)]  -- TODO: unicode?  PDF says unique Strings

-- little lower level
type Offset = Int

-- stream objects (data streams, top level)
data StreamObj = StreamObj { so_dict   :: Dict    -- spec: pg. 46
                           , so_filter :: Filter
                           , so_strm   :: BS
                           }
                 deriving (Eq,Ord,Read,Show)

data Filter = NoFilter | ASCIIHexDecode
              deriving (Eq,Ord,Read,Show)
                
---- XREF and pre-dom types --------------------------------------------------

type Update = ([TopDecl], XRefAndTrailer)  -- useful? mixed levels

type TopDecl = (ObjRef, TopDeclDef)        -- following our ddl naming

-- | TopDeclDef - the right side of top level definitions
--    - more declarative way of distinguishing Object Streams (which are just stream objects)
--    - tihs enforces that object streams can only be at top level

data TopDeclDef =
    Value   Value               -- traditional
  | StrmObj StreamObj           -- just old fashioned streams, must exist only at top level
  | ObjStrm [(OID,TopDeclDef)]  -- 1.5+ , Object streams / compressed objects
                                --  - technically this is just a special kind of StrmObj
                                --  - CAVEAT: an ObjStrm cannot occur recursively here!!
  deriving (Eq,Ord,Read,Show)
         
data XRefAndTrailer = XRAT_Trad [(OID, XRefEntry_Trad)]
                                   -- unique id's
                                   -- there is a minimal form in terms of xref sections
                                Dict
                                   -- trailer dictionary; then startxref and %%EOF
                    | XRAT_Strm [(OID, XRefEntry_Strm)]
                                Dict
                                -- this is rendered as a special stream object
                                -- see 7.5.8.2
                                -- still: startxref and %%EOF
                    deriving (Eq,Ord,Read,Show)

-- Traditional XRefs:
data XRefEntry_Trad =
    XRR_InUse Offset Gen -- 'n' - new
  | XRR_Free  OID Gen    -- 'f' - free (OID for next free object)
  deriving (Eq,Ord,Read,Show)


-- Stream XRefs:
data XRefEntry_Strm = XRES_Free  OID Gen      -- Type0: next free object 
                    | XRES_InUse1 Offset Gen  -- Type1: traditional object reference
                    | XRES_InUse2 OID Word64  -- Type2: object is indexed via object stream
                    deriving (Eq,Ord,Read,Show)
                     
---- abstracting over dictionaries -------------------------------------------
-- simple and non "typeful"

data DictSpec = DictSpec { ds_type    :: String
                         , ds_reqKeys :: [String]
                         , ds_optKeys :: [String]
                         }
                         deriving (Eq,Ord,Read,Show)
  
mkDict :: DictSpec -> [Value] -> Dict -> Dict
mkDict (DictSpec ty rkeys _okeys) rvals dict' =
  ("Type", V_Name ty) : zipSafe rkeys rvals ++ dict'
  -- up to user to provide right keys in right order.
  -- TODO: verify '_okeys'

xRefStrm_Dict = DictSpec "Xref" [ "Size"  
                                , "W"
                                ]
                                [ "Prev"
                                , "Index" -- :: [(Int,Int)] , Default value: [0 Size].
                                ]

---- helper functions for creating DOM objects -------------------------------

vToTopDecl :: OID -> Value -> TopDecl
vToTopDecl a b = ((a,0), Value b)

vToCompDecl :: OID -> Value -> (OID,TopDeclDef)
vToCompDecl a b = (a, Value b)

mk_PDF_DOM h1 h2 root tds =
  (h1, h2, tds, [("Size", V_Int (maxObjectId tds + 1))
                  -- +1 because we also have the 0 object
                  -- DEFACTO: Not getting warns/errors when wrong!
                ,("Root", V_Indirect (root,0))
                ])
  
maxObjectId :: [TopDecl] -> Int
maxObjectId ds = maximum (topIds ++ compressedIds)
  where
  topIds = map (\((x,_),_)->x) ds
  compressedIds = [ i | (_, ObjStrm is) <- ds,  (i,_) <- is]
    
