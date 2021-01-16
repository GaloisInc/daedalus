module Daedalus.ParserGen.AST where

-- import Data.Text(Text)
-- import Debug.Trace

-- import Data.ByteString(ByteString)
-- import Data.Word
import qualified Data.Map as Map

import Daedalus.Type.AST hiding (ppBinder)
import Daedalus.SourceRange

-- import Daedalus.Normalise.AST (NType)

type Contx = Name

data Annot = Annot
  { annotStates :: [Int]
  , annotContx :: Maybe Contx
  }
  deriving (Show)

emptyAnnot :: Annot
emptyAnnot = Annot { annotStates = [], annotContx = Nothing}

mkAnnot :: [Int] -> Contx -> Annot
mkAnnot lst ctx =
  Annot {annotStates = lst, annotContx = Just ctx}

getState :: Annot -> Int -> Int
getState ann i = (annotStates ann) !! i

statesToAnnot :: [Int] -> Annot
statesToAnnot lst = Annot { annotStates = lst, annotContx = Nothing}

getStatesLength :: Annot -> Int
getStatesLength ann = length $ annotStates ann

nameToContx :: Name -> Contx
nameToContx n = n

-- No type variables.
--data NType = NType (TypeF NType)
--  deriving (Show)


-- Only Value variables
-- data NName = NName { nName :: Name } --, nType :: NType }
--   deriving (Eq, Ord)

-- instance (Show NName) where
--   show x =
--     case (nameScope (nName x)) of
--       Local str -> show str
--       Unknown str -> show str
--       ModScope _ i -> show i


-- data NCExpr =
--     NSetAny
--   | NSetSingle NVExpr
--   | NSetComplement NCExpr
--   | NSetUnion  [NCExpr]
--   | NSetOneOf  ByteString
--   | NSetDiff   NCExpr NCExpr
--   | NSetRange  NVExpr NVExpr
--   | NCCall     NName [NVExpr]
--   | NCLet      NName NVExpr NCExpr
--   | NCFor NName NVExpr NName NVExpr NCExpr
--   deriving (Show)

-- data NVExpr =
--     NVLet NName NVExpr NVExpr
--   | NCoerce NType NType NVExpr
--   | NNumber Integer NType
--   | NBool   Bool
--   | NNothing NType
--   | NJust NVExpr
--   | NByte   Word8
--   | NStruct [ (Label, NVExpr) ] NType
--   | NUnit
--   | NByteArray ByteString
--   | NArray     [NVExpr] NType
--   | NMapEmpty NType
--   | NIn Label NVExpr NType
--   | NBinOp BinOp NVExpr NVExpr
--   | NUniOp UniOp NVExpr
--   | NSelStruct NVExpr Label NType
--   | NIf        NVExpr NVExpr NVExpr
--   | NVCall     NName [NVExpr]
--   | NVFor NName NVExpr NName NVExpr NVExpr
--   | NVar NName
--   deriving (Show)

-- type NGExpr = (NGExprCstr, Annot)
-- data NGExprCstr =
--     NGPure NVExpr
--   | NLabel Text NGrammar
--   | NGuard NVExpr
--   | NCurrnetStream
--   | NSetStream NVExpr
--   | NStreamLen WithSem NVExpr NVExpr
--   | NStreamOff WithSem NVExpr NVExpr

--   | NGetByte WithSem
--   | NMatch WithSem NCExpr
--   | NMatchBytes WithSem NVExpr
--   | NChoice Commit [NGrammar] NType
--   | NOptional Commit NGrammar
--   | NMany WithSem Commit (ManyBounds NVExpr) NGrammar
--   | NEnd
--   | NOffset
--   | NMapLookup WithSem NVExpr NVExpr
--   | NMapInsert WithSem NVExpr NVExpr NVExpr
--   | NCoerceCheck WithSem NType NType NVExpr
--   | NSelUnion WithSem NVExpr Label NType
--   | NSelJust WithSem NVExpr NType
--   | NGFor NName NVExpr NName NVExpr NGrammar
--   | NGMap NName NVExpr NGrammar
--   | NGCall NName [NVExpr]
--   | NGErrorMode Commit NGrammar
--   | NGFail (Maybe NVExpr) NType
--   deriving (Show)

-- type NGrammar = (NGrammarCstr, Annot)
-- data NGrammarCstr =
--   NPure NVExpr | NBind (Maybe NName) NGExpr NGrammar
--   deriving (Show)

-- type NDeclBody = (NDeclBodyCstr, Annot)
-- data NDeclBodyCstr  =
--     NCDecl NCExpr
--   | NVDecl NVExpr
--   | NGDecl NGrammar
--   | NExtern
--   deriving (Show)

-- type NDecl = (NDeclCstr, Annot)
-- data NDeclCstr = NDecl { nDeclName     :: !Name
--                        -- , nDeclCtrs     :: ![Constraint]
--                        , nDeclParams   :: ![NName]
--                        , nDeclType     :: NType
--                        , nDeclDef      :: !NDeclBody
--                        }
--   deriving (Show)


-- type GblAlloc = Map.Map Name NDecl

-- data CorV =
--     ClassExpression NCExpr
--   | ValueExpression NVExpr
--   deriving (Show)

-- type GblFuns = Map.Map Name ([NName], CorV)

type NCExpr = TC (SourceRange, Annot) Class
type NVExpr = TC (SourceRange, Annot) Value


type GblAlloc = Map.Map Name (TCModule (SourceRange, Annot))

data CorV =
    ClassExpr (TC (SourceRange, Annot) Class)
  | ValueExpr (TC (SourceRange, Annot) Value)
  deriving (Show)

type GblFuns = Map.Map Name ([Name], CorV)

type GblGrammar = Map.Map Name (TCDecl (SourceRange, Annot))


showSourceRange :: SourceRange -> String
showSourceRange sr =
  "(" ++ show (sourceLine $ sourceFrom sr) ++
  "," ++ show (sourceColumn $ sourceFrom sr) ++
  ")-(" ++ show (sourceLine $ sourceTo sr) ++
  "," ++ show (sourceColumn $ sourceTo sr) ++
  ")"
