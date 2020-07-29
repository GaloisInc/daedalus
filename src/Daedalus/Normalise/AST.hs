{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Daedalus.Normalise.AST where

import Data.Text(Text)


import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List(intersperse)
import Data.Word


import Daedalus.Type.AST hiding (ppBinder)
import Daedalus.PP

-- No type variables.
data NType = NType (TypeF NType)
           | NTCon TCTyName [NType]
  deriving (Show)

-- Only Value variables
data NName = NName { nName :: Name, nType :: NType }
  deriving (Show)


data NCExpr =
    NSetAny
  | NSetSingle NVExpr
  | NSetComplement NCExpr
  | NSetUnion  [NCExpr]
  | NSetOneOf  ByteString
  | NSetDiff   NCExpr NCExpr
  | NSetRange  NVExpr NVExpr
  | NCCall     NName [NVExpr]
  deriving (Show)

data NVExpr =
    NCoerce NType NType NVExpr
  | NNumber Integer NType
  | NBool   Bool
  | NNothing NType
  | NJust NVExpr
  | NByte   Word8
  | NStruct [ (Label, NVExpr) ] NType
  | NUnit
  | NByteArray ByteString
  | NArray     [NVExpr] NType
  | NMapEmpty NType
  | NArrayLength NVExpr  
  | NIn Label NVExpr NType
  | NTriOp TriOp NVExpr NVExpr NVExpr NType
  | NBinOp BinOp NVExpr NVExpr NType
  | NUniOp UniOp NVExpr
  | NSelStruct NVExpr Label NType
  | NIf        NVExpr NVExpr NVExpr
  | NVCall     NName [NVExpr]
  | NVFor NName NVExpr (Maybe NName) NName NVExpr NVExpr
  | NVMap              (Maybe NName) NName NVExpr NVExpr
  | NVar NName
  deriving (Show)

data NGExpr =
  NGPure NVExpr
  | NLabel Text NGrammar
  | NGuard NVExpr
  | NCurrnetStream
  | NSetStream NVExpr
  | NStreamLen WithSem NVExpr NVExpr
  | NStreamOff WithSem NVExpr NVExpr

  | NGetByte WithSem
  | NMatch WithSem NCExpr
  | NMatchBytes WithSem NVExpr
  | NChoice Commit [NGrammar] NType
  | NOptional Commit NGrammar
  | NMany WithSem Commit (ManyBounds NVExpr) NGrammar
  | NEnd
  | NOffset
  | NMapLookup WithSem NVExpr NVExpr
  | NMapInsert WithSem NVExpr NVExpr NVExpr
  | NArrayIndex WithSem NVExpr NVExpr
  | NCoerceCheck WithSem NType NType NVExpr
  | NSelUnion WithSem NVExpr Label NType
  | NSelJust WithSem NVExpr NType
  | NGFor NName NVExpr (Maybe NName) NName NVExpr NGrammar
  | NGMap              (Maybe NName) NName NVExpr NGrammar
  | NGCall NName [NVExpr]
  | NGErrorMode Commit NGrammar
  | NGFail (Maybe NVExpr) NType
  deriving (Show)

data NGrammar =
  NPure NVExpr | NBind (Maybe NName) NGExpr NGrammar
  deriving (Show)


data NDeclBody  =
    NCDecl NCExpr
  | NVDecl NVExpr
  | NGDecl NGrammar
  | NExtern
  deriving (Show)


data NDecl     = NDecl { nDeclName     :: !Name
                       -- , nDeclCtrs     :: ![Constraint]
                       , nDeclParams   :: ![NName]
                       , nDeclType     :: NType
                       , nDeclDef      :: !NDeclBody
                       }
  deriving (Show)

-- -----------------------------------------------------------------------------
-- Pretty printing

instance PP NDecl where
  pp NDecl {..} = pp nDeclName <+>
         hsep (map pp nDeclParams) <+>
         ":" <+> pp nDeclType <+> "="
         $$ nest 2 (pp nDeclDef)

instance PP NDeclBody where
  pp (NCDecl e) = pp e
  pp (NVDecl e) = pp e
  pp (NGDecl e) = pp e
  pp NExtern    = "(extern)"

instance PP NCExpr where
  ppPrec n cexpr =
    case cexpr of

      NCCall f []  -> pp f
      NCCall f xs -> wrapIf (n > 0) (pp f <+> hsep (map (ppPrec 1) xs))

      -- Sets
      NSetAny -> "UInt8"
      NSetSingle e -> braces (pp e)
      NSetComplement e -> wrapIf (n > 0) ("!" <> ppPrec 1 e)
      NSetRange e1 e2 -> wrapIf (n > 0) (pp e1 <+> ".." <+> pp e2)
      NSetUnion [] -> "{}"
      NSetUnion [e] -> ppPrec n e
      NSetUnion es -> wrapIf (n > 0)
                     $ fsep (intersperse "|" (map (ppPrec 1) es))
      NSetOneOf bs -> text (show (BS8.unpack bs))
      NSetDiff e1 e2 -> wrapIf (n > 0) (ppPrec 1 e1 <+> "-" <+> ppPrec 1 e2)

instance PP NVExpr where
  ppPrec n texpr =
    case texpr of
      NMapEmpty _   -> "empty"
      NArrayLength e -> 
          wrapIf (n > 0) ("Length" <+> ppPrec 1 e)      
      NVar x -> pp x

      -- Eliminators
      NVFor x s mb i is e -> wrapIf (n > 0) $
        "for" <+> parens (ppBinder x <+> "="  <+> pp s <.> semi <+>
                          ppK <+>
                          ppBinder i <+> "in" <+> pp is) $$ nest 2 (ppPrec 1 e)
        where ppK = case mb of
                      Nothing -> empty
                      Just k  -> ppBinder k <.> comma

      NVMap mb i is e -> wrapIf (n > 0) $
        "map" <+> parens (ppK <+>
                          ppBinder i <+> "in" <+> pp is) $$ nest 2 (ppPrec 1 e)
        where ppK = case mb of
                      Nothing -> empty
                      Just k  -> ppBinder k <.> comma



      NIf be te fe -> wrapIf (n > 0) $
        "if" <+> ppPrec 1 be <+> "then" <+> pp te <+> "else" <+> pp fe

      -- Values
      NIn l e _ -> braces (pp l <.> colon <+> ppPrec 1 e)
      NByte b      -> text (show (toEnum (fromEnum b) :: Char))
      NNumber i _  -> integer i
      NBool i      -> if i then "true" else "false"
      NNothing _   -> "nothing"
      NJust e      -> wrapIf (n > 0) ("just" <+> ppPrec 1 e)
      NUnit        -> "{}"
      NStruct xs _ -> braces (vcat (punctuate comma (map ppF xs)))
        where ppF (x,e) = pp x <+> "=" <+> pp e
      NArray xs _  -> brackets (vcat (punctuate comma (map pp xs)))
      NByteArray b -> text (show (BS8.unpack b))

      NVCall f []  -> pp f
      NVCall f xs -> wrapIf (n > 0) (pp f <+> hsep (map (ppPrec 1) xs))

      NBinOp op e1 e2 _ -> wrapIf (n > 0) (ppPrec 1 e1 <+> pp op <+> ppPrec 1 e2)
      NTriOp op e1 e2 e3 _ ->
        wrapIf (n > 0) (pp op <+> ppPrec 1 e1 <+> ppPrec 1 e2 <+> ppPrec 1 e3)


      NUniOp op e -> wrapIf (n > 0) (pp op <+> ppPrec 1 e)
      NSelStruct x l _ -> wrapIf (n > 0) (ppPrec 1 x <.> "." <.> pp l)
      NCoerce _ t2 e      -> wrapIf (n > 0) (pp e <+> "as" <+> pp t2)

instance PP NGExpr where
  ppPrec n gexpr =
    case gexpr of
      NGPure e      -> wrapIf (n > 0) ("pure" <+> ppPrec 1 e)
      NLabel l p    -> "{-" <+> pp l <+> "-}" <+> ppPrec n p

      NGuard e      -> wrapIf (n > 0) ("Guard" <+> ppPrec 1 e)
      NGetByte s    -> annotKW s Commit "GetByte"
      NMatch s b    -> wrapIf (n > 0)
                       (annotKW s Commit "Match" <+> ppPrec 1 b)

      NMatchBytes s b -> wrapIf (n > 0)
                         (annotKW s Commit "MatchBytes" <+> ppPrec 1 b)

      NEnd          -> "END"
      NOffset       -> "Offset"


      NCurrnetStream -> "CurrentStream"
      NSetStream s   -> wrapIf (n > 0) ("SetStream" <+> ppPrec 1 s)

      NStreamLen sem e s  ->
        wrapIf (n > 0)
        (annotKW sem Commit "StreamLen" <+> ppPrec 1 e <+> ppPrec 1 s)

      NStreamOff sem e s  ->
        wrapIf (n > 0)
        (annotKW sem Commit "StreamOff" <+> ppPrec 1 e <+> ppPrec 1 s)




      NMapInsert s k v m ->
        wrapIf (n > 0)
        (annotKW s Commit "Insert" <+> ppPrec 1 k <+> ppPrec 1 v <+> ppPrec 1 m)

      NMapLookup s k m ->
        wrapIf (n > 0)
        (annotKW s Commit "Lookup" <+> ppPrec 1 k <+> ppPrec 1 m)
          
      NArrayIndex s v ix -> 
          wrapIf (n > 0) (annotKW s Commit "Index" <+> ppPrec 1 v <+> ppPrec 1 ix)

      NChoice c es _ -> "Choose" <+> pp c $$ nest 2 (block "{" "|" "}" (map pp es))
      NOptional c e -> wrapIf (n > 0) $ annotKW YesSem c "Optional" <+> ppPrec 1 e

      NMany s c bnds e -> wrapIf (n > 0) (annotKW s c "Many" <.> pp bnds <+> pp e)

      -- Eliminators
      NGFor x s mb i is e -> wrapIf (n > 0) $
        "for" <+> parens (ppBinder x <+> "="  <+> pp s <.> semi <+>
                          ppK <+>
                          ppBinder i <+> "in" <+> pp is) $$ nest 2 (ppPrec 1 e)
        where ppK = case mb of
                      Nothing -> empty
                      Just k  -> ppBinder k <.> comma

      NGMap mb i is e -> wrapIf (n > 0) $
        "map" <+> parens (ppK <+>
                          ppBinder i <+> "in" <+> pp is) $$ nest 2 (ppPrec 1 e)
        where ppK = case mb of
                      Nothing -> empty
                      Just k  -> ppBinder k <.> comma



      NGCall f []  -> pp f
      NGCall f xs -> wrapIf (n > 0) (pp f <+> hsep (map (ppPrec 1) xs))
      NSelUnion s x l _ -> wrapQuietIf s (n > 0) (ppPrec 1 x <+> "is" <.> pp l)
      NSelJust s x _ -> wrapQuietIf s (n > 0) (ppPrec 1 x <+> "is just")


      -- Coercions
      NCoerceCheck s _ t2 e -> wrapQuietIf s (n > 0) (pp e <+> "AS" <+> pp t2)

      NGErrorMode c p -> wrapIf (n > 0) (hang kw 2 (ppPrec 1 p))
        where kw = case c of
                     Commit    -> "Commit"
                     Backtrack -> "Try"

      NGFail mbMsg _ ->
        case mbMsg of
          Nothing -> "Fail"
          Just msg -> wrapIf (n > 0) ("Fail" <+> ppPrec 1 msg)


instance PP NGrammar where
  ppPrec n (NPure v) = ppPrec n v
  ppPrec n dos       = wrapIf (n > 0) $ hang "do" 3 (vcat $ go dos)
    where
      go (NPure v) = ["pure" <+> pp v]
      go (NBind m_x l r) =
        ((maybe empty (\x -> ppBinder x <+> "<- ") m_x) <> pp l) : go r

instance PP NName where
  ppPrec n = ppPrec n . nName

instance PP NType where
  ppPrec n ty = case ty of
                  NTCon c [] -> pp c
                  NTCon c ts -> wrapIf (n > 0)
                                (pp c <+> hsep (map (ppPrec 1) ts))
                  NType t -> ppPrec n t



ppBinder :: NName -> Doc
ppBinder x = parens (pp (nName x) <+> ":" <+> pp (nType x))
