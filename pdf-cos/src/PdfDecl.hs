{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language ViewPatterns #-}
module PdfDecl where
 
import qualified Primitives.Resolve as D
import qualified Primitives.Deflate as D
import qualified Primitives.LZW as D
import qualified Primitives.ASCIIHex as D
import qualified Primitives.ASCII85 as D
import qualified Primitives.Decrypt as D
import qualified PdfMonad as D
import qualified PdfValue
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Input as RTS
import qualified RTS.Map as Map
import qualified RTS.Vector as Vector
 
 
data ApplyFilter
  = ApplyFilter_ok RTS.Input
  | ApplyFilter_unsupported (Vector.Vector (RTS.UInt 8))
  
 
deriving instance HS.Eq ApplyFilter
 
deriving instance HS.Ord ApplyFilter
 
deriving instance HS.Show ApplyFilter
 
instance RTS.DDL ApplyFilter where
 
instance HS.HasField "ok" ApplyFilter (HS.Maybe RTS.Input) where
  getField (ApplyFilter_ok x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "unsupported" ApplyFilter
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (ApplyFilter_unsupported x) = HS.Just x
   
  getField _ = HS.Nothing
 
data Stream
  = Stream (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
      ApplyFilter
  
 
deriving instance HS.Eq Stream
 
deriving instance HS.Ord Stream
 
deriving instance HS.Show Stream
 
instance RTS.DDL Stream where
 
instance HS.HasField "header" Stream
           (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) where
  getField (Stream x _) = x
 
instance HS.HasField "body" Stream ApplyFilter where
  getField (Stream _ x) = x
 
data TopDeclDef
  = TopDeclDef_stream Stream
  | TopDeclDef_value PdfValue.Value
  
 
deriving instance HS.Eq TopDeclDef
 
deriving instance HS.Ord TopDeclDef
 
deriving instance HS.Show TopDeclDef
 
instance RTS.DDL TopDeclDef where
 
instance HS.HasField "stream" TopDeclDef (HS.Maybe Stream) where
  getField (TopDeclDef_stream x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "value" TopDeclDef
           (HS.Maybe PdfValue.Value) where
  getField (TopDeclDef_value x) = HS.Just x
   
  getField _ = HS.Nothing
 
data TopDecl
  = TopDecl HS.Integer HS.Integer TopDeclDef
  
 
deriving instance HS.Eq TopDecl
 
deriving instance HS.Ord TopDecl
 
deriving instance HS.Show TopDecl
 
instance RTS.DDL TopDecl where
 
instance HS.HasField "id" TopDecl HS.Integer where
  getField (TopDecl x _ _) = x
 
instance HS.HasField "gen" TopDecl HS.Integer where
  getField (TopDecl _ x _) = x
 
instance HS.HasField "obj" TopDecl TopDeclDef where
  getField (TopDecl _ _ x) = x
 
data ObjectStreamEntry a
  = ObjectStreamEntry a PdfValue.Value
  
 
deriving instance HS.Eq a => HS.Eq (ObjectStreamEntry a)
 
deriving instance HS.Ord a => HS.Ord (ObjectStreamEntry a)
 
deriving instance HS.Show a => HS.Show (ObjectStreamEntry a)
 
instance RTS.DDL a => RTS.DDL (ObjectStreamEntry a) where
 
instance HS.HasField "oid" (ObjectStreamEntry a) a where
  getField (ObjectStreamEntry x _) = x
 
instance HS.HasField "val" (ObjectStreamEntry a)
           PdfValue.Value where
  getField (ObjectStreamEntry _ x) = x
 
data ObjStreamMeta
  = ObjStreamMeta HS.Integer (RTS.UInt 64)
  
 
deriving instance HS.Eq ObjStreamMeta
 
deriving instance HS.Ord ObjStreamMeta
 
deriving instance HS.Show ObjStreamMeta
 
instance RTS.DDL ObjStreamMeta where
 
instance HS.HasField "oid" ObjStreamMeta HS.Integer where
  getField (ObjStreamMeta x _) = x
 
instance HS.HasField "off" ObjStreamMeta (RTS.UInt 64) where
  getField (ObjStreamMeta _ x) = x
 
data Filter
  = Filter (Vector.Vector (RTS.UInt 8))
      (HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value))
  
 
deriving instance HS.Eq Filter
 
deriving instance HS.Ord Filter
 
deriving instance HS.Show Filter
 
instance RTS.DDL Filter where
 
instance HS.HasField "name" Filter
           (Vector.Vector (RTS.UInt 8)) where
  getField (Filter x _) = x
 
instance HS.HasField "param" Filter
           (HS.Maybe
              (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) where
  getField (Filter _ x) = x
 
data TfdDefaults
  = TfdDefaults HS.Integer HS.Integer HS.Integer HS.Integer
  
 
deriving instance HS.Eq TfdDefaults
 
deriving instance HS.Ord TfdDefaults
 
deriving instance HS.Show TfdDefaults
 
instance RTS.DDL TfdDefaults where
 
instance HS.HasField "predictor" TfdDefaults HS.Integer where
  getField (TfdDefaults x _ _ _) = x
 
instance HS.HasField "colors" TfdDefaults HS.Integer where
  getField (TfdDefaults _ x _ _) = x
 
instance HS.HasField "bpc" TfdDefaults HS.Integer where
  getField (TfdDefaults _ _ x _) = x
 
instance HS.HasField "columns" TfdDefaults HS.Integer where
  getField (TfdDefaults _ _ _ x) = x
 
data TlzwDefaults
  = TlzwDefaults HS.Integer HS.Integer HS.Integer HS.Integer
      HS.Integer
  
 
deriving instance HS.Eq TlzwDefaults
 
deriving instance HS.Ord TlzwDefaults
 
deriving instance HS.Show TlzwDefaults
 
instance RTS.DDL TlzwDefaults where
 
instance HS.HasField "predictor" TlzwDefaults HS.Integer where
  getField (TlzwDefaults x _ _ _ _) = x
 
instance HS.HasField "colors" TlzwDefaults HS.Integer where
  getField (TlzwDefaults _ x _ _ _) = x
 
instance HS.HasField "bpc" TlzwDefaults HS.Integer where
  getField (TlzwDefaults _ _ x _ _) = x
 
instance HS.HasField "columns" TlzwDefaults HS.Integer where
  getField (TlzwDefaults _ _ _ x _) = x
 
instance HS.HasField "earlychange" TlzwDefaults HS.Integer where
  getField (TlzwDefaults _ _ _ _ x) = x
 
pResolveRef :: PdfValue.Ref -> D.Parser (HS.Maybe TopDecl)
 
pResolveRef (r :: PdfValue.Ref) =
  D.resolveImpl PdfDecl.pTopDecl PdfDecl.pResolveObjectStreamEntry
    (HS.getField @"obj" r)
    (HS.getField @"gen" r)
    :: D.Parser (HS.Maybe TopDecl)
 
pCheckExpected :: PdfValue.Ref -> (TopDecl -> D.Parser TopDeclDef)
 
pCheckExpected (r :: PdfValue.Ref) (d :: TopDecl) =
  do RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard
          ((HS.getField @"id" d HS.== HS.getField @"obj" r)
             HS.&& (HS.getField @"gen" d HS.== HS.getField @"gen" r)))
     (__ :: TopDeclDef) <- HS.pure (HS.getField @"obj" d)
     HS.pure __
 
pResolveValRef :: PdfValue.Ref -> D.Parser PdfValue.Value
 
pResolveValRef (r :: PdfValue.Ref) =
  do (mb :: HS.Maybe TopDecl) <-
       RTS.pEnter "PdfDecl.ResolveRef" (pResolveRef r)
     (__ :: PdfValue.Value) <-
       (RTS.|||)
         (do case mb of
               HS.Nothing -> HS.pure ()
               _ -> RTS.pError RTS.FromSystem "85:7--85:19"
                      "Pattern match failure"
             (__ :: PdfValue.Value) <- HS.pure PdfValue.nullValue
             HS.pure __)
         (do (_481 :: TopDeclDef) <-
               do (_480 :: TopDecl) <-
                    case mb of
                      HS.Just (_479 :: TopDecl) -> HS.pure _479
                      _ -> RTS.pError RTS.FromSystem "86:22--86:31"
                             "Pattern match failure"
                  RTS.pEnter "PdfDecl.CheckExpected" (pCheckExpected r _480)
             case _481 of
               TopDeclDef_value (_482 :: PdfValue.Value) -> HS.pure _482
               _ -> RTS.pError RTS.FromSystem "86:5--86:41"
                      "Pattern match failure")
     HS.pure __
 
pDefault :: forall a. RTS.DDL a => a -> (D.Parser a -> D.Parser a)
 
pDefault (x :: a) (pP :: D.Parser a) = (RTS.<||) pP (HS.pure x)
 
pResolveVal :: PdfValue.Value -> D.Parser PdfValue.Value
 
pResolveVal (v :: PdfValue.Value) =
  RTS.pEnter "PdfDecl.Default"
    (pDefault @PdfValue.Value v
       (do (r :: PdfValue.Ref) <-
             case v of
               PdfValue.Value_ref (_483 :: PdfValue.Ref) -> HS.pure _483
               _ -> RTS.pError RTS.FromSystem "109:8--109:15"
                      "Pattern match failure"
           RTS.pErrorMode RTS.Abort
             (do (__ :: PdfValue.Value) <-
                   RTS.pEnter "PdfDecl.ResolveValRef" (pResolveValRef r)
                 HS.pure __)))
 
pLookupResolve ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser PdfValue.Value)
 
pLookupResolve (k :: Vector.Vector (RTS.UInt 8))
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (v :: PdfValue.Value) <-
       RTS.pIsJust "115:8--115:22"
         ("Missing key: " HS.++ HS.show (k :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup k header)
     (__ :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveVal" (pResolveVal v)
     HS.pure __
 
pStreamLen ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> D.Parser (RTS.UInt 64)
 
pStreamLen
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (lenV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve"
         (pLookupResolve (Vector.vecFromRep "Length") header)
     (lenI :: PdfValue.Number) <-
       case lenV of
         PdfValue.Value_number (_486 :: PdfValue.Number) -> HS.pure _486
         _ -> RTS.pError RTS.FromSystem "134:11--134:24"
                "Pattern match failure"
     (__ :: RTS.UInt 64) <-
       do (_488 :: HS.Integer) <-
            RTS.pEnter "PdfValue.NumberAsNat" (PdfValue.pNumberAsNat lenI)
          RTS.pIsJust "135:3--135:29" "Value does not fit in target type"
            (RTS.convertMaybe _488 :: HS.Maybe (RTS.UInt 64))
     HS.pure __
 
pFilterParam ::
      PdfValue.Value
        -> D.Parser
             (HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value))
 
pFilterParam (param :: PdfValue.Value) =
  (RTS.|||)
    (do case param of
          PdfValue.Value_null (_449 :: ()) -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "157:7--157:24"
                 "Pattern match failure"
        (__
           :: HS.Maybe
                (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) <-
          HS.pure
            (HS.Nothing
               :: HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value))
        HS.pure __)
    (do (x :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
          case param of
            PdfValue.Value_dict
              (_451
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _451
            _ -> RTS.pError RTS.FromSystem "158:12--158:24"
                   "Pattern match failure"
        (__
           :: HS.Maybe
                (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) <-
          HS.pure (HS.Just x)
        HS.pure __)
 
pFilter :: PdfValue.Value -> (PdfValue.Value -> D.Parser Filter)
 
pFilter (name :: PdfValue.Value) (param :: PdfValue.Value) =
  do (name :: Vector.Vector (RTS.UInt 8)) <-
       case name of
         PdfValue.Value_name (_453 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                       _453
         _ -> RTS.pError RTS.FromSystem "152:11--152:22"
                "Pattern match failure"
     (param
        :: HS.Maybe
             (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) <-
       RTS.pEnter "PdfDecl.FilterParam" (pFilterParam param)
     HS.pure (Filter name param)
 
pDecrypt :: RTS.Input -> D.Parser RTS.Input
 
pDecrypt (body :: RTS.Input) = D.decrypt body :: D.Parser RTS.Input
 
fdDefaults :: TfdDefaults
 
fdDefaults =
  TfdDefaults (RTS.lit 1 :: HS.Integer) (RTS.lit 1 :: HS.Integer)
    (RTS.lit 8 :: HS.Integer)
    (RTS.lit 1 :: HS.Integer)
 
pLookupNat ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser HS.Integer)
 
pLookupNat (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (vV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve k m)
     (v :: PdfValue.Number) <-
       case vV of
         PdfValue.Value_number (_458 :: PdfValue.Number) -> HS.pure _458
         _ -> RTS.pError RTS.FromSystem "314:11--314:22"
                "Pattern match failure"
     (__ :: HS.Integer) <-
       RTS.pEnter "PdfValue.NumberAsNat" (PdfValue.pNumberAsNat v)
     HS.pure __
 
pFlateDecodeParams ::
      HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
        -> D.Parser TfdDefaults
 
pFlateDecodeParams
  (params
     :: HS.Maybe
          (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) =
  (RTS.<||)
    (do case params of
          HS.Nothing -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "215:5--215:21"
                 "Pattern match failure"
        (__ :: TfdDefaults) <- HS.pure fdDefaults
        HS.pure __)
    (do (ps :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
          case params of
            HS.Just
              (_461
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _461
            _ -> RTS.pError RTS.FromSystem "219:17--219:30"
                   "Pattern match failure"
        (predictor :: HS.Integer) <-
          RTS.pEnter "PdfDecl.Default"
            (pDefault @HS.Integer (HS.getField @"predictor" fdDefaults)
               (RTS.pEnter "PdfDecl.LookupNat"
                  (pLookupNat (Vector.vecFromRep "Predictor") ps)))
        (colors :: HS.Integer) <-
          RTS.pEnter "PdfDecl.Default"
            (pDefault @HS.Integer (HS.getField @"colors" fdDefaults)
               (RTS.pEnter "PdfDecl.LookupNat"
                  (pLookupNat (Vector.vecFromRep "Colors") ps)))
        (bpc :: HS.Integer) <-
          RTS.pEnter "PdfDecl.Default"
            (pDefault @HS.Integer (HS.getField @"bpc" fdDefaults)
               (RTS.pEnter "PdfDecl.LookupNat"
                  (pLookupNat (Vector.vecFromRep "BitsPerComponent") ps)))
        (columns :: HS.Integer) <-
          RTS.pEnter "PdfDecl.Default"
            (pDefault @HS.Integer (HS.getField @"columns" fdDefaults)
               (RTS.pEnter "PdfDecl.LookupNat"
                  (pLookupNat (Vector.vecFromRep "Columns") ps)))
        HS.pure (TfdDefaults predictor colors bpc columns))
 
pFlateDecode ::
      HS.Integer
        -> (HS.Integer
              -> (HS.Integer
                    -> (HS.Integer -> (RTS.Input -> D.Parser RTS.Input))))
 
pFlateDecode (predictor :: HS.Integer) (colors :: HS.Integer)
  (bpc :: HS.Integer)
  (columns :: HS.Integer)
  (body :: RTS.Input) =
  D.flateDecode predictor colors bpc columns body
    :: D.Parser RTS.Input
 
lzwDefaults :: TlzwDefaults
 
lzwDefaults =
  TlzwDefaults (RTS.lit 1 :: HS.Integer) (RTS.lit 1 :: HS.Integer)
    (RTS.lit 8 :: HS.Integer)
    (RTS.lit 1 :: HS.Integer)
    (RTS.lit 1 :: HS.Integer)
 
pLZWDecodeParams ::
      HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
        -> D.Parser TlzwDefaults
 
pLZWDecodeParams
  (params
     :: HS.Maybe
          (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) =
  (RTS.<||)
    (do case params of
          HS.Nothing -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "242:5--242:21"
                 "Pattern match failure"
        (__ :: TlzwDefaults) <- HS.pure lzwDefaults
        HS.pure __)
    (do (ps :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
          case params of
            HS.Just
              (_457
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _457
            _ -> RTS.pError RTS.FromSystem "246:19--246:32"
                   "Pattern match failure"
        (predictor :: HS.Integer) <-
          RTS.pEnter "PdfDecl.Default"
            (pDefault @HS.Integer (HS.getField @"predictor" lzwDefaults)
               (RTS.pEnter "PdfDecl.LookupNat"
                  (pLookupNat (Vector.vecFromRep "Predictor") ps)))
        (colors :: HS.Integer) <-
          RTS.pEnter "PdfDecl.Default"
            (pDefault @HS.Integer (HS.getField @"colors" lzwDefaults)
               (RTS.pEnter "PdfDecl.LookupNat"
                  (pLookupNat (Vector.vecFromRep "Colors") ps)))
        (bpc :: HS.Integer) <-
          RTS.pEnter "PdfDecl.Default"
            (pDefault @HS.Integer (HS.getField @"bpc" lzwDefaults)
               (RTS.pEnter "PdfDecl.LookupNat"
                  (pLookupNat (Vector.vecFromRep "BitsPerComponent") ps)))
        (columns :: HS.Integer) <-
          RTS.pEnter "PdfDecl.Default"
            (pDefault @HS.Integer (HS.getField @"columns" lzwDefaults)
               (RTS.pEnter "PdfDecl.LookupNat"
                  (pLookupNat (Vector.vecFromRep "Columns") ps)))
        (earlychange :: HS.Integer) <-
          RTS.pEnter "PdfDecl.Default"
            (pDefault @HS.Integer (HS.getField @"earlychange" lzwDefaults)
               (RTS.pEnter "PdfDecl.LookupNat"
                  (pLookupNat (Vector.vecFromRep "EarlyChange") ps)))
        HS.pure (TlzwDefaults predictor colors bpc columns earlychange))
 
pLZWDecode ::
      HS.Integer
        -> (HS.Integer
              -> (HS.Integer
                    -> (HS.Integer
                          -> (HS.Integer -> (RTS.Input -> D.Parser RTS.Input)))))
 
pLZWDecode (predictor :: HS.Integer) (colors :: HS.Integer)
  (bpc :: HS.Integer)
  (columns :: HS.Integer)
  (earlychange :: HS.Integer)
  (body :: RTS.Input) =
  D.lzwDecode predictor colors bpc columns earlychange body
    :: D.Parser RTS.Input
 
pASCIIHexDecode :: RTS.Input -> D.Parser RTS.Input
 
pASCIIHexDecode (body :: RTS.Input) =
  D.asciiHexDecode body :: D.Parser RTS.Input
 
pASCII85Decode :: RTS.Input -> D.Parser RTS.Input
 
pASCII85Decode (body :: RTS.Input) =
  D.ascii85Decode body :: D.Parser RTS.Input
 
pApplyFilter :: Filter -> (RTS.Input -> D.Parser ApplyFilter)
 
pApplyFilter (f :: Filter) (body :: RTS.Input) =
  (RTS.<||)
    (RTS.pEnter "ok"
       (do (_463 :: RTS.Input) <-
             do RTS.pEnter "PdfValue._Guard"
                  (PdfValue._Guard
                     (HS.getField @"name" f HS.== Vector.vecFromRep "FlateDecode"))
                (params :: TfdDefaults) <-
                  RTS.pEnter "PdfDecl.FlateDecodeParams"
                    (pFlateDecodeParams (HS.getField @"param" f))
                RTS.pEnter "PdfValue._Guard"
                  (PdfValue._Guard
                     ((HS.getField @"predictor" params HS.== (RTS.lit 1 :: HS.Integer))
                        HS.|| (HS.getField @"predictor" params
                                 HS.== (RTS.lit 12 :: HS.Integer))))
                RTS.pEnter "PdfValue._Guard"
                  (PdfValue._Guard
                     (HS.getField @"colors" params HS.== (RTS.lit 1 :: HS.Integer)))
                RTS.pEnter "PdfValue._Guard"
                  (PdfValue._Guard
                     (HS.getField @"bpc" params HS.== (RTS.lit 8 :: HS.Integer)))
                RTS.pErrorMode RTS.Abort
                  (do (__ :: RTS.Input) <-
                        RTS.pEnter "PdfDecl.FlateDecode"
                          (pFlateDecode (HS.getField @"predictor" params)
                             (HS.getField @"colors" params)
                             (HS.getField @"bpc" params)
                             (HS.getField @"columns" params)
                             body)
                      HS.pure __)
           HS.pure (ApplyFilter_ok _463)))
    ((RTS.<||)
       (RTS.pEnter "ok"
          (do (_465 :: RTS.Input) <-
                do RTS.pEnter "PdfValue._Guard"
                     (PdfValue._Guard
                        (HS.getField @"name" f HS.== Vector.vecFromRep "LZWDecode"))
                   (params :: TlzwDefaults) <-
                     RTS.pEnter "PdfDecl.LZWDecodeParams"
                       (pLZWDecodeParams (HS.getField @"param" f))
                   RTS.pEnter "PdfValue._Guard"
                     (PdfValue._Guard
                        ((HS.getField @"predictor" params HS.== (RTS.lit 1 :: HS.Integer))
                           HS.|| (HS.getField @"predictor" params
                                    HS.== (RTS.lit 12 :: HS.Integer))))
                   RTS.pEnter "PdfValue._Guard"
                     (PdfValue._Guard
                        (HS.getField @"colors" params HS.== (RTS.lit 1 :: HS.Integer)))
                   RTS.pEnter "PdfValue._Guard"
                     (PdfValue._Guard
                        (HS.getField @"bpc" params HS.== (RTS.lit 8 :: HS.Integer)))
                   RTS.pErrorMode RTS.Abort
                     (do (__ :: RTS.Input) <-
                           RTS.pEnter "PdfDecl.LZWDecode"
                             (pLZWDecode (HS.getField @"predictor" params)
                                (HS.getField @"colors" params)
                                (HS.getField @"bpc" params)
                                (HS.getField @"columns" params)
                                (HS.getField @"earlychange" params)
                                body)
                         HS.pure __)
              HS.pure (ApplyFilter_ok _465)))
       ((RTS.<||)
          (RTS.pEnter "ok"
             (do (_467 :: RTS.Input) <-
                   do RTS.pEnter "PdfValue._Guard"
                        (PdfValue._Guard
                           (HS.getField @"name" f HS.== Vector.vecFromRep "ASCIIHexDecode"))
                      RTS.pErrorMode RTS.Abort
                        (do (__ :: RTS.Input) <-
                              RTS.pEnter "PdfDecl.ASCIIHexDecode" (pASCIIHexDecode body)
                            HS.pure __)
                 HS.pure (ApplyFilter_ok _467)))
          ((RTS.<||)
             (RTS.pEnter "ok"
                (do (_469 :: RTS.Input) <-
                      do RTS.pEnter "PdfValue._Guard"
                           (PdfValue._Guard
                              (HS.getField @"name" f HS.== Vector.vecFromRep "ASCII85Decode"))
                         RTS.pErrorMode RTS.Abort
                           (do (__ :: RTS.Input) <-
                                 RTS.pEnter "PdfDecl.ASCII85Decode" (pASCII85Decode body)
                               HS.pure __)
                    HS.pure (ApplyFilter_ok _469)))
             (RTS.pEnter "unsupported"
                (do (_471 :: Vector.Vector (RTS.UInt 8)) <-
                      (RTS.<||)
                        (do case HS.getField @"param" f of
                              HS.Nothing -> HS.pure ()
                              _ -> RTS.pError RTS.FromSystem "209:19--209:36"
                                     "Pattern match failure"
                            (__ :: Vector.Vector (RTS.UInt 8)) <-
                              HS.pure (HS.getField @"name" f)
                            HS.pure __)
                        (HS.pure
                           (Vector.concat
                              (Vector.fromList
                                 [HS.getField @"name" f, Vector.vecFromRep " (with params)"])))
                    HS.pure (ApplyFilter_unsupported _471))))))
 
pOneOrArray ::
      PdfValue.Value -> D.Parser (Vector.Vector PdfValue.Value)
 
pOneOrArray (v :: PdfValue.Value) =
  RTS.pEnter "PdfDecl.Default"
    (pDefault @(Vector.Vector PdfValue.Value) (Vector.fromList [v])
       (case v of
          PdfValue.Value_array
            (_454 :: Vector.Vector PdfValue.Value) -> HS.pure _454
          _ -> RTS.pError RTS.FromSystem "282:43--282:52"
                 "Pattern match failure"))
 
pLookOptArray ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser (Vector.Vector PdfValue.Value))
 
pLookOptArray (key :: Vector.Vector (RTS.UInt 8))
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  RTS.pEnter "PdfDecl.Default"
    (pDefault @(Vector.Vector PdfValue.Value)
       (Vector.empty :: Vector.Vector PdfValue.Value)
       (do (x :: PdfValue.Value) <-
             RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve key header)
           (__ :: Vector.Vector PdfValue.Value) <-
             RTS.pEnter "PdfDecl.OneOrArray" (pOneOrArray x)
           HS.pure __))
 
pApplyFilters ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> (RTS.Input -> D.Parser ApplyFilter)
 
pApplyFilters
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  (initialBody :: RTS.Input) =
  do (decrypt :: RTS.Input) <-
       RTS.pEnter "PdfDecl.Decrypt" (pDecrypt initialBody)
     (filter_names :: Vector.Vector PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookOptArray"
         (pLookOptArray (Vector.vecFromRep "Filter") header)
     (filter_params :: Vector.Vector PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookOptArray"
         (pLookOptArray (Vector.vecFromRep "DecodeParms") header)
     (__ :: ApplyFilter) <-
       RTS.loopIFoldM
         (\(bytes :: ApplyFilter) (ix :: RTS.UInt 64) (name
                                                         :: PdfValue.Value) ->
            do (param :: PdfValue.Value) <-
                 RTS.pEnter "PdfDecl.Default"
                   (pDefault @PdfValue.Value PdfValue.nullValue
                      (RTS.pIsJust "144:34--144:52" "Index out of bounds"
                         ((Vector.!?) filter_params ix)))
               (filter :: Filter) <-
                 RTS.pEnter "PdfDecl.Filter" (pFilter name param)
               (__ :: ApplyFilter) <-
                 RTS.pEnter "PdfDecl.Default"
                   (pDefault @ApplyFilter bytes
                      (do (bs :: RTS.Input) <-
                            case bytes of
                              ApplyFilter_ok (_474 :: RTS.Input) -> HS.pure _474
                              _ -> RTS.pError RTS.FromSystem "146:27--146:37"
                                     "Pattern match failure"
                          RTS.pErrorMode RTS.Abort
                            (do (__ :: ApplyFilter) <-
                                  RTS.pEnter "PdfDecl.ApplyFilter" (pApplyFilter filter bs)
                                HS.pure __)))
               HS.pure __)
         (ApplyFilter_ok decrypt)
         filter_names
     HS.pure __
 
pChunk ::
  forall f. RTS.DDL f => RTS.UInt 64 -> (D.Parser f -> D.Parser f)
 
pChunk (n :: RTS.UInt 64) (pP :: D.Parser f) =
  do (cur :: RTS.Input) <- RTS.pPeek
     (this :: RTS.Input) <-
       RTS.pIsJust "288:11--288:16" "Not enough bytes"
         (RTS.limitLen n cur)
     (next :: RTS.Input) <-
       RTS.pIsJust "289:11--289:16" "Not enough bytes"
         (RTS.advanceBy n cur)
     RTS.pSetInput this
     (__ :: f) <- pP
     RTS.pSetInput next
     HS.pure __
 
pStreamBody ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> D.Parser ApplyFilter
 
pStreamBody
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @ApplyFilter
       (do (len :: RTS.UInt 64) <-
             RTS.pEnter "PdfDecl.StreamLen" (pStreamLen header)
           (__ :: ApplyFilter) <-
             RTS.pEnter "PdfDecl.Chunk"
               (pChunk @ApplyFilter len
                  (do (body :: RTS.Input) <- RTS.pPeek
                      (__ :: ApplyFilter) <-
                        RTS.pEnter "PdfDecl.ApplyFilters" (pApplyFilters header body)
                      HS.pure __))
           HS.pure __))
 
pStream :: PdfValue.Value -> D.Parser Stream
 
pStream (val :: PdfValue.Value) =
  do (header
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case val of
         PdfValue.Value_dict
           (_491
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _491
         _ -> RTS.pError RTS.FromSystem "18:12--18:22"
                "Pattern match failure"
     HS.const ()
       HS.<$> RTS.pMatch "19:3--19:16" (Vector.vecFromRep "stream")
     RTS.pErrorMode RTS.Abort
       (do RTS.pEnter "PdfValue._SimpleEOL" PdfValue._SimpleEOL
           (body :: ApplyFilter) <-
             RTS.pEnter "PdfDecl.StreamBody" (pStreamBody header)
           RTS.pEnter "PdfValue._KW"
             (PdfValue._KW (Vector.vecFromRep "endstream"))
           HS.pure (Stream header body))
 
pTopDeclDef :: PdfValue.Value -> D.Parser TopDeclDef
 
pTopDeclDef (val :: PdfValue.Value) =
  (RTS.<||)
    (RTS.pEnter "stream"
       (do (_492 :: Stream) <- RTS.pEnter "PdfDecl.Stream" (pStream val)
           HS.pure (TopDeclDef_stream _492)))
    (RTS.pEnter "value"
       (do (_493 :: PdfValue.Value) <- HS.pure val
           HS.pure (TopDeclDef_value _493)))
 
pTopDecl :: D.Parser TopDecl
 
pTopDecl =
  do (id :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (PdfValue.pToken @HS.Integer
            (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
     (gen :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (PdfValue.pToken @HS.Integer
            (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
     RTS.pEnter "PdfValue._KW" (PdfValue._KW (Vector.vecFromRep "obj"))
     (val :: PdfValue.Value) <-
       RTS.pEnter "PdfValue.Value" PdfValue.pValue
     (obj :: TopDeclDef) <-
       RTS.pEnter "PdfDecl.TopDeclDef" (pTopDeclDef val)
     HS.const ()
       HS.<$> RTS.pMatch "9:3--9:16" (Vector.vecFromRep "endobj")
     HS.pure (TopDecl id gen obj)
 
pObjectStreamEntry ::
  forall a. RTS.DDL a => a -> D.Parser (ObjectStreamEntry a)
 
pObjectStreamEntry (oid :: a) =
  do (oid :: a) <- HS.pure oid
     (val :: PdfValue.Value) <-
       RTS.pEnter "PdfValue.Value" PdfValue.pValue
     HS.pure (ObjectStreamEntry oid val)
 
pObjStreamMeta :: RTS.UInt 64 -> D.Parser ObjStreamMeta
 
pObjStreamMeta (first :: RTS.UInt 64) =
  do (oid :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (PdfValue.pToken @HS.Integer
            (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
     (off :: RTS.UInt 64) <-
       do (_495 :: RTS.UInt 64) <-
            do (_494 :: HS.Integer) <-
                 RTS.pEnter "PdfValue.Token"
                   (PdfValue.pToken @HS.Integer
                      (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
               RTS.pIsJust "41:14--41:37" "Value does not fit in target type"
                 (RTS.convertMaybe _494 :: HS.Maybe (RTS.UInt 64))
          HS.pure (RTS.add _495 first)
     HS.pure (ObjStreamMeta oid off)
 
_Chunk ::
  forall f. RTS.DDL f => RTS.UInt 64 -> (D.Parser () -> D.Parser ())
 
_Chunk (n :: RTS.UInt 64) (_P :: D.Parser ()) =
  do (cur :: RTS.Input) <- RTS.pPeek
     (this :: RTS.Input) <-
       RTS.pIsJust "288:11--288:16" "Not enough bytes"
         (RTS.limitLen n cur)
     (next :: RTS.Input) <-
       RTS.pIsJust "289:11--289:16" "Not enough bytes"
         (RTS.advanceBy n cur)
     RTS.pSetInput this
     _P
     RTS.pSetInput next
 
_SkipBytes :: RTS.UInt 64 -> D.Parser ()
 
_SkipBytes (n :: RTS.UInt 64) =
  RTS.pEnter "PdfDecl._Chunk" (_Chunk @() n (HS.pure ()))
 
pObjectStream ::
      RTS.UInt 64
        -> (RTS.UInt 64
              -> D.Parser (Vector.Vector (ObjectStreamEntry HS.Integer)))
 
pObjectStream (n :: RTS.UInt 64) (first :: RTS.UInt 64) =
  do (meta :: Vector.Vector ObjStreamMeta) <-
       Vector.replicateM n
         (RTS.pEnter "PdfDecl.ObjStreamMeta" (pObjStreamMeta first))
     (__ :: Vector.Vector (ObjectStreamEntry HS.Integer)) <-
       RTS.loopMapM
         (\(entry :: ObjStreamMeta) ->
            do (here :: RTS.UInt 64) <- RTS.pOffset
               RTS.pEnter "PdfValue._Guard"
                 (PdfValue._Guard (here HS.<= HS.getField @"off" entry))
               RTS.pEnter "PdfDecl._SkipBytes"
                 (_SkipBytes (RTS.sub (HS.getField @"off" entry) here))
               (__ :: ObjectStreamEntry HS.Integer) <-
                 RTS.pEnter "PdfDecl.ObjectStreamEntry"
                   (pObjectStreamEntry @HS.Integer (HS.getField @"oid" entry))
               HS.pure __)
         meta
         :: D.Parser (Vector.Vector (ObjectStreamEntry HS.Integer))
     HS.pure __
 
pObjectStreamNth ::
      RTS.UInt 64
        -> (RTS.UInt 64
              -> (RTS.UInt 64 -> D.Parser (ObjectStreamEntry HS.Integer)))
 
pObjectStreamNth (n :: RTS.UInt 64) (first :: RTS.UInt 64)
  (idx :: RTS.UInt 64) =
  do (meta :: Vector.Vector ObjStreamMeta) <-
       Vector.replicateM n
         (RTS.pEnter "PdfDecl.ObjStreamMeta" (pObjStreamMeta first))
     (entry :: ObjStreamMeta) <-
       RTS.pIsJust "57:12--57:21" "Index out of bounds"
         ((Vector.!?) meta idx)
     (here :: RTS.UInt 64) <- RTS.pOffset
     RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (here HS.<= HS.getField @"off" entry))
     RTS.pEnter "PdfDecl._SkipBytes"
       (_SkipBytes (RTS.sub (HS.getField @"off" entry) here))
     (__ :: ObjectStreamEntry HS.Integer) <-
       RTS.pEnter "PdfDecl.ObjectStreamEntry"
         (pObjectStreamEntry @HS.Integer (HS.getField @"oid" entry))
     HS.pure __
 
pSkipBytes :: RTS.UInt 64 -> D.Parser ()
 
pSkipBytes (n :: RTS.UInt 64) =
  RTS.pEnter "PdfDecl.Chunk" (pChunk @() n (HS.pure ()))
 
pResolveStream :: PdfValue.Value -> D.Parser Stream
 
pResolveStream (v :: PdfValue.Value) =
  do (r :: PdfValue.Ref) <-
       case v of
         PdfValue.Value_ref (_499 :: PdfValue.Ref) -> HS.pure _499
         _ -> RTS.pError RTS.FromSystem "79:9--79:16"
                "Pattern match failure"
     (__ :: Stream) <-
       do (_504 :: TopDeclDef) <-
            do (_503 :: TopDecl) <-
                 do (_501 :: HS.Maybe TopDecl) <-
                      RTS.pEnter "PdfDecl.ResolveRef" (pResolveRef r)
                    case _501 of
                      HS.Just (_502 :: TopDecl) -> HS.pure _502
                      _ -> RTS.pError RTS.FromSystem "80:20--80:39"
                             "Pattern match failure"
               RTS.pEnter "PdfDecl.CheckExpected" (pCheckExpected r _503)
          case _504 of
            TopDeclDef_stream (_505 :: Stream) -> HS.pure _505
            _ -> RTS.pError RTS.FromSystem "80:3--80:50"
                   "Pattern match failure"
     HS.pure __
 
pLookupSize ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser (RTS.UInt 64))
 
pLookupSize (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (_510 :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat" (pLookupNat k m)
     RTS.pIsJust "318:22--318:45" "Value does not fit in target type"
       (RTS.convertMaybe _510 :: HS.Maybe (RTS.UInt 64))
 
pWithStream ::
  forall d. RTS.DDL d => RTS.Input -> (D.Parser d -> D.Parser d)
 
pWithStream (s :: RTS.Input) (pP :: D.Parser d) =
  do (cur :: RTS.Input) <- RTS.pPeek
     RTS.pSetInput s
     (__ :: d) <- pP
     RTS.pSetInput cur
     HS.pure __
 
_CheckType ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_CheckType (x :: Vector.Vector (RTS.UInt 8))
  (h :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (_509 :: HS.Bool) <-
       do (_508 :: Vector.Vector (RTS.UInt 8)) <-
            do (_506 :: PdfValue.Value) <-
                 RTS.pEnter "PdfDecl.LookupResolve"
                   (pLookupResolve (Vector.vecFromRep "Type") h)
               case _506 of
                 PdfValue.Value_name (_507 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                               _507
                 _ -> RTS.pError RTS.FromSystem "300:29--300:58"
                        "Pattern match failure"
          HS.pure (_508 HS.== x)
     RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _509)
 
pResolveObjectStream ::
      PdfValue.Value
        -> D.Parser (Vector.Vector (ObjectStreamEntry HS.Integer))
 
pResolveObjectStream (v :: PdfValue.Value) =
  do (stm :: Stream) <-
       RTS.pEnter "PdfDecl.ResolveStream" (pResolveStream v)
     RTS.pEnter "PdfDecl._CheckType"
       (_CheckType (Vector.vecFromRep "ObjStm")
          (HS.getField @"header" stm))
     (n :: RTS.UInt 64) <-
       RTS.pEnter "PdfDecl.LookupSize"
         (pLookupSize (Vector.vecFromRep "N") (HS.getField @"header" stm))
     (first :: RTS.UInt 64) <-
       RTS.pEnter "PdfDecl.LookupSize"
         (pLookupSize (Vector.vecFromRep "First")
            (HS.getField @"header" stm))
     (__ :: Vector.Vector (ObjectStreamEntry HS.Integer)) <-
       do (_513 :: RTS.Input) <-
            case HS.getField @"body" stm of
              ApplyFilter_ok (_512 :: RTS.Input) -> HS.pure _512
              _ -> RTS.pError RTS.FromSystem "94:15--94:28"
                     "Pattern match failure"
          RTS.pEnter "PdfDecl.WithStream"
            (pWithStream @(Vector.Vector (ObjectStreamEntry HS.Integer)) _513
               (RTS.pEnter "PdfDecl.ObjectStream" (pObjectStream n first)))
     HS.pure __
 
pResolveObjectStreamEntry ::
      HS.Integer -> (HS.Integer -> (RTS.UInt 64 -> D.Parser TopDecl))
 
pResolveObjectStreamEntry (oid :: HS.Integer) (gen :: HS.Integer)
  (idx :: RTS.UInt 64) =
  do (stm :: Stream) <-
       RTS.pEnter "PdfDecl.ResolveStream"
         (pResolveStream (PdfValue.Value_ref (PdfValue.Ref oid gen)))
     RTS.pEnter "PdfDecl._CheckType"
       (_CheckType (Vector.vecFromRep "ObjStm")
          (HS.getField @"header" stm))
     (n :: RTS.UInt 64) <-
       RTS.pEnter "PdfDecl.LookupSize"
         (pLookupSize (Vector.vecFromRep "N") (HS.getField @"header" stm))
     (first :: RTS.UInt 64) <-
       RTS.pEnter "PdfDecl.LookupSize"
         (pLookupSize (Vector.vecFromRep "First")
            (HS.getField @"header" stm))
     (s :: RTS.Input) <-
       case HS.getField @"body" stm of
         ApplyFilter_ok (_514 :: RTS.Input) -> HS.pure _514
         _ -> RTS.pError RTS.FromSystem "103:14--103:27"
                "Pattern match failure"
     (entry :: ObjectStreamEntry HS.Integer) <-
       RTS.pEnter "PdfDecl.WithStream"
         (pWithStream @(ObjectStreamEntry HS.Integer) s
            (RTS.pEnter "PdfDecl.ObjectStreamNth"
               (pObjectStreamNth n first idx)))
     (__ :: TopDecl) <-
       HS.pure
         (TopDecl (HS.getField @"oid" entry) (RTS.lit 0 :: HS.Integer)
            (TopDeclDef_value (HS.getField @"val" entry)))
     HS.pure __
 
pCheckType ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
pCheckType (x :: Vector.Vector (RTS.UInt 8))
  (h :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (_509 :: HS.Bool) <-
       do (_508 :: Vector.Vector (RTS.UInt 8)) <-
            do (_506 :: PdfValue.Value) <-
                 RTS.pEnter "PdfDecl.LookupResolve"
                   (pLookupResolve (Vector.vecFromRep "Type") h)
               case _506 of
                 PdfValue.Value_name (_507 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                               _507
                 _ -> RTS.pError RTS.FromSystem "300:29--300:58"
                        "Pattern match failure"
          HS.pure (_508 HS.== x)
     RTS.pEnter "PdfValue.Guard" (PdfValue.pGuard _509)
 
pBEBytes :: RTS.UInt 64 -> D.Parser HS.Integer
 
pBEBytes (n :: RTS.UInt 64) =
  do (bs :: Vector.Vector (RTS.UInt 8)) <-
       Vector.replicateM n (RTS.uint8 HS.<$> RTS.pByte "304:28--304:32")
     (__ :: HS.Integer) <-
       HS.pure
         (RTS.loopFold
            (\(v :: HS.Integer) (b :: RTS.UInt 8) ->
               RTS.add (RTS.mul v (RTS.lit 256 :: HS.Integer))
                 (RTS.convert b :: HS.Integer))
            (RTS.lit 0 :: HS.Integer)
            bs)
     HS.pure __
 
pNatN :: RTS.UInt 64 -> D.Parser HS.Integer
 
pNatN (n :: RTS.UInt 64) =
  do (ds :: Vector.Vector HS.Integer) <-
       Vector.replicateM n (RTS.pEnter "PdfValue.Digit" PdfValue.pDigit)
     (__ :: HS.Integer) <-
       HS.pure (PdfValue.numBase (RTS.lit 10 :: HS.Integer) ds)
     HS.pure __
 
pLookupNats ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser (Vector.Vector HS.Integer))
 
pLookupNats (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (kV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve k m)
     (vs :: Vector.Vector PdfValue.Value) <-
       case kV of
         PdfValue.Value_array
           (_518 :: Vector.Vector PdfValue.Value) -> HS.pure _518
         _ -> RTS.pError RTS.FromSystem "322:9--322:19"
                "Pattern match failure"
     (__ :: Vector.Vector HS.Integer) <-
       RTS.loopMapM
         (\(v :: PdfValue.Value) ->
            do (v1 :: PdfValue.Value) <-
                 RTS.pEnter "PdfDecl.ResolveVal" (pResolveVal v)
               (rV :: PdfValue.Number) <-
                 case v1 of
                   PdfValue.Value_number (_520 :: PdfValue.Number) -> HS.pure _520
                   _ -> RTS.pError RTS.FromSystem "325:11--325:22"
                          "Pattern match failure"
               (__ :: HS.Integer) <-
                 RTS.pEnter "PdfValue.NumberAsNat" (PdfValue.pNumberAsNat rV)
               HS.pure __)
         vs
         :: D.Parser (Vector.Vector HS.Integer)
     HS.pure __
 
pLookupRef ::
  forall a.
    RTS.DDL a =>
      a -> (Map.Map a PdfValue.Value -> D.Parser PdfValue.Ref)
 
pLookupRef (k :: a) (m :: Map.Map a PdfValue.Value) =
  do (vV :: PdfValue.Value) <-
       RTS.pIsJust "331:11--331:20"
         ("Missing key: " HS.++ HS.show (k :: a))
         (Map.lookup k m)
     (__ :: PdfValue.Ref) <-
       case vV of
         PdfValue.Value_ref (_523 :: PdfValue.Ref) -> HS.pure _523
         _ -> RTS.pError RTS.FromSystem "332:5--332:13"
                "Pattern match failure"
     HS.pure __
 
pLookupName ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser (Vector.Vector (RTS.UInt 8)))
 
pLookupName (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (vV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve k m)
     (__ :: Vector.Vector (RTS.UInt 8)) <-
       case vV of
         PdfValue.Value_name (_525 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                       _525
         _ -> RTS.pError RTS.FromSystem "337:3--337:12"
                "Pattern match failure"
     HS.pure __
 
_Default :: forall a. RTS.DDL a => D.Parser () -> D.Parser ()
 
_Default (_P :: D.Parser ()) = (RTS.<||) _P (HS.pure ())
 
_FilterParam :: PdfValue.Value -> D.Parser ()
 
_FilterParam (param :: PdfValue.Value) =
  (RTS.|||)
    (case param of
       PdfValue.Value_null (_449 :: ()) -> HS.pure ()
       _ -> RTS.pError RTS.FromSystem "157:7--157:24"
              "Pattern match failure")
    (case param of
       PdfValue.Value_dict
         (_451
            :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                         ()
       _ -> RTS.pError RTS.FromSystem "158:12--158:24"
              "Pattern match failure")
 
_Filter :: PdfValue.Value -> (PdfValue.Value -> D.Parser ())
 
_Filter (name :: PdfValue.Value) (param :: PdfValue.Value) =
  do case name of
       PdfValue.Value_name (_453 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                     ()
       _ -> RTS.pError RTS.FromSystem "152:11--152:22"
              "Pattern match failure"
     RTS.pEnter "PdfDecl._FilterParam" (_FilterParam param)
 
_Decrypt :: RTS.Input -> D.Parser ()
 
_Decrypt (body :: RTS.Input) =
  do HS.void (RTS.pEnter "PdfDecl.Decrypt" (pDecrypt body))
     HS.pure ()
 
_FlateDecode ::
      HS.Integer
        -> (HS.Integer
              -> (HS.Integer -> (HS.Integer -> (RTS.Input -> D.Parser ()))))
 
_FlateDecode (predictor :: HS.Integer) (colors :: HS.Integer)
  (bpc :: HS.Integer)
  (columns :: HS.Integer)
  (body :: RTS.Input) =
  do HS.void
       (RTS.pEnter "PdfDecl.FlateDecode"
          (pFlateDecode predictor colors bpc columns body))
     HS.pure ()
 
_LZWDecode ::
      HS.Integer
        -> (HS.Integer
              -> (HS.Integer
                    -> (HS.Integer -> (HS.Integer -> (RTS.Input -> D.Parser ())))))
 
_LZWDecode (predictor :: HS.Integer) (colors :: HS.Integer)
  (bpc :: HS.Integer)
  (columns :: HS.Integer)
  (earlychange :: HS.Integer)
  (body :: RTS.Input) =
  do HS.void
       (RTS.pEnter "PdfDecl.LZWDecode"
          (pLZWDecode predictor colors bpc columns earlychange body))
     HS.pure ()
 
_ASCIIHexDecode :: RTS.Input -> D.Parser ()
 
_ASCIIHexDecode (body :: RTS.Input) =
  do HS.void
       (RTS.pEnter "PdfDecl.ASCIIHexDecode" (pASCIIHexDecode body))
     HS.pure ()
 
_ASCII85Decode :: RTS.Input -> D.Parser ()
 
_ASCII85Decode (body :: RTS.Input) =
  do HS.void
       (RTS.pEnter "PdfDecl.ASCII85Decode" (pASCII85Decode body))
     HS.pure ()
 
_OneOrArray :: PdfValue.Value -> D.Parser ()
 
_OneOrArray (v :: PdfValue.Value) =
  RTS.pEnter "PdfDecl._Default"
    (_Default @(Vector.Vector PdfValue.Value)
       (case v of
          PdfValue.Value_array
            (_454 :: Vector.Vector PdfValue.Value) -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "282:43--282:52"
                 "Pattern match failure"))
 
_TopDecl :: D.Parser ()
 
_TopDecl =
  do RTS.pEnter "PdfValue._Token"
       (PdfValue._Token @HS.Integer
          (RTS.pEnter "PdfValue._Natural" PdfValue._Natural))
     RTS.pEnter "PdfValue._Token"
       (PdfValue._Token @HS.Integer
          (RTS.pEnter "PdfValue._Natural" PdfValue._Natural))
     RTS.pEnter "PdfValue._KW" (PdfValue._KW (Vector.vecFromRep "obj"))
     (val :: PdfValue.Value) <-
       RTS.pEnter "PdfValue.Value" PdfValue.pValue
     do HS.void (RTS.pEnter "PdfDecl.TopDeclDef" (pTopDeclDef val))
        HS.pure ()
     HS.const ()
       HS.<$> RTS.pMatch "9:3--9:16" (Vector.vecFromRep "endobj")
 
_LookOptArray ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_LookOptArray (key :: Vector.Vector (RTS.UInt 8))
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  RTS.pEnter "PdfDecl._Default"
    (_Default @(Vector.Vector PdfValue.Value)
       (do (x :: PdfValue.Value) <-
             RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve key header)
           RTS.pEnter "PdfDecl._OneOrArray" (_OneOrArray x)))
 
_LZWDecodeParams ::
      HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
        -> D.Parser ()
 
_LZWDecodeParams
  (params
     :: HS.Maybe
          (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) =
  (RTS.<||)
    (case params of
       HS.Nothing -> HS.pure ()
       _ -> RTS.pError RTS.FromSystem "242:5--242:21"
              "Pattern match failure")
    (do (ps :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
          case params of
            HS.Just
              (_457
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _457
            _ -> RTS.pError RTS.FromSystem "246:19--246:32"
                   "Pattern match failure"
        RTS.pEnter "PdfDecl._Default"
          (_Default @HS.Integer
             (do HS.void
                   (RTS.pEnter "PdfDecl.LookupNat"
                      (pLookupNat (Vector.vecFromRep "Predictor") ps))
                 HS.pure ()))
        RTS.pEnter "PdfDecl._Default"
          (_Default @HS.Integer
             (do HS.void
                   (RTS.pEnter "PdfDecl.LookupNat"
                      (pLookupNat (Vector.vecFromRep "Colors") ps))
                 HS.pure ()))
        RTS.pEnter "PdfDecl._Default"
          (_Default @HS.Integer
             (do HS.void
                   (RTS.pEnter "PdfDecl.LookupNat"
                      (pLookupNat (Vector.vecFromRep "BitsPerComponent") ps))
                 HS.pure ()))
        RTS.pEnter "PdfDecl._Default"
          (_Default @HS.Integer
             (do HS.void
                   (RTS.pEnter "PdfDecl.LookupNat"
                      (pLookupNat (Vector.vecFromRep "Columns") ps))
                 HS.pure ()))
        RTS.pEnter "PdfDecl._Default"
          (_Default @HS.Integer
             (do HS.void
                   (RTS.pEnter "PdfDecl.LookupNat"
                      (pLookupNat (Vector.vecFromRep "EarlyChange") ps))
                 HS.pure ())))
 
_LookupNat ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_LookupNat (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (vV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve k m)
     (v :: PdfValue.Number) <-
       case vV of
         PdfValue.Value_number (_458 :: PdfValue.Number) -> HS.pure _458
         _ -> RTS.pError RTS.FromSystem "314:11--314:22"
                "Pattern match failure"
     RTS.pEnter "PdfValue._NumberAsNat" (PdfValue._NumberAsNat v)
 
_FlateDecodeParams ::
      HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
        -> D.Parser ()
 
_FlateDecodeParams
  (params
     :: HS.Maybe
          (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) =
  (RTS.<||)
    (case params of
       HS.Nothing -> HS.pure ()
       _ -> RTS.pError RTS.FromSystem "215:5--215:21"
              "Pattern match failure")
    (do (ps :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
          case params of
            HS.Just
              (_461
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _461
            _ -> RTS.pError RTS.FromSystem "219:17--219:30"
                   "Pattern match failure"
        RTS.pEnter "PdfDecl._Default"
          (_Default @HS.Integer
             (RTS.pEnter "PdfDecl._LookupNat"
                (_LookupNat (Vector.vecFromRep "Predictor") ps)))
        RTS.pEnter "PdfDecl._Default"
          (_Default @HS.Integer
             (RTS.pEnter "PdfDecl._LookupNat"
                (_LookupNat (Vector.vecFromRep "Colors") ps)))
        RTS.pEnter "PdfDecl._Default"
          (_Default @HS.Integer
             (RTS.pEnter "PdfDecl._LookupNat"
                (_LookupNat (Vector.vecFromRep "BitsPerComponent") ps)))
        RTS.pEnter "PdfDecl._Default"
          (_Default @HS.Integer
             (RTS.pEnter "PdfDecl._LookupNat"
                (_LookupNat (Vector.vecFromRep "Columns") ps))))
 
_ApplyFilter :: Filter -> (RTS.Input -> D.Parser ())
 
_ApplyFilter (f :: Filter) (body :: RTS.Input) =
  (RTS.<||)
    (RTS.pEnter "ok"
       (do RTS.pEnter "PdfValue._Guard"
             (PdfValue._Guard
                (HS.getField @"name" f HS.== Vector.vecFromRep "FlateDecode"))
           (params :: TfdDefaults) <-
             RTS.pEnter "PdfDecl.FlateDecodeParams"
               (pFlateDecodeParams (HS.getField @"param" f))
           RTS.pEnter "PdfValue._Guard"
             (PdfValue._Guard
                ((HS.getField @"predictor" params HS.== (RTS.lit 1 :: HS.Integer))
                   HS.|| (HS.getField @"predictor" params
                            HS.== (RTS.lit 12 :: HS.Integer))))
           RTS.pEnter "PdfValue._Guard"
             (PdfValue._Guard
                (HS.getField @"colors" params HS.== (RTS.lit 1 :: HS.Integer)))
           RTS.pEnter "PdfValue._Guard"
             (PdfValue._Guard
                (HS.getField @"bpc" params HS.== (RTS.lit 8 :: HS.Integer)))
           RTS.pErrorMode RTS.Abort
             (RTS.pEnter "PdfDecl._FlateDecode"
                (_FlateDecode (HS.getField @"predictor" params)
                   (HS.getField @"colors" params)
                   (HS.getField @"bpc" params)
                   (HS.getField @"columns" params)
                   body))))
    ((RTS.<||)
       (RTS.pEnter "ok"
          (do RTS.pEnter "PdfValue._Guard"
                (PdfValue._Guard
                   (HS.getField @"name" f HS.== Vector.vecFromRep "LZWDecode"))
              (params :: TlzwDefaults) <-
                RTS.pEnter "PdfDecl.LZWDecodeParams"
                  (pLZWDecodeParams (HS.getField @"param" f))
              RTS.pEnter "PdfValue._Guard"
                (PdfValue._Guard
                   ((HS.getField @"predictor" params HS.== (RTS.lit 1 :: HS.Integer))
                      HS.|| (HS.getField @"predictor" params
                               HS.== (RTS.lit 12 :: HS.Integer))))
              RTS.pEnter "PdfValue._Guard"
                (PdfValue._Guard
                   (HS.getField @"colors" params HS.== (RTS.lit 1 :: HS.Integer)))
              RTS.pEnter "PdfValue._Guard"
                (PdfValue._Guard
                   (HS.getField @"bpc" params HS.== (RTS.lit 8 :: HS.Integer)))
              RTS.pErrorMode RTS.Abort
                (RTS.pEnter "PdfDecl._LZWDecode"
                   (_LZWDecode (HS.getField @"predictor" params)
                      (HS.getField @"colors" params)
                      (HS.getField @"bpc" params)
                      (HS.getField @"columns" params)
                      (HS.getField @"earlychange" params)
                      body))))
       ((RTS.<||)
          (RTS.pEnter "ok"
             (do RTS.pEnter "PdfValue._Guard"
                   (PdfValue._Guard
                      (HS.getField @"name" f HS.== Vector.vecFromRep "ASCIIHexDecode"))
                 RTS.pErrorMode RTS.Abort
                   (RTS.pEnter "PdfDecl._ASCIIHexDecode" (_ASCIIHexDecode body))))
          ((RTS.<||)
             (RTS.pEnter "ok"
                (do RTS.pEnter "PdfValue._Guard"
                      (PdfValue._Guard
                         (HS.getField @"name" f HS.== Vector.vecFromRep "ASCII85Decode"))
                    RTS.pErrorMode RTS.Abort
                      (RTS.pEnter "PdfDecl._ASCII85Decode" (_ASCII85Decode body))))
             (RTS.pEnter "unsupported"
                ((RTS.<||)
                   (case HS.getField @"param" f of
                      HS.Nothing -> HS.pure ()
                      _ -> RTS.pError RTS.FromSystem "209:19--209:36"
                             "Pattern match failure")
                   (HS.pure ()))))))
 
_ApplyFilters ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> (RTS.Input -> D.Parser ())
 
_ApplyFilters
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  (initialBody :: RTS.Input) =
  do (decrypt :: RTS.Input) <-
       RTS.pEnter "PdfDecl.Decrypt" (pDecrypt initialBody)
     (filter_names :: Vector.Vector PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookOptArray"
         (pLookOptArray (Vector.vecFromRep "Filter") header)
     (filter_params :: Vector.Vector PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookOptArray"
         (pLookOptArray (Vector.vecFromRep "DecodeParms") header)
     HS.void
       (RTS.loopIFoldM
          (\(bytes :: ApplyFilter) (ix :: RTS.UInt 64) (name
                                                          :: PdfValue.Value) ->
             do (param :: PdfValue.Value) <-
                  RTS.pEnter "PdfDecl.Default"
                    (pDefault @PdfValue.Value PdfValue.nullValue
                       (RTS.pIsJust "144:34--144:52" "Index out of bounds"
                          ((Vector.!?) filter_params ix)))
                (filter :: Filter) <-
                  RTS.pEnter "PdfDecl.Filter" (pFilter name param)
                (__ :: ApplyFilter) <-
                  RTS.pEnter "PdfDecl.Default"
                    (pDefault @ApplyFilter bytes
                       (do (bs :: RTS.Input) <-
                             case bytes of
                               ApplyFilter_ok (_474 :: RTS.Input) -> HS.pure _474
                               _ -> RTS.pError RTS.FromSystem "146:27--146:37"
                                      "Pattern match failure"
                           RTS.pErrorMode RTS.Abort
                             (do (__ :: ApplyFilter) <-
                                   RTS.pEnter "PdfDecl.ApplyFilter" (pApplyFilter filter bs)
                                 HS.pure __)))
                HS.pure __)
          (ApplyFilter_ok decrypt)
          filter_names)
     HS.pure ()
 
_CheckExpected :: PdfValue.Ref -> (TopDecl -> D.Parser ())
 
_CheckExpected (r :: PdfValue.Ref) (d :: TopDecl) =
  RTS.pEnter "PdfValue._Guard"
    (PdfValue._Guard
       ((HS.getField @"id" d HS.== HS.getField @"obj" r)
          HS.&& (HS.getField @"gen" d HS.== HS.getField @"gen" r)))
 
_ResolveRef :: PdfValue.Ref -> D.Parser ()
 
_ResolveRef (r :: PdfValue.Ref) =
  do HS.void (RTS.pEnter "PdfDecl.ResolveRef" (pResolveRef r))
     HS.pure ()
 
_ResolveValRef :: PdfValue.Ref -> D.Parser ()
 
_ResolveValRef (r :: PdfValue.Ref) =
  do (mb :: HS.Maybe TopDecl) <-
       RTS.pEnter "PdfDecl.ResolveRef" (pResolveRef r)
     (RTS.|||)
       (case mb of
          HS.Nothing -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "85:7--85:19"
                 "Pattern match failure")
       (do (_481 :: TopDeclDef) <-
             do (_480 :: TopDecl) <-
                  case mb of
                    HS.Just (_479 :: TopDecl) -> HS.pure _479
                    _ -> RTS.pError RTS.FromSystem "86:22--86:31"
                           "Pattern match failure"
                RTS.pEnter "PdfDecl.CheckExpected" (pCheckExpected r _480)
           case _481 of
             TopDeclDef_value (_482 :: PdfValue.Value) -> HS.pure ()
             _ -> RTS.pError RTS.FromSystem "86:5--86:41"
                    "Pattern match failure")
 
_ResolveVal :: PdfValue.Value -> D.Parser ()
 
_ResolveVal (v :: PdfValue.Value) =
  RTS.pEnter "PdfDecl._Default"
    (_Default @PdfValue.Value
       (do (r :: PdfValue.Ref) <-
             case v of
               PdfValue.Value_ref (_483 :: PdfValue.Ref) -> HS.pure _483
               _ -> RTS.pError RTS.FromSystem "109:8--109:15"
                      "Pattern match failure"
           RTS.pErrorMode RTS.Abort
             (RTS.pEnter "PdfDecl._ResolveValRef" (_ResolveValRef r))))
 
_LookupResolve ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_LookupResolve (k :: Vector.Vector (RTS.UInt 8))
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (v :: PdfValue.Value) <-
       RTS.pIsJust "115:8--115:22"
         ("Missing key: " HS.++ HS.show (k :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup k header)
     RTS.pEnter "PdfDecl._ResolveVal" (_ResolveVal v)
 
_StreamLen ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_StreamLen
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (lenV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve"
         (pLookupResolve (Vector.vecFromRep "Length") header)
     (lenI :: PdfValue.Number) <-
       case lenV of
         PdfValue.Value_number (_486 :: PdfValue.Number) -> HS.pure _486
         _ -> RTS.pError RTS.FromSystem "134:11--134:24"
                "Pattern match failure"
     (_488 :: HS.Integer) <-
       RTS.pEnter "PdfValue.NumberAsNat" (PdfValue.pNumberAsNat lenI)
     RTS.pIsJust_ "135:3--135:29" "Value does not fit in target type"
       (RTS.convertMaybe _488 :: HS.Maybe (RTS.UInt 64))
 
_StreamBody ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_StreamBody
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @ApplyFilter
       (do (len :: RTS.UInt 64) <-
             RTS.pEnter "PdfDecl.StreamLen" (pStreamLen header)
           RTS.pEnter "PdfDecl._Chunk"
             (_Chunk @ApplyFilter len
                (do (body :: RTS.Input) <- RTS.pPeek
                    RTS.pEnter "PdfDecl._ApplyFilters" (_ApplyFilters header body)))))
 
_Stream :: PdfValue.Value -> D.Parser ()
 
_Stream (val :: PdfValue.Value) =
  do (header
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case val of
         PdfValue.Value_dict
           (_491
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _491
         _ -> RTS.pError RTS.FromSystem "18:12--18:22"
                "Pattern match failure"
     HS.const ()
       HS.<$> RTS.pMatch "19:3--19:16" (Vector.vecFromRep "stream")
     RTS.pErrorMode RTS.Abort
       (do RTS.pEnter "PdfValue._SimpleEOL" PdfValue._SimpleEOL
           RTS.pEnter "PdfDecl._StreamBody" (_StreamBody header)
           RTS.pEnter "PdfValue._KW"
             (PdfValue._KW (Vector.vecFromRep "endstream")))
 
_TopDeclDef :: PdfValue.Value -> D.Parser ()
 
_TopDeclDef (val :: PdfValue.Value) =
  (RTS.<||)
    (RTS.pEnter "stream" (RTS.pEnter "PdfDecl._Stream" (_Stream val)))
    (RTS.pEnter "value" (HS.pure ()))
 
_ObjectStreamEntry :: forall a. RTS.DDL a => D.Parser ()
 
_ObjectStreamEntry = RTS.pEnter "PdfValue._Value" PdfValue._Value
 
_ObjStreamMeta :: D.Parser ()
 
_ObjStreamMeta =
  do RTS.pEnter "PdfValue._Token"
       (PdfValue._Token @HS.Integer
          (RTS.pEnter "PdfValue._Natural" PdfValue._Natural))
     (_494 :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (PdfValue.pToken @HS.Integer
            (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
     RTS.pIsJust_ "41:14--41:37" "Value does not fit in target type"
       (RTS.convertMaybe _494 :: HS.Maybe (RTS.UInt 64))
 
_ObjectStream :: RTS.UInt 64 -> (RTS.UInt 64 -> D.Parser ())
 
_ObjectStream (n :: RTS.UInt 64) (first :: RTS.UInt 64) =
  do (meta :: Vector.Vector ObjStreamMeta) <-
       Vector.replicateM n
         (RTS.pEnter "PdfDecl.ObjStreamMeta" (pObjStreamMeta first))
     HS.void
       (RTS.loopMapM
          (\(entry :: ObjStreamMeta) ->
             do (here :: RTS.UInt 64) <- RTS.pOffset
                RTS.pEnter "PdfValue._Guard"
                  (PdfValue._Guard (here HS.<= HS.getField @"off" entry))
                RTS.pEnter "PdfDecl._SkipBytes"
                  (_SkipBytes (RTS.sub (HS.getField @"off" entry) here))
                (__ :: ObjectStreamEntry HS.Integer) <-
                  RTS.pEnter "PdfDecl.ObjectStreamEntry"
                    (pObjectStreamEntry @HS.Integer (HS.getField @"oid" entry))
                HS.pure __)
          meta
          :: D.Parser (Vector.Vector (ObjectStreamEntry HS.Integer)))
     HS.pure ()
 
_ObjectStreamNth ::
      RTS.UInt 64 -> (RTS.UInt 64 -> (RTS.UInt 64 -> D.Parser ()))
 
_ObjectStreamNth (n :: RTS.UInt 64) (first :: RTS.UInt 64)
  (idx :: RTS.UInt 64) =
  do (meta :: Vector.Vector ObjStreamMeta) <-
       Vector.replicateM n
         (RTS.pEnter "PdfDecl.ObjStreamMeta" (pObjStreamMeta first))
     (entry :: ObjStreamMeta) <-
       RTS.pIsJust "57:12--57:21" "Index out of bounds"
         ((Vector.!?) meta idx)
     (here :: RTS.UInt 64) <- RTS.pOffset
     RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (here HS.<= HS.getField @"off" entry))
     RTS.pEnter "PdfDecl._SkipBytes"
       (_SkipBytes (RTS.sub (HS.getField @"off" entry) here))
     RTS.pEnter "PdfDecl._ObjectStreamEntry"
       (_ObjectStreamEntry @HS.Integer)
 
_ResolveStream :: PdfValue.Value -> D.Parser ()
 
_ResolveStream (v :: PdfValue.Value) =
  do (r :: PdfValue.Ref) <-
       case v of
         PdfValue.Value_ref (_499 :: PdfValue.Ref) -> HS.pure _499
         _ -> RTS.pError RTS.FromSystem "79:9--79:16"
                "Pattern match failure"
     (_504 :: TopDeclDef) <-
       do (_503 :: TopDecl) <-
            do (_501 :: HS.Maybe TopDecl) <-
                 RTS.pEnter "PdfDecl.ResolveRef" (pResolveRef r)
               case _501 of
                 HS.Just (_502 :: TopDecl) -> HS.pure _502
                 _ -> RTS.pError RTS.FromSystem "80:20--80:39"
                        "Pattern match failure"
          RTS.pEnter "PdfDecl.CheckExpected" (pCheckExpected r _503)
     case _504 of
       TopDeclDef_stream (_505 :: Stream) -> HS.pure ()
       _ -> RTS.pError RTS.FromSystem "80:3--80:50"
              "Pattern match failure"
 
_LookupSize ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_LookupSize (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (_510 :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat" (pLookupNat k m)
     RTS.pIsJust_ "318:22--318:45" "Value does not fit in target type"
       (RTS.convertMaybe _510 :: HS.Maybe (RTS.UInt 64))
 
_WithStream ::
  forall d. RTS.DDL d => RTS.Input -> (D.Parser () -> D.Parser ())
 
_WithStream (s :: RTS.Input) (_P :: D.Parser ()) =
  do (cur :: RTS.Input) <- RTS.pPeek
     RTS.pSetInput s
     _P
     RTS.pSetInput cur
 
_ResolveObjectStream :: PdfValue.Value -> D.Parser ()
 
_ResolveObjectStream (v :: PdfValue.Value) =
  do (stm :: Stream) <-
       RTS.pEnter "PdfDecl.ResolveStream" (pResolveStream v)
     RTS.pEnter "PdfDecl._CheckType"
       (_CheckType (Vector.vecFromRep "ObjStm")
          (HS.getField @"header" stm))
     (n :: RTS.UInt 64) <-
       RTS.pEnter "PdfDecl.LookupSize"
         (pLookupSize (Vector.vecFromRep "N") (HS.getField @"header" stm))
     (first :: RTS.UInt 64) <-
       RTS.pEnter "PdfDecl.LookupSize"
         (pLookupSize (Vector.vecFromRep "First")
            (HS.getField @"header" stm))
     (_513 :: RTS.Input) <-
       case HS.getField @"body" stm of
         ApplyFilter_ok (_512 :: RTS.Input) -> HS.pure _512
         _ -> RTS.pError RTS.FromSystem "94:15--94:28"
                "Pattern match failure"
     RTS.pEnter "PdfDecl._WithStream"
       (_WithStream @(Vector.Vector (ObjectStreamEntry HS.Integer)) _513
          (RTS.pEnter "PdfDecl._ObjectStream" (_ObjectStream n first)))
 
_ResolveObjectStreamEntry ::
      HS.Integer -> (HS.Integer -> (RTS.UInt 64 -> D.Parser ()))
 
_ResolveObjectStreamEntry (oid :: HS.Integer) (gen :: HS.Integer)
  (idx :: RTS.UInt 64) =
  do (stm :: Stream) <-
       RTS.pEnter "PdfDecl.ResolveStream"
         (pResolveStream (PdfValue.Value_ref (PdfValue.Ref oid gen)))
     RTS.pEnter "PdfDecl._CheckType"
       (_CheckType (Vector.vecFromRep "ObjStm")
          (HS.getField @"header" stm))
     (n :: RTS.UInt 64) <-
       RTS.pEnter "PdfDecl.LookupSize"
         (pLookupSize (Vector.vecFromRep "N") (HS.getField @"header" stm))
     (first :: RTS.UInt 64) <-
       RTS.pEnter "PdfDecl.LookupSize"
         (pLookupSize (Vector.vecFromRep "First")
            (HS.getField @"header" stm))
     (s :: RTS.Input) <-
       case HS.getField @"body" stm of
         ApplyFilter_ok (_514 :: RTS.Input) -> HS.pure _514
         _ -> RTS.pError RTS.FromSystem "103:14--103:27"
                "Pattern match failure"
     RTS.pEnter "PdfDecl._WithStream"
       (_WithStream @(ObjectStreamEntry HS.Integer) s
          (RTS.pEnter "PdfDecl._ObjectStreamNth"
             (_ObjectStreamNth n first idx)))
 
_BEBytes :: RTS.UInt 64 -> D.Parser ()
 
_BEBytes (n :: RTS.UInt 64) =
  RTS.pSkipExact n (HS.const () HS.<$> RTS.pByte "304:28--304:32")
 
_NatN :: RTS.UInt 64 -> D.Parser ()
 
_NatN (n :: RTS.UInt 64) =
  RTS.pSkipExact n (RTS.pEnter "PdfValue._Digit" PdfValue._Digit)
 
_LookupNats ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_LookupNats (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (kV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve k m)
     (vs :: Vector.Vector PdfValue.Value) <-
       case kV of
         PdfValue.Value_array
           (_518 :: Vector.Vector PdfValue.Value) -> HS.pure _518
         _ -> RTS.pError RTS.FromSystem "322:9--322:19"
                "Pattern match failure"
     HS.void
       (RTS.loopMapM
          (\(v :: PdfValue.Value) ->
             do (v1 :: PdfValue.Value) <-
                  RTS.pEnter "PdfDecl.ResolveVal" (pResolveVal v)
                (rV :: PdfValue.Number) <-
                  case v1 of
                    PdfValue.Value_number (_520 :: PdfValue.Number) -> HS.pure _520
                    _ -> RTS.pError RTS.FromSystem "325:11--325:22"
                           "Pattern match failure"
                (__ :: HS.Integer) <-
                  RTS.pEnter "PdfValue.NumberAsNat" (PdfValue.pNumberAsNat rV)
                HS.pure __)
          vs
          :: D.Parser (Vector.Vector HS.Integer))
     HS.pure ()
 
_LookupRef ::
  forall a.
    RTS.DDL a => a -> (Map.Map a PdfValue.Value -> D.Parser ())
 
_LookupRef (k :: a) (m :: Map.Map a PdfValue.Value) =
  do (vV :: PdfValue.Value) <-
       RTS.pIsJust "331:11--331:20"
         ("Missing key: " HS.++ HS.show (k :: a))
         (Map.lookup k m)
     case vV of
       PdfValue.Value_ref (_523 :: PdfValue.Ref) -> HS.pure ()
       _ -> RTS.pError RTS.FromSystem "332:5--332:13"
              "Pattern match failure"
 
_LookupName ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_LookupName (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (vV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve k m)
     case vV of
       PdfValue.Value_name (_525 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                     ()
       _ -> RTS.pError RTS.FromSystem "337:3--337:12"
              "Pattern match failure"