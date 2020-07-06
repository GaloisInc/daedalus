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
module PdfDecl where
 
import qualified Primitives.Resolve as D
import qualified Primitives.Deflate as D
import qualified Primitives.LZW as D
import qualified Primitives.ASCIIHex as D
import qualified Primitives.ASCII85 as D
import qualified PdfMonad as D
import qualified PdfValue
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Vector as Vector
import qualified RTS.Map as Map
 
 
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
 
data TopDeclDef_0
  = TopDeclDef_0
      (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
      ApplyFilter
  
 
deriving instance HS.Eq TopDeclDef_0
 
deriving instance HS.Ord TopDeclDef_0
 
deriving instance HS.Show TopDeclDef_0
 
instance RTS.DDL TopDeclDef_0 where
 
instance HS.HasField "header" TopDeclDef_0
           (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) where
  getField (TopDeclDef_0 x _) = x
 
instance HS.HasField "body" TopDeclDef_0 ApplyFilter where
  getField (TopDeclDef_0 _ x) = x
 
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
 
data ObjStreamMeta
  = ObjStreamMeta HS.Integer HS.Integer
  
 
deriving instance HS.Eq ObjStreamMeta
 
deriving instance HS.Ord ObjStreamMeta
 
deriving instance HS.Show ObjStreamMeta
 
instance RTS.DDL ObjStreamMeta where
 
instance HS.HasField "oid" ObjStreamMeta HS.Integer where
  getField (ObjStreamMeta x _) = x
 
instance HS.HasField "off" ObjStreamMeta HS.Integer where
  getField (ObjStreamMeta _ x) = x
 
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
 
data TopDeclDef
  = TopDeclDef_stream TopDeclDef_0
  | TopDeclDef_value PdfValue.Value
  
 
deriving instance HS.Eq TopDeclDef
 
deriving instance HS.Ord TopDeclDef
 
deriving instance HS.Show TopDeclDef
 
instance RTS.DDL TopDeclDef where
 
instance HS.HasField "stream" TopDeclDef
           (HS.Maybe TopDeclDef_0) where
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
 
pASCII85Decode :: RTS.Input -> D.Parser RTS.Input
 
pASCII85Decode (body :: RTS.Input) =
  D.ascii85Decode body :: D.Parser RTS.Input
 
pASCIIHexDecode :: RTS.Input -> D.Parser RTS.Input
 
pASCIIHexDecode (body :: RTS.Input) =
  D.asciiHexDecode body :: D.Parser RTS.Input
 
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
 
pDefault :: forall a. RTS.DDL a => a -> (D.Parser a -> D.Parser a)
 
pDefault (x :: a) (pP :: D.Parser a) = (RTS.<||) pP (HS.pure x)
 
pCheckExpected :: PdfValue.Ref -> (TopDecl -> D.Parser TopDeclDef)
 
pCheckExpected (r :: PdfValue.Ref) (d :: TopDecl) =
  do RTS.pGuard "73:3--73:16" "guard failed"
       (HS.getField @"id" d HS.== HS.getField @"obj" r)
     RTS.pGuard "74:3--74:16" "guard failed"
       (HS.getField @"gen" d HS.== HS.getField @"gen" r)
     (__ :: TopDeclDef) <- HS.pure (HS.getField @"obj" d)
     HS.pure __
 
pResolveRef :: PdfValue.Ref -> D.Parser (HS.Maybe TopDecl)
 
pResolveRef (r :: PdfValue.Ref) =
  D.resolveImpl PdfDecl.pTopDecl PdfDecl.pResolveObjectStreamEntry
    (HS.getField @"obj" r)
    (HS.getField @"gen" r)
    :: D.Parser (HS.Maybe TopDecl)
 
pResolveValRef :: PdfValue.Ref -> D.Parser PdfValue.Value
 
pResolveValRef (r :: PdfValue.Ref) =
  do (mb :: HS.Maybe TopDecl) <-
       RTS.pEnter "PdfDecl.ResolveRef" (pResolveRef r)
     (__ :: PdfValue.Value) <-
       (RTS.|||)
         (do RTS.pGuard "88:7--88:19" "guard failed"
               (mb HS.== (HS.Nothing :: HS.Maybe TopDecl))
             (__ :: PdfValue.Value) <- HS.pure PdfValue.nullValue
             HS.pure __)
         (do (d :: TopDecl) <-
               RTS.pIsJust "89:12--89:21" "Expected `Just`" mb
             (x :: TopDeclDef) <-
               RTS.pEnter "PdfDecl.CheckExpected" (pCheckExpected r d)
             (__ :: PdfValue.Value) <-
               RTS.pIsJust "91:7--91:16" "Expected `value`"
                 (HS.getField @"value" x)
             HS.pure __)
     HS.pure __
 
pResolveVal :: PdfValue.Value -> D.Parser PdfValue.Value
 
pResolveVal (v :: PdfValue.Value) =
  RTS.pEnter "PdfDecl.Default"
    (pDefault @PdfValue.Value v
       (do (r :: PdfValue.Ref) <-
             RTS.pIsJust "115:8--115:15" "Expected `ref`" (HS.getField @"ref" v)
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
       RTS.pIsJust "121:8--121:22"
         ("Missing key: " HS.++ HS.show (k :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup k header)
     (__ :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveVal" (pResolveVal v)
     HS.pure __
 
pLookupNat ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser HS.Integer)
 
pLookupNat (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (vV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve k m)
     (v :: PdfValue.Number) <-
       RTS.pIsJust "322:11--322:22" "Expected `number`"
         (HS.getField @"number" vV)
     (__ :: HS.Integer) <-
       RTS.pEnter "PdfValue.NumberAsNat" (PdfValue.pNumberAsNat v)
     HS.pure __
 
fdDefaults :: TfdDefaults
 
fdDefaults =
  TfdDefaults (RTS.lit 1 :: HS.Integer) (RTS.lit 1 :: HS.Integer)
    (RTS.lit 8 :: HS.Integer)
    (RTS.lit 1 :: HS.Integer)
 
pFlateDecodeParams ::
      HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
        -> D.Parser TfdDefaults
 
pFlateDecodeParams
  (params
     :: HS.Maybe
          (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) =
  (RTS.<||)
    (do RTS.pGuard "219:5--219:21" "guard failed"
          (params
             HS.== (HS.Nothing
                      :: HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)))
        (__ :: TfdDefaults) <- HS.pure fdDefaults
        HS.pure __)
    (do (ps :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
          RTS.pIsJust "223:17--223:30" "Expected `Just`" params
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
    (do RTS.pGuard "246:5--246:21" "guard failed"
          (params
             HS.== (HS.Nothing
                      :: HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)))
        (__ :: TlzwDefaults) <- HS.pure lzwDefaults
        HS.pure __)
    (do (ps :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
          RTS.pIsJust "250:19--250:32" "Expected `Just`" params
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
 
pApplyFilter :: Filter -> (RTS.Input -> D.Parser ApplyFilter)
 
pApplyFilter (f :: Filter) (body :: RTS.Input) =
  (RTS.<||)
    (RTS.pEnter "ok"
       (do (_0 :: RTS.Input) <-
             do RTS.pGuard "171:5--171:27" "guard failed"
                  (HS.getField @"name" f HS.== Vector.vecFromRep "FlateDecode")
                (params :: TfdDefaults) <-
                  RTS.pEnter "PdfDecl.FlateDecodeParams"
                    (pFlateDecodeParams (HS.getField @"param" f))
                (RTS.<||)
                  (RTS.pGuard "174:5--174:25" "guard failed"
                     (HS.getField @"predictor" params HS.== (RTS.lit 1 :: HS.Integer)))
                  (RTS.pGuard "174:30--174:51" "guard failed"
                     (HS.getField @"predictor" params HS.== (RTS.lit 12 :: HS.Integer)))
                RTS.pGuard "175:5--175:22" "guard failed"
                  (HS.getField @"colors" params HS.== (RTS.lit 1 :: HS.Integer))
                RTS.pGuard "176:5--176:19" "guard failed"
                  (HS.getField @"bpc" params HS.== (RTS.lit 8 :: HS.Integer))
                RTS.pErrorMode RTS.Abort
                  (do (__ :: RTS.Input) <-
                        RTS.pEnter "PdfDecl.FlateDecode"
                          (pFlateDecode (HS.getField @"predictor" params)
                             (HS.getField @"colors" params)
                             (HS.getField @"bpc" params)
                             (HS.getField @"columns" params)
                             body)
                      HS.pure __)
           HS.pure (ApplyFilter_ok _0)))
    ((RTS.<||)
       (RTS.pEnter "ok"
          (do (_1 :: RTS.Input) <-
                do RTS.pGuard "186:5--186:25" "guard failed"
                     (HS.getField @"name" f HS.== Vector.vecFromRep "LZWDecode")
                   (params :: TlzwDefaults) <-
                     RTS.pEnter "PdfDecl.LZWDecodeParams"
                       (pLZWDecodeParams (HS.getField @"param" f))
                   (RTS.<||)
                     (RTS.pGuard "188:5--188:25" "guard failed"
                        (HS.getField @"predictor" params HS.== (RTS.lit 1 :: HS.Integer)))
                     (RTS.pGuard "188:30--188:51" "guard failed"
                        (HS.getField @"predictor" params HS.== (RTS.lit 12 :: HS.Integer)))
                   RTS.pGuard "189:5--189:22" "guard failed"
                     (HS.getField @"colors" params HS.== (RTS.lit 1 :: HS.Integer))
                   RTS.pGuard "190:5--190:19" "guard failed"
                     (HS.getField @"bpc" params HS.== (RTS.lit 8 :: HS.Integer))
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
              HS.pure (ApplyFilter_ok _1)))
       ((RTS.<||)
          (RTS.pEnter "ok"
             (do (_2 :: RTS.Input) <-
                   do RTS.pGuard "201:5--201:30" "guard failed"
                        (HS.getField @"name" f HS.== Vector.vecFromRep "ASCIIHexDecode")
                      RTS.pErrorMode RTS.Abort
                        (do (__ :: RTS.Input) <-
                              RTS.pEnter "PdfDecl.ASCIIHexDecode" (pASCIIHexDecode body)
                            HS.pure __)
                 HS.pure (ApplyFilter_ok _2)))
          ((RTS.<||)
             (RTS.pEnter "ok"
                (do (_3 :: RTS.Input) <-
                      do RTS.pGuard "207:5--207:29" "guard failed"
                           (HS.getField @"name" f HS.== Vector.vecFromRep "ASCII85Decode")
                         RTS.pErrorMode RTS.Abort
                           (do (__ :: RTS.Input) <-
                                 RTS.pEnter "PdfDecl.ASCII85Decode" (pASCII85Decode body)
                               HS.pure __)
                    HS.pure (ApplyFilter_ok _3)))
             (RTS.pEnter "unsupported"
                (do (_4 :: Vector.Vector (RTS.UInt 8)) <-
                      (RTS.<||)
                        (do RTS.pGuard "213:19--213:36" "guard failed"
                              (HS.getField @"param" f
                                 HS.== (HS.Nothing
                                          :: HS.Maybe
                                               (Map.Map (Vector.Vector (RTS.UInt 8))
                                                  PdfValue.Value)))
                            (__ :: Vector.Vector (RTS.UInt 8)) <-
                              HS.pure (HS.getField @"name" f)
                            HS.pure __)
                        (HS.pure
                           (Vector.concat
                              (Vector.fromList
                                 [HS.getField @"name" f, Vector.vecFromRep " (with params)"])))
                    HS.pure (ApplyFilter_unsupported _4))))))
 
pFilterParam ::
  forall a b e.
    (RTS.DDL a, RTS.DDL b, RTS.DDL e, RTS.HasUnion a "null" b,
     RTS.HasUnion a "dict" e) =>
      a -> D.Parser (HS.Maybe e)
 
pFilterParam (param :: a) =
  (RTS.|||)
    (do RTS.pIsJust_ "162:7--162:24" "Expected `null`"
          (HS.getField @"null" param)
        (__ :: HS.Maybe e) <- HS.pure (HS.Nothing :: HS.Maybe e)
        HS.pure __)
    (do (x :: e) <-
          RTS.pIsJust "163:12--163:24" "Expected `dict`"
            (HS.getField @"dict" param)
        (__ :: HS.Maybe e) <- HS.pure (HS.Just x)
        HS.pure __)
 
pFilter :: PdfValue.Value -> (PdfValue.Value -> D.Parser Filter)
 
pFilter (name :: PdfValue.Value) (param :: PdfValue.Value) =
  do (name :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "157:11--157:22" "Expected `name`"
         (HS.getField @"name" name)
     (param
        :: HS.Maybe
             (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) <-
       RTS.pEnter "PdfDecl.FilterParam"
         (pFilterParam @PdfValue.Value @()
            @(Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
            param)
     HS.pure (Filter name param)
 
pOneOrArray ::
      PdfValue.Value -> D.Parser (Vector.Vector PdfValue.Value)
 
pOneOrArray (v :: PdfValue.Value) =
  RTS.pEnter "PdfDecl.Default"
    (pDefault @(Vector.Vector PdfValue.Value) (Vector.fromList [v])
       (RTS.pIsJust "286:43--286:52" "Expected `array`"
          (HS.getField @"array" v)))
 
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
  do (filter_names :: Vector.Vector PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookOptArray"
         (pLookOptArray (Vector.vecFromRep "Filter") header)
     (filter_params :: Vector.Vector PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookOptArray"
         (pLookOptArray (Vector.vecFromRep "DecodeParms") header)
     (__ :: ApplyFilter) <-
       RTS.loopIFoldM
         (\(bytes :: ApplyFilter) (ix :: HS.Integer) (name
                                                        :: PdfValue.Value) ->
            do (param :: PdfValue.Value) <-
                 RTS.pEnter "PdfDecl.Default"
                   (pDefault @PdfValue.Value PdfValue.nullValue
                      (RTS.pIsJust "149:34--149:52" "Index out of bounds"
                         ((Vector.!?) filter_params ix)))
               (filter :: Filter) <-
                 RTS.pEnter "PdfDecl.Filter" (pFilter name param)
               (__ :: ApplyFilter) <-
                 RTS.pEnter "PdfDecl.Default"
                   (pDefault @ApplyFilter bytes
                      (do (bs :: RTS.Input) <-
                            RTS.pIsJust "151:27--151:37" "Expected `ok`"
                              (HS.getField @"ok" bytes)
                          RTS.pErrorMode RTS.Abort
                            (do (__ :: ApplyFilter) <-
                                  RTS.pEnter "PdfDecl.ApplyFilter" (pApplyFilter filter bs)
                                HS.pure __)))
               HS.pure __)
         (ApplyFilter_ok initialBody)
         filter_names
     HS.pure __
 
pBEBytes :: HS.Integer -> D.Parser HS.Integer
 
pBEBytes (n :: HS.Integer) =
  do (bs :: Vector.Vector (RTS.UInt 8)) <-
       Vector.replicateM n (RTS.uint8 HS.<$> RTS.pByte "312:28--312:32")
     (__ :: HS.Integer) <-
       HS.pure
         (RTS.loopFold
            (\(v :: HS.Integer) (b :: RTS.UInt 8) ->
               RTS.add (RTS.mul v (RTS.lit 256 :: HS.Integer))
                 (RTS.convert b :: HS.Integer))
            (RTS.lit 0 :: HS.Integer)
            bs)
     HS.pure __
 
pCheckType ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
pCheckType (x :: Vector.Vector (RTS.UInt 8))
  (h :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (t :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve"
         (pLookupResolve (Vector.vecFromRep "Type") h)
     (n :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "306:10--306:18" "Expected `name`"
         (HS.getField @"name" t)
     (__ :: ()) <- RTS.pGuard "307:5--307:10" "guard failed" (x HS.== n)
     HS.pure __
 
pChunk ::
  forall b. RTS.DDL b => HS.Integer -> (D.Parser b -> D.Parser b)
 
pChunk (n :: HS.Integer) (pP :: D.Parser b) =
  do (cur :: RTS.Input) <- RTS.pPeek
     (this :: RTS.Input) <-
       RTS.pIsJust "292:11--292:16" "Not enough bytes"
         (RTS.limitLen n cur)
     (next :: RTS.Input) <-
       RTS.pIsJust "293:11--293:16" "Not enough bytes"
         (RTS.advanceBy n cur)
     RTS.pSetInput this
     (__ :: b) <- pP
     RTS.pSetInput next
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
       RTS.pIsJust "343:3--343:12" "Expected `name`"
         (HS.getField @"name" vV)
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
       RTS.pIsJust "328:9--328:19" "Expected `array`"
         (HS.getField @"array" kV)
     (__ :: Vector.Vector HS.Integer) <-
       RTS.loopMapM
         (\(v :: PdfValue.Value) ->
            do (v1 :: PdfValue.Value) <-
                 RTS.pEnter "PdfDecl.ResolveVal" (pResolveVal v)
               (rV :: PdfValue.Number) <-
                 RTS.pIsJust "331:11--331:22" "Expected `number`"
                   (HS.getField @"number" v1)
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
       RTS.pIsJust "337:11--337:20"
         ("Missing key: " HS.++ HS.show (k :: a))
         (Map.lookup k m)
     (__ :: PdfValue.Ref) <-
       RTS.pIsJust "338:5--338:13" "Expected `ref`"
         (HS.getField @"ref" vV)
     HS.pure __
 
pNatN :: HS.Integer -> D.Parser HS.Integer
 
pNatN (n :: HS.Integer) =
  do (ds :: Vector.Vector HS.Integer) <-
       Vector.replicateM n (RTS.pEnter "PdfValue.Digit" PdfValue.pDigit)
     (__ :: HS.Integer) <-
       HS.pure
         (PdfValue.numBase @(Vector.Vector HS.Integer) @HS.Integer
            @HS.Integer
            (RTS.lit 10 :: HS.Integer)
            ds)
     HS.pure __
 
pObjStreamMeta :: HS.Integer -> D.Parser ObjStreamMeta
 
pObjStreamMeta (first :: HS.Integer) =
  do (oid :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (PdfValue.pToken @HS.Integer
            (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
     (reloff :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (PdfValue.pToken @HS.Integer
            (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
     (off :: HS.Integer) <- HS.pure (RTS.add reloff first)
     HS.pure (ObjStreamMeta oid off)
 
pObjectStreamEntry ::
  forall a. RTS.DDL a => a -> D.Parser (ObjectStreamEntry a)
 
pObjectStreamEntry (oid :: a) =
  do (oid :: a) <- HS.pure oid
     (val :: PdfValue.Value) <-
       RTS.pEnter "PdfValue.Value" PdfValue.pValue
     HS.pure (ObjectStreamEntry oid val)
 
_Chunk ::
  forall b. RTS.DDL b => HS.Integer -> (D.Parser () -> D.Parser ())
 
_Chunk (n :: HS.Integer) (_P :: D.Parser ()) =
  do (cur :: RTS.Input) <- RTS.pPeek
     (this :: RTS.Input) <-
       RTS.pIsJust "292:11--292:16" "Not enough bytes"
         (RTS.limitLen n cur)
     (next :: RTS.Input) <-
       RTS.pIsJust "293:11--293:16" "Not enough bytes"
         (RTS.advanceBy n cur)
     RTS.pSetInput this
     _P
     RTS.pSetInput next
 
_SkipBytes :: HS.Integer -> D.Parser ()
 
_SkipBytes (n :: HS.Integer) =
  RTS.pEnter "PdfDecl._Chunk" (_Chunk @() n (HS.pure ()))
 
pObjectStream ::
      HS.Integer
        -> (HS.Integer
              -> D.Parser (Vector.Vector (ObjectStreamEntry HS.Integer)))
 
pObjectStream (n :: HS.Integer) (first :: HS.Integer) =
  do (meta :: Vector.Vector ObjStreamMeta) <-
       Vector.replicateM n
         (RTS.pEnter "PdfDecl.ObjStreamMeta" (pObjStreamMeta first))
     (__ :: Vector.Vector (ObjectStreamEntry HS.Integer)) <-
       RTS.loopMapM
         (\(entry :: ObjStreamMeta) ->
            do (here :: HS.Integer) <- HS.toInteger HS.<$> RTS.pOffset
               RTS.pGuard "47:5--47:21" "guard failed"
                 (here HS.<= HS.getField @"off" entry)
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
      HS.Integer
        -> (HS.Integer
              -> (HS.Integer -> D.Parser (ObjectStreamEntry HS.Integer)))
 
pObjectStreamNth (n :: HS.Integer) (first :: HS.Integer)
  (idx :: HS.Integer) =
  do (meta :: Vector.Vector ObjStreamMeta) <-
       Vector.replicateM n
         (RTS.pEnter "PdfDecl.ObjStreamMeta" (pObjStreamMeta first))
     (entry :: ObjStreamMeta) <-
       RTS.pIsJust "56:12--56:21" "Index out of bounds"
         ((Vector.!?) meta idx)
     (here :: HS.Integer) <- HS.toInteger HS.<$> RTS.pOffset
     RTS.pGuard "58:3--58:19" "guard failed"
       (here HS.<= HS.getField @"off" entry)
     RTS.pEnter "PdfDecl._SkipBytes"
       (_SkipBytes (RTS.sub (HS.getField @"off" entry) here))
     (__ :: ObjectStreamEntry HS.Integer) <-
       RTS.pEnter "PdfDecl.ObjectStreamEntry"
         (pObjectStreamEntry @HS.Integer (HS.getField @"oid" entry))
     HS.pure __
 
pResolveStream :: PdfValue.Value -> D.Parser TopDeclDef_0
 
pResolveStream (v :: PdfValue.Value) =
  do (r :: PdfValue.Ref) <-
       RTS.pIsJust "79:9--79:16" "Expected `ref`" (HS.getField @"ref" v)
     (mb :: HS.Maybe TopDecl) <-
       RTS.pEnter "PdfDecl.ResolveRef" (pResolveRef r)
     (d :: TopDecl) <- RTS.pIsJust "81:9--81:18" "Expected `Just`" mb
     (x :: TopDeclDef) <-
       RTS.pEnter "PdfDecl.CheckExpected" (pCheckExpected r d)
     (__ :: TopDeclDef_0) <-
       RTS.pIsJust "83:3--83:13" "Expected `stream`"
         (HS.getField @"stream" x)
     HS.pure __
 
pWithStream ::
  forall b. RTS.DDL b => RTS.Input -> (D.Parser b -> D.Parser b)
 
pWithStream (s :: RTS.Input) (pP :: D.Parser b) =
  do (cur :: RTS.Input) <- RTS.pPeek
     RTS.pSetInput s
     (__ :: b) <- pP
     RTS.pSetInput cur
     HS.pure __
 
_CheckType ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_CheckType (x :: Vector.Vector (RTS.UInt 8))
  (h :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (t :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve"
         (pLookupResolve (Vector.vecFromRep "Type") h)
     (n :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "306:10--306:18" "Expected `name`"
         (HS.getField @"name" t)
     RTS.pGuard "307:5--307:10" "guard failed" (x HS.== n)
 
pResolveObjectStream ::
      PdfValue.Value
        -> D.Parser (Vector.Vector (ObjectStreamEntry HS.Integer))
 
pResolveObjectStream (v :: PdfValue.Value) =
  do (stm :: TopDeclDef_0) <-
       RTS.pEnter "PdfDecl.ResolveStream" (pResolveStream v)
     RTS.pEnter "PdfDecl._CheckType"
       (_CheckType (Vector.vecFromRep "ObjStm")
          (HS.getField @"header" stm))
     (n :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (pLookupNat (Vector.vecFromRep "N") (HS.getField @"header" stm))
     (first :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (pLookupNat (Vector.vecFromRep "First")
            (HS.getField @"header" stm))
     (s :: RTS.Input) <-
       RTS.pIsJust "100:14--100:27" "Expected `ok`"
         (HS.getField @"ok" (HS.getField @"body" stm))
     (__ :: Vector.Vector (ObjectStreamEntry HS.Integer)) <-
       RTS.pEnter "PdfDecl.WithStream"
         (pWithStream @(Vector.Vector (ObjectStreamEntry HS.Integer)) s
            (RTS.pEnter "PdfDecl.ObjectStream" (pObjectStream n first)))
     HS.pure __
 
pResolveObjectStreamEntry ::
      HS.Integer -> (HS.Integer -> (HS.Integer -> D.Parser TopDecl))
 
pResolveObjectStreamEntry (oid :: HS.Integer) (gen :: HS.Integer)
  (idx :: HS.Integer) =
  do (stm :: TopDeclDef_0) <-
       RTS.pEnter "PdfDecl.ResolveStream"
         (pResolveStream (PdfValue.Value_ref (PdfValue.Ref oid gen)))
     RTS.pEnter "PdfDecl._CheckType"
       (_CheckType (Vector.vecFromRep "ObjStm")
          (HS.getField @"header" stm))
     (n :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (pLookupNat (Vector.vecFromRep "N") (HS.getField @"header" stm))
     (first :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (pLookupNat (Vector.vecFromRep "First")
            (HS.getField @"header" stm))
     (s :: RTS.Input) <-
       RTS.pIsJust "109:14--109:27" "Expected `ok`"
         (HS.getField @"ok" (HS.getField @"body" stm))
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
 
pSkipBytes :: HS.Integer -> D.Parser ()
 
pSkipBytes (n :: HS.Integer) =
  RTS.pEnter "PdfDecl.Chunk" (pChunk @() n (HS.pure ()))
 
pStreamLen ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> D.Parser HS.Integer
 
pStreamLen
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (lenV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve"
         (pLookupResolve (Vector.vecFromRep "Length") header)
     (lenI :: PdfValue.Number) <-
       RTS.pIsJust "140:11--140:24" "Expected `number`"
         (HS.getField @"number" lenV)
     (__ :: HS.Integer) <-
       RTS.pEnter "PdfValue.NumberAsNat" (PdfValue.pNumberAsNat lenI)
     HS.pure __
 
pStreamBody ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> D.Parser ApplyFilter
 
pStreamBody
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  RTS.pEnter "PdfValue.Token"
    (PdfValue.pToken @ApplyFilter
       (do (len :: HS.Integer) <-
             RTS.pEnter "PdfDecl.StreamLen" (pStreamLen header)
           (__ :: ApplyFilter) <-
             RTS.pEnter "PdfDecl.Chunk"
               (pChunk @ApplyFilter len
                  (do (body :: RTS.Input) <- RTS.pPeek
                      (__ :: ApplyFilter) <-
                        RTS.pEnter "PdfDecl.ApplyFilters" (pApplyFilters header body)
                      HS.pure __))
           HS.pure __))
 
pTopDeclDef :: PdfValue.Value -> D.Parser TopDeclDef
 
pTopDeclDef (val :: PdfValue.Value) =
  (RTS.<||)
    (RTS.pEnter "stream"
       (do (_5 :: TopDeclDef_0) <-
             do (header
                   :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
                  RTS.pIsJust "14:14--14:24" "Expected `dict`"
                    (HS.getField @"dict" val)
                HS.const ()
                  HS.<$> RTS.pMatch "15:5--15:12" (Vector.vecFromRep "stream")
                RTS.pErrorMode RTS.Abort
                  (do RTS.pEnter "PdfValue._SimpleEOL" PdfValue._SimpleEOL
                      (body :: ApplyFilter) <-
                        RTS.pEnter "PdfDecl.StreamBody" (pStreamBody header)
                      RTS.pEnter "PdfValue._KW"
                        (PdfValue._KW @(Vector.Vector (RTS.UInt 8))
                           (HS.const ()
                              HS.<$> RTS.pMatch "19:8--19:18" (Vector.vecFromRep "endstream")))
                      HS.pure (TopDeclDef_0 header body))
           HS.pure (TopDeclDef_stream _5)))
    (RTS.pEnter "value"
       (do (_6 :: PdfValue.Value) <- HS.pure val
           HS.pure (TopDeclDef_value _6)))
 
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
     RTS.pEnter "PdfValue._KW"
       (PdfValue._KW @(Vector.Vector (RTS.UInt 8))
          (HS.const ()
             HS.<$> RTS.pMatch "6:6--6:10" (Vector.vecFromRep "obj")))
     (val :: PdfValue.Value) <-
       RTS.pEnter "PdfValue.Value" PdfValue.pValue
     (obj :: TopDeclDef) <-
       RTS.pEnter "PdfDecl.TopDeclDef" (pTopDeclDef val)
     HS.const ()
       HS.<$> RTS.pMatch "9:3--9:10" (Vector.vecFromRep "endobj")
     HS.pure (TopDecl id gen obj)
 
_ASCII85Decode :: RTS.Input -> D.Parser ()
 
_ASCII85Decode (body :: RTS.Input) =
  do HS.void
       (RTS.pEnter "PdfDecl.ASCII85Decode" (pASCII85Decode body))
     HS.pure ()
 
_ASCIIHexDecode :: RTS.Input -> D.Parser ()
 
_ASCIIHexDecode (body :: RTS.Input) =
  do HS.void
       (RTS.pEnter "PdfDecl.ASCIIHexDecode" (pASCIIHexDecode body))
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
 
_ApplyFilter :: Filter -> (RTS.Input -> D.Parser ())
 
_ApplyFilter (f :: Filter) (body :: RTS.Input) =
  (RTS.<||)
    (RTS.pEnter "ok"
       (do RTS.pGuard "171:5--171:27" "guard failed"
             (HS.getField @"name" f HS.== Vector.vecFromRep "FlateDecode")
           (params :: TfdDefaults) <-
             RTS.pEnter "PdfDecl.FlateDecodeParams"
               (pFlateDecodeParams (HS.getField @"param" f))
           (RTS.<||)
             (RTS.pGuard "174:5--174:25" "guard failed"
                (HS.getField @"predictor" params HS.== (RTS.lit 1 :: HS.Integer)))
             (RTS.pGuard "174:30--174:51" "guard failed"
                (HS.getField @"predictor" params HS.== (RTS.lit 12 :: HS.Integer)))
           RTS.pGuard "175:5--175:22" "guard failed"
             (HS.getField @"colors" params HS.== (RTS.lit 1 :: HS.Integer))
           RTS.pGuard "176:5--176:19" "guard failed"
             (HS.getField @"bpc" params HS.== (RTS.lit 8 :: HS.Integer))
           RTS.pErrorMode RTS.Abort
             (RTS.pEnter "PdfDecl._FlateDecode"
                (_FlateDecode (HS.getField @"predictor" params)
                   (HS.getField @"colors" params)
                   (HS.getField @"bpc" params)
                   (HS.getField @"columns" params)
                   body))))
    ((RTS.<||)
       (RTS.pEnter "ok"
          (do RTS.pGuard "186:5--186:25" "guard failed"
                (HS.getField @"name" f HS.== Vector.vecFromRep "LZWDecode")
              (params :: TlzwDefaults) <-
                RTS.pEnter "PdfDecl.LZWDecodeParams"
                  (pLZWDecodeParams (HS.getField @"param" f))
              (RTS.<||)
                (RTS.pGuard "188:5--188:25" "guard failed"
                   (HS.getField @"predictor" params HS.== (RTS.lit 1 :: HS.Integer)))
                (RTS.pGuard "188:30--188:51" "guard failed"
                   (HS.getField @"predictor" params HS.== (RTS.lit 12 :: HS.Integer)))
              RTS.pGuard "189:5--189:22" "guard failed"
                (HS.getField @"colors" params HS.== (RTS.lit 1 :: HS.Integer))
              RTS.pGuard "190:5--190:19" "guard failed"
                (HS.getField @"bpc" params HS.== (RTS.lit 8 :: HS.Integer))
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
             (do RTS.pGuard "201:5--201:30" "guard failed"
                   (HS.getField @"name" f HS.== Vector.vecFromRep "ASCIIHexDecode")
                 RTS.pErrorMode RTS.Abort
                   (RTS.pEnter "PdfDecl._ASCIIHexDecode" (_ASCIIHexDecode body))))
          ((RTS.<||)
             (RTS.pEnter "ok"
                (do RTS.pGuard "207:5--207:29" "guard failed"
                      (HS.getField @"name" f HS.== Vector.vecFromRep "ASCII85Decode")
                    RTS.pErrorMode RTS.Abort
                      (RTS.pEnter "PdfDecl._ASCII85Decode" (_ASCII85Decode body))))
             (RTS.pEnter "unsupported"
                ((RTS.<||)
                   (RTS.pGuard "213:19--213:36" "guard failed"
                      (HS.getField @"param" f
                         HS.== (HS.Nothing
                                  :: HS.Maybe
                                       (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value))))
                   (HS.pure ()))))))
 
_ApplyFilters ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> (RTS.Input -> D.Parser ())
 
_ApplyFilters
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  (initialBody :: RTS.Input) =
  do (filter_names :: Vector.Vector PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookOptArray"
         (pLookOptArray (Vector.vecFromRep "Filter") header)
     (filter_params :: Vector.Vector PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookOptArray"
         (pLookOptArray (Vector.vecFromRep "DecodeParms") header)
     HS.void
       (RTS.loopIFoldM
          (\(bytes :: ApplyFilter) (ix :: HS.Integer) (name
                                                         :: PdfValue.Value) ->
             do (param :: PdfValue.Value) <-
                  RTS.pEnter "PdfDecl.Default"
                    (pDefault @PdfValue.Value PdfValue.nullValue
                       (RTS.pIsJust "149:34--149:52" "Index out of bounds"
                          ((Vector.!?) filter_params ix)))
                (filter :: Filter) <-
                  RTS.pEnter "PdfDecl.Filter" (pFilter name param)
                (__ :: ApplyFilter) <-
                  RTS.pEnter "PdfDecl.Default"
                    (pDefault @ApplyFilter bytes
                       (do (bs :: RTS.Input) <-
                             RTS.pIsJust "151:27--151:37" "Expected `ok`"
                               (HS.getField @"ok" bytes)
                           RTS.pErrorMode RTS.Abort
                             (do (__ :: ApplyFilter) <-
                                   RTS.pEnter "PdfDecl.ApplyFilter" (pApplyFilter filter bs)
                                 HS.pure __)))
                HS.pure __)
          (ApplyFilter_ok initialBody)
          filter_names)
     HS.pure ()
 
_BEBytes :: HS.Integer -> D.Parser ()
 
_BEBytes (n :: HS.Integer) =
  RTS.pSkipExact n (HS.const () HS.<$> RTS.pByte "312:28--312:32")
 
_CheckExpected :: PdfValue.Ref -> (TopDecl -> D.Parser ())
 
_CheckExpected (r :: PdfValue.Ref) (d :: TopDecl) =
  do RTS.pGuard "73:3--73:16" "guard failed"
       (HS.getField @"id" d HS.== HS.getField @"obj" r)
     RTS.pGuard "74:3--74:16" "guard failed"
       (HS.getField @"gen" d HS.== HS.getField @"gen" r)
 
_Default :: forall a. RTS.DDL a => D.Parser () -> D.Parser ()
 
_Default (_P :: D.Parser ()) = (RTS.<||) _P (HS.pure ())
 
_FilterParam ::
  forall a b e.
    (RTS.DDL a, RTS.DDL b, RTS.DDL e, RTS.HasUnion a "null" b,
     RTS.HasUnion a "dict" e) =>
      a -> D.Parser ()
 
_FilterParam (param :: a) =
  (RTS.|||)
    (RTS.pIsJust_ "162:7--162:24" "Expected `null`"
       (HS.getField @"null" param))
    (RTS.pIsJust_ "163:12--163:24" "Expected `dict`"
       (HS.getField @"dict" param))
 
_Filter :: PdfValue.Value -> (PdfValue.Value -> D.Parser ())
 
_Filter (name :: PdfValue.Value) (param :: PdfValue.Value) =
  do RTS.pIsJust_ "157:11--157:22" "Expected `name`"
       (HS.getField @"name" name)
     RTS.pEnter "PdfDecl._FilterParam"
       (_FilterParam @PdfValue.Value @()
          @(Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
          param)
 
_LookupNat ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_LookupNat (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (vV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve k m)
     (v :: PdfValue.Number) <-
       RTS.pIsJust "322:11--322:22" "Expected `number`"
         (HS.getField @"number" vV)
     RTS.pEnter "PdfValue._NumberAsNat" (PdfValue._NumberAsNat v)
 
_FlateDecodeParams ::
      HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
        -> D.Parser ()
 
_FlateDecodeParams
  (params
     :: HS.Maybe
          (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) =
  (RTS.<||)
    (RTS.pGuard "219:5--219:21" "guard failed"
       (params
          HS.== (HS.Nothing
                   :: HS.Maybe
                        (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value))))
    (do (ps :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
          RTS.pIsJust "223:17--223:30" "Expected `Just`" params
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
 
_LZWDecodeParams ::
      HS.Maybe (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
        -> D.Parser ()
 
_LZWDecodeParams
  (params
     :: HS.Maybe
          (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) =
  (RTS.<||)
    (RTS.pGuard "246:5--246:21" "guard failed"
       (params
          HS.== (HS.Nothing
                   :: HS.Maybe
                        (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value))))
    (do (ps :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
          RTS.pIsJust "250:19--250:32" "Expected `Just`" params
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
 
_OneOrArray :: PdfValue.Value -> D.Parser ()
 
_OneOrArray (v :: PdfValue.Value) =
  RTS.pEnter "PdfDecl._Default"
    (_Default @(Vector.Vector PdfValue.Value)
       (RTS.pIsJust_ "286:43--286:52" "Expected `array`"
          (HS.getField @"array" v)))
 
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
 
_LookupName ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_LookupName (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (vV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve k m)
     RTS.pIsJust_ "343:3--343:12" "Expected `name`"
       (HS.getField @"name" vV)
 
_LookupNats ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_LookupNats (k :: Vector.Vector (RTS.UInt 8))
  (m :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (kV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve" (pLookupResolve k m)
     (vs :: Vector.Vector PdfValue.Value) <-
       RTS.pIsJust "328:9--328:19" "Expected `array`"
         (HS.getField @"array" kV)
     HS.void
       (RTS.loopMapM
          (\(v :: PdfValue.Value) ->
             do (v1 :: PdfValue.Value) <-
                  RTS.pEnter "PdfDecl.ResolveVal" (pResolveVal v)
                (rV :: PdfValue.Number) <-
                  RTS.pIsJust "331:11--331:22" "Expected `number`"
                    (HS.getField @"number" v1)
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
       RTS.pIsJust "337:11--337:20"
         ("Missing key: " HS.++ HS.show (k :: a))
         (Map.lookup k m)
     RTS.pIsJust_ "338:5--338:13" "Expected `ref`"
       (HS.getField @"ref" vV)
 
_ResolveValRef :: PdfValue.Ref -> D.Parser ()
 
_ResolveValRef (r :: PdfValue.Ref) =
  do (mb :: HS.Maybe TopDecl) <-
       RTS.pEnter "PdfDecl.ResolveRef" (pResolveRef r)
     (RTS.|||)
       (RTS.pGuard "88:7--88:19" "guard failed"
          (mb HS.== (HS.Nothing :: HS.Maybe TopDecl)))
       (do (d :: TopDecl) <-
             RTS.pIsJust "89:12--89:21" "Expected `Just`" mb
           (x :: TopDeclDef) <-
             RTS.pEnter "PdfDecl.CheckExpected" (pCheckExpected r d)
           RTS.pIsJust_ "91:7--91:16" "Expected `value`"
             (HS.getField @"value" x))
 
_ResolveVal :: PdfValue.Value -> D.Parser ()
 
_ResolveVal (v :: PdfValue.Value) =
  RTS.pEnter "PdfDecl._Default"
    (_Default @PdfValue.Value
       (do (r :: PdfValue.Ref) <-
             RTS.pIsJust "115:8--115:15" "Expected `ref`" (HS.getField @"ref" v)
           RTS.pErrorMode RTS.Abort
             (RTS.pEnter "PdfDecl._ResolveValRef" (_ResolveValRef r))))
 
_LookupResolve ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_LookupResolve (k :: Vector.Vector (RTS.UInt 8))
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (v :: PdfValue.Value) <-
       RTS.pIsJust "121:8--121:22"
         ("Missing key: " HS.++ HS.show (k :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup k header)
     RTS.pEnter "PdfDecl._ResolveVal" (_ResolveVal v)
 
_NatN :: HS.Integer -> D.Parser ()
 
_NatN (n :: HS.Integer) =
  RTS.pSkipExact n (RTS.pEnter "PdfValue._Digit" PdfValue._Digit)
 
_ObjStreamMeta :: D.Parser ()
 
_ObjStreamMeta =
  do RTS.pEnter "PdfValue._Token"
       (PdfValue._Token @HS.Integer
          (RTS.pEnter "PdfValue._Natural" PdfValue._Natural))
     RTS.pEnter "PdfValue._Token"
       (PdfValue._Token @HS.Integer
          (RTS.pEnter "PdfValue._Natural" PdfValue._Natural))
 
_ObjectStream :: HS.Integer -> (HS.Integer -> D.Parser ())
 
_ObjectStream (n :: HS.Integer) (first :: HS.Integer) =
  do (meta :: Vector.Vector ObjStreamMeta) <-
       Vector.replicateM n
         (RTS.pEnter "PdfDecl.ObjStreamMeta" (pObjStreamMeta first))
     HS.void
       (RTS.loopMapM
          (\(entry :: ObjStreamMeta) ->
             do (here :: HS.Integer) <- HS.toInteger HS.<$> RTS.pOffset
                RTS.pGuard "47:5--47:21" "guard failed"
                  (here HS.<= HS.getField @"off" entry)
                RTS.pEnter "PdfDecl._SkipBytes"
                  (_SkipBytes (RTS.sub (HS.getField @"off" entry) here))
                (__ :: ObjectStreamEntry HS.Integer) <-
                  RTS.pEnter "PdfDecl.ObjectStreamEntry"
                    (pObjectStreamEntry @HS.Integer (HS.getField @"oid" entry))
                HS.pure __)
          meta
          :: D.Parser (Vector.Vector (ObjectStreamEntry HS.Integer)))
     HS.pure ()
 
_ObjectStreamEntry :: forall a. RTS.DDL a => D.Parser ()
 
_ObjectStreamEntry = RTS.pEnter "PdfValue._Value" PdfValue._Value
 
_ObjectStreamNth ::
      HS.Integer -> (HS.Integer -> (HS.Integer -> D.Parser ()))
 
_ObjectStreamNth (n :: HS.Integer) (first :: HS.Integer)
  (idx :: HS.Integer) =
  do (meta :: Vector.Vector ObjStreamMeta) <-
       Vector.replicateM n
         (RTS.pEnter "PdfDecl.ObjStreamMeta" (pObjStreamMeta first))
     (entry :: ObjStreamMeta) <-
       RTS.pIsJust "56:12--56:21" "Index out of bounds"
         ((Vector.!?) meta idx)
     (here :: HS.Integer) <- HS.toInteger HS.<$> RTS.pOffset
     RTS.pGuard "58:3--58:19" "guard failed"
       (here HS.<= HS.getField @"off" entry)
     RTS.pEnter "PdfDecl._SkipBytes"
       (_SkipBytes (RTS.sub (HS.getField @"off" entry) here))
     RTS.pEnter "PdfDecl._ObjectStreamEntry"
       (_ObjectStreamEntry @HS.Integer)
 
_WithStream ::
  forall b. RTS.DDL b => RTS.Input -> (D.Parser () -> D.Parser ())
 
_WithStream (s :: RTS.Input) (_P :: D.Parser ()) =
  do (cur :: RTS.Input) <- RTS.pPeek
     RTS.pSetInput s
     _P
     RTS.pSetInput cur
 
_ResolveObjectStream :: PdfValue.Value -> D.Parser ()
 
_ResolveObjectStream (v :: PdfValue.Value) =
  do (stm :: TopDeclDef_0) <-
       RTS.pEnter "PdfDecl.ResolveStream" (pResolveStream v)
     RTS.pEnter "PdfDecl._CheckType"
       (_CheckType (Vector.vecFromRep "ObjStm")
          (HS.getField @"header" stm))
     (n :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (pLookupNat (Vector.vecFromRep "N") (HS.getField @"header" stm))
     (first :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (pLookupNat (Vector.vecFromRep "First")
            (HS.getField @"header" stm))
     (s :: RTS.Input) <-
       RTS.pIsJust "100:14--100:27" "Expected `ok`"
         (HS.getField @"ok" (HS.getField @"body" stm))
     RTS.pEnter "PdfDecl._WithStream"
       (_WithStream @(Vector.Vector (ObjectStreamEntry HS.Integer)) s
          (RTS.pEnter "PdfDecl._ObjectStream" (_ObjectStream n first)))
 
_ResolveObjectStreamEntry ::
      HS.Integer -> (HS.Integer -> (HS.Integer -> D.Parser ()))
 
_ResolveObjectStreamEntry (oid :: HS.Integer) (gen :: HS.Integer)
  (idx :: HS.Integer) =
  do (stm :: TopDeclDef_0) <-
       RTS.pEnter "PdfDecl.ResolveStream"
         (pResolveStream (PdfValue.Value_ref (PdfValue.Ref oid gen)))
     RTS.pEnter "PdfDecl._CheckType"
       (_CheckType (Vector.vecFromRep "ObjStm")
          (HS.getField @"header" stm))
     (n :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (pLookupNat (Vector.vecFromRep "N") (HS.getField @"header" stm))
     (first :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (pLookupNat (Vector.vecFromRep "First")
            (HS.getField @"header" stm))
     (s :: RTS.Input) <-
       RTS.pIsJust "109:14--109:27" "Expected `ok`"
         (HS.getField @"ok" (HS.getField @"body" stm))
     RTS.pEnter "PdfDecl._WithStream"
       (_WithStream @(ObjectStreamEntry HS.Integer) s
          (RTS.pEnter "PdfDecl._ObjectStreamNth"
             (_ObjectStreamNth n first idx)))
 
_ResolveRef :: PdfValue.Ref -> D.Parser ()
 
_ResolveRef (r :: PdfValue.Ref) =
  do HS.void (RTS.pEnter "PdfDecl.ResolveRef" (pResolveRef r))
     HS.pure ()
 
_ResolveStream :: PdfValue.Value -> D.Parser ()
 
_ResolveStream (v :: PdfValue.Value) =
  do (r :: PdfValue.Ref) <-
       RTS.pIsJust "79:9--79:16" "Expected `ref`" (HS.getField @"ref" v)
     (mb :: HS.Maybe TopDecl) <-
       RTS.pEnter "PdfDecl.ResolveRef" (pResolveRef r)
     (d :: TopDecl) <- RTS.pIsJust "81:9--81:18" "Expected `Just`" mb
     (x :: TopDeclDef) <-
       RTS.pEnter "PdfDecl.CheckExpected" (pCheckExpected r d)
     RTS.pIsJust_ "83:3--83:13" "Expected `stream`"
       (HS.getField @"stream" x)
 
_StreamBody ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_StreamBody
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  RTS.pEnter "PdfValue._Token"
    (PdfValue._Token @ApplyFilter
       (do (len :: HS.Integer) <-
             RTS.pEnter "PdfDecl.StreamLen" (pStreamLen header)
           RTS.pEnter "PdfDecl._Chunk"
             (_Chunk @ApplyFilter len
                (do (body :: RTS.Input) <- RTS.pPeek
                    RTS.pEnter "PdfDecl._ApplyFilters" (_ApplyFilters header body)))))
 
_StreamLen ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_StreamLen
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (lenV :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve"
         (pLookupResolve (Vector.vecFromRep "Length") header)
     (lenI :: PdfValue.Number) <-
       RTS.pIsJust "140:11--140:24" "Expected `number`"
         (HS.getField @"number" lenV)
     RTS.pEnter "PdfValue._NumberAsNat" (PdfValue._NumberAsNat lenI)
 
_TopDeclDef :: PdfValue.Value -> D.Parser ()
 
_TopDeclDef (val :: PdfValue.Value) =
  (RTS.<||)
    (RTS.pEnter "stream"
       (do (header
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
             RTS.pIsJust "14:14--14:24" "Expected `dict`"
               (HS.getField @"dict" val)
           HS.const ()
             HS.<$> RTS.pMatch "15:5--15:12" (Vector.vecFromRep "stream")
           RTS.pErrorMode RTS.Abort
             (do RTS.pEnter "PdfValue._SimpleEOL" PdfValue._SimpleEOL
                 RTS.pEnter "PdfDecl._StreamBody" (_StreamBody header)
                 RTS.pEnter "PdfValue._KW"
                   (PdfValue._KW @(Vector.Vector (RTS.UInt 8))
                      (HS.const ()
                         HS.<$> RTS.pMatch "19:8--19:18"
                                  (Vector.vecFromRep "endstream"))))))
    (RTS.pEnter "value" (HS.pure ()))
 
_TopDecl :: D.Parser ()
 
_TopDecl =
  do RTS.pEnter "PdfValue._Token"
       (PdfValue._Token @HS.Integer
          (RTS.pEnter "PdfValue._Natural" PdfValue._Natural))
     RTS.pEnter "PdfValue._Token"
       (PdfValue._Token @HS.Integer
          (RTS.pEnter "PdfValue._Natural" PdfValue._Natural))
     RTS.pEnter "PdfValue._KW"
       (PdfValue._KW @(Vector.Vector (RTS.UInt 8))
          (HS.const ()
             HS.<$> RTS.pMatch "6:6--6:10" (Vector.vecFromRep "obj")))
     (val :: PdfValue.Value) <-
       RTS.pEnter "PdfValue.Value" PdfValue.pValue
     RTS.pEnter "PdfDecl._TopDeclDef" (_TopDeclDef val)
     HS.const ()
       HS.<$> RTS.pMatch "9:3--9:10" (Vector.vecFromRep "endobj")