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
module PdfDemo where
 
import qualified PdfMonad as D
import qualified PdfXRef
import qualified PdfDecl
import qualified PdfValue
import qualified Stdlib
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Input as RTS
import qualified RTS.Map as Map
import qualified RTS.Vector as Vector
 
 
data CheckContents_0
  = CheckContents_0_isarr (Vector.Vector PdfValue.Value)
  | CheckContents_0_isref ()
  
 
deriving instance HS.Eq CheckContents_0
 
deriving instance HS.Ord CheckContents_0
 
deriving instance HS.Show CheckContents_0
 
instance RTS.DDL CheckContents_0 where
 
instance HS.HasField "isarr" CheckContents_0
           (HS.Maybe (Vector.Vector PdfValue.Value)) where
  getField (CheckContents_0_isarr x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "isref" CheckContents_0 (HS.Maybe ()) where
  getField (CheckContents_0_isref x) = HS.Just x
   
  getField _ = HS.Nothing
 
data TsafetyInfo
  = TsafetyInfo HS.Bool HS.Bool
  
 
deriving instance HS.Eq TsafetyInfo
 
deriving instance HS.Ord TsafetyInfo
 
deriving instance HS.Show TsafetyInfo
 
instance RTS.DDL TsafetyInfo where
 
instance HS.HasField "hasJS" TsafetyInfo HS.Bool where
  getField (TsafetyInfo x _) = x
 
instance HS.HasField "hasURI" TsafetyInfo HS.Bool where
  getField (TsafetyInfo _ x) = x
 
data CheckDecl f
  = CheckDecl f TsafetyInfo
  
 
deriving instance HS.Eq f => HS.Eq (CheckDecl f)
 
deriving instance HS.Ord f => HS.Ord (CheckDecl f)
 
deriving instance HS.Show f => HS.Show (CheckDecl f)
 
instance RTS.DDL f => RTS.DDL (CheckDecl f) where
 
instance HS.HasField "obj" (CheckDecl f) f where
  getField (CheckDecl x _) = x
 
instance HS.HasField "isSafe" (CheckDecl f) TsafetyInfo where
  getField (CheckDecl _ x) = x
 
pCheckParent ::
      HS.Maybe PdfValue.Ref
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
pCheckParent (p :: HS.Maybe PdfValue.Ref)
  (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  (RTS.<||)
    (do case p of
          HS.Nothing -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "37:7--37:18"
                 "Pattern match failure"
        (v :: HS.Maybe PdfValue.Ref) <-
          RTS.pOptional (RTS.<||) HS.Just
            (RTS.pEnter "PdfDecl.LookupRef"
               (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
                  (Vector.vecFromRep "Parent")
                  dict))
        (__ :: ()) <-
          case v of
            HS.Nothing -> HS.pure ()
            _ -> RTS.pError RTS.FromSystem "39:7--39:18"
                   "Pattern match failure"
        HS.pure __)
    (do (pref :: PdfValue.Ref) <-
          case p of
            HS.Just (_713 :: PdfValue.Ref) -> HS.pure _713
            _ -> RTS.pError RTS.FromSystem "42:15--42:23"
                   "Pattern match failure"
        (dpref :: PdfValue.Ref) <-
          RTS.pEnter "PdfDecl.LookupRef"
            (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
               (Vector.vecFromRep "Parent")
               dict)
        (__ :: ()) <-
          RTS.pEnter "Stdlib.Guard" (Stdlib.pGuard (dpref HS.== pref))
        HS.pure __)
 
pIsPage :: HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
pIsPage (p :: HS.Maybe PdfValue.Ref) (r :: PdfValue.Ref) =
  do (v :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case v of
         PdfValue.Value_dict
           (_715
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _715
         _ -> RTS.pError RTS.FromSystem "15:13--15:21"
                "Pattern match failure"
     RTS.pEnter "PdfDecl._CheckType"
       (PdfDecl._CheckType (Vector.vecFromRep "Page") dict)
     (__ :: ()) <-
       RTS.pEnter "PdfDemo.CheckParent" (pCheckParent p dict)
     HS.pure __
 
_CheckParent ::
      HS.Maybe PdfValue.Ref
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_CheckParent (p :: HS.Maybe PdfValue.Ref)
  (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  (RTS.<||)
    (do case p of
          HS.Nothing -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "37:7--37:18"
                 "Pattern match failure"
        (v :: HS.Maybe PdfValue.Ref) <-
          RTS.pOptional (RTS.<||) HS.Just
            (RTS.pEnter "PdfDecl.LookupRef"
               (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
                  (Vector.vecFromRep "Parent")
                  dict))
        case v of
          HS.Nothing -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "39:7--39:18"
                 "Pattern match failure")
    (do (pref :: PdfValue.Ref) <-
          case p of
            HS.Just (_713 :: PdfValue.Ref) -> HS.pure _713
            _ -> RTS.pError RTS.FromSystem "42:15--42:23"
                   "Pattern match failure"
        (dpref :: PdfValue.Ref) <-
          RTS.pEnter "PdfDecl.LookupRef"
            (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
               (Vector.vecFromRep "Parent")
               dict)
        RTS.pEnter "Stdlib._Guard" (Stdlib._Guard (dpref HS.== pref)))
 
pIsPages :: HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
pIsPages (p :: HS.Maybe PdfValue.Ref) (r :: PdfValue.Ref) =
  do (v :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case v of
         PdfValue.Value_dict
           (_717
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _717
         _ -> RTS.pError RTS.FromSystem "24:13--24:21"
                "Pattern match failure"
     RTS.pEnter "PdfDecl._CheckType"
       (PdfDecl._CheckType (Vector.vecFromRep "Pages") dict)
     RTS.pEnter "PdfDemo._CheckParent" (_CheckParent p dict)
     (kidsv :: PdfValue.Value) <-
       RTS.pIsJust "28:14--28:31"
         ("Missing key: "
            HS.++ HS.show
                    (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup (Vector.vecFromRep "Kids") dict)
     (kids :: Vector.Vector PdfValue.Value) <-
       case kidsv of
         PdfValue.Value_array
           (_718 :: Vector.Vector PdfValue.Value) -> HS.pure _718
         _ -> RTS.pError RTS.FromSystem "29:14--29:27"
                "Pattern match failure"
     (__ :: ()) <-
       RTS.loopFoldM
         (\(acc :: ()) (refv :: PdfValue.Value) ->
            do (ref :: PdfValue.Ref) <-
                 case refv of
                   PdfValue.Value_ref (_720 :: PdfValue.Ref) -> HS.pure _720
                   _ -> RTS.pError RTS.FromSystem "31:16--31:26"
                          "Pattern match failure"
               (__ :: ()) <-
                 RTS.pEnter "PdfDemo.IsPageOrPages" (pIsPageOrPages (HS.Just r) ref)
               HS.pure __)
         ()
         kids
     HS.pure __
 
pIsPageOrPages ::
      HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
pIsPageOrPages (p :: HS.Maybe PdfValue.Ref) (c :: PdfValue.Ref) =
  (RTS.|||) (RTS.pEnter "PdfDemo.IsPage" (pIsPage p c))
    (RTS.pEnter "PdfDemo.IsPages" (pIsPages p c))
 
_IsPage :: HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
_IsPage (p :: HS.Maybe PdfValue.Ref) (r :: PdfValue.Ref) =
  do (v :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case v of
         PdfValue.Value_dict
           (_715
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _715
         _ -> RTS.pError RTS.FromSystem "15:13--15:21"
                "Pattern match failure"
     RTS.pEnter "PdfDecl._CheckType"
       (PdfDecl._CheckType (Vector.vecFromRep "Page") dict)
     RTS.pEnter "PdfDemo._CheckParent" (_CheckParent p dict)
 
_IsPageOrPages ::
      HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
_IsPageOrPages (p :: HS.Maybe PdfValue.Ref) (c :: PdfValue.Ref) =
  (RTS.|||) (RTS.pEnter "PdfDemo._IsPage" (_IsPage p c))
    (RTS.pEnter "PdfDemo.IsPages" (pIsPages p c))
 
pIsRootPages :: PdfValue.Ref -> D.Parser HS.Bool
 
pIsRootPages (r :: PdfValue.Ref) =
  RTS.pEnter "PdfDecl.Default"
    (PdfDecl.pDefault @HS.Bool HS.False
       (do RTS.pEnter "PdfDemo._IsPageOrPages"
             (_IsPageOrPages (HS.Nothing :: HS.Maybe PdfValue.Ref) r)
           (__ :: HS.Bool) <- HS.pure HS.True
           HS.pure __))
 
pCheckContents ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
pCheckContents
  (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  (RTS.<||)
    (do (s :: PdfValue.Value) <-
          RTS.pIsJust "49:8--49:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "Contents" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "Contents") d)
        (RTS.<||)
          (RTS.pEnter "isarr"
             (case s of
                PdfValue.Value_array
                  (_724 :: Vector.Vector PdfValue.Value) -> HS.pure ()
                _ -> RTS.pError RTS.FromSystem "51:12--51:21"
                       "Pattern match failure"))
          (RTS.pEnter "isref"
             (do (strm :: PdfDecl.Stream) <-
                   RTS.pEnter "PdfDecl.ResolveStream" (PdfDecl.pResolveStream s)
                 RTS.pErrorMode RTS.Abort
                   (case HS.getField @"body" strm of
                      PdfDecl.ApplyFilter_ok (_726 :: RTS.Input) -> HS.pure ()
                      _ -> RTS.pError RTS.FromSystem "55:18--55:32"
                             "Pattern match failure"))))
    (HS.pure ())
 
pCatalogIsOK :: PdfValue.Ref -> D.Parser HS.Bool
 
pCatalogIsOK (r :: PdfValue.Ref) =
  do (catv :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (cat :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case catv of
         PdfValue.Value_dict
           (_728
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _728
         _ -> RTS.pError RTS.FromSystem "69:12--69:23"
                "Pattern match failure"
     RTS.pEnter "PdfDecl._CheckType"
       (PdfDecl._CheckType (Vector.vecFromRep "Catalog") cat)
     (pages :: PdfValue.Ref) <-
       RTS.pEnter "PdfDecl.LookupRef"
         (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
            (Vector.vecFromRep "Pages")
            cat)
     (__ :: HS.Bool) <-
       RTS.pEnter "PdfDemo.IsRootPages" (pIsRootPages pages)
     HS.pure __
 
safetyInfo :: HS.Bool -> (HS.Bool -> TsafetyInfo)
 
safetyInfo (js :: HS.Bool) (uri :: HS.Bool) = TsafetyInfo js uri
 
safeSafetyInfo :: TsafetyInfo
 
safeSafetyInfo = safetyInfo HS.False HS.False
 
bor :: HS.Bool -> (HS.Bool -> HS.Bool)
 
bor (b1 :: HS.Bool) (b2 :: HS.Bool) =
  if b1
    then HS.True
    else b2
 
mergeSafetyInfo ::
  forall a b.
    (RTS.DDL a, RTS.DDL b, RTS.HasStruct a "hasJS" HS.Bool,
     RTS.HasStruct b "hasJS" HS.Bool, RTS.HasStruct a "hasURI" HS.Bool,
     RTS.HasStruct b "hasURI" HS.Bool) =>
      a -> (b -> TsafetyInfo)
 
mergeSafetyInfo (si1 :: a) (si2 :: b) =
  safetyInfo
    (bor (HS.getField @"hasJS" si1) (HS.getField @"hasJS" si2))
    (bor (HS.getField @"hasURI" si1) (HS.getField @"hasURI" si2))
 
pDictIsAction ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
pDictIsAction (a :: Vector.Vector (RTS.UInt 8))
  (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (n :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pEnter "PdfDecl.LookupName"
         (PdfDecl.pLookupName (Vector.vecFromRep "S") d)
     (__ :: ()) <- RTS.pEnter "Stdlib.Guard" (Stdlib.pGuard (a HS.== n))
     HS.pure __
 
pValueIsSafe :: PdfValue.Value -> D.Parser TsafetyInfo
 
pValueIsSafe (v :: PdfValue.Value) =
  (RTS.|||)
    (RTS.pEnter "Stdlib.When"
       (Stdlib.pWhen @TsafetyInfo @()
          ((RTS.|||)
             ((RTS.|||)
                ((RTS.|||)
                   ((RTS.|||)
                      ((RTS.|||)
                         (case v of
                            PdfValue.Value_null (_731 :: ()) -> HS.pure ()
                            _ -> RTS.pError RTS.FromSystem "112:17--112:25"
                                   "Pattern match failure")
                         (case v of
                            PdfValue.Value_bool (_732 :: HS.Bool) -> HS.pure ()
                            _ -> RTS.pError RTS.FromSystem "112:32--112:40"
                                   "Pattern match failure"))
                      (case v of
                         PdfValue.Value_ref (_733 :: PdfValue.Ref) -> HS.pure ()
                         _ -> RTS.pError RTS.FromSystem "112:47--112:54"
                                "Pattern match failure"))
                   (case v of
                      PdfValue.Value_name (_734 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                                    ()
                      _ -> RTS.pError RTS.FromSystem "112:61--112:69"
                             "Pattern match failure"))
                (case v of
                   PdfValue.Value_string
                     (_735 :: Vector.Vector (RTS.UInt 8)) -> HS.pure ()
                   _ -> RTS.pError RTS.FromSystem "113:19--113:29"
                          "Pattern match failure"))
             (case v of
                PdfValue.Value_number (_736 :: PdfValue.Number) -> HS.pure ()
                _ -> RTS.pError RTS.FromSystem "113:36--113:46"
                       "Pattern match failure"))
          safeSafetyInfo))
    ((RTS.|||)
       (do (dict
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
             case v of
               PdfValue.Value_dict
                 (_737
                    :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                                 _737
               _ -> RTS.pError RTS.FromSystem "114:19--114:27"
                      "Pattern match failure"
           (__ :: TsafetyInfo) <-
             (RTS.<||)
               (RTS.pEnter "Stdlib.When"
                  (Stdlib.pWhen @TsafetyInfo @()
                     (RTS.pEnter "PdfDemo.DictIsAction"
                        (pDictIsAction (Vector.vecFromRep "JavaScript") dict))
                     (safetyInfo HS.True HS.False)))
               ((RTS.<||)
                  (RTS.pEnter "Stdlib.When"
                     (Stdlib.pWhen @TsafetyInfo @()
                        (RTS.pEnter "PdfDemo.DictIsAction"
                           (pDictIsAction (Vector.vecFromRep "URI") dict))
                        (safetyInfo HS.False HS.True)))
                  (RTS.loopFoldM
                     (\(acc :: TsafetyInfo) (v :: PdfValue.Value) ->
                        do (r :: TsafetyInfo) <-
                             RTS.pEnter "PdfDemo.ValueIsSafe" (pValueIsSafe v)
                           (__ :: TsafetyInfo) <-
                             HS.pure (mergeSafetyInfo @TsafetyInfo @TsafetyInfo acc r)
                           HS.pure __)
                     safeSafetyInfo
                     dict))
           HS.pure __)
       (do (arr :: Vector.Vector PdfValue.Value) <-
             case v of
               PdfValue.Value_array
                 (_740 :: Vector.Vector PdfValue.Value) -> HS.pure _7