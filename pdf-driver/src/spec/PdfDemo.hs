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
import qualified PdfContentStream
import qualified PdfXRef
import qualified PdfDecl
import qualified PdfValue
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
  | CheckContents_0_isref
      (Vector.Vector PdfContentStream.PageDescription_0)
  
 
deriving instance HS.Eq CheckContents_0
 
deriving instance HS.Ord CheckContents_0
 
deriving instance HS.Show CheckContents_0
 
instance RTS.DDL CheckContents_0 where
 
instance HS.HasField "isarr" CheckContents_0
           (HS.Maybe (Vector.Vector PdfValue.Value)) where
  getField (CheckContents_0_isarr x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "isref" CheckContents_0
           (HS.Maybe (Vector.Vector PdfContentStream.PageDescription_0)) where
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
 
data CheckDecl
  = CheckDecl PdfDecl.TopDeclDef TsafetyInfo
  
 
deriving instance HS.Eq CheckDecl
 
deriving instance HS.Ord CheckDecl
 
deriving instance HS.Show CheckDecl
 
instance RTS.DDL CheckDecl where
 
instance HS.HasField "obj" CheckDecl PdfDecl.TopDeclDef where
  getField (CheckDecl x _) = x
 
instance HS.HasField "isSafe" CheckDecl TsafetyInfo where
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
            HS.Just (_932 :: PdfValue.Ref) -> HS.pure _932
            _ -> RTS.pError RTS.FromSystem "42:15--42:23"
                   "Pattern match failure"
        (dpref :: PdfValue.Ref) <-
          RTS.pEnter "PdfDecl.LookupRef"
            (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
               (Vector.vecFromRep "Parent")
               dict)
        (__ :: ()) <-
          RTS.pEnter "PdfValue.Guard" (PdfValue.pGuard (dpref HS.== pref))
        HS.pure __)
 
pIsPage :: HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
pIsPage (p :: HS.Maybe PdfValue.Ref) (r :: PdfValue.Ref) =
  do (v :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case v of
         PdfValue.Value_dict
           (_934
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _934
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
            HS.Just (_932 :: PdfValue.Ref) -> HS.pure _932
            _ -> RTS.pError RTS.FromSystem "42:15--42:23"
                   "Pattern match failure"
        (dpref :: PdfValue.Ref) <-
          RTS.pEnter "PdfDecl.LookupRef"
            (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
               (Vector.vecFromRep "Parent")
               dict)
        RTS.pEnter "PdfValue._Guard" (PdfValue._Guard (dpref HS.== pref)))
 
pIsPages :: HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
pIsPages (p :: HS.Maybe PdfValue.Ref) (r :: PdfValue.Ref) =
  do (v :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case v of
         PdfValue.Value_dict
           (_936
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _936
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
           (_937 :: Vector.Vector PdfValue.Value) -> HS.pure _937
         _ -> RTS.pError RTS.FromSystem "29:14--29:27"
                "Pattern match failure"
     (__ :: ()) <-
       RTS.loopFoldM
         (\(acc :: ()) (refv :: PdfValue.Value) ->
            do (ref :: PdfValue.Ref) <-
                 case refv of
                   PdfValue.Value_ref (_939 :: PdfValue.Ref) -> HS.pure _939
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
           (_934
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _934
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
                  (_943 :: Vector.Vector PdfValue.Value) -> HS.pure ()
                _ -> RTS.pError RTS.FromSystem "51:12--51:21"
                       "Pattern match failure"))
          (RTS.pEnter "isref"
             (do (strm :: PdfDecl.Stream) <-
                   RTS.pEnter "PdfDecl.ResolveStream" (PdfDecl.pResolveStream s)
                 RTS.pErrorMode RTS.Abort
                   (do (strmBody :: RTS.Input) <-
                         case HS.getField @"body" strm of
                           PdfDecl.ApplyFilter_ok (_945 :: RTS.Input) -> HS.pure _945
                           _ -> RTS.pError RTS.FromSystem "55:18--55:32"
                                  "Pattern match failure"
                       RTS.pEnter "PdfDecl._WithStream"
                         (PdfDecl._WithStream
                            @(Vector.Vector PdfContentStream.PageDescription_0)
                            strmBody
                            (RTS.pEnter "PdfValue._Only"
                               (PdfValue._Only @(Vector.Vector PdfContentStream.PageDescription_0)
                                  (RTS.pEnter "PdfContentStream._ContentStream"
                                     PdfContentStream._ContentStream))))))))
    (HS.pure ())
 
pCatalogIsOK :: PdfValue.Ref -> D.Parser HS.Bool
 
pCatalogIsOK (r :: PdfValue.Ref) =
  do (catv :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (cat :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case catv of
         PdfValue.Value_dict
           (_948
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _948
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
     (__ :: ()) <-
       RTS.pEnter "PdfValue.Guard" (PdfValue.pGuard (a HS.== n))
     HS.pure __
 
pValueIsSafe :: PdfValue.Value -> D.Parser TsafetyInfo
 
pValueIsSafe (v :: PdfValue.Value) =
  (RTS.|||)
    (RTS.pEnter "PdfValue.When"
       (PdfValue.pWhen @TsafetyInfo @()
          ((RTS.|||)
             ((RTS.|||)
                ((RTS.|||)
                   ((RTS.|||)
                      ((RTS.|||)
                         (case v of
                            PdfValue.Value_null (_951 :: ()) -> HS.pure ()
                            _ -> RTS.pError RTS.FromSystem "112:17--112:25"
                                   "Pattern match failure")
                         (case v of
                            PdfValue.Value_bool (_952 :: HS.Bool) -> HS.pure ()
                            _ -> RTS.pError RTS.FromSystem "112:32--112:40"
                                   "Pattern match failure"))
                      (case v of
                         PdfValue.Value_ref (_953 :: PdfValue.Ref) -> HS.pure ()
                         _ -> RTS.pError RTS.FromSystem "112:47--112:54"
                                "Pattern match failure"))
                   (case v of
                      PdfValue.Value_name (_954 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                                    ()
                      _ -> RTS.pError RTS.FromSystem "112:61--112:69"
                             "Pattern match failure"))
                (case v of
                   PdfValue.Value_string
                     (_955 :: Vector.Vector (RTS.UInt 8)) -> HS.pure ()
                   _ -> RTS.pError RTS.FromSystem "113:19--113:29"
                          "Pattern match failure"))
             (case v of
                PdfValue.Value_number (_956 :: PdfValue.Number) -> HS.pure ()
                _ -> RTS.pError RTS.FromSystem "113:36--113:46"
                       "Pattern match failure"))
          safeSafetyInfo))
    ((RTS.|||)
       (do (dict
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
             case v of
               PdfValue.Value_dict
                 (_957
                    :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                                 _957
               _ -> RTS.pError RTS.FromSystem "114:19--114:27"
                      "Pattern match failure"
           (__ :: TsafetyInfo) <-
             (RTS.<||)
               (RTS.pEnter "PdfValue.When"
                  (PdfValue.pWhen @TsafetyInfo @()
                     (RTS.pEnter "PdfDemo.DictIsAction"
                        (pDictIsAction (Vector.vecFromRep "JavaScript") dict))
                     (safetyInfo HS.True HS.False)))
               ((RTS.<||)
                  (RTS.pEnter "PdfValue.When"
                     (PdfValue.pWhen @TsafetyInfo @()
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
                 (_960 :: Vector.Vector PdfValue.Value) -> HS.pure _960
               _ -> RTS.pError RTS.FromSystem "126:18--126:27"
                      "Pattern match failure"
           (__ :: TsafetyInfo) <-
             RTS.loopFoldM
               (\(acc :: TsafetyInfo) (v :: PdfValue.Value) ->
                  do (r :: TsafetyInfo) <-
                       RTS.pEnter "PdfDemo.ValueIsSafe" (pValueIsSafe v)
                     (__ :: TsafetyInfo) <-
                       HS.pure (mergeSafetyInfo @TsafetyInfo @TsafetyInfo acc r)
                     HS.pure __)
               safeSafetyInfo
               arr
           HS.pure __))
 
pCheckDecl ::
      HS.Integer
        -> (HS.Integer -> (PdfDecl.TopDecl -> D.Parser CheckDecl))
 
pCheckDecl (expectId :: HS.Integer) (expectGen :: HS.Integer)
  (decl :: PdfDecl.TopDecl) =
  do RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (HS.getField @"id" decl HS.== expectId))
     RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (HS.getField @"gen" decl HS.== expectGen))
     (obj :: PdfDecl.TopDeclDef) <- HS.pure (HS.getField @"obj" decl)
     (isSafe :: TsafetyInfo) <-
       (RTS.|||)
         (do (v :: PdfValue.Value) <-
               case HS.getField @"obj" decl of
                 PdfDecl.TopDeclDef_value (_963 :: PdfValue.Value) -> HS.pure _963
                 _ -> RTS.pError RTS.FromSystem "82:19--82:35"
                        "Pattern match failure"
             (__ :: TsafetyInfo) <-
               RTS.pEnter "PdfDemo.ValueIsSafe" (pValueIsSafe v)
             HS.pure __)
         (do case HS.getField @"obj" decl of
               PdfDecl.TopDeclDef_stream (_965 :: PdfDecl.Stream) -> HS.pure ()
               _ -> RTS.pError RTS.FromSystem "83:19--83:36"
                      "Pattern match failure"
             (__ :: TsafetyInfo) <- HS.pure safeSafetyInfo
             HS.pure __)
     HS.pure (CheckDecl obj isSafe)
 
pTopDeclCheck :: HS.Integer -> (HS.Integer -> D.Parser CheckDecl)
 
pTopDeclCheck (expectId :: HS.Integer) (expectGen :: HS.Integer) =
  do (decl :: PdfDecl.TopDecl) <-
       RTS.pEnter "PdfDecl.TopDecl" PdfDecl.pTopDecl
     (__ :: CheckDecl) <-
       RTS.pEnter "PdfDemo.CheckDecl" (pCheckDecl expectId expectGen decl)
     HS.pure __
 
pResolveObjectStreamEntryCheck ::
      HS.Integer
        -> (HS.Integer
              -> (HS.Integer
                    -> (HS.Integer -> (RTS.UInt 64 -> D.Parser CheckDecl))))
 
pResolveObjectStreamEntryCheck (expectId :: HS.Integer)
  (expectGen :: HS.Integer)
  (oid :: HS.Integer)
  (gen :: HS.Integer)
  (idx :: RTS.UInt 64) =
  do (decl :: PdfDecl.TopDecl) <-
       RTS.pEnter "PdfDecl.ResolveObjectStreamEntry"
         (PdfDecl.pResolveObjectStreamEntry oid gen idx)
     (__ :: CheckDecl) <-
       RTS.pEnter "PdfDemo.CheckDecl" (pCheckDecl expectId expectGen decl)
     HS.pure __
 
band :: HS.Bool -> (HS.Bool -> HS.Bool)
 
band (b1 :: HS.Bool) (b2 :: HS.Bool) =
  if b1
    then b2
    else HS.False
 
_IsPages :: HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
_IsPages (p :: HS.Maybe PdfValue.Ref) (r :: PdfValue.Ref) =
  do (v :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case v of
         PdfValue.Value_dict
           (_936
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _936
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
           (_937 :: Vector.Vector PdfValue.Value) -> HS.pure _937
         _ -> RTS.pError RTS.FromSystem "29:14--29:27"
                "Pattern match failure"
     RTS.loopFoldM
       (\(acc :: ()) (refv :: PdfValue.Value) ->
          do (ref :: PdfValue.Ref) <-
               case refv of
                 PdfValue.Value_ref (_939 :: PdfValue.Ref) -> HS.pure _939
                 _ -> RTS.pError RTS.FromSystem "31:16--31:26"
                        "Pattern match failure"
             RTS.pEnter "PdfDemo._IsPageOrPages"
               (_IsPageOrPages (HS.Just r) ref))
       ()
       kids
 
_IsRootPages :: PdfValue.Ref -> D.Parser ()
 
_IsRootPages (r :: PdfValue.Ref) =
  RTS.pEnter "PdfDecl._Default"
    (PdfDecl._Default @HS.Bool
       (RTS.pEnter "PdfDemo._IsPageOrPages"
          (_IsPageOrPages (HS.Nothing :: HS.Maybe PdfValue.Ref) r)))
 
_CheckContents ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_CheckContents
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
                  (_943 :: Vector.Vector PdfValue.Value) -> HS.pure ()
                _ -> RTS.pError RTS.FromSystem "51:12--51:21"
                       "Pattern match failure"))
          (RTS.pEnter "isref"
             (do (strm :: PdfDecl.Stream) <-
                   RTS.pEnter "PdfDecl.ResolveStream" (PdfDecl.pResolveStream s)
                 RTS.pErrorMode RTS.Abort
                   (do (strmBody :: RTS.Input) <-
                         case HS.getField @"body" strm of
                           PdfDecl.ApplyFilter_ok (_945 :: RTS.Input) -> HS.pure _945
                           _ -> RTS.pError RTS.FromSystem "55:18--55:32"
                                  "Pattern match failure"
                       RTS.pEnter "PdfDecl._WithStream"
                         (PdfDecl._WithStream
                            @(Vector.Vector PdfContentStream.PageDescription_0)
                            strmBody
                            (RTS.pEnter "PdfValue._Only"
                               (PdfValue._Only @(Vector.Vector PdfContentStream.PageDescription_0)
                                  (RTS.pEnter "PdfContentStream._ContentStream"
                                     PdfContentStream._ContentStream))))))))
    (HS.pure ())
 
_CatalogIsOK :: PdfValue.Ref -> D.Parser ()
 
_CatalogIsOK (r :: PdfValue.Ref) =
  do (catv :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (cat :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case catv of
         PdfValue.Value_dict
           (_948
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _948
         _ -> RTS.pError RTS.FromSystem "69:12--69:23"
                "Pattern match failure"
     RTS.pEnter "PdfDecl._CheckType"
       (PdfDecl._CheckType (Vector.vecFromRep "Catalog") cat)
     (pages :: PdfValue.Ref) <-
       RTS.pEnter "PdfDecl.LookupRef"
         (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
            (Vector.vecFromRep "Pages")
            cat)
     RTS.pEnter "PdfDemo._IsRootPages" (_IsRootPages pages)
 
_DictIsAction ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
_DictIsAction (a :: Vector.Vector (RTS.UInt 8))
  (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (n :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pEnter "PdfDecl.LookupName"
         (PdfDecl.pLookupName (Vector.vecFromRep "S") d)
     RTS.pEnter "PdfValue._Guard" (PdfValue._Guard (a HS.== n))
 
_ValueIsSafe :: PdfValue.Value -> D.Parser ()
 
_ValueIsSafe (v :: PdfValue.Value) =
  (RTS.|||)
    (RTS.pEnter "PdfValue._When"
       (PdfValue._When @TsafetyInfo @()
          ((RTS.|||)
             ((RTS.|||)
                ((RTS.|||)
                   ((RTS.|||)
                      ((RTS.|||)
                         (case v of
                            PdfValue.Value_null (_951 :: ()) -> HS.pure ()
                            _ -> RTS.pError RTS.FromSystem "112:17--112:25"
                                   "Pattern match failure")
                         (case v of
                            PdfValue.Value_bool (_952 :: HS.Bool) -> HS.pure ()
                            _ -> RTS.pError RTS.FromSystem "112:32--112:40"
                                   "Pattern match failure"))
                      (case v of
                         PdfValue.Value_ref (_953 :: PdfValue.Ref) -> HS.pure ()
                         _ -> RTS.pError RTS.FromSystem "112:47--112:54"
                                "Pattern match failure"))
                   (case v of
                      PdfValue.Value_name (_954 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                                    ()
                      _ -> RTS.pError RTS.FromSystem "112:61--112:69"
                             "Pattern match failure"))
                (case v of
                   PdfValue.Value_string
                     (_955 :: Vector.Vector (RTS.UInt 8)) -> HS.pure ()
                   _ -> RTS.pError RTS.FromSystem "113:19--113:29"
                          "Pattern match failure"))
             (case v of
                PdfValue.Value_number (_956 :: PdfValue.Number) -> HS.pure ()
                _ -> RTS.pError RTS.FromSystem "113:36--113:46"
                       "Pattern match failure"))))
    ((RTS.|||)
       (do (dict
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
             case v of
               PdfValue.Value_dict
                 (_957
                    :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                                 _957
               _ -> RTS.pError RTS.FromSystem "114:19--114:27"
                      "Pattern match failure"
           (RTS.<||)
             (RTS.pEnter "PdfValue._When"
                (PdfValue._When @TsafetyInfo @()
                   (RTS.pEnter "PdfDemo._DictIsAction"
                      (_DictIsAction (Vector.vecFromRep "JavaScript") dict))))
             ((RTS.<||)
                (RTS.pEnter "PdfValue._When"
                   (PdfValue._When @TsafetyInfo @()
                      (RTS.pEnter "PdfDemo._DictIsAction"
                         (_DictIsAction (Vector.vecFromRep "URI") dict))))
                (RTS.loopFoldM
                   (\(acc :: ()) (v :: PdfValue.Value) ->
                      do HS.void (RTS.pEnter "PdfDemo.ValueIsSafe" (pValueIsSafe v))
                         HS.pure ())
                   ()
                   dict)))
       (do (arr :: Vector.Vector PdfValue.Value) <-
             case v of
               PdfValue.Value_array
                 (_960 :: Vector.Vector PdfValue.Value) -> HS.pure _960
               _ -> RTS.pError RTS.FromSystem "126:18--126:27"
                      "Pattern match failure"
           RTS.loopFoldM
             (\(acc :: ()) (v :: PdfValue.Value) ->
                do HS.void (RTS.pEnter "PdfDemo.ValueIsSafe" (pValueIsSafe v))
                   HS.pure ())
             ()
             arr))
 
_CheckDecl ::
      HS.Integer -> (HS.Integer -> (PdfDecl.TopDecl -> D.Parser ()))
 
_CheckDecl (expectId :: HS.Integer) (expectGen :: HS.Integer)
  (decl :: PdfDecl.TopDecl) =
  do RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (HS.getField @"id" decl HS.== expectId))
     RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (HS.getField @"gen" decl HS.== expectGen))
     (RTS.|||)
       (do (v :: PdfValue.Value) <-
             case HS.getField @"obj" decl of
               PdfDecl.TopDeclDef_value (_963 :: PdfValue.Value) -> HS.pure _963
               _ -> RTS.pError RTS.FromSystem "82:19--82:35"
                      "Pattern match failure"
           RTS.pEnter "PdfDemo._ValueIsSafe" (_ValueIsSafe v))
       (case HS.getField @"obj" decl of
          PdfDecl.TopDeclDef_stream (_965 :: PdfDecl.Stream) -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "83:19--83:36"
                 "Pattern match failure")
 
_TopDeclCheck :: HS.Integer -> (HS.Integer -> D.Parser ())
 
_TopDeclCheck (expectId :: HS.Integer) (expectGen :: HS.Integer) =
  do (decl :: PdfDecl.TopDecl) <-
       RTS.pEnter "PdfDecl.TopDecl" PdfDecl.pTopDecl
     RTS.pEnter "PdfDemo._CheckDecl"
       (_CheckDecl expectId expectGen decl)
 
_ResolveObjectStreamEntryCheck ::
      HS.Integer
        -> (HS.Integer
              -> (HS.Integer -> (HS.Integer -> (RTS.UInt 64 -> D.Parser ()))))
 
_ResolveObjectStreamEntryCheck (expectId :: HS.Integer)
  (expectGen :: HS.Integer)
  (oid :: HS.Integer)
  (gen :: HS.Integer)
  (idx :: RTS.UInt 64) =
  do (decl :: PdfDecl.TopDecl) <-
       RTS.pEnter "PdfDecl.ResolveObjectStreamEntry"
         (PdfDecl.pResolveObjectStreamEntry oid gen idx)
     RTS.pEnter "PdfDemo._CheckDecl"
       (_CheckDecl expectId expectGen decl)