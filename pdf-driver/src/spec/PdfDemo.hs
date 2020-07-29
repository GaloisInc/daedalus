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
module PdfDemo where
 
import qualified PdfMonad as D
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
 
data CheckDecl h
  = CheckDecl h TsafetyInfo
  
 
deriving instance HS.Eq h => HS.Eq (CheckDecl h)
 
deriving instance HS.Ord h => HS.Ord (CheckDecl h)
 
deriving instance HS.Show h => HS.Show (CheckDecl h)
 
instance RTS.DDL h => RTS.DDL (CheckDecl h) where
 
instance HS.HasField "obj" (CheckDecl h) h where
  getField (CheckDecl x _) = x
 
instance HS.HasField "isSafe" (CheckDecl h) TsafetyInfo where
  getField (CheckDecl _ x) = x
 
pCheckParent ::
      HS.Maybe PdfValue.Ref
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
pCheckParent (p :: HS.Maybe PdfValue.Ref)
  (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  (RTS.<||)
    (do RTS.pGuard "35:7--35:18" "guard failed"
          (p HS.== (HS.Nothing :: HS.Maybe PdfValue.Ref))
        (v :: HS.Maybe PdfValue.Ref) <-
          RTS.pOptional (RTS.<||) HS.Just
            (RTS.pEnter "PdfDecl.LookupRef"
               (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
                  (Vector.vecFromRep "Parent")
                  dict))
        (__ :: ()) <-
          RTS.pGuard "37:9--37:20" "guard failed"
            (v HS.== (HS.Nothing :: HS.Maybe PdfValue.Ref))
        HS.pure __)
    (do (pref :: PdfValue.Ref) <-
          RTS.pIsJust "40:15--40:23" "Expected `Just`" p
        (dpref :: PdfValue.Ref) <-
          RTS.pEnter "PdfDecl.LookupRef"
            (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
               (Vector.vecFromRep "Parent")
               dict)
        (__ :: ()) <-
          RTS.pGuard "42:7--42:19" "guard failed" (dpref HS.== pref)
        HS.pure __)
 
pIsPage :: HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
pIsPage (p :: HS.Maybe PdfValue.Ref) (r :: PdfValue.Ref) =
  do (v :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "14:13--14:21" "Expected `dict`"
         (HS.getField @"dict" v)
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
    (do RTS.pGuard "35:7--35:18" "guard failed"
          (p HS.== (HS.Nothing :: HS.Maybe PdfValue.Ref))
        (v :: HS.Maybe PdfValue.Ref) <-
          RTS.pOptional (RTS.<||) HS.Just
            (RTS.pEnter "PdfDecl.LookupRef"
               (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
                  (Vector.vecFromRep "Parent")
                  dict))
        RTS.pGuard "37:9--37:20" "guard failed"
          (v HS.== (HS.Nothing :: HS.Maybe PdfValue.Ref)))
    (do (pref :: PdfValue.Ref) <-
          RTS.pIsJust "40:15--40:23" "Expected `Just`" p
        (dpref :: PdfValue.Ref) <-
          RTS.pEnter "PdfDecl.LookupRef"
            (PdfDecl.pLookupRef @(Vector.Vector (RTS.UInt 8))
               (Vector.vecFromRep "Parent")
               dict)
        RTS.pGuard "42:7--42:19" "guard failed" (dpref HS.== pref))
 
pIsPages :: HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
pIsPages (p :: HS.Maybe PdfValue.Ref) (r :: PdfValue.Ref) =
  do (v :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "22:13--22:21" "Expected `dict`"
         (HS.getField @"dict" v)
     RTS.pEnter "PdfDecl._CheckType"
       (PdfDecl._CheckType (Vector.vecFromRep "Pages") dict)
     RTS.pEnter "PdfDemo._CheckParent" (_CheckParent p dict)
     (kidsv :: PdfValue.Value) <-
       RTS.pIsJust "26:14--26:31"
         ("Missing key: "
            HS.++ HS.show
                    (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup (Vector.vecFromRep "Kids") dict)
     (kids :: Vector.Vector PdfValue.Value) <-
       RTS.pIsJust "27:14--27:27" "Expected `array`"
         (HS.getField @"array" kidsv)
     (__ :: ()) <-
       RTS.loopFoldM
         (\(acc :: ()) (refv :: PdfValue.Value) ->
            do (ref :: PdfValue.Ref) <-
                 RTS.pIsJust "29:16--29:26" "Expected `ref`"
                   (HS.getField @"ref" refv)
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
       RTS.pIsJust "14:13--14:21" "Expected `dict`"
         (HS.getField @"dict" v)
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
 
pCatalogIsOK :: PdfValue.Ref -> D.Parser HS.Bool
 
pCatalogIsOK (r :: PdfValue.Ref) =
  do (catv :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (cat :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "53:12--53:23" "Expected `dict`"
         (HS.getField @"dict" catv)
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
 
pDictIsAction ::
      Vector.Vector (RTS.UInt 8)
        -> (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
              -> D.Parser ())
 
pDictIsAction (a :: Vector.Vector (RTS.UInt 8))
  (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (n :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pEnter "PdfDecl.LookupName"
         (PdfDecl.pLookupName (Vector.vecFromRep "S") d)
     (__ :: ()) <- RTS.pGuard "121:3--121:8" "guard failed" (a HS.== n)
     HS.pure __
 
bor :: HS.Bool -> (HS.Bool -> HS.Bool)
 
bor (b1 :: HS.Bool) (b2 :: HS.Bool) =
  if b1
    then HS.True
    else b2
 
safetyInfo :: HS.Bool -> (HS.Bool -> TsafetyInfo)
 
safetyInfo (js :: HS.Bool) (uri :: HS.Bool) = TsafetyInfo js uri
 
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
 
safeSafetyInfo :: TsafetyInfo
 
safeSafetyInfo = safetyInfo HS.False HS.False
 
pValueIsSafe :: PdfValue.Value -> D.Parser TsafetyInfo
 
pValueIsSafe (v :: PdfValue.Value) =
  (RTS.|||)
    (RTS.pEnter "PdfValue.When"
       (PdfValue.pWhen @() @TsafetyInfo
          ((RTS.|||)
             ((RTS.|||)
                ((RTS.|||)
                   ((RTS.|||)
                      ((RTS.|||)
                         (RTS.pIsJust_ "96:17--96:25" "Expected `null`"
                            (HS.getField @"null" v))
                         (RTS.pIsJust_ "96:32--96:40" "Expected `bool`"
                            (HS.getField @"bool" v)))
                      (RTS.pIsJust_ "96:47--96:54" "Expected `ref`"
                         (HS.getField @"ref" v)))
                   (RTS.pIsJust_ "96:61--96:69" "Expected `name`"
                      (HS.getField @"name" v)))
                (RTS.pIsJust_ "97:19--97:29" "Expected `string`"
                   (HS.getField @"string" v)))
             (RTS.pIsJust_ "97:36--97:46" "Expected `number`"
                (HS.getField @"number" v)))
          safeSafetyInfo))
    ((RTS.|||)
       (do (dict
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
             RTS.pIsJust "98:19--98:27" "Expected `dict`"
               (HS.getField @"dict" v)
           (__ :: TsafetyInfo) <-
             (RTS.<||)
               (RTS.pEnter "PdfValue.When"
                  (PdfValue.pWhen @() @TsafetyInfo
                     (RTS.pEnter "PdfDemo.DictIsAction"
                        (pDictIsAction (Vector.vecFromRep "JavaScript") dict))
                     (safetyInfo HS.True HS.False)))
               ((RTS.<||)
                  (RTS.pEnter "PdfValue.When"
                     (PdfValue.pWhen @() @TsafetyInfo
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
             RTS.pIsJust "110:18--110:27" "Expected `array`"
               (HS.getField @"array" v)
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
  forall a b c h n.
    (RTS.DDL a, RTS.DDL b, RTS.DDL c, RTS.DDL h, RTS.DDL n,
     RTS.HasStruct c "id" a, RTS.HasStruct c "gen" b,
     RTS.HasStruct c "obj" h, RTS.HasUnion h "value" PdfValue.Value,
     RTS.HasUnion h "stream" n) =>
      a -> (b -> (c -> D.Parser (CheckDecl h)))
 
pCheckDecl (expectId :: a) (expectGen :: b) (decl :: c) =
  do RTS.pGuard "63:3--63:21" "guard failed"
       (HS.getField @"id" decl HS.== expectId)
     RTS.pGuard "64:3--64:23" "guard failed"
       (HS.getField @"gen" decl HS.== expectGen)
     (obj :: h) <- HS.pure (HS.getField @"obj" decl)
     (isSafe :: TsafetyInfo) <-
       (RTS.|||)
         (do (v :: PdfValue.Value) <-
               RTS.pIsJust "66:19--66:35" "Expected `value`"
                 (HS.getField @"value" (HS.getField @"obj" decl))
             (__ :: TsafetyInfo) <-
               RTS.pEnter "PdfDemo.ValueIsSafe" (pValueIsSafe v)
             HS.pure __)
         (do RTS.pIsJust_ "67:19--67:36" "Expected `stream`"
               (HS.getField @"stream" (HS.getField @"obj" decl))
             (__ :: TsafetyInfo) <- HS.pure safeSafetyInfo
             HS.pure __)
     HS.pure (CheckDecl obj isSafe)
 
pResolveObjectStreamEntryCheck ::
      HS.Integer
        -> (HS.Integer
              -> (HS.Integer
                    -> (HS.Integer
                          -> (HS.Integer -> D.Parser (CheckDecl PdfDecl.TopDeclDef)))))
 
pResolveObjectStreamEntryCheck (expectId :: HS.Integer)
  (expectGen :: HS.Integer)
  (oid :: HS.Integer)
  (gen :: HS.Integer)
  (idx :: HS.Integer) =
  do (decl :: PdfDecl.TopDecl) <-
       RTS.pEnter "PdfDecl.ResolveObjectStreamEntry"
         (PdfDecl.pResolveObjectStreamEntry oid gen idx)
     (__ :: CheckDecl PdfDecl.TopDeclDef) <-
       RTS.pEnter "PdfDemo.CheckDecl"
         (pCheckDecl @HS.Integer @HS.Integer @PdfDecl.TopDecl
            @PdfDecl.TopDeclDef
            @PdfDecl.TopDeclDef_0
            expectId
            expectGen
            decl)
     HS.pure __
 
pTopDeclCheck ::
      HS.Integer
        -> (HS.Integer -> D.Parser (CheckDecl PdfDecl.TopDeclDef))
 
pTopDeclCheck (expectId :: HS.Integer) (expectGen :: HS.Integer) =
  do (decl :: PdfDecl.TopDecl) <-
       RTS.pEnter "PdfDecl.TopDecl" PdfDecl.pTopDecl
     (__ :: CheckDecl PdfDecl.TopDeclDef) <-
       RTS.pEnter "PdfDemo.CheckDecl"
         (pCheckDecl @HS.Integer @HS.Integer @PdfDecl.TopDecl
            @PdfDecl.TopDeclDef
            @PdfDecl.TopDeclDef_0
            expectId
            expectGen
            decl)
     HS.pure __
 
_IsRootPages :: PdfValue.Ref -> D.Parser ()
 
_IsRootPages (r :: PdfValue.Ref) =
  RTS.pEnter "PdfDecl._Default"
    (PdfDecl._Default @HS.Bool
       (RTS.pEnter "PdfDemo._IsPageOrPages"
          (_IsPageOrPages (HS.Nothing :: HS.Maybe PdfValue.Ref) r)))
 
_CatalogIsOK :: PdfValue.Ref -> D.Parser ()
 
_CatalogIsOK (r :: PdfValue.Ref) =
  do (catv :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (cat :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "53:12--53:23" "Expected `dict`"
         (HS.getField @"dict" catv)
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
     RTS.pGuard "121:3--121:8" "guard failed" (a HS.== n)
 
_ValueIsSafe :: PdfValue.Value -> D.Parser ()
 
_ValueIsSafe (v :: PdfValue.Value) =
  (RTS.|||)
    (RTS.pEnter "PdfValue._When"
       (PdfValue._When @() @TsafetyInfo
          ((RTS.|||)
             ((RTS.|||)
                ((RTS.|||)
                   ((RTS.|||)
                      ((RTS.|||)
                         (RTS.pIsJust_ "96:17--96:25" "Expected `null`"
                            (HS.getField @"null" v))
                         (RTS.pIsJust_ "96:32--96:40" "Expected `bool`"
                            (HS.getField @"bool" v)))
                      (RTS.pIsJust_ "96:47--96:54" "Expected `ref`"
                         (HS.getField @"ref" v)))
                   (RTS.pIsJust_ "96:61--96:69" "Expected `name`"
                      (HS.getField @"name" v)))
                (RTS.pIsJust_ "97:19--97:29" "Expected `string`"
                   (HS.getField @"string" v)))
             (RTS.pIsJust_ "97:36--97:46" "Expected `number`"
                (HS.getField @"number" v)))))
    ((RTS.|||)
       (do (dict
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
             RTS.pIsJust "98:19--98:27" "Expected `dict`"
               (HS.getField @"dict" v)
           (RTS.<||)
             (RTS.pEnter "PdfValue._When"
                (PdfValue._When @() @TsafetyInfo
                   (RTS.pEnter "PdfDemo._DictIsAction"
                      (_DictIsAction (Vector.vecFromRep "JavaScript") dict))))
             ((RTS.<||)
                (RTS.pEnter "PdfValue._When"
                   (PdfValue._When @() @TsafetyInfo
                      (RTS.pEnter "PdfDemo._DictIsAction"
                         (_DictIsAction (Vector.vecFromRep "URI") dict))))
                (RTS.loopFoldM
                   (\(acc :: ()) (v :: PdfValue.Value) ->
                      do HS.void (RTS.pEnter "PdfDemo.ValueIsSafe" (pValueIsSafe v))
                         HS.pure ())
                   ()
                   dict)))
       (do (arr :: Vector.Vector PdfValue.Value) <-
             RTS.pIsJust "110:18--110:27" "Expected `array`"
               (HS.getField @"array" v)
           RTS.loopFoldM
             (\(acc :: ()) (v :: PdfValue.Value) ->
                do HS.void (RTS.pEnter "PdfDemo.ValueIsSafe" (pValueIsSafe v))
                   HS.pure ())
             ()
             arr))
 
_CheckDecl ::
  forall a b c h n.
    (RTS.DDL a, RTS.DDL b, RTS.DDL c, RTS.DDL h, RTS.DDL n,
     RTS.HasStruct c "id" a, RTS.HasStruct c "gen" b,
     RTS.HasStruct c "obj" h, RTS.HasUnion h "value" PdfValue.Value,
     RTS.HasUnion h "stream" n) =>
      a -> (b -> (c -> D.Parser ()))
 
_CheckDecl (expectId :: a) (expectGen :: b) (decl :: c) =
  do RTS.pGuard "63:3--63:21" "guard failed"
       (HS.getField @"id" decl HS.== expectId)
     RTS.pGuard "64:3--64:23" "guard failed"
       (HS.getField @"gen" decl HS.== expectGen)
     (RTS.|||)
       (do (v :: PdfValue.Value) <-
             RTS.pIsJust "66:19--66:35" "Expected `value`"
               (HS.getField @"value" (HS.getField @"obj" decl))
           RTS.pEnter "PdfDemo._ValueIsSafe" (_ValueIsSafe v))
       (RTS.pIsJust_ "67:19--67:36" "Expected `stream`"
          (HS.getField @"stream" (HS.getField @"obj" decl)))
 
_IsPages :: HS.Maybe PdfValue.Ref -> (PdfValue.Ref -> D.Parser ())
 
_IsPages (p :: HS.Maybe PdfValue.Ref) (r :: PdfValue.Ref) =
  do (v :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
     (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "22:13--22:21" "Expected `dict`"
         (HS.getField @"dict" v)
     RTS.pEnter "PdfDecl._CheckType"
       (PdfDecl._CheckType (Vector.vecFromRep "Pages") dict)
     RTS.pEnter "PdfDemo._CheckParent" (_CheckParent p dict)
     (kidsv :: PdfValue.Value) <-
       RTS.pIsJust "26:14--26:31"
         ("Missing key: "
            HS.++ HS.show
                    (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup (Vector.vecFromRep "Kids") dict)
     (kids :: Vector.Vector PdfValue.Value) <-
       RTS.pIsJust "27:14--27:27" "Expected `array`"
         (HS.getField @"array" kidsv)
     RTS.loopFoldM
       (\(acc :: ()) (refv :: PdfValue.Value) ->
          do (ref :: PdfValue.Ref) <-
               RTS.pIsJust "29:16--29:26" "Expected `ref`"
                 (HS.getField @"ref" refv)
             RTS.pEnter "PdfDemo._IsPageOrPages"
               (_IsPageOrPages (HS.Just r) ref))
       ()
       kids
 
_ResolveObjectStreamEntryCheck ::
      HS.Integer
        -> (HS.Integer
              -> (HS.Integer -> (HS.Integer -> (HS.Integer -> D.Parser ()))))
 
_ResolveObjectStreamEntryCheck (expectId :: HS.Integer)
  (expectGen :: HS.Integer)
  (oid :: HS.Integer)
  (gen :: HS.Integer)
  (idx :: HS.Integer) =
  do (decl :: PdfDecl.TopDecl) <-
       RTS.pEnter "PdfDecl.ResolveObjectStreamEntry"
         (PdfDecl.pResolveObjectStreamEntry oid gen idx)
     RTS.pEnter "PdfDemo._CheckDecl"
       (_CheckDecl @HS.Integer @HS.Integer @PdfDecl.TopDecl
          @PdfDecl.TopDeclDef
          @PdfDecl.TopDeclDef_0
          expectId
          expectGen
          decl)
 
_TopDeclCheck :: HS.Integer -> (HS.Integer -> D.Parser ())
 
_TopDeclCheck (expectId :: HS.Integer) (expectGen :: HS.Integer) =
  do (decl :: PdfDecl.TopDecl) <-
       RTS.pEnter "PdfDecl.TopDecl" PdfDecl.pTopDecl
     RTS.pEnter "PdfDemo._CheckDecl"
       (_CheckDecl @HS.Integer @HS.Integer @PdfDecl.TopDecl
          @PdfDecl.TopDeclDef
          @PdfDecl.TopDeclDef_0
          expectId
          expectGen
          decl)
 
band :: HS.Bool -> (HS.Bool -> HS.Bool)
 
band (b1 :: HS.Bool) (b2 :: HS.Bool) =
  if b1
    then b2
    else HS.False