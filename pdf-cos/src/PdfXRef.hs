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
module PdfXRef where
 
import qualified PdfMonad as D
import qualified PdfValue
import qualified PdfDecl
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Input as RTS
import qualified RTS.Map as Map
import qualified RTS.Vector as Vector
 
 
data XRefIndex_0
  = XRefIndex_0 HS.Integer (RTS.UInt 64)
  
 
deriving instance HS.Eq XRefIndex_0
 
deriving instance HS.Ord XRefIndex_0
 
deriving instance HS.Show XRefIndex_0
 
instance RTS.DDL XRefIndex_0 where
 
instance HS.HasField "firstId" XRefIndex_0 HS.Integer where
  getField (XRefIndex_0 x _) = x
 
instance HS.HasField "num" XRefIndex_0 (RTS.UInt 64) where
  getField (XRefIndex_0 _ x) = x
 
data XRefFree
  = XRefFree HS.Integer HS.Integer
  
 
deriving instance HS.Eq XRefFree
 
deriving instance HS.Ord XRefFree
 
deriving instance HS.Show XRefFree
 
instance RTS.DDL XRefFree where
 
instance HS.HasField "obj" XRefFree HS.Integer where
  getField (XRefFree x _) = x
 
instance HS.HasField "gen" XRefFree HS.Integer where
  getField (XRefFree _ x) = x
 
data XRefOffset
  = XRefOffset HS.Integer HS.Integer
  
 
deriving instance HS.Eq XRefOffset
 
deriving instance HS.Ord XRefOffset
 
deriving instance HS.Show XRefOffset
 
instance RTS.DDL XRefOffset where
 
instance HS.HasField "offset" XRefOffset HS.Integer where
  getField (XRefOffset x _) = x
 
instance HS.HasField "gen" XRefOffset HS.Integer where
  getField (XRefOffset _ x) = x
 
data XRefCompressed
  = XRefCompressed HS.Integer HS.Integer
  
 
deriving instance HS.Eq XRefCompressed
 
deriving instance HS.Ord XRefCompressed
 
deriving instance HS.Show XRefCompressed
 
instance RTS.DDL XRefCompressed where
 
instance HS.HasField "container_obj" XRefCompressed
           HS.Integer where
  getField (XRefCompressed x _) = x
 
instance HS.HasField "obj_index" XRefCompressed HS.Integer where
  getField (XRefCompressed _ x) = x
 
data XRefObjEntry
  = XRefObjEntry_compressed XRefCompressed
  | XRefObjEntry_free XRefFree
  | XRefObjEntry_inUse XRefOffset
  | XRefObjEntry_null ()
  
 
deriving instance HS.Eq XRefObjEntry
 
deriving instance HS.Ord XRefObjEntry
 
deriving instance HS.Show XRefObjEntry
 
instance RTS.DDL XRefObjEntry where
 
instance HS.HasField "compressed" XRefObjEntry
           (HS.Maybe XRefCompressed) where
  getField (XRefObjEntry_compressed x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "free" XRefObjEntry (HS.Maybe XRefFree) where
  getField (XRefObjEntry_free x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "inUse" XRefObjEntry
           (HS.Maybe XRefOffset) where
  getField (XRefObjEntry_inUse x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "null" XRefObjEntry (HS.Maybe ()) where
  getField (XRefObjEntry_null x) = HS.Just x
   
  getField _ = HS.Nothing
 
data XRefObjTable_0
  = XRefObjTable_0 HS.Integer (Vector.Vector XRefObjEntry)
  
 
deriving instance HS.Eq XRefObjTable_0
 
deriving instance HS.Ord XRefObjTable_0
 
deriving instance HS.Show XRefObjTable_0
 
instance RTS.DDL XRefObjTable_0 where
 
instance HS.HasField "firstId" XRefObjTable_0 HS.Integer where
  getField (XRefObjTable_0 x _) = x
 
instance HS.HasField "entries" XRefObjTable_0
           (Vector.Vector XRefObjEntry) where
  getField (XRefObjTable_0 _ x) = x
 
data UsedEntry
  = UsedEntry HS.Integer HS.Integer
  
 
deriving instance HS.Eq UsedEntry
 
deriving instance HS.Ord UsedEntry
 
deriving instance HS.Show UsedEntry
 
instance RTS.DDL UsedEntry where
 
instance HS.HasField "offset" UsedEntry HS.Integer where
  getField (UsedEntry x _) = x
 
instance HS.HasField "gen" UsedEntry HS.Integer where
  getField (UsedEntry _ x) = x
 
data FreeEntry
  = FreeEntry HS.Integer HS.Integer
  
 
deriving instance HS.Eq FreeEntry
 
deriving instance HS.Ord FreeEntry
 
deriving instance HS.Show FreeEntry
 
instance RTS.DDL FreeEntry where
 
instance HS.HasField "obj" FreeEntry HS.Integer where
  getField (FreeEntry x _) = x
 
instance HS.HasField "gen" FreeEntry HS.Integer where
  getField (FreeEntry _ x) = x
 
data CrossRefEntry
  = CrossRefEntry_free FreeEntry
  | CrossRefEntry_inUse UsedEntry
  
 
deriving instance HS.Eq CrossRefEntry
 
deriving instance HS.Ord CrossRefEntry
 
deriving instance HS.Show CrossRefEntry
 
instance RTS.DDL CrossRefEntry where
 
instance HS.HasField "free" CrossRefEntry
           (HS.Maybe FreeEntry) where
  getField (CrossRefEntry_free x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "inUse" CrossRefEntry
           (HS.Maybe UsedEntry) where
  getField (CrossRefEntry_inUse x) = HS.Just x
   
  getField _ = HS.Nothing
 
data CrossRefSubSection
  = CrossRefSubSection HS.Integer (Vector.Vector CrossRefEntry)
  
 
deriving instance HS.Eq CrossRefSubSection
 
deriving instance HS.Ord CrossRefSubSection
 
deriving instance HS.Show CrossRefSubSection
 
instance RTS.DDL CrossRefSubSection where
 
instance HS.HasField "firstId" CrossRefSubSection HS.Integer where
  getField (CrossRefSubSection x _) = x
 
instance HS.HasField "entries" CrossRefSubSection
           (Vector.Vector CrossRefEntry) where
  getField (CrossRefSubSection _ x) = x
 
data TrailerDictEncrypt
  = TrailerDictEncrypt PdfValue.Value PdfValue.Ref
      (Vector.Vector (RTS.UInt 8))
      (Vector.Vector (RTS.UInt 8))
  
 
deriving instance HS.Eq TrailerDictEncrypt
 
deriving instance HS.Ord TrailerDictEncrypt
 
deriving instance HS.Show TrailerDictEncrypt
 
instance RTS.DDL TrailerDictEncrypt where
 
instance HS.HasField "d" TrailerDictEncrypt PdfValue.Value where
  getField (TrailerDictEncrypt x _ _ _) = x
 
instance HS.HasField "eref" TrailerDictEncrypt PdfValue.Ref where
  getField (TrailerDictEncrypt _ x _ _) = x
 
instance HS.HasField "id0" TrailerDictEncrypt
           (Vector.Vector (RTS.UInt 8)) where
  getField (TrailerDictEncrypt _ _ x _) = x
 
instance HS.HasField "id1" TrailerDictEncrypt
           (Vector.Vector (RTS.UInt 8)) where
  getField (TrailerDictEncrypt _ _ _ x) = x
 
data TrailerDict
  = TrailerDict HS.Integer (HS.Maybe PdfValue.Ref)
      (HS.Maybe HS.Integer)
      (HS.Maybe TrailerDictEncrypt)
      (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  
 
deriving instance HS.Eq TrailerDict
 
deriving instance HS.Ord TrailerDict
 
deriving instance HS.Show TrailerDict
 
instance RTS.DDL TrailerDict where
 
instance HS.HasField "size" TrailerDict HS.Integer where
  getField (TrailerDict x _ _ _ _) = x
 
instance HS.HasField "root" TrailerDict
           (HS.Maybe PdfValue.Ref) where
  getField (TrailerDict _ x _ _ _) = x
 
instance HS.HasField "prev" TrailerDict (HS.Maybe HS.Integer) where
  getField (TrailerDict _ _ x _ _) = x
 
instance HS.HasField "encrypt" TrailerDict
           (HS.Maybe TrailerDictEncrypt) where
  getField (TrailerDict _ _ _ x _) = x
 
instance HS.HasField "all" TrailerDict
           (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) where
  getField (TrailerDict _ _ _ _ x) = x
 
data CrossRefAndTrailer
  = CrossRefAndTrailer (Vector.Vector CrossRefSubSection) TrailerDict
  
 
deriving instance HS.Eq CrossRefAndTrailer
 
deriving instance HS.Ord CrossRefAndTrailer
 
deriving instance HS.Show CrossRefAndTrailer
 
instance RTS.DDL CrossRefAndTrailer where
 
instance HS.HasField "xref" CrossRefAndTrailer
           (Vector.Vector CrossRefSubSection) where
  getField (CrossRefAndTrailer x _) = x
 
instance HS.HasField "trailer" CrossRefAndTrailer TrailerDict where
  getField (CrossRefAndTrailer _ x) = x
 
data XRefObjTable
  = XRefObjTable (Vector.Vector XRefObjTable_0) TrailerDict
  
 
deriving instance HS.Eq XRefObjTable
 
deriving instance HS.Ord XRefObjTable
 
deriving instance HS.Show XRefObjTable
 
instance RTS.DDL XRefObjTable where
 
instance HS.HasField "xref" XRefObjTable
           (Vector.Vector XRefObjTable_0) where
  getField (XRefObjTable x _) = x
 
instance HS.HasField "trailer" XRefObjTable TrailerDict where
  getField (XRefObjTable _ x) = x
 
data CrossRef
  = CrossRef_newXref XRefObjTable
  | CrossRef_oldXref CrossRefAndTrailer
  
 
deriving instance HS.Eq CrossRef
 
deriving instance HS.Ord CrossRef
 
deriving instance HS.Show CrossRef
 
instance RTS.DDL CrossRef where
 
instance HS.HasField "newXref" CrossRef
           (HS.Maybe XRefObjTable) where
  getField (CrossRef_newXref x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "oldXref" CrossRef
           (HS.Maybe CrossRefAndTrailer) where
  getField (CrossRef_oldXref x) = HS.Just x
   
  getField _ = HS.Nothing
 
data XRefFormat
  = XRefFormat (RTS.UInt 64) (RTS.UInt 64) (RTS.UInt 64)
      (RTS.UInt 64)
  
 
deriving instance HS.Eq XRefFormat
 
deriving instance HS.Ord XRefFormat
 
deriving instance HS.Show XRefFormat
 
instance RTS.DDL XRefFormat where
 
instance HS.HasField "b1" XRefFormat (RTS.UInt 64) where
  getField (XRefFormat x _ _ _) = x
 
instance HS.HasField "b2" XRefFormat (RTS.UInt 64) where
  getField (XRefFormat _ x _ _) = x
 
instance HS.HasField "b3" XRefFormat (RTS.UInt 64) where
  getField (XRefFormat _ _ x _) = x
 
instance HS.HasField "width" XRefFormat (RTS.UInt 64) where
  getField (XRefFormat _ _ _ x) = x
 
data XRefMeta
  = XRefMeta (Vector.Vector XRefIndex_0) XRefFormat
      (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  
 
deriving instance HS.Eq XRefMeta
 
deriving instance HS.Ord XRefMeta
 
deriving instance HS.Show XRefMeta
 
instance RTS.DDL XRefMeta where
 
instance HS.HasField "index" XRefMeta
           (Vector.Vector XRefIndex_0) where
  getField (XRefMeta x _ _) = x
 
instance HS.HasField "widths" XRefMeta XRefFormat where
  getField (XRefMeta _ x _) = x
 
instance HS.HasField "header" XRefMeta
           (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) where
  getField (XRefMeta _ _ x) = x
 
pUsedEntry :: HS.Integer -> (HS.Integer -> D.Parser UsedEntry)
 
pUsedEntry (num :: HS.Integer) (gen :: HS.Integer) =
  do HS.const ()
       HS.<$> RTS.pMatch1 "42:3--42:12" (RTS.bcSingle (RTS.uint8 110))
     (offset :: HS.Integer) <- HS.pure num
     (gen :: HS.Integer) <- HS.pure gen
     HS.pure (UsedEntry offset gen)
 
pFreeEntry :: HS.Integer -> (HS.Integer -> D.Parser FreeEntry)
 
pFreeEntry (num :: HS.Integer) (gen :: HS.Integer) =
  do HS.const ()
       HS.<$> RTS.pMatch1 "46:3--46:12" (RTS.bcSingle (RTS.uint8 102))
     (obj :: HS.Integer) <- HS.pure num
     (gen :: HS.Integer) <- HS.pure gen
     HS.pure (FreeEntry obj gen)
 
pCrossRefEntry :: D.Parser CrossRefEntry
 
pCrossRefEntry =
  do (num :: HS.Integer) <-
       RTS.pEnter "PdfDecl.NatN"
         (PdfDecl.pNatN (RTS.lit 10 :: RTS.UInt 64))
     HS.const () HS.<$> RTS.pMatch1 "32:19--32:27" PdfValue.cs_simpleWS
     (gen :: HS.Integer) <-
       RTS.pEnter "PdfDecl.NatN"
         (PdfDecl.pNatN (RTS.lit 5 :: RTS.UInt 64))
     HS.const () HS.<$> RTS.pMatch1 "33:19--33:27" PdfValue.cs_simpleWS
     (__ :: CrossRefEntry) <-
       (RTS.|||)
         (RTS.pEnter "inUse"
            (do (_526 :: UsedEntry) <-
                  RTS.pEnter "PdfXRef.UsedEntry" (pUsedEntry num gen)
                HS.pure (CrossRefEntry_inUse _526)))
         (RTS.pEnter "free"
            (do (_527 :: FreeEntry) <-
                  RTS.pEnter "PdfXRef.FreeEntry" (pFreeEntry num gen)
                HS.pure (CrossRefEntry_free _527)))
     (RTS.|||)
       (do HS.const ()
             HS.<$> RTS.pMatch1 "38:5--38:13" PdfValue.cs_simpleWS
           (RTS.|||)
             (HS.const () HS.<$> RTS.pMatch1 "38:16--38:18" PdfValue.cs_cr)
             (HS.const () HS.<$> RTS.pMatch1 "38:22--38:24" PdfValue.cs_lf))
       (do HS.const () HS.<$> RTS.pMatch1 "38:32--38:34" PdfValue.cs_cr
           HS.const () HS.<$> RTS.pMatch1 "38:37--38:39" PdfValue.cs_lf)
     HS.pure __
 
pCrossRefSubSection :: D.Parser CrossRefSubSection
 
pCrossRefSubSection =
  do (firstId :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (PdfValue.pToken @HS.Integer
            (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
     (num :: RTS.UInt 64) <-
       do (_530 :: HS.Integer) <-
            RTS.pEnter "PdfValue.Token"
              (PdfValue.pToken @HS.Integer
                 (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
          RTS.pIsJust "27:13--27:36" "Value does not fit in target type"
            (RTS.convertMaybe _530 :: HS.Maybe (RTS.UInt 64))
     (entries :: Vector.Vector CrossRefEntry) <-
       Vector.replicateM num
         (RTS.pEnter "PdfXRef.CrossRefEntry" pCrossRefEntry)
     HS.pure (CrossRefSubSection firstId entries)
 
pCrossRefSection :: D.Parser (Vector.Vector CrossRefSubSection)
 
pCrossRefSection =
  do RTS.pEnter "PdfValue._KW"
       (PdfValue._KW (Vector.vecFromRep "xref"))
     (__ :: Vector.Vector CrossRefSubSection) <-
       RTS.pMinLength "22:3--22:31" (RTS.lit 1 :: RTS.UInt 64)
         (RTS.pMany (RTS.<||)
            (RTS.pEnter "PdfXRef.CrossRefSubSection" pCrossRefSubSection))
     HS.pure __
 
pTrailerDictEncrypt ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> D.Parser TrailerDictEncrypt
 
pTrailerDictEncrypt
  (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (d :: PdfValue.Value) <-
       RTS.pIsJust "157:7--157:27"
         ("Missing key: "
            HS.++ HS.show
                    (Vector.vecFromRep "Encrypt" :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup (Vector.vecFromRep "Encrypt") dict)
     RTS.pErrorMode RTS.Abort
       (do (eref :: PdfValue.Ref) <-
             case d of
               PdfValue.Value_ref (_532 :: PdfValue.Ref) -> HS.pure _532
               _ -> RTS.pError RTS.FromSystem "159:10--159:17"
                      "Pattern match failure"
           (i :: PdfValue.Value) <-
             RTS.pIsJust "160:8--160:23"
               ("Missing key: "
                  HS.++ HS.show
                          (Vector.vecFromRep "ID" :: Vector.Vector (RTS.UInt 8)))
               (Map.lookup (Vector.vecFromRep "ID") dict)
           case i of
             PdfValue.Value_array
               (_533 :: Vector.Vector PdfValue.Value) -> HS.pure ()
             _ -> RTS.pError RTS.FromSystem "161:11--161:20"
                    "Pattern match failure"
           (id0 :: Vector.Vector (RTS.UInt 8)) <-
             do (_538 :: PdfValue.Value) <-
                  do (_537 :: Vector.Vector PdfValue.Value) <-
                       case i of
                         PdfValue.Value_array
                           (_536 :: Vector.Vector PdfValue.Value) -> HS.pure _536
                         _ -> RTS.pError RTS.FromSystem "162:16--162:25"
                                "Pattern match failure"
                     RTS.pIsJust "162:9--162:25" "Index out of bounds"
                       ((Vector.!?) _537 (RTS.lit 0 :: RTS.UInt 64))
                case _538 of
                  PdfValue.Value_string
                    (_539 :: Vector.Vector (RTS.UInt 8)) -> HS.pure _539
                  _ -> RTS.pError RTS.FromSystem "162:9--162:38"
                         "Pattern match failure"
           (id1 :: Vector.Vector (RTS.UInt 8)) <-
             do (_542 :: PdfValue.Value) <-
                  do (_541 :: Vector.Vector PdfValue.Value) <-
                       case i of
                         PdfValue.Value_array
                           (_540 :: Vector.Vector PdfValue.Value) -> HS.pure _540
                         _ -> RTS.pError RTS.FromSystem "163:16--163:25"
                                "Pattern match failure"
                     RTS.pIsJust "163:9--163:25" "Index out of bounds"
                       ((Vector.!?) _541 (RTS.lit 1 :: RTS.UInt 64))
                case _542 of
                  PdfValue.Value_string
                    (_543 :: Vector.Vector (RTS.UInt 8)) -> HS.pure _543
                  _ -> RTS.pError RTS.FromSystem "163:9--163:38"
                         "Pattern match failure"
           HS.pure (TrailerDictEncrypt d eref id0 id1))
 
pTrailerDict ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> D.Parser TrailerDict
 
pTrailerDict
  (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (size :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "Size") dict)
     (root :: HS.Maybe PdfValue.Ref) <-
       RTS.pEnter "PdfDecl.Default"
         (PdfDecl.pDefault @(HS.Maybe PdfValue.Ref)
            (HS.Nothing :: HS.Maybe PdfValue.Ref)
            (do (x :: PdfValue.Value) <-
                  RTS.pIsJust "147:22--147:39"
                    ("Missing key: "
                       HS.++ HS.show
                               (Vector.vecFromRep "Root" :: Vector.Vector (RTS.UInt 8)))
                    (Map.lookup (Vector.vecFromRep "Root") dict)
                RTS.pErrorMode RTS.Abort
                  (do (__ :: HS.Maybe PdfValue.Ref) <-
                        do (_546 :: PdfValue.Ref) <-
                             case x of
                               PdfValue.Value_ref (_545 :: PdfValue.Ref) -> HS.pure _545
                               _ -> RTS.pError RTS.FromSystem "149:23--149:30"
                                      "Pattern match failure"
                           HS.pure (HS.Just _546)
                      HS.pure __)))
     (prev :: HS.Maybe HS.Integer) <-
       RTS.pOptional (RTS.<||) HS.Just
         (RTS.pEnter "PdfDecl.LookupNat"
            (PdfDecl.pLookupNat (Vector.vecFromRep "Prev") dict))
     (encrypt :: HS.Maybe TrailerDictEncrypt) <-
       RTS.pOptional (RTS.<||) HS.Just
         (RTS.pEnter "PdfXRef.TrailerDictEncrypt"
            (pTrailerDictEncrypt dict))
     (all :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       HS.pure dict
     HS.pure (TrailerDict size root prev encrypt all)
 
pCrossRefAndTrailer :: D.Parser CrossRefAndTrailer
 
pCrossRefAndTrailer =
  do (xref :: Vector.Vector CrossRefSubSection) <-
       RTS.pEnter "PdfXRef.CrossRefSection" pCrossRefSection
     RTS.pEnter "PdfValue._KW"
       (PdfValue._KW (Vector.vecFromRep "trailer"))
     (t :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pEnter "PdfValue.Dict" PdfValue.pDict
     (trailer :: TrailerDict) <-
       RTS.pEnter "PdfXRef.TrailerDict" (pTrailerDict t)
     HS.pure (CrossRefAndTrailer xref trailer)
 
pLookupInt ::
      Vector.Vector PdfValue.Value
        -> (RTS.UInt 64 -> D.Parser HS.Integer)
 
pLookupInt (arr :: Vector.Vector PdfValue.Value)
  (i :: RTS.UInt 64) =
  RTS.pEnter "PdfDecl.Default"
    (PdfDecl.pDefault @HS.Integer (RTS.lit 0 :: HS.Integer)
       (do (n :: PdfValue.Value) <-
             RTS.pIsJust "79:8--79:16" "Index out of bounds" ((Vector.!?) arr i)
           RTS.pErrorMode RTS.Abort
             (do (__ :: HS.Integer) <-
                   RTS.pEnter "PdfValue.NatValue" (PdfValue.pNatValue n)
                 HS.pure __)))
 
pXRefFormat ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> D.Parser XRefFormat
 
pXRefFormat
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (kv :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve"
         (PdfDecl.pLookupResolve (Vector.vecFromRep "W") header)
     (vs :: Vector.Vector PdfValue.Value) <-
       case kv of
         PdfValue.Value_array
           (_548 :: Vector.Vector PdfValue.Value) -> HS.pure _548
         _ -> RTS.pError RTS.FromSystem "70:12--70:22"
                "Pattern match failure"
     (b1 :: RTS.UInt 64) <-
       do (_549 :: HS.Integer) <-
            RTS.pEnter "PdfXRef.LookupInt"
              (pLookupInt vs (RTS.lit 0 :: RTS.UInt 64))
          RTS.pIsJust "71:12--71:36" "Value does not fit in target type"
            (RTS.convertMaybe _549 :: HS.Maybe (RTS.UInt 64))
     (b2 :: RTS.UInt 64) <-
       do (_550 :: HS.Integer) <-
            RTS.pEnter "PdfXRef.LookupInt"
              (pLookupInt vs (RTS.lit 1 :: RTS.UInt 64))
          RTS.pIsJust "72:12--72:36" "Value does not fit in target type"
            (RTS.convertMaybe _550 :: HS.Maybe (RTS.UInt 64))
     (b3 :: RTS.UInt 64) <-
       do (_551 :: HS.Integer) <-
            RTS.pEnter "PdfXRef.LookupInt"
              (pLookupInt vs (RTS.lit 2 :: RTS.UInt 64))
          RTS.pIsJust "73:12--73:36" "Value does not fit in target type"
            (RTS.convertMaybe _551 :: HS.Maybe (RTS.UInt 64))
     (bigwidth :: HS.Integer) <-
       RTS.loopFoldM
         (\(s :: HS.Integer) (x :: PdfValue.Value) ->
            do (__ :: HS.Integer) <-
                 do (_553 :: HS.Integer) <-
                      RTS.pEnter "PdfValue.NatValue" (PdfValue.pNatValue x)
                    HS.pure (RTS.add s _553)
               HS.pure __)
         (RTS.lit 0 :: HS.Integer)
         vs
     (width :: RTS.UInt 64) <-
       RTS.pIsJust "75:12--75:30" "Value does not fit in target type"
         (RTS.convertMaybe bigwidth :: HS.Maybe (RTS.UInt 64))
     HS.pure (XRefFormat b1 b2 b3 width)
 
pXRefIndex ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> D.Parser (Vector.Vector XRefIndex_0)
 
pXRefIndex
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (size :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "Size") header)
     (arr :: Vector.Vector HS.Integer) <-
       RTS.pEnter "PdfDecl.Default"
         (PdfDecl.pDefault @(Vector.Vector HS.Integer)
            (Vector.fromList [RTS.lit 0 :: HS.Integer, size])
            (RTS.pEnter "PdfDecl.LookupNats"
               (PdfDecl.pLookupNats (Vector.vecFromRep "Index") header)))
     (__ :: Vector.Vector XRefIndex_0) <-
       RTS.loopMapM
         (\(i :: RTS.UInt 64) ->
            do (firstId :: HS.Integer) <-
                 RTS.pIsJust "89:15--89:23" "Index out of bounds"
                   ((Vector.!?) arr i)
               (num :: RTS.UInt 64) <-
                 do (_555 :: HS.Integer) <-
                      RTS.pIsJust "90:15--90:23" "Index out of bounds"
                        ((Vector.!?) arr (RTS.add i (RTS.lit 1 :: RTS.UInt 64)))
                    RTS.pIsJust "90:15--90:40" "Value does not fit in target type"
                      (RTS.convertMaybe _555 :: HS.Maybe (RTS.UInt 64))
               HS.pure (XRefIndex_0 firstId num))
         (Vector.rangeUp (RTS.lit 0 :: RTS.UInt 64) (Vector.length arr)
            (RTS.lit 2 :: RTS.UInt 64))
         :: D.Parser (Vector.Vector XRefIndex_0)
     HS.pure __
 
pXRefMeta ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> D.Parser XRefMeta
 
pXRefMeta
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do RTS.pEnter "PdfDecl._CheckType"
       (PdfDecl._CheckType (Vector.vecFromRep "XRef") header)
     (index :: Vector.Vector XRefIndex_0) <-
       RTS.pEnter "PdfXRef.XRefIndex" (pXRefIndex header)
     (widths :: XRefFormat) <-
       RTS.pEnter "PdfXRef.XRefFormat" (pXRefFormat header)
     (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       HS.pure header
     HS.pure (XRefMeta index widths header)
 
pXRefFieldWithDefault ::
      HS.Integer -> (RTS.UInt 64 -> D.Parser HS.Integer)
 
pXRefFieldWithDefault (x :: HS.Integer) (n :: RTS.UInt 64) =
  (RTS.<||)
    (do RTS.pEnter "PdfValue._Guard"
          (PdfValue._Guard (n HS.== (RTS.lit 0 :: RTS.UInt 64)))
        (__ :: HS.Integer) <- HS.pure x
        HS.pure __)
    (RTS.pEnter "PdfDecl.BEBytes" (PdfDecl.pBEBytes n))
 
pXRefFieldRequired :: RTS.UInt 64 -> D.Parser HS.Integer
 
pXRefFieldRequired (n :: RTS.UInt 64) =
  do RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (n HS./= (RTS.lit 0 :: RTS.UInt 64)))
     (__ :: HS.Integer) <-
       RTS.pEnter "PdfDecl.BEBytes" (PdfDecl.pBEBytes n)
     HS.pure __
 
pXRefFree :: XRefFormat -> D.Parser XRefFree
 
pXRefFree (w :: XRefFormat) =
  do (obj :: HS.Integer) <-
       RTS.pEnter "PdfXRef.XRefFieldRequired"
         (pXRefFieldRequired (HS.getField @"b2" w))
     (gen :: HS.Integer) <-
       RTS.pEnter "PdfXRef.XRefFieldWithDefault"
         (pXRefFieldWithDefault (RTS.lit 0 :: HS.Integer)
            (HS.getField @"b3" w))
     HS.pure (XRefFree obj gen)
 
pXRefOffset :: XRefFormat -> D.Parser XRefOffset
 
pXRefOffset (w :: XRefFormat) =
  do (offset :: HS.Integer) <-
       RTS.pEnter "PdfXRef.XRefFieldRequired"
         (pXRefFieldRequired (HS.getField @"b2" w))
     (gen :: HS.Integer) <-
       RTS.pEnter "PdfXRef.XRefFieldWithDefault"
         (pXRefFieldWithDefault (RTS.lit 0 :: HS.Integer)
            (HS.getField @"b3" w))
     HS.pure (XRefOffset offset gen)
 
pXRefCompressed :: XRefFormat -> D.Parser XRefCompressed
 
pXRefCompressed (w :: XRefFormat) =
  do (container_obj :: HS.Integer) <-
       RTS.pEnter "PdfXRef.XRefFieldRequired"
         (pXRefFieldRequired (HS.getField @"b2" w))
     (obj_index :: HS.Integer) <-
       RTS.pEnter "PdfXRef.XRefFieldRequired"
         (pXRefFieldRequired (HS.getField @"b3" w))
     HS.pure (XRefCompressed container_obj obj_index)
 
pXRefObjEntry :: XRefFormat -> D.Parser XRefObjEntry
 
pXRefObjEntry (w :: XRefFormat) =
  RTS.pEnter "PdfDecl.Chunk"
    (PdfDecl.pChunk @XRefObjEntry (HS.getField @"width" w)
       (do (ftype :: HS.Integer) <-
             RTS.pEnter "PdfXRef.XRefFieldWithDefault"
               (pXRefFieldWithDefault (RTS.lit 1 :: HS.Integer)
                  (HS.getField @"b1" w))
           (__ :: XRefObjEntry) <-
             (RTS.|||)
               (RTS.pEnter "free"
                  (do (_560 :: XRefFree) <-
                        do RTS.pEnter "PdfValue._Guard"
                             (PdfValue._Guard (ftype HS.== (RTS.lit 0 :: HS.Integer)))
                           (__ :: XRefFree) <- RTS.pEnter "PdfXRef.XRefFree" (pXRefFree w)
                           HS.pure __
                      HS.pure (XRefObjEntry_free _560)))
               ((RTS.|||)
                  (RTS.pEnter "inUse"
                     (do (_562 :: XRefOffset) <-
                           do RTS.pEnter "PdfValue._Guard"
                                (PdfValue._Guard (ftype HS.== (RTS.lit 1 :: HS.Integer)))
                              (__ :: XRefOffset) <-
                                RTS.pEnter "PdfXRef.XRefOffset" (pXRefOffset w)
                              HS.pure __
                         HS.pure (XRefObjEntry_inUse _562)))
                  ((RTS.|||)
                     (RTS.pEnter "compressed"
                        (do (_564 :: XRefCompressed) <-
                              do RTS.pEnter "PdfValue._Guard"
                                   (PdfValue._Guard (ftype HS.== (RTS.lit 2 :: HS.Integer)))
                                 (__ :: XRefCompressed) <-
                                   RTS.pEnter "PdfXRef.XRefCompressed" (pXRefCompressed w)
                                 HS.pure __
                            HS.pure (XRefObjEntry_compressed _564)))
                     (RTS.pEnter "null"
                        (do (_566 :: ()) <-
                              do (__ :: ()) <-
                                   RTS.pEnter "PdfValue.Guard"
                                     (PdfValue.pGuard ((RTS.lit 2 :: HS.Integer) HS.< ftype))
                                 HS.pure __
                            HS.pure (XRefObjEntry_null _566)))))
           HS.pure __))
 
pXRefObjTable :: XRefMeta -> D.Parser XRefObjTable
 
pXRefObjTable (meta :: XRefMeta) =
  do (xref :: Vector.Vector XRefObjTable_0) <-
       RTS.loopMapM
         (\(idx :: XRefIndex_0) ->
            do (firstId :: HS.Integer) <- HS.pure (HS.getField @"firstId" idx)
               (entries :: Vector.Vector XRefObjEntry) <-
                 Vector.replicateM (HS.getField @"num" idx)
                   (RTS.pEnter "PdfXRef.XRefObjEntry"
                      (pXRefObjEntry (HS.getField @"widths" meta)))
               HS.pure (XRefObjTable_0 firstId entries))
         (HS.getField @"index" meta)
         :: D.Parser (Vector.Vector XRefObjTable_0)
     (trailer :: TrailerDict) <-
       RTS.pEnter "PdfXRef.TrailerDict"
         (pTrailerDict (HS.getField @"header" meta))
     HS.pure (XRefObjTable xref trailer)
 
pXRefObj :: D.Parser XRefObjTable
 
pXRefObj =
  do (str :: PdfDecl.Stream) <-
       do (_568 :: PdfDecl.TopDeclDef) <-
            do (_567 :: PdfDecl.TopDecl) <-
                 RTS.pEnter "PdfDecl.TopDecl" PdfDecl.pTopDecl
               HS.pure (HS.getField @"obj" _567)
          case _568 of
            PdfDecl.TopDeclDef_stream (_569 :: PdfDecl.Stream) -> HS.pure _569
            _ -> RTS.pError RTS.FromSystem "57:11--57:31"
                   "Pattern match failure"
     (__ :: XRefObjTable) <-
       do (_572 :: RTS.Input) <-
            case HS.getField @"body" str of
              PdfDecl.ApplyFilter_ok (_571 :: RTS.Input) -> HS.pure _571
              _ -> RTS.pError RTS.FromSystem "58:15--58:28"
                     "Pattern match failure"
          RTS.pEnter "PdfDecl.WithStream"
            (PdfDecl.pWithStream @XRefObjTable _572
               (do (_573 :: XRefMeta) <-
                     RTS.pEnter "PdfXRef.XRefMeta"
                       (pXRefMeta (HS.getField @"header" str))
                   RTS.pEnter "PdfXRef.XRefObjTable" (pXRefObjTable _573)))
     HS.pure __
 
pCrossRef :: D.Parser CrossRef
 
pCrossRef =
  (RTS.|||)
    (RTS.pEnter "oldXref"
       (do (_574 :: CrossRefAndTrailer) <-
             RTS.pEnter "PdfXRef.CrossRefAndTrailer" pCrossRefAndTrailer
           HS.pure (CrossRef_oldXref _574)))
    (RTS.pEnter "newXref"
       (do (_575 :: XRefObjTable) <- RTS.pEnter "PdfXRef.XRefObj" pXRefObj
           HS.pure (CrossRef_newXref _575)))
 
_UsedEntry :: D.Parser ()
 
_UsedEntry =
  HS.const ()
    HS.<$> RTS.pMatch1 "42:3--42:12" (RTS.bcSingle (RTS.uint8 110))
 
_FreeEntry :: D.Parser ()
 
_FreeEntry =
  HS.const ()
    HS.<$> RTS.pMatch1 "46:3--46:12" (RTS.bcSingle (RTS.uint8 102))
 
_CrossRefEntry :: D.Parser ()
 
_CrossRefEntry =
  do RTS.pEnter "PdfDecl._NatN"
       (PdfDecl._NatN (RTS.lit 10 :: RTS.UInt 64))
     HS.const () HS.<$> RTS.pMatch1 "32:19--32:27" PdfValue.cs_simpleWS
     RTS.pEnter "PdfDecl._NatN"
       (PdfDecl._NatN (RTS.lit 5 :: RTS.UInt 64))
     HS.const () HS.<$> RTS.pMatch1 "33:19--33:27" PdfValue.cs_simpleWS
     (RTS.|||)
       (RTS.pEnter "inUse" (RTS.pEnter "PdfXRef._UsedEntry" _UsedEntry))
       (RTS.pEnter "free" (RTS.pEnter "PdfXRef._FreeEntry" _FreeEntry))
     (RTS.|||)
       (do HS.const ()
             HS.<$> RTS.pMatch1 "38:5--38:13" PdfValue.cs_simpleWS
           (RTS.|||)
             (HS.const () HS.<$> RTS.pMatch1 "38:16--38:18" PdfValue.cs_cr)
             (HS.const () HS.<$> RTS.pMatch1 "38:22--38:24" PdfValue.cs_lf))
       (do HS.const () HS.<$> RTS.pMatch1 "38:32--38:34" PdfValue.cs_cr
           HS.const () HS.<$> RTS.pMatch1 "38:37--38:39" PdfValue.cs_lf)
 
_CrossRefSubSection :: D.Parser ()
 
_CrossRefSubSection =
  do RTS.pEnter "PdfValue._Token"
       (PdfValue._Token @HS.Integer
          (RTS.pEnter "PdfValue._Natural" PdfValue._Natural))
     (num :: RTS.UInt 64) <-
       do (_530 :: HS.Integer) <-
            RTS.pEnter "PdfValue.Token"
              (PdfValue.pToken @HS.Integer
                 (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
          RTS.pIsJust "27:13--27:36" "Value does not fit in target type"
            (RTS.convertMaybe _530 :: HS.Maybe (RTS.UInt 64))
     RTS.pSkipExact num
       (RTS.pEnter "PdfXRef._CrossRefEntry" _CrossRefEntry)
 
_CrossRefSection :: D.Parser ()
 
_CrossRefSection =
  do RTS.pEnter "PdfValue._KW"
       (PdfValue._KW (Vector.vecFromRep "xref"))
     RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: RTS.UInt 64)
       (RTS.pEnter "PdfXRef._CrossRefSubSection" _CrossRefSubSection)
 
_TrailerDictEncrypt ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_TrailerDictEncrypt
  (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (d :: PdfValue.Value) <-
       RTS.pIsJust "157:7--157:27"
         ("Missing key: "
            HS.++ HS.show
                    (Vector.vecFromRep "Encrypt" :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup (Vector.vecFromRep "Encrypt") dict)
     RTS.pErrorMode RTS.Abort
       (do case d of
             PdfValue.Value_ref (_532 :: PdfValue.Ref) -> HS.pure ()
             _ -> RTS.pError RTS.FromSystem "159:10--159:17"
                    "Pattern match failure"
           (i :: PdfValue.Value) <-
             RTS.pIsJust "160:8--160:23"
               ("Missing key: "
                  HS.++ HS.show
                          (Vector.vecFromRep "ID" :: Vector.Vector (RTS.UInt 8)))
               (Map.lookup (Vector.vecFromRep "ID") dict)
           case i of
             PdfValue.Value_array
               (_533 :: Vector.Vector PdfValue.Value) -> HS.pure ()
             _ -> RTS.pError RTS.FromSystem "161:11--161:20"
                    "Pattern match failure"
           do (_538 :: PdfValue.Value) <-
                do (_537 :: Vector.Vector PdfValue.Value) <-
                     case i of
                       PdfValue.Value_array
                         (_536 :: Vector.Vector PdfValue.Value) -> HS.pure _536
                       _ -> RTS.pError RTS.FromSystem "162:16--162:25"
                              "Pattern match failure"
                   RTS.pIsJust "162:9--162:25" "Index out of bounds"
                     ((Vector.!?) _537 (RTS.lit 0 :: RTS.UInt 64))
              case _538 of
                PdfValue.Value_string
                  (_539 :: Vector.Vector (RTS.UInt 8)) -> HS.pure ()
                _ -> RTS.pError RTS.FromSystem "162:9--162:38"
                       "Pattern match failure"
           (_542 :: PdfValue.Value) <-
             do (_541 :: Vector.Vector PdfValue.Value) <-
                  case i of
                    PdfValue.Value_array
                      (_540 :: Vector.Vector PdfValue.Value) -> HS.pure _540
                    _ -> RTS.pError RTS.FromSystem "163:16--163:25"
                           "Pattern match failure"
                RTS.pIsJust "163:9--163:25" "Index out of bounds"
                  ((Vector.!?) _541 (RTS.lit 1 :: RTS.UInt 64))
           case _542 of
             PdfValue.Value_string
               (_543 :: Vector.Vector (RTS.UInt 8)) -> HS.pure ()
             _ -> RTS.pError RTS.FromSystem "163:9--163:38"
                    "Pattern match failure")
 
_TrailerDict ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_TrailerDict
  (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Size") dict)
     RTS.pEnter "PdfDecl._Default"
       (PdfDecl._Default @(HS.Maybe PdfValue.Ref)
          (do (x :: PdfValue.Value) <-
                RTS.pIsJust "147:22--147:39"
                  ("Missing key: "
                     HS.++ HS.show
                             (Vector.vecFromRep "Root" :: Vector.Vector (RTS.UInt 8)))
                  (Map.lookup (Vector.vecFromRep "Root") dict)
              RTS.pErrorMode RTS.Abort
                (case x of
                   PdfValue.Value_ref (_545 :: PdfValue.Ref) -> HS.pure ()
                   _ -> RTS.pError RTS.FromSystem "149:23--149:30"
                          "Pattern match failure")))
     (RTS.<||)
       (RTS.pEnter "PdfDecl._LookupNat"
          (PdfDecl._LookupNat (Vector.vecFromRep "Prev") dict))
       (HS.pure ())
     (RTS.<||)
       (RTS.pEnter "PdfXRef._TrailerDictEncrypt"
          (_TrailerDictEncrypt dict))
       (HS.pure ())
 
_CrossRefAndTrailer :: D.Parser ()
 
_CrossRefAndTrailer =
  do RTS.pEnter "PdfXRef._CrossRefSection" _CrossRefSection
     RTS.pEnter "PdfValue._KW"
       (PdfValue._KW (Vector.vecFromRep "trailer"))
     (t :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pEnter "PdfValue.Dict" PdfValue.pDict
     RTS.pEnter "PdfXRef._TrailerDict" (_TrailerDict t)
 
_LookupInt ::
      Vector.Vector PdfValue.Value -> (RTS.UInt 64 -> D.Parser ())
 
_LookupInt (arr :: Vector.Vector PdfValue.Value)
  (i :: RTS.UInt 64) =
  RTS.pEnter "PdfDecl._Default"
    (PdfDecl._Default @HS.Integer
       (do (n :: PdfValue.Value) <-
             RTS.pIsJust "79:8--79:16" "Index out of bounds" ((Vector.!?) arr i)
           RTS.pErrorMode RTS.Abort
             (RTS.pEnter "PdfValue._NatValue" (PdfValue._NatValue n))))
 
_XRefFormat ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_XRefFormat
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (kv :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve"
         (PdfDecl.pLookupResolve (Vector.vecFromRep "W") header)
     (vs :: Vector.Vector PdfValue.Value) <-
       case kv of
         PdfValue.Value_array
           (_548 :: Vector.Vector PdfValue.Value) -> HS.pure _548
         _ -> RTS.pError RTS.FromSystem "70:12--70:22"
                "Pattern match failure"
     do (_549 :: HS.Integer) <-
          RTS.pEnter "PdfXRef.LookupInt"
            (pLookupInt vs (RTS.lit 0 :: RTS.UInt 64))
        RTS.pIsJust_ "71:12--71:36" "Value does not fit in target type"
          (RTS.convertMaybe _549 :: HS.Maybe (RTS.UInt 64))
     do (_550 :: HS.Integer) <-
          RTS.pEnter "PdfXRef.LookupInt"
            (pLookupInt vs (RTS.lit 1 :: RTS.UInt 64))
        RTS.pIsJust_ "72:12--72:36" "Value does not fit in target type"
          (RTS.convertMaybe _550 :: HS.Maybe (RTS.UInt 64))
     do (_551 :: HS.Integer) <-
          RTS.pEnter "PdfXRef.LookupInt"
            (pLookupInt vs (RTS.lit 2 :: RTS.UInt 64))
        RTS.pIsJust_ "73:12--73:36" "Value does not fit in target type"
          (RTS.convertMaybe _551 :: HS.Maybe (RTS.UInt 64))
     (bigwidth :: HS.Integer) <-
       RTS.loopFoldM
         (\(s :: HS.Integer) (x :: PdfValue.Value) ->
            do (__ :: HS.Integer) <-
                 do (_553 :: HS.Integer) <-
                      RTS.pEnter "PdfValue.NatValue" (PdfValue.pNatValue x)
                    HS.pure (RTS.add s _553)
               HS.pure __)
         (RTS.lit 0 :: HS.Integer)
         vs
     RTS.pIsJust_ "75:12--75:30" "Value does not fit in target type"
       (RTS.convertMaybe bigwidth :: HS.Maybe (RTS.UInt 64))
 
_XRefIndex ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_XRefIndex
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (size :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "Size") header)
     (arr :: Vector.Vector HS.Integer) <-
       RTS.pEnter "PdfDecl.Default"
         (PdfDecl.pDefault @(Vector.Vector HS.Integer)
            (Vector.fromList [RTS.lit 0 :: HS.Integer, size])
            (RTS.pEnter "PdfDecl.LookupNats"
               (PdfDecl.pLookupNats (Vector.vecFromRep "Index") header)))
     HS.void
       (RTS.loopMapM
          (\(i :: RTS.UInt 64) ->
             do (firstId :: HS.Integer) <-
                  RTS.pIsJust "89:15--89:23" "Index out of bounds"
                    ((Vector.!?) arr i)
                (num :: RTS.UInt 64) <-
                  do (_555 :: HS.Integer) <-
                       RTS.pIsJust "90:15--90:23" "Index out of bounds"
                         ((Vector.!?) arr (RTS.add i (RTS.lit 1 :: RTS.UInt 64)))
                     RTS.pIsJust "90:15--90:40" "Value does not fit in target type"
                       (RTS.convertMaybe _555 :: HS.Maybe (RTS.UInt 64))
                HS.pure (XRefIndex_0 firstId num))
          (Vector.rangeUp (RTS.lit 0 :: RTS.UInt 64) (Vector.length arr)
             (RTS.lit 2 :: RTS.UInt 64))
          :: D.Parser (Vector.Vector XRefIndex_0))
     HS.pure ()
 
_XRefMeta ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_XRefMeta
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do RTS.pEnter "PdfDecl._CheckType"
       (PdfDecl._CheckType (Vector.vecFromRep "XRef") header)
     RTS.pEnter "PdfXRef._XRefIndex" (_XRefIndex header)
     RTS.pEnter "PdfXRef._XRefFormat" (_XRefFormat header)
 
_XRefFieldWithDefault :: RTS.UInt 64 -> D.Parser ()
 
_XRefFieldWithDefault (n :: RTS.UInt 64) =
  (RTS.<||)
    (RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (n HS.== (RTS.lit 0 :: RTS.UInt 64))))
    (RTS.pEnter "PdfDecl._BEBytes" (PdfDecl._BEBytes n))
 
_XRefFieldRequired :: RTS.UInt 64 -> D.Parser ()
 
_XRefFieldRequired (n :: RTS.UInt 64) =
  do RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (n HS./= (RTS.lit 0 :: RTS.UInt 64)))
     RTS.pEnter "PdfDecl._BEBytes" (PdfDecl._BEBytes n)
 
_XRefFree :: XRefFormat -> D.Parser ()
 
_XRefFree (w :: XRefFormat) =
  do RTS.pEnter "PdfXRef._XRefFieldRequired"
       (_XRefFieldRequired (HS.getField @"b2" w))
     RTS.pEnter "PdfXRef._XRefFieldWithDefault"
       (_XRefFieldWithDefault (HS.getField @"b3" w))
 
_XRefOffset :: XRefFormat -> D.Parser ()
 
_XRefOffset (w :: XRefFormat) =
  do RTS.pEnter "PdfXRef._XRefFieldRequired"
       (_XRefFieldRequired (HS.getField @"b2" w))
     RTS.pEnter "PdfXRef._XRefFieldWithDefault"
       (_XRefFieldWithDefault (HS.getField @"b3" w))
 
_XRefCompressed :: XRefFormat -> D.Parser ()
 
_XRefCompressed (w :: XRefFormat) =
  do RTS.pEnter "PdfXRef._XRefFieldRequired"
       (_XRefFieldRequired (HS.getField @"b2" w))
     RTS.pEnter "PdfXRef._XRefFieldRequired"
       (_XRefFieldRequired (HS.getField @"b3" w))
 
_XRefObjEntry :: XRefFormat -> D.Parser ()
 
_XRefObjEntry (w :: XRefFormat) =
  RTS.pEnter "PdfDecl._Chunk"
    (PdfDecl._Chunk @XRefObjEntry (HS.getField @"width" w)
       (do (ftype :: HS.Integer) <-
             RTS.pEnter "PdfXRef.XRefFieldWithDefault"
               (pXRefFieldWithDefault (RTS.lit 1 :: HS.Integer)
                  (HS.getField @"b1" w))
           (RTS.|||)
             (RTS.pEnter "free"
                (do RTS.pEnter "PdfValue._Guard"
                      (PdfValue._Guard (ftype HS.== (RTS.lit 0 :: HS.Integer)))
                    RTS.pEnter "PdfXRef._XRefFree" (_XRefFree w)))
             ((RTS.|||)
                (RTS.pEnter "inUse"
                   (do RTS.pEnter "PdfValue._Guard"
                         (PdfValue._Guard (ftype HS.== (RTS.lit 1 :: HS.Integer)))
                       RTS.pEnter "PdfXRef._XRefOffset" (_XRefOffset w)))
                ((RTS.|||)
                   (RTS.pEnter "compressed"
                      (do RTS.pEnter "PdfValue._Guard"
                            (PdfValue._Guard (ftype HS.== (RTS.lit 2 :: HS.Integer)))
                          RTS.pEnter "PdfXRef._XRefCompressed" (_XRefCompressed w)))
                   (RTS.pEnter "null"
                      (RTS.pEnter "PdfValue._Guard"
                         (PdfValue._Guard ((RTS.lit 2 :: HS.Integer) HS.< ftype))))))))
 
_XRefObjTable :: XRefMeta -> D.Parser ()
 
_XRefObjTable (meta :: XRefMeta) =
  do HS.void
       (RTS.loopMapM
          (\(idx :: XRefIndex_0) ->
             do (firstId :: HS.Integer) <- HS.pure (HS.getField @"firstId" idx)
                (entries :: Vector.Vector XRefObjEntry) <-
                  Vector.replicateM (HS.getField @"num" idx)
                    (RTS.pEnter "PdfXRef.XRefObjEntry"
                       (pXRefObjEntry (HS.getField @"widths" meta)))
                HS.pure (XRefObjTable_0 firstId entries))
          (HS.getField @"index" meta)
          :: D.Parser (Vector.Vector XRefObjTable_0))
     RTS.pEnter "PdfXRef._TrailerDict"
       (_TrailerDict (HS.getField @"header" meta))
 
_XRefObj :: D.Parser ()
 
_XRefObj =
  do (str :: PdfDecl.Stream) <-
       do (_568 :: PdfDecl.TopDeclDef) <-
            do (_567 :: PdfDecl.TopDecl) <-
                 RTS.pEnter "PdfDecl.TopDecl" PdfDecl.pTopDecl
               HS.pure (HS.getField @"obj" _567)
          case _568 of
            PdfDecl.TopDeclDef_stream (_569 :: PdfDecl.Stream) -> HS.pure _569
            _ -> RTS.pError RTS.FromSystem "57:11--57:31"
                   "Pattern match failure"
     (_572 :: RTS.Input) <-
       case HS.getField @"body" str of
         PdfDecl.ApplyFilter_ok (_571 :: RTS.Input) -> HS.pure _571
         _ -> RTS.pError RTS.FromSystem "58:15--58:28"
                "Pattern match failure"
     RTS.pEnter "PdfDecl._WithStream"
       (PdfDecl._WithStream @XRefObjTable _572
          (do (_573 :: XRefMeta) <-
                RTS.pEnter "PdfXRef.XRefMeta"
                  (pXRefMeta (HS.getField @"header" str))
              RTS.pEnter "PdfXRef._XRefObjTable" (_XRefObjTable _573)))
 
_CrossRef :: D.Parser ()
 
_CrossRef =
  (RTS.|||)
    (RTS.pEnter "oldXref"
       (RTS.pEnter "PdfXRef._CrossRefAndTrailer" _CrossRefAndTrailer))
    (RTS.pEnter "newXref" (RTS.pEnter "PdfXRef._XRefObj" _XRefObj))