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
 
 
data TrailerDict_0
  = TrailerDict_0 PdfValue.Value PdfValue.Ref
      (Vector.Vector (RTS.UInt 8))
      (Vector.Vector (RTS.UInt 8))
  
 
deriving instance HS.Eq TrailerDict_0
 
deriving instance HS.Ord TrailerDict_0
 
deriving instance HS.Show TrailerDict_0
 
instance RTS.DDL TrailerDict_0 where
 
instance HS.HasField "d" TrailerDict_0 PdfValue.Value where
  getField (TrailerDict_0 x _ _ _) = x
 
instance HS.HasField "eref" TrailerDict_0 PdfValue.Ref where
  getField (TrailerDict_0 _ x _ _) = x
 
instance HS.HasField "id0" TrailerDict_0
           (Vector.Vector (RTS.UInt 8)) where
  getField (TrailerDict_0 _ _ x _) = x
 
instance HS.HasField "id1" TrailerDict_0
           (Vector.Vector (RTS.UInt 8)) where
  getField (TrailerDict_0 _ _ _ x) = x
 
data XRefIndex_0
  = XRefIndex_0 HS.Integer HS.Integer
  
 
deriving instance HS.Eq XRefIndex_0
 
deriving instance HS.Ord XRefIndex_0
 
deriving instance HS.Show XRefIndex_0
 
instance RTS.DDL XRefIndex_0 where
 
instance HS.HasField "firstId" XRefIndex_0 HS.Integer where
  getField (XRefIndex_0 x _) = x
 
instance HS.HasField "num" XRefIndex_0 HS.Integer where
  getField (XRefIndex_0 _ x) = x
 
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
 
data TrailerDict
  = TrailerDict HS.Integer (HS.Maybe PdfValue.Ref)
      (HS.Maybe HS.Integer)
      (HS.Maybe TrailerDict_0)
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
           (HS.Maybe TrailerDict_0) where
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
 
data EncryptionDict
  = EncryptionDict (Vector.Vector (RTS.UInt 8))
      (HS.Maybe (Vector.Vector (RTS.UInt 8)))
      HS.Integer
      HS.Integer
      (Vector.Vector (RTS.UInt 8))
      (Vector.Vector (RTS.UInt 8))
      HS.Integer
      HS.Integer
  
 
deriving instance HS.Eq EncryptionDict
 
deriving instance HS.Ord EncryptionDict
 
deriving instance HS.Show EncryptionDict
 
instance RTS.DDL EncryptionDict where
 
instance HS.HasField "encFilter" EncryptionDict
           (Vector.Vector (RTS.UInt 8)) where
  getField (EncryptionDict x _ _ _ _ _ _ _) = x
 
instance HS.HasField "encSubFilter" EncryptionDict
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (EncryptionDict _ x _ _ _ _ _ _) = x
 
instance HS.HasField "encV" EncryptionDict HS.Integer where
  getField (EncryptionDict _ _ x _ _ _ _ _) = x
 
instance HS.HasField "encR" EncryptionDict HS.Integer where
  getField (EncryptionDict _ _ _ x _ _ _ _) = x
 
instance HS.HasField "encO" EncryptionDict
           (Vector.Vector (RTS.UInt 8)) where
  getField (EncryptionDict _ _ _ _ x _ _ _) = x
 
instance HS.HasField "encU" EncryptionDict
           (Vector.Vector (RTS.UInt 8)) where
  getField (EncryptionDict _ _ _ _ _ x _ _) = x
 
instance HS.HasField "encP" EncryptionDict HS.Integer where
  getField (EncryptionDict _ _ _ _ _ _ x _) = x
 
instance HS.HasField "stmFLength" EncryptionDict HS.Integer where
  getField (EncryptionDict _ _ _ _ _ _ _ x) = x
 
data XRefFormat
  = XRefFormat HS.Integer HS.Integer HS.Integer HS.Integer
  
 
deriving instance HS.Eq XRefFormat
 
deriving instance HS.Ord XRefFormat
 
deriving instance HS.Show XRefFormat
 
instance RTS.DDL XRefFormat where
 
instance HS.HasField "b1" XRefFormat HS.Integer where
  getField (XRefFormat x _ _ _) = x
 
instance HS.HasField "b2" XRefFormat HS.Integer where
  getField (XRefFormat _ x _ _) = x
 
instance HS.HasField "b3" XRefFormat HS.Integer where
  getField (XRefFormat _ _ x _) = x
 
instance HS.HasField "witdth" XRefFormat HS.Integer where
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
 
pFreeEntry :: HS.Integer -> (HS.Integer -> D.Parser FreeEntry)
 
pFreeEntry (num :: HS.Integer) (gen :: HS.Integer) =
  do HS.const ()
       HS.<$> RTS.pMatch1 "46:3--46:12" (RTS.bcSingle (RTS.uint8 102))
     (obj :: HS.Integer) <- HS.pure num
     (gen :: HS.Integer) <- HS.pure gen
     HS.pure (FreeEntry obj gen)
 
pUsedEntry :: HS.Integer -> (HS.Integer -> D.Parser UsedEntry)
 
pUsedEntry (num :: HS.Integer) (gen :: HS.Integer) =
  do HS.const ()
       HS.<$> RTS.pMatch1 "42:3--42:12" (RTS.bcSingle (RTS.uint8 110))
     (offset :: HS.Integer) <- HS.pure num
     (gen :: HS.Integer) <- HS.pure gen
     HS.pure (UsedEntry offset gen)
 
pCrossRefEntry :: D.Parser CrossRefEntry
 
pCrossRefEntry =
  do (num :: HS.Integer) <-
       RTS.pEnter "PdfDecl.NatN"
         (PdfDecl.pNatN (RTS.lit 10 :: HS.Integer))
     HS.const () HS.<$> RTS.pMatch1 "32:19--32:27" PdfValue.cs_simpleWS
     (gen :: HS.Integer) <-
       RTS.pEnter "PdfDecl.NatN" (PdfDecl.pNatN (RTS.lit 5 :: HS.Integer))
     HS.const () HS.<$> RTS.pMatch1 "33:19--33:27" PdfValue.cs_simpleWS
     (__ :: CrossRefEntry) <-
       (RTS.|||)
         (RTS.pEnter "inUse"
            (do (_0 :: UsedEntry) <-
                  RTS.pEnter "PdfXRef.UsedEntry" (pUsedEntry num gen)
                HS.pure (CrossRefEntry_inUse _0)))
         (RTS.pEnter "free"
            (do (_1 :: FreeEntry) <-
                  RTS.pEnter "PdfXRef.FreeEntry" (pFreeEntry num gen)
                HS.pure (CrossRefEntry_free _1)))
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
     (num :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (PdfValue.pToken @HS.Integer
            (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
     (entries :: Vector.Vector CrossRefEntry) <-
       Vector.replicateM num
         (RTS.pEnter "PdfXRef.CrossRefEntry" pCrossRefEntry)
     HS.pure (CrossRefSubSection firstId entries)
 
pCrossRefSection :: D.Parser (Vector.Vector CrossRefSubSection)
 
pCrossRefSection =
  do RTS.pEnter "PdfValue._KW"
       (PdfValue._KW (Vector.vecFromRep "xref"))
     (__ :: Vector.Vector CrossRefSubSection) <-
       RTS.pMinLength "22:3--22:31" (RTS.lit 1 :: HS.Integer)
         (RTS.pMany (RTS.<||)
            (RTS.pEnter "PdfXRef.CrossRefSubSection" pCrossRefSubSection))
     HS.pure __
 
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
                  RTS.pIsJust "149:22--149:39"
                    ("Missing key: "
                       HS.++ HS.show
                               (Vector.vecFromRep "Root" :: Vector.Vector (RTS.UInt 8)))
                    (Map.lookup (Vector.vecFromRep "Root") dict)
                RTS.pErrorMode RTS.Abort
                  (do (__ :: HS.Maybe PdfValue.Ref) <-
                        do (_2 :: PdfValue.Ref) <-
                             RTS.pIsJust "151:23--151:30" "Expected `ref`"
                               (HS.getField @"ref" x)
                           HS.pure (HS.Just _2)
                      HS.pure __)))
     (prev :: HS.Maybe HS.Integer) <-
       RTS.pOptional (RTS.<||) HS.Just
         (RTS.pEnter "PdfDecl.LookupNat"
            (PdfDecl.pLookupNat (Vector.vecFromRep "Prev") dict))
     (encrypt :: HS.Maybe TrailerDict_0) <-
       RTS.pOptional (RTS.<||) HS.Just
         (do (d :: PdfValue.Value) <-
               RTS.pIsJust "155:9--155:29"
                 ("Missing key: "
                    HS.++ HS.show
                            (Vector.vecFromRep "Encrypt" :: Vector.Vector (RTS.UInt 8)))
                 (Map.lookup (Vector.vecFromRep "Encrypt") dict)
             RTS.pErrorMode RTS.Abort
               (do (eref :: PdfValue.Ref) <-
                     RTS.pIsJust "157:12--157:19" "Expected `ref`"
                       (HS.getField @"ref" d)
                   (i :: PdfValue.Value) <-
                     RTS.pIsJust "158:10--158:25"
                       ("Missing key: "
                          HS.++ HS.show
                                  (Vector.vecFromRep "ID" :: Vector.Vector (RTS.UInt 8)))
                       (Map.lookup (Vector.vecFromRep "ID") dict)
                   RTS.pIsJust_ "159:13--159:22" "Expected `array`"
                     (HS.getField @"array" i)
                   (id0 :: Vector.Vector (RTS.UInt 8)) <-
                     do (_6 :: PdfValue.Value) <-
                          do (_5 :: Vector.Vector PdfValue.Value) <-
                               RTS.pIsJust "160:18--160:27" "Expected `array`"
                                 (HS.getField @"array" i)
                             RTS.pIsJust "160:11--160:27" "Index out of bounds"
                               ((Vector.!?) _5 (RTS.lit 0 :: HS.Integer))
                        RTS.pIsJust "160:11--160:40" "Expected `string`"
                          (HS.getField @"string" _6)
                   (id1 :: Vector.Vector (RTS.UInt 8)) <-
                     do (_8 :: PdfValue.Value) <-
                          do (_7 :: Vector.Vector PdfValue.Value) <-
                               RTS.pIsJust "161:18--161:27" "Expected `array`"
                                 (HS.getField @"array" i)
                             RTS.pIsJust "161:11--161:27" "Index out of bounds"
                               ((Vector.!?) _7 (RTS.lit 1 :: HS.Integer))
                        RTS.pIsJust "161:11--161:40" "Expected `string`"
                          (HS.getField @"string" _8)
                   HS.pure (TrailerDict_0 d eref id0 id1)))
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
      Vector.Vector PdfValue.Value -> (HS.Integer -> D.Parser HS.Integer)
 
pLookupInt (arr :: Vector.Vector PdfValue.Value)
  (i :: HS.Integer) =
  RTS.pEnter "PdfDecl.Default"
    (PdfDecl.pDefault @HS.Integer (RTS.lit 0 :: HS.Integer)
       (do (n :: PdfValue.Value) <-
             RTS.pIsJust "81:8--81:16" "Index out of bounds" ((Vector.!?) arr i)
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
       RTS.pIsJust "70:12--70:22" "Expected `array`"
         (HS.getField @"array" kv)
     (b1 :: HS.Integer) <-
       RTS.pEnter "PdfXRef.LookupInt"
         (pLookupInt vs (RTS.lit 0 :: HS.Integer))
     (b2 :: HS.Integer) <-
       RTS.pEnter "PdfXRef.LookupInt"
         (pLookupInt vs (RTS.lit 1 :: HS.Integer))
     (b3 :: HS.Integer) <-
       RTS.pEnter "PdfXRef.LookupInt"
         (pLookupInt vs (RTS.lit 2 :: HS.Integer))
     (witdth :: HS.Integer) <-
       RTS.loopFoldM
         (\(s :: HS.Integer) (x :: PdfValue.Value) ->
            do (n :: HS.Integer) <-
                 RTS.pEnter "PdfValue.NatValue" (PdfValue.pNatValue x)
               (__ :: HS.Integer) <- HS.pure (RTS.add s n)
               HS.pure __)
         (RTS.lit 0 :: HS.Integer)
         vs
     HS.pure (XRefFormat b1 b2 b3 witdth)
 
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
         (\(i :: HS.Integer) ->
            do (firstId :: HS.Integer) <-
                 RTS.pIsJust "91:15--91:23" "Index out of bounds"
                   ((Vector.!?) arr i)
               (num :: HS.Integer) <-
                 RTS.pIsJust "92:15--92:23" "Index out of bounds"
                   ((Vector.!?) arr (RTS.add i (RTS.lit 1 :: HS.Integer)))
               HS.pure (XRefIndex_0 firstId num))
         (Vector.rangeUp (RTS.lit 0 :: HS.Integer)
            (HS.toInteger (Vector.length arr))
            (RTS.lit 2 :: HS.Integer))
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
 
pXRefFieldRequired :: HS.Integer -> D.Parser HS.Integer
 
pXRefFieldRequired (n :: HS.Integer) =
  do RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (n HS./= (RTS.lit 0 :: HS.Integer)))
     (__ :: HS.Integer) <-
       RTS.pEnter "PdfDecl.BEBytes" (PdfDecl.pBEBytes n)
     HS.pure __
 
pXRefCompressed :: XRefFormat -> D.Parser XRefCompressed
 
pXRefCompressed (w :: XRefFormat) =
  do (container_obj :: HS.Integer) <-
       RTS.pEnter "PdfXRef.XRefFieldRequired"
         (pXRefFieldRequired (HS.getField @"b2" w))
     (obj_index :: HS.Integer) <-
       RTS.pEnter "PdfXRef.XRefFieldRequired"
         (pXRefFieldRequired (HS.getField @"b3" w))
     HS.pure (XRefCompressed container_obj obj_index)
 
pXRefFieldWithDefault ::
      HS.Integer -> (HS.Integer -> D.Parser HS.Integer)
 
pXRefFieldWithDefault (x :: HS.Integer) (n :: HS.Integer) =
  (RTS.<||)
    (do RTS.pEnter "PdfValue._Guard"
          (PdfValue._Guard (n HS.== (RTS.lit 0 :: HS.Integer)))
        (__ :: HS.Integer) <- HS.pure x
        HS.pure __)
    (RTS.pEnter "PdfDecl.BEBytes" (PdfDecl.pBEBytes n))
 
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
 
pXRefObjEntry :: XRefFormat -> D.Parser XRefObjEntry
 
pXRefObjEntry (w :: XRefFormat) =
  RTS.pEnter "PdfDecl.Chunk"
    (PdfDecl.pChunk @XRefObjEntry (HS.getField @"witdth" w)
       (do (ftype :: HS.Integer) <-
             RTS.pEnter "PdfXRef.XRefFieldWithDefault"
               (pXRefFieldWithDefault (RTS.lit 1 :: HS.Integer)
                  (HS.getField @"b1" w))
           (__ :: XRefObjEntry) <-
             (RTS.|||)
               (RTS.pEnter "free"
                  (do (_9 :: XRefFree) <-
                        do RTS.pEnter "PdfValue._Guard"
                             (PdfValue._Guard (ftype HS.== (RTS.lit 0 :: HS.Integer)))
                           (__ :: XRefFree) <- RTS.pEnter "PdfXRef.XRefFree" (pXRefFree w)
                           HS.pure __
                      HS.pure (XRefObjEntry_free _9)))
               ((RTS.|||)
                  (RTS.pEnter "inUse"
                     (do (_10 :: XRefOffset) <-
                           do RTS.pEnter "PdfValue._Guard"
                                (PdfValue._Guard (ftype HS.== (RTS.lit 1 :: HS.Integer)))
                              (__ :: XRefOffset) <-
                                RTS.pEnter "PdfXRef.XRefOffset" (pXRefOffset w)
                              HS.pure __
                         HS.pure (XRefObjEntry_inUse _10)))
                  ((RTS.|||)
                     (RTS.pEnter "compressed"
                        (do (_11 :: XRefCompressed) <-
                              do RTS.pEnter "PdfValue._Guard"
                                   (PdfValue._Guard (ftype HS.== (RTS.lit 2 :: HS.Integer)))
                                 (__ :: XRefCompressed) <-
                                   RTS.pEnter "PdfXRef.XRefCompressed" (pXRefCompressed w)
                                 HS.pure __
                            HS.pure (XRefObjEntry_compressed _11)))
                     (RTS.pEnter "null"
                        (do (_12 :: ()) <-
                              do (__ :: ()) <-
                                   RTS.pEnter "PdfValue.Guard"
                                     (PdfValue.pGuard ((RTS.lit 2 :: HS.Integer) HS.< ftype))
                                 HS.pure __
                            HS.pure (XRefObjEntry_null _12)))))
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
       do (_14 :: PdfDecl.TopDeclDef) <-
            do (_13 :: PdfDecl.TopDecl) <-
                 RTS.pEnter "PdfDecl.TopDecl" PdfDecl.pTopDecl
               HS.pure (HS.getField @"obj" _13)
          RTS.pIsJust "57:11--57:31" "Expected `stream`"
            (HS.getField @"stream" _14)
     (__ :: XRefObjTable) <-
       do (_15 :: RTS.Input) <-
            RTS.pIsJust "58:15--58:28" "Expected `ok`"
              (HS.getField @"ok" (HS.getField @"body" str))
          RTS.pEnter "PdfDecl.WithStream"
            (PdfDecl.pWithStream @XRefObjTable _15
               (do (_16 :: XRefMeta) <-
                     RTS.pEnter "PdfXRef.XRefMeta"
                       (pXRefMeta (HS.getField @"header" str))
                   RTS.pEnter "PdfXRef.XRefObjTable" (pXRefObjTable _16)))
     HS.pure __
 
pCrossRef :: D.Parser CrossRef
 
pCrossRef =
  (RTS.|||)
    (RTS.pEnter "oldXref"
       (do (_17 :: CrossRefAndTrailer) <-
             RTS.pEnter "PdfXRef.CrossRefAndTrailer" pCrossRefAndTrailer
           HS.pure (CrossRef_oldXref _17)))
    (RTS.pEnter "newXref"
       (do (_18 :: XRefObjTable) <- RTS.pEnter "PdfXRef.XRefObj" pXRefObj
           HS.pure (CrossRef_newXref _18)))
 
pEncryptionDict :: PdfValue.Ref -> D.Parser EncryptionDict
 
pEncryptionDict (eref :: PdfValue.Ref) =
  do (edict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_19 :: PdfValue.Value) <-
            RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef eref)
          RTS.pIsJust "168:13--168:39" "Expected `dict`"
            (HS.getField @"dict" _19)
     (encFilter :: Vector.Vector (RTS.UInt 8)) <-
       do (_20 :: PdfValue.Value) <-
            RTS.pIsJust "170:16--170:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Filter" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Filter") edict)
          RTS.pIsJust "170:16--170:45" "Expected `name`"
            (HS.getField @"name" _20)
     RTS.pGuard "171:3--171:33" "guard failed"
       (encFilter HS.== Vector.vecFromRep "Standard")
     (encSubFilter :: HS.Maybe (Vector.Vector (RTS.UInt 8))) <-
       RTS.pOptional (RTS.<||) HS.Just
         (do (_21 :: PdfValue.Value) <-
               RTS.pIsJust "173:29--173:52"
                 ("Missing key: "
                    HS.++ HS.show
                            (Vector.vecFromRep "SubFilter" :: Vector.Vector (RTS.UInt 8)))
                 (Map.lookup (Vector.vecFromRep "SubFilter") edict)
             RTS.pIsJust "173:29--173:61" "Expected `name`"
               (HS.getField @"name" _21))
     (encV :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "V") edict)
     (encR :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "R") edict)
     RTS.pGuard "179:3--179:47" "guard failed"
       ((encR HS.== (RTS.lit 3 :: HS.Integer))
          HS.|| ((encV HS.== (RTS.lit 4 :: HS.Integer))
                   HS.&& (encR HS.== (RTS.lit 4 :: HS.Integer))))
     (encO :: Vector.Vector (RTS.UInt 8)) <-
       do (_22 :: PdfValue.Value) <-
            RTS.pIsJust "181:11--181:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "O" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "O") edict)
          RTS.pIsJust "181:11--181:37" "Expected `string`"
            (HS.getField @"string" _22)
     (encU :: Vector.Vector (RTS.UInt 8)) <-
       do (_23 :: PdfValue.Value) <-
            RTS.pIsJust "182:11--182:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "U" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "U") edict)
          RTS.pIsJust "182:11--182:37" "Expected `string`"
            (HS.getField @"string" _23)
     (encP :: HS.Integer) <-
       do (v :: PdfValue.Number) <-
            do (_24 :: PdfValue.Value) <-
                 RTS.pIsJust "185:11--185:26"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "P" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "P") edict)
               RTS.pIsJust "185:11--185:37" "Expected `number`"
                 (HS.getField @"number" _24)
          (__ :: HS.Integer) <- HS.pure (HS.getField @"num" v)
          HS.pure __
     (stmFLength :: HS.Integer) <-
       (RTS.<||)
         (do RTS.pGuard "193:7--193:23" "guard failed"
               (encV HS.== (RTS.lit 2 :: HS.Integer))
             RTS.pErrorMode RTS.Abort
               (do (len :: HS.Integer) <-
                     (RTS.<||)
                       (RTS.pEnter "PdfDecl.LookupNat"
                          (PdfDecl.pLookupNat (Vector.vecFromRep "Length") edict))
                       (HS.pure (RTS.lit 40 :: HS.Integer))
                   RTS.pGuard "197:7--197:24" "guard failed"
                     (len HS.== (RTS.lit 128 :: HS.Integer))
                   (__ :: HS.Integer) <- HS.pure (RTS.lit 16 :: HS.Integer)
                   HS.pure __))
         (do RTS.pGuard "201:7--201:23" "guard failed"
               (encV HS.== (RTS.lit 4 :: HS.Integer))
             (stmF :: Vector.Vector (RTS.UInt 8)) <-
               do (_25 :: PdfValue.Value) <-
                    RTS.pIsJust "203:16--203:34"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "StmF" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "StmF") edict)
                  RTS.pIsJust "203:16--203:43" "Expected `name`"
                    (HS.getField @"name" _25)
             (strF :: Vector.Vector (RTS.UInt 8)) <-
               do (_26 :: PdfValue.Value) <-
                    RTS.pIsJust "204:16--204:34"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "StrF" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "StrF") edict)
                  RTS.pIsJust "204:16--204:43" "Expected `name`"
                    (HS.getField @"name" _26)
             (cf :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
               do (_27 :: PdfValue.Value) <-
                    RTS.pIsJust "205:14--205:30"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "CF" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "CF") edict)
                  RTS.pIsJust "205:14--205:39" "Expected `dict`"
                    (HS.getField @"dict" _27)
             (stmFdict
                :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
               do (_28 :: PdfValue.Value) <-
                    RTS.pIsJust "208:20--208:33"
                      ("Missing key: "
                         HS.++ HS.show (stmF :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup stmF cf)
                  RTS.pIsJust "208:20--208:42" "Expected `dict`"
                    (HS.getField @"dict" _28)
             (stmFname :: Vector.Vector (RTS.UInt 8)) <-
               do (_29 :: PdfValue.Value) <-
                    RTS.pIsJust "209:20--209:40"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "CFM") stmFdict)
                  RTS.pIsJust "209:20--209:49" "Expected `name`"
                    (HS.getField @"name" _29)
             (stmFLen :: HS.Integer) <-
               RTS.pEnter "PdfDecl.LookupNat"
                 (PdfDecl.pLookupNat (Vector.vecFromRep "Length") stmFdict)
             (strFdict
                :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
               do (_30 :: PdfValue.Value) <-
                    RTS.pIsJust "213:20--213:33"
                      ("Missing key: "
                         HS.++ HS.show (strF :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup strF cf)
                  RTS.pIsJust "213:20--213:42" "Expected `dict`"
                    (HS.getField @"dict" _30)
             (strFname :: Vector.Vector (RTS.UInt 8)) <-
               do (_31 :: PdfValue.Value) <-
                    RTS.pIsJust "214:20--214:40"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "CFM") strFdict)
                  RTS.pIsJust "214:20--214:49" "Expected `name`"
                    (HS.getField @"name" _31)
             RTS.pEnter "PdfDecl._LookupNat"
               (PdfDecl._LookupNat (Vector.vecFromRep "Length") strFdict)
             RTS.pGuard "218:7--218:33" "guard failed"
               (stmFname HS.== Vector.vecFromRep "AESV2")
             RTS.pGuard "219:7--219:33" "guard failed"
               (strFname HS.== Vector.vecFromRep "AESV2")
             (__ :: HS.Integer) <- HS.pure stmFLen
             HS.pure __)
     RTS.pGuard "228:3--228:46" "guard failed"
       ((stmFLength HS.== (RTS.lit 16 :: HS.Integer))
          HS.|| (stmFLength HS.== (RTS.lit 32 :: HS.Integer)))
     HS.pure
       (EncryptionDict encFilter encSubFilter encV encR encO encU encP
          stmFLength)
 
_FreeEntry :: D.Parser ()
 
_FreeEntry =
  HS.const ()
    HS.<$> RTS.pMatch1 "46:3--46:12" (RTS.bcSingle (RTS.uint8 102))
 
_UsedEntry :: D.Parser ()
 
_UsedEntry =
  HS.const ()
    HS.<$> RTS.pMatch1 "42:3--42:12" (RTS.bcSingle (RTS.uint8 110))
 
_CrossRefEntry :: D.Parser ()
 
_CrossRefEntry =
  do RTS.pEnter "PdfDecl._NatN"
       (PdfDecl._NatN (RTS.lit 10 :: HS.Integer))
     HS.const () HS.<$> RTS.pMatch1 "32:19--32:27" PdfValue.cs_simpleWS
     RTS.pEnter "PdfDecl._NatN"
       (PdfDecl._NatN (RTS.lit 5 :: HS.Integer))
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
     (num :: HS.Integer) <-
       RTS.pEnter "PdfValue.Token"
         (PdfValue.pToken @HS.Integer
            (RTS.pEnter "PdfValue.Natural" PdfValue.pNatural))
     RTS.pSkipExact num
       (RTS.pEnter "PdfXRef._CrossRefEntry" _CrossRefEntry)
 
_CrossRefSection :: D.Parser ()
 
_CrossRefSection =
  do RTS.pEnter "PdfValue._KW"
       (PdfValue._KW (Vector.vecFromRep "xref"))
     RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: HS.Integer)
       (RTS.pEnter "PdfXRef._CrossRefSubSection" _CrossRefSubSection)
 
_TrailerDict ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_TrailerDict
  (dict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Size") dict)
     RTS.pEnter "PdfDecl._Default"
       (PdfDecl._Default @(HS.Maybe PdfValue.Ref)
          (do (x :: PdfValue.Value) <-
                RTS.pIsJust "149:22--149:39"
                  ("Missing key: "
                     HS.++ HS.show
                             (Vector.vecFromRep "Root" :: Vector.Vector (RTS.UInt 8)))
                  (Map.lookup (Vector.vecFromRep "Root") dict)
              RTS.pErrorMode RTS.Abort
                (RTS.pIsJust_ "151:23--151:30" "Expected `ref`"
                   (HS.getField @"ref" x))))
     (RTS.<||)
       (RTS.pEnter "PdfDecl._LookupNat"
          (PdfDecl._LookupNat (Vector.vecFromRep "Prev") dict))
       (HS.pure ())
     (RTS.<||)
       (do (d :: PdfValue.Value) <-
             RTS.pIsJust "155:9--155:29"
               ("Missing key: "
                  HS.++ HS.show
                          (Vector.vecFromRep "Encrypt" :: Vector.Vector (RTS.UInt 8)))
               (Map.lookup (Vector.vecFromRep "Encrypt") dict)
           RTS.pErrorMode RTS.Abort
             (do RTS.pIsJust_ "157:12--157:19" "Expected `ref`"
                   (HS.getField @"ref" d)
                 (i :: PdfValue.Value) <-
                   RTS.pIsJust "158:10--158:25"
                     ("Missing key: "
                        HS.++ HS.show
                                (Vector.vecFromRep "ID" :: Vector.Vector (RTS.UInt 8)))
                     (Map.lookup (Vector.vecFromRep "ID") dict)
                 RTS.pIsJust_ "159:13--159:22" "Expected `array`"
                   (HS.getField @"array" i)
                 do (_6 :: PdfValue.Value) <-
                      do (_5 :: Vector.Vector PdfValue.Value) <-
                           RTS.pIsJust "160:18--160:27" "Expected `array`"
                             (HS.getField @"array" i)
                         RTS.pIsJust "160:11--160:27" "Index out of bounds"
                           ((Vector.!?) _5 (RTS.lit 0 :: HS.Integer))
                    RTS.pIsJust_ "160:11--160:40" "Expected `string`"
                      (HS.getField @"string" _6)
                 (_8 :: PdfValue.Value) <-
                   do (_7 :: Vector.Vector PdfValue.Value) <-
                        RTS.pIsJust "161:18--161:27" "Expected `array`"
                          (HS.getField @"array" i)
                      RTS.pIsJust "161:11--161:27" "Index out of bounds"
                        ((Vector.!?) _7 (RTS.lit 1 :: HS.Integer))
                 RTS.pIsJust_ "161:11--161:40" "Expected `string`"
                   (HS.getField @"string" _8)))
       (HS.pure ())
 
_CrossRefAndTrailer :: D.Parser ()
 
_CrossRefAndTrailer =
  do RTS.pEnter "PdfXRef._CrossRefSection" _CrossRefSection
     RTS.pEnter "PdfValue._KW"
       (PdfValue._KW (Vector.vecFromRep "trailer"))
     (t :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pEnter "PdfValue.Dict" PdfValue.pDict
     RTS.pEnter "PdfXRef._TrailerDict" (_TrailerDict t)
 
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
       do (_14 :: PdfDecl.TopDeclDef) <-
            do (_13 :: PdfDecl.TopDecl) <-
                 RTS.pEnter "PdfDecl.TopDecl" PdfDecl.pTopDecl
               HS.pure (HS.getField @"obj" _13)
          RTS.pIsJust "57:11--57:31" "Expected `stream`"
            (HS.getField @"stream" _14)
     (_15 :: RTS.Input) <-
       RTS.pIsJust "58:15--58:28" "Expected `ok`"
         (HS.getField @"ok" (HS.getField @"body" str))
     RTS.pEnter "PdfDecl._WithStream"
       (PdfDecl._WithStream @XRefObjTable _15
          (do (_16 :: XRefMeta) <-
                RTS.pEnter "PdfXRef.XRefMeta"
                  (pXRefMeta (HS.getField @"header" str))
              RTS.pEnter "PdfXRef._XRefObjTable" (_XRefObjTable _16)))
 
_CrossRef :: D.Parser ()
 
_CrossRef =
  (RTS.|||)
    (RTS.pEnter "oldXref"
       (RTS.pEnter "PdfXRef._CrossRefAndTrailer" _CrossRefAndTrailer))
    (RTS.pEnter "newXref" (RTS.pEnter "PdfXRef._XRefObj" _XRefObj))
 
_EncryptionDict :: PdfValue.Ref -> D.Parser ()
 
_EncryptionDict (eref :: PdfValue.Ref) =
  do (edict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_19 :: PdfValue.Value) <-
            RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef eref)
          RTS.pIsJust "168:13--168:39" "Expected `dict`"
            (HS.getField @"dict" _19)
     (encFilter :: Vector.Vector (RTS.UInt 8)) <-
       do (_20 :: PdfValue.Value) <-
            RTS.pIsJust "170:16--170:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Filter" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Filter") edict)
          RTS.pIsJust "170:16--170:45" "Expected `name`"
            (HS.getField @"name" _20)
     RTS.pGuard "171:3--171:33" "guard failed"
       (encFilter HS.== Vector.vecFromRep "Standard")
     (RTS.<||)
       (do (_21 :: PdfValue.Value) <-
             RTS.pIsJust "173:29--173:52"
               ("Missing key: "
                  HS.++ HS.show
                          (Vector.vecFromRep "SubFilter" :: Vector.Vector (RTS.UInt 8)))
               (Map.lookup (Vector.vecFromRep "SubFilter") edict)
           RTS.pIsJust_ "173:29--173:61" "Expected `name`"
             (HS.getField @"name" _21))
       (HS.pure ())
     (encV :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "V") edict)
     (encR :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "R") edict)
     RTS.pGuard "179:3--179:47" "guard failed"
       ((encR HS.== (RTS.lit 3 :: HS.Integer))
          HS.|| ((encV HS.== (RTS.lit 4 :: HS.Integer))
                   HS.&& (encR HS.== (RTS.lit 4 :: HS.Integer))))
     do (_22 :: PdfValue.Value) <-
          RTS.pIsJust "181:11--181:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "O" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "O") edict)
        RTS.pIsJust_ "181:11--181:37" "Expected `string`"
          (HS.getField @"string" _22)
     do (_23 :: PdfValue.Value) <-
          RTS.pIsJust "182:11--182:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "U" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "U") edict)
        RTS.pIsJust_ "182:11--182:37" "Expected `string`"
          (HS.getField @"string" _23)
     do (_24 :: PdfValue.Value) <-
          RTS.pIsJust "185:11--185:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "P" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "P") edict)
        RTS.pIsJust_ "185:11--185:37" "Expected `number`"
          (HS.getField @"number" _24)
     (stmFLength :: HS.Integer) <-
       (RTS.<||)
         (do RTS.pGuard "193:7--193:23" "guard failed"
               (encV HS.== (RTS.lit 2 :: HS.Integer))
             RTS.pErrorMode RTS.Abort
               (do (len :: HS.Integer) <-
                     (RTS.<||)
                       (RTS.pEnter "PdfDecl.LookupNat"
                          (PdfDecl.pLookupNat (Vector.vecFromRep "Length") edict))
                       (HS.pure (RTS.lit 40 :: HS.Integer))
                   RTS.pGuard "197:7--197:24" "guard failed"
                     (len HS.== (RTS.lit 128 :: HS.Integer))
                   (__ :: HS.Integer) <- HS.pure (RTS.lit 16 :: HS.Integer)
                   HS.pure __))
         (do RTS.pGuard "201:7--201:23" "guard failed"
               (encV HS.== (RTS.lit 4 :: HS.Integer))
             (stmF :: Vector.Vector (RTS.UInt 8)) <-
               do (_25 :: PdfValue.Value) <-
                    RTS.pIsJust "203:16--203:34"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "StmF" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "StmF") edict)
                  RTS.pIsJust "203:16--203:43" "Expected `name`"
                    (HS.getField @"name" _25)
             (strF :: Vector.Vector (RTS.UInt 8)) <-
               do (_26 :: PdfValue.Value) <-
                    RTS.pIsJust "204:16--204:34"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "StrF" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "StrF") edict)
                  RTS.pIsJust "204:16--204:43" "Expected `name`"
                    (HS.getField @"name" _26)
             (cf :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
               do (_27 :: PdfValue.Value) <-
                    RTS.pIsJust "205:14--205:30"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "CF" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "CF") edict)
                  RTS.pIsJust "205:14--205:39" "Expected `dict`"
                    (HS.getField @"dict" _27)
             (stmFdict
                :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
               do (_28 :: PdfValue.Value) <-
                    RTS.pIsJust "208:20--208:33"
                      ("Missing key: "
                         HS.++ HS.show (stmF :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup stmF cf)
                  RTS.pIsJust "208:20--208:42" "Expected `dict`"
                    (HS.getField @"dict" _28)
             (stmFname :: Vector.Vector (RTS.UInt 8)) <-
               do (_29 :: PdfValue.Value) <-
                    RTS.pIsJust "209:20--209:40"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "CFM") stmFdict)
                  RTS.pIsJust "209:20--209:49" "Expected `name`"
                    (HS.getField @"name" _29)
             (stmFLen :: HS.Integer) <-
               RTS.pEnter "PdfDecl.LookupNat"
                 (PdfDecl.pLookupNat (Vector.vecFromRep "Length") stmFdict)
             (strFdict
                :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
               do (_30 :: PdfValue.Value) <-
                    RTS.pIsJust "213:20--213:33"
                      ("Missing key: "
                         HS.++ HS.show (strF :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup strF cf)
                  RTS.pIsJust "213:20--213:42" "Expected `dict`"
                    (HS.getField @"dict" _30)
             (strFname :: Vector.Vector (RTS.UInt 8)) <-
               do (_31 :: PdfValue.Value) <-
                    RTS.pIsJust "214:20--214:40"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "CFM") strFdict)
                  RTS.pIsJust "214:20--214:49" "Expected `name`"
                    (HS.getField @"name" _31)
             RTS.pEnter "PdfDecl._LookupNat"
               (PdfDecl._LookupNat (Vector.vecFromRep "Length") strFdict)
             RTS.pGuard "218:7--218:33" "guard failed"
               (stmFname HS.== Vector.vecFromRep "AESV2")
             RTS.pGuard "219:7--219:33" "guard failed"
               (strFname HS.== Vector.vecFromRep "AESV2")
             (__ :: HS.Integer) <- HS.pure stmFLen
             HS.pure __)
     RTS.pGuard "228:3--228:46" "guard failed"
       ((stmFLength HS.== (RTS.lit 16 :: HS.Integer))
          HS.|| (stmFLength HS.== (RTS.lit 32 :: HS.Integer)))
 
_LookupInt ::
      Vector.Vector PdfValue.Value -> (HS.Integer -> D.Parser ())
 
_LookupInt (arr :: Vector.Vector PdfValue.Value)
  (i :: HS.Integer) =
  RTS.pEnter "PdfDecl._Default"
    (PdfDecl._Default @HS.Integer
       (do (n :: PdfValue.Value) <-
             RTS.pIsJust "81:8--81:16" "Index out of bounds" ((Vector.!?) arr i)
           RTS.pErrorMode RTS.Abort
             (RTS.pEnter "PdfValue._NatValue" (PdfValue._NatValue n))))
 
_XRefFieldRequired :: HS.Integer -> D.Parser ()
 
_XRefFieldRequired (n :: HS.Integer) =
  do RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (n HS./= (RTS.lit 0 :: HS.Integer)))
     RTS.pEnter "PdfDecl._BEBytes" (PdfDecl._BEBytes n)
 
_XRefCompressed :: XRefFormat -> D.Parser ()
 
_XRefCompressed (w :: XRefFormat) =
  do RTS.pEnter "PdfXRef._XRefFieldRequired"
       (_XRefFieldRequired (HS.getField @"b2" w))
     RTS.pEnter "PdfXRef._XRefFieldRequired"
       (_XRefFieldRequired (HS.getField @"b3" w))
 
_XRefFieldWithDefault :: HS.Integer -> D.Parser ()
 
_XRefFieldWithDefault (n :: HS.Integer) =
  (RTS.<||)
    (RTS.pEnter "PdfValue._Guard"
       (PdfValue._Guard (n HS.== (RTS.lit 0 :: HS.Integer))))
    (RTS.pEnter "PdfDecl._BEBytes" (PdfDecl._BEBytes n))
 
_XRefFormat ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_XRefFormat
  (header :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (kv :: PdfValue.Value) <-
       RTS.pEnter "PdfDecl.LookupResolve"
         (PdfDecl.pLookupResolve (Vector.vecFromRep "W") header)
     (vs :: Vector.Vector PdfValue.Value) <-
       RTS.pIsJust "70:12--70:22" "Expected `array`"
         (HS.getField @"array" kv)
     RTS.pEnter "PdfXRef._LookupInt"
       (_LookupInt vs (RTS.lit 0 :: HS.Integer))
     RTS.pEnter "PdfXRef._LookupInt"
       (_LookupInt vs (RTS.lit 1 :: HS.Integer))
     RTS.pEnter "PdfXRef._LookupInt"
       (_LookupInt vs (RTS.lit 2 :: HS.Integer))
     RTS.loopFoldM
       (\(s :: ()) (x :: PdfValue.Value) ->
          RTS.pEnter "PdfValue._NatValue" (PdfValue._NatValue x))
       ()
       vs
 
_XRefFree :: XRefFormat -> D.Parser ()
 
_XRefFree (w :: XRefFormat) =
  do RTS.pEnter "PdfXRef._XRefFieldRequired"
       (_XRefFieldRequired (HS.getField @"b2" w))
     RTS.pEnter "PdfXRef._XRefFieldWithDefault"
       (_XRefFieldWithDefault (HS.getField @"b3" w))
 
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
          (\(i :: HS.Integer) ->
             do (firstId :: HS.Integer) <-
                  RTS.pIsJust "91:15--91:23" "Index out of bounds"
                    ((Vector.!?) arr i)
                (num :: HS.Integer) <-
                  RTS.pIsJust "92:15--92:23" "Index out of bounds"
                    ((Vector.!?) arr (RTS.add i (RTS.lit 1 :: HS.Integer)))
                HS.pure (XRefIndex_0 firstId num))
          (Vector.rangeUp (RTS.lit 0 :: HS.Integer)
             (HS.toInteger (Vector.length arr))
             (RTS.lit 2 :: HS.Integer))
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
 
_XRefOffset :: XRefFormat -> D.Parser ()
 
_XRefOffset (w :: XRefFormat) =
  do RTS.pEnter "PdfXRef._XRefFieldRequired"
       (_XRefFieldRequired (HS.getField @"b2" w))
     RTS.pEnter "PdfXRef._XRefFieldWithDefault"
       (_XRefFieldWithDefault (HS.getField @"b3" w))
 
_XRefObjEntry :: XRefFormat -> D.Parser ()
 
_XRefObjEntry (w :: XRefFormat) =
  RTS.pEnter "PdfDecl._Chunk"
    (PdfDecl._Chunk @XRefObjEntry (HS.getField @"witdth" w)
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