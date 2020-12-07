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
module PdfCrypto where
 
import qualified PdfMonad as D
import qualified PdfXRef
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
 
 
data ChooseCiph
  = ChooseCiph_v2RC4 ()
  | ChooseCiph_v4AES ()
  | ChooseCiph_v4RC4 ()
  
 
deriving instance HS.Eq ChooseCiph
 
deriving instance HS.Ord ChooseCiph
 
deriving instance HS.Show ChooseCiph
 
instance RTS.DDL ChooseCiph where
 
instance HS.HasField "v2RC4" ChooseCiph (HS.Maybe ()) where
  getField (ChooseCiph_v2RC4 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "v4AES" ChooseCiph (HS.Maybe ()) where
  getField (ChooseCiph_v4AES x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "v4RC4" ChooseCiph (HS.Maybe ()) where
  getField (ChooseCiph_v4RC4 x) = HS.Just x
   
  getField _ = HS.Nothing
 
data EncryptionDict
  = EncryptionDict (Vector.Vector (RTS.UInt 8))
      (Vector.Vector (RTS.UInt 8))
      (HS.Maybe (Vector.Vector (RTS.UInt 8)))
      HS.Integer
      HS.Integer
      (Vector.Vector (RTS.UInt 8))
      (Vector.Vector (RTS.UInt 8))
      HS.Integer
      ChooseCiph
  
 
deriving instance HS.Eq EncryptionDict
 
deriving instance HS.Ord EncryptionDict
 
deriving instance HS.Show EncryptionDict
 
instance RTS.DDL EncryptionDict where
 
instance HS.HasField "id0" EncryptionDict
           (Vector.Vector (RTS.UInt 8)) where
  getField (EncryptionDict x _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "encFilter" EncryptionDict
           (Vector.Vector (RTS.UInt 8)) where
  getField (EncryptionDict _ x _ _ _ _ _ _ _) = x
 
instance HS.HasField "encSubFilter" EncryptionDict
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (EncryptionDict _ _ x _ _ _ _ _ _) = x
 
instance HS.HasField "encV" EncryptionDict HS.Integer where
  getField (EncryptionDict _ _ _ x _ _ _ _ _) = x
 
instance HS.HasField "encR" EncryptionDict HS.Integer where
  getField (EncryptionDict _ _ _ _ x _ _ _ _) = x
 
instance HS.HasField "encO" EncryptionDict
           (Vector.Vector (RTS.UInt 8)) where
  getField (EncryptionDict _ _ _ _ _ x _ _ _) = x
 
instance HS.HasField "encU" EncryptionDict
           (Vector.Vector (RTS.UInt 8)) where
  getField (EncryptionDict _ _ _ _ _ _ x _ _) = x
 
instance HS.HasField "encP" EncryptionDict HS.Integer where
  getField (EncryptionDict _ _ _ _ _ _ _ x _) = x
 
instance HS.HasField "ciph" EncryptionDict ChooseCiph where
  getField (EncryptionDict _ _ _ _ _ _ _ _ x) = x
 
data MakeContext
  = MakeContext_encryption EncryptionDict
  | MakeContext_noencryption ()
  
 
deriving instance HS.Eq MakeContext
 
deriving instance HS.Ord MakeContext
 
deriving instance HS.Show MakeContext
 
instance RTS.DDL MakeContext where
 
instance HS.HasField "encryption" MakeContext
           (HS.Maybe EncryptionDict) where
  getField (MakeContext_encryption x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "noencryption" MakeContext (HS.Maybe ()) where
  getField (MakeContext_noencryption x) = HS.Just x
   
  getField _ = HS.Nothing
 
pV4stmFname ::
  forall b c o.
    (RTS.DDL b, RTS.DDL c, RTS.DDL o, RTS.HasUnion c "name" b,
     RTS.HasUnion c "dict" (Map.Map b o),
     RTS.HasUnion o "dict"
       (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) =>
      Map.Map (Vector.Vector (RTS.UInt 8)) c
        -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pV4stmFname (edict :: Map.Map (Vector.Vector (RTS.UInt 8)) c) =
  do (stmF :: b) <-
       do (_0 :: c) <-
            RTS.pIsJust "52:12--52:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StmF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StmF") edict)
          RTS.pIsJust "52:12--52:39" "Expected `name`"
            (HS.getField @"name" _0)
     (strF :: b) <-
       do (_1 :: c) <-
            RTS.pIsJust "53:12--53:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StrF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StrF") edict)
          RTS.pIsJust "53:12--53:39" "Expected `name`"
            (HS.getField @"name" _1)
     (cf :: Map.Map b o) <-
       do (_2 :: c) <-
            RTS.pIsJust "54:10--54:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "CF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "CF") edict)
          RTS.pIsJust "54:10--54:35" "Expected `dict`"
            (HS.getField @"dict" _2)
     (stmFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_3 :: o) <-
            RTS.pIsJust "57:16--57:29"
              ("Missing key: " HS.++ HS.show (stmF :: b))
              (Map.lookup stmF cf)
          RTS.pIsJust "57:16--57:38" "Expected `dict`"
            (HS.getField @"dict" _3)
     (stmFname :: Vector.Vector (RTS.UInt 8)) <-
       do (_4 :: PdfValue.Value) <-
            RTS.pIsJust "58:16--58:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "CFM") stmFdict)
          RTS.pIsJust "58:16--58:45" "Expected `name`"
            (HS.getField @"name" _4)
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") stmFdict)
     (strFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_5 :: o) <-
            RTS.pIsJust "62:16--62:29"
              ("Missing key: " HS.++ HS.show (strF :: b))
              (Map.lookup strF cf)
          RTS.pIsJust "62:16--62:38" "Expected `dict`"
            (HS.getField @"dict" _5)
     do (_6 :: PdfValue.Value) <-
          RTS.pIsJust "63:16--63:36"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "CFM") strFdict)
        RTS.pIsJust_ "63:16--63:45" "Expected `name`"
          (HS.getField @"name" _6)
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") strFdict)
     (__ :: Vector.Vector (RTS.UInt 8)) <- HS.pure stmFname
     HS.pure __
 
pChooseCiph ::
  forall u.
    (RTS.DDL u, RTS.Literal 2 u, RTS.Literal 4 u) =>
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> (u -> D.Parser ChooseCiph)
 
pChooseCiph
  (edict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  (v :: u) =
  (RTS.<||)
    (RTS.pEnter "v2RC4"
       (do (_7 :: ()) <-
             do RTS.pGuard "35:5--35:18" "guard failed"
                  (v HS.== (RTS.lit 2 :: u))
                (len :: HS.Integer) <-
                  RTS.pEnter "PdfDecl.LookupNat"
                    (PdfDecl.pLookupNat (Vector.vecFromRep "Length") edict)
                (__ :: ()) <-
                  RTS.pGuard "37:5--37:22" "guard failed"
                    (len HS.== (RTS.lit 128 :: HS.Integer))
                HS.pure __
           HS.pure (ChooseCiph_v2RC4 _7)))
    ((RTS.<||)
       (RTS.pEnter "v4RC4"
          (do (_8 :: ()) <-
                do RTS.pGuard "40:5--40:18" "guard failed"
                     (v HS.== (RTS.lit 4 :: u))
                   (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                     RTS.pEnter "PdfCrypto.V4stmFname"
                       (pV4stmFname @(Vector.Vector (RTS.UInt 8)) @PdfValue.Value
                          @PdfValue.Value
                          edict)
                   (__ :: ()) <-
                     RTS.pGuard "42:5--42:28" "guard failed"
                       (stmFname HS.== Vector.vecFromRep "V2")
                   HS.pure __
              HS.pure (ChooseCiph_v4RC4 _8)))
       (RTS.pEnter "v4AES"
          (do (_9 :: ()) <-
                do RTS.pGuard "45:5--45:18" "guard failed"
                     (v HS.== (RTS.lit 4 :: u))
                   (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                     RTS.pEnter "PdfCrypto.V4stmFname"
                       (pV4stmFname @(Vector.Vector (RTS.UInt 8)) @PdfValue.Value
                          @PdfValue.Value
                          edict)
                   (__ :: ()) <-
                     RTS.pGuard "47:5--47:31" "guard failed"
                       (stmFname HS.== Vector.vecFromRep "AESV2")
                   HS.pure __
              HS.pure (ChooseCiph_v4AES _9))))
 
pEncryptionDict ::
      PdfXRef.TrailerDictEncrypt -> D.Parser EncryptionDict
 
pEncryptionDict (enc :: PdfXRef.TrailerDictEncrypt) =
  do (edict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_10 :: PdfValue.Value) <-
            RTS.pEnter "PdfDecl.ResolveValRef"
              (PdfDecl.pResolveValRef (HS.getField @"eref" enc))
          RTS.pIsJust "7:13--7:43" "Expected `dict`"
            (HS.getField @"dict" _10)
     (id0 :: Vector.Vector (RTS.UInt 8)) <-
       HS.pure (HS.getField @"id0" enc)
     (encFilter :: Vector.Vector (RTS.UInt 8)) <-
       do (_11 :: PdfValue.Value) <-
            RTS.pIsJust "11:16--11:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Filter" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Filter") edict)
          RTS.pIsJust "11:16--11:45" "Expected `name`"
            (HS.getField @"name" _11)
     RTS.pGuard "12:3--12:33" "guard failed"
       (encFilter HS.== Vector.vecFromRep "Standard")
     (encSubFilter :: HS.Maybe (Vector.Vector (RTS.UInt 8))) <-
       RTS.pOptional (RTS.<||) HS.Just
         (do (_12 :: PdfValue.Value) <-
               RTS.pIsJust "14:29--14:52"
                 ("Missing key: "
                    HS.++ HS.show
                            (Vector.vecFromRep "SubFilter" :: Vector.Vector (RTS.UInt 8)))
                 (Map.lookup (Vector.vecFromRep "SubFilter") edict)
             RTS.pIsJust "14:29--14:61" "Expected `name`"
               (HS.getField @"name" _12))
     (encV :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "V") edict)
     (encR :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "R") edict)
     RTS.pGuard "20:3--20:47" "guard failed"
       ((encR HS.== (RTS.lit 3 :: HS.Integer))
          HS.|| ((encV HS.== (RTS.lit 4 :: HS.Integer))
                   HS.&& (encR HS.== (RTS.lit 4 :: HS.Integer))))
     (encO :: Vector.Vector (RTS.UInt 8)) <-
       do (_13 :: PdfValue.Value) <-
            RTS.pIsJust "22:11--22:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "O" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "O") edict)
          RTS.pIsJust "22:11--22:37" "Expected `string`"
            (HS.getField @"string" _13)
     (encU :: Vector.Vector (RTS.UInt 8)) <-
       do (_14 :: PdfValue.Value) <-
            RTS.pIsJust "23:11--23:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "U" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "U") edict)
          RTS.pIsJust "23:11--23:37" "Expected `string`"
            (HS.getField @"string" _14)
     (encP :: HS.Integer) <-
       do (v :: PdfValue.Number) <-
            do (_15 :: PdfValue.Value) <-
                 RTS.pIsJust "26:11--26:26"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "P" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "P") edict)
               RTS.pIsJust "26:11--26:37" "Expected `number`"
                 (HS.getField @"number" _15)
          (__ :: HS.Integer) <- HS.pure (HS.getField @"num" v)
          HS.pure __
     (ciph :: ChooseCiph) <-
       RTS.pEnter "PdfCrypto.ChooseCiph"
         (pChooseCiph @HS.Integer edict encV)
     HS.pure
       (EncryptionDict id0 encFilter encSubFilter encV encR encO encU encP
          ciph)
 
pMakeContext :: PdfXRef.TrailerDict -> D.Parser MakeContext
 
pMakeContext (t :: PdfXRef.TrailerDict) =
  (RTS.<||)
    (RTS.pEnter "encryption"
       (do (_16 :: EncryptionDict) <-
             do (enc :: PdfXRef.TrailerDictEncrypt) <-
                  RTS.pIsJust "71:12--71:28" "Expected `Just`"
                    (HS.getField @"encrypt" t)
                RTS.pErrorMode RTS.Abort
                  (do (__ :: EncryptionDict) <-
                        RTS.pEnter "PdfCrypto.EncryptionDict" (pEncryptionDict enc)
                      HS.pure __)
           HS.pure (MakeContext_encryption _16)))
    (RTS.pEnter "noencryption"
       (do (_17 :: ()) <- HS.pure ()
           HS.pure (MakeContext_noencryption _17)))
 
_ChooseCiph ::
  forall u.
    (RTS.DDL u, RTS.Literal 2 u, RTS.Literal 4 u) =>
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> (u -> D.Parser ())
 
_ChooseCiph
  (edict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  (v :: u) =
  (RTS.<||)
    (RTS.pEnter "v2RC4"
       (do RTS.pGuard "35:5--35:18" "guard failed"
             (v HS.== (RTS.lit 2 :: u))
           (len :: HS.Integer) <-
             RTS.pEnter "PdfDecl.LookupNat"
               (PdfDecl.pLookupNat (Vector.vecFromRep "Length") edict)
           RTS.pGuard "37:5--37:22" "guard failed"
             (len HS.== (RTS.lit 128 :: HS.Integer))))
    ((RTS.<||)
       (RTS.pEnter "v4RC4"
          (do RTS.pGuard "40:5--40:18" "guard failed"
                (v HS.== (RTS.lit 4 :: u))
              (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                RTS.pEnter "PdfCrypto.V4stmFname"
                  (pV4stmFname @(Vector.Vector (RTS.UInt 8)) @PdfValue.Value
                     @PdfValue.Value
                     edict)
              RTS.pGuard "42:5--42:28" "guard failed"
                (stmFname HS.== Vector.vecFromRep "V2")))
       (RTS.pEnter "v4AES"
          (do RTS.pGuard "45:5--45:18" "guard failed"
                (v HS.== (RTS.lit 4 :: u))
              (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                RTS.pEnter "PdfCrypto.V4stmFname"
                  (pV4stmFname @(Vector.Vector (RTS.UInt 8)) @PdfValue.Value
                     @PdfValue.Value
                     edict)
              RTS.pGuard "47:5--47:31" "guard failed"
                (stmFname HS.== Vector.vecFromRep "AESV2"))))
 
_EncryptionDict :: PdfXRef.TrailerDictEncrypt -> D.Parser ()
 
_EncryptionDict (enc :: PdfXRef.TrailerDictEncrypt) =
  do (edict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_10 :: PdfValue.Value) <-
            RTS.pEnter "PdfDecl.ResolveValRef"
              (PdfDecl.pResolveValRef (HS.getField @"eref" enc))
          RTS.pIsJust "7:13--7:43" "Expected `dict`"
            (HS.getField @"dict" _10)
     (encFilter :: Vector.Vector (RTS.UInt 8)) <-
       do (_11 :: PdfValue.Value) <-
            RTS.pIsJust "11:16--11:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Filter" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Filter") edict)
          RTS.pIsJust "11:16--11:45" "Expected `name`"
            (HS.getField @"name" _11)
     RTS.pGuard "12:3--12:33" "guard failed"
       (encFilter HS.== Vector.vecFromRep "Standard")
     (RTS.<||)
       (do (_12 :: PdfValue.Value) <-
             RTS.pIsJust "14:29--14:52"
               ("Missing key: "
                  HS.++ HS.show
                          (Vector.vecFromRep "SubFilter" :: Vector.Vector (RTS.UInt 8)))
               (Map.lookup (Vector.vecFromRep "SubFilter") edict)
           RTS.pIsJust_ "14:29--14:61" "Expected `name`"
             (HS.getField @"name" _12))
       (HS.pure ())
     (encV :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "V") edict)
     (encR :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "R") edict)
     RTS.pGuard "20:3--20:47" "guard failed"
       ((encR HS.== (RTS.lit 3 :: HS.Integer))
          HS.|| ((encV HS.== (RTS.lit 4 :: HS.Integer))
                   HS.&& (encR HS.== (RTS.lit 4 :: HS.Integer))))
     do (_13 :: PdfValue.Value) <-
          RTS.pIsJust "22:11--22:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "O" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "O") edict)
        RTS.pIsJust_ "22:11--22:37" "Expected `string`"
          (HS.getField @"string" _13)
     do (_14 :: PdfValue.Value) <-
          RTS.pIsJust "23:11--23:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "U" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "U") edict)
        RTS.pIsJust_ "23:11--23:37" "Expected `string`"
          (HS.getField @"string" _14)
     do (_15 :: PdfValue.Value) <-
          RTS.pIsJust "26:11--26:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "P" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "P") edict)
        RTS.pIsJust_ "26:11--26:37" "Expected `number`"
          (HS.getField @"number" _15)
     RTS.pEnter "PdfCrypto._ChooseCiph"
       (_ChooseCiph @HS.Integer edict encV)
 
_MakeContext :: PdfXRef.TrailerDict -> D.Parser ()
 
_MakeContext (t :: PdfXRef.TrailerDict) =
  (RTS.<||)
    (RTS.pEnter "encryption"
       (do (enc :: PdfXRef.TrailerDictEncrypt) <-
             RTS.pIsJust "71:12--71:28" "Expected `Just`"
               (HS.getField @"encrypt" t)
           RTS.pErrorMode RTS.Abort
             (RTS.pEnter "PdfCrypto._EncryptionDict" (_EncryptionDict enc))))
    (RTS.pEnter "noencryption" (HS.pure ()))
 
_V4stmFname ::
  forall b c o.
    (RTS.DDL b, RTS.DDL c, RTS.DDL o, RTS.HasUnion c "name" b,
     RTS.HasUnion c "dict" (Map.Map b o),
     RTS.HasUnion o "dict"
       (Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)) =>
      Map.Map (Vector.Vector (RTS.UInt 8)) c -> D.Parser ()
 
_V4stmFname (edict :: Map.Map (Vector.Vector (RTS.UInt 8)) c) =
  do (stmF :: b) <-
       do (_0 :: c) <-
            RTS.pIsJust "52:12--52:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StmF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StmF") edict)
          RTS.pIsJust "52:12--52:39" "Expected `name`"
            (HS.getField @"name" _0)
     (strF :: b) <-
       do (_1 :: c) <-
            RTS.pIsJust "53:12--53:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StrF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StrF") edict)
          RTS.pIsJust "53:12--53:39" "Expected `name`"
            (HS.getField @"name" _1)
     (cf :: Map.Map b o) <-
       do (_2 :: c) <-
            RTS.pIsJust "54:10--54:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "CF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "CF") edict)
          RTS.pIsJust "54:10--54:35" "Expected `dict`"
            (HS.getField @"dict" _2)
     (stmFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_3 :: o) <-
            RTS.pIsJust "57:16--57:29"
              ("Missing key: " HS.++ HS.show (stmF :: b))
              (Map.lookup stmF cf)
          RTS.pIsJust "57:16--57:38" "Expected `dict`"
            (HS.getField @"dict" _3)
     do (_4 :: PdfValue.Value) <-
          RTS.pIsJust "58:16--58:36"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "CFM") stmFdict)
        RTS.pIsJust_ "58:16--58:45" "Expected `name`"
          (HS.getField @"name" _4)
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") stmFdict)
     (strFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_5 :: o) <-
            RTS.pIsJust "62:16--62:29"
              ("Missing key: " HS.++ HS.show (strF :: b))
              (Map.lookup strF cf)
          RTS.pIsJust "62:16--62:38" "Expected `dict`"
            (HS.getField @"dict" _5)
     do (_6 :: PdfValue.Value) <-
          RTS.pIsJust "63:16--63:36"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "CFM") strFdict)
        RTS.pIsJust_ "63:16--63:45" "Expected `name`"
          (HS.getField @"name" _6)
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") strFdict)