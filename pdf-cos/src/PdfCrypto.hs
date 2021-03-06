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
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pV4stmFname
  (edict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (stmF :: Vector.Vector (RTS.UInt 8)) <-
       do (_932 :: PdfValue.Value) <-
            RTS.pIsJust "52:12--52:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StmF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StmF") edict)
          case _932 of
            PdfValue.Value_name (_933 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                          _933
            _ -> RTS.pError RTS.FromSystem "52:12--52:39"
                   "Pattern match failure"
     (strF :: Vector.Vector (RTS.UInt 8)) <-
       do (_934 :: PdfValue.Value) <-
            RTS.pIsJust "53:12--53:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StrF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StrF") edict)
          case _934 of
            PdfValue.Value_name (_935 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                          _935
            _ -> RTS.pError RTS.FromSystem "53:12--53:39"
                   "Pattern match failure"
     (cf :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_936 :: PdfValue.Value) <-
            RTS.pIsJust "54:10--54:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "CF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "CF") edict)
          case _936 of
            PdfValue.Value_dict
              (_937
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _937
            _ -> RTS.pError RTS.FromSystem "54:10--54:35"
                   "Pattern match failure"
     (stmFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_938 :: PdfValue.Value) <-
            RTS.pIsJust "57:16--57:29"
              ("Missing key: "
                 HS.++ HS.show (stmF :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup stmF cf)
          case _938 of
            PdfValue.Value_dict
              (_939
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _939
            _ -> RTS.pError RTS.FromSystem "57:16--57:38"
                   "Pattern match failure"
     (stmFname :: Vector.Vector (RTS.UInt 8)) <-
       do (_940 :: PdfValue.Value) <-
            RTS.pIsJust "58:16--58:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "CFM") stmFdict)
          case _940 of
            PdfValue.Value_name (_941 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                          _941
            _ -> RTS.pError RTS.FromSystem "58:16--58:45"
                   "Pattern match failure"
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") stmFdict)
     (strFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_942 :: PdfValue.Value) <-
            RTS.pIsJust "62:16--62:29"
              ("Missing key: "
                 HS.++ HS.show (strF :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup strF cf)
          case _942 of
            PdfValue.Value_dict
              (_943
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _943
            _ -> RTS.pError RTS.FromSystem "62:16--62:38"
                   "Pattern match failure"
     do (_944 :: PdfValue.Value) <-
          RTS.pIsJust "63:16--63:36"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "CFM") strFdict)
        case _944 of
          PdfValue.Value_name (_945 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                        ()
          _ -> RTS.pError RTS.FromSystem "63:16--63:45"
                 "Pattern match failure"
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") strFdict)
     (__ :: Vector.Vector (RTS.UInt 8)) <- HS.pure stmFname
     HS.pure __
 
pChooseCiph ::
  forall r.
    (RTS.DDL r, RTS.Literal 2 r, RTS.Literal 4 r) =>
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> (r -> D.Parser ChooseCiph)
 
pChooseCiph
  (edict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  (v :: r) =
  (RTS.<||)
    (RTS.pEnter "v2RC4"
       (do (_948 :: ()) <-
             do RTS.pGuard "35:5--35:18" "guard failed"
                  (v HS.== (RTS.lit 2 :: r))
                (len :: HS.Integer) <-
                  RTS.pEnter "PdfDecl.LookupNat"
                    (PdfDecl.pLookupNat (Vector.vecFromRep "Length") edict)
                (__ :: ()) <-
                  RTS.pGuard "37:5--37:22" "guard failed"
                    (len HS.== (RTS.lit 128 :: HS.Integer))
                HS.pure __
           HS.pure (ChooseCiph_v2RC4 _948)))
    ((RTS.<||)
       (RTS.pEnter "v4RC4"
          (do (_950 :: ()) <-
                do RTS.pGuard "40:5--40:18" "guard failed"
                     (v HS.== (RTS.lit 4 :: r))
                   (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                     RTS.pEnter "PdfCrypto.V4stmFname" (pV4stmFname edict)
                   (__ :: ()) <-
                     RTS.pGuard "42:5--42:28" "guard failed"
                       (stmFname HS.== Vector.vecFromRep "V2")
                   HS.pure __
              HS.pure (ChooseCiph_v4RC4 _950)))
       (RTS.pEnter "v4AES"
          (do (_952 :: ()) <-
                do RTS.pGuard "45:5--45:18" "guard failed"
                     (v HS.== (RTS.lit 4 :: r))
                   (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                     RTS.pEnter "PdfCrypto.V4stmFname" (pV4stmFname edict)
                   (__ :: ()) <-
                     RTS.pGuard "47:5--47:31" "guard failed"
                       (stmFname HS.== Vector.vecFromRep "AESV2")
                   HS.pure __
              HS.pure (ChooseCiph_v4AES _952))))
 
pEncryptionDict ::
      PdfXRef.TrailerDictEncrypt -> D.Parser EncryptionDict
 
pEncryptionDict (enc :: PdfXRef.TrailerDictEncrypt) =
  do (edict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_953 :: PdfValue.Value) <-
            RTS.pEnter "PdfDecl.ResolveValRef"
              (PdfDecl.pResolveValRef (HS.getField @"eref" enc))
          case _953 of
            PdfValue.Value_dict
              (_954
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _954
            _ -> RTS.pError RTS.FromSystem "7:13--7:43" "Pattern match failure"
     (id0 :: Vector.Vector (RTS.UInt 8)) <-
       HS.pure (HS.getField @"id0" enc)
     (encFilter :: Vector.Vector (RTS.UInt 8)) <-
       do (_955 :: PdfValue.Value) <-
            RTS.pIsJust "11:16--11:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Filter" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Filter") edict)
          case _955 of
            PdfValue.Value_name (_956 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                          _956
            _ -> RTS.pError RTS.FromSystem "11:16--11:45"
                   "Pattern match failure"
     RTS.pGuard "12:3--12:33" "guard failed"
       (encFilter HS.== Vector.vecFromRep "Standard")
     (encSubFilter :: HS.Maybe (Vector.Vector (RTS.UInt 8))) <-
       RTS.pOptional (RTS.<||) HS.Just
         (do (_957 :: PdfValue.Value) <-
               RTS.pIsJust "14:29--14:52"
                 ("Missing key: "
                    HS.++ HS.show
                            (Vector.vecFromRep "SubFilter" :: Vector.Vector (RTS.UInt 8)))
                 (Map.lookup (Vector.vecFromRep "SubFilter") edict)
             case _957 of
               PdfValue.Value_name (_958 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                             _958
               _ -> RTS.pError RTS.FromSystem "14:29--14:61"
                      "Pattern match failure")
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
       do (_959 :: PdfValue.Value) <-
            RTS.pIsJust "22:11--22:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "O" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "O") edict)
          case _959 of
            PdfValue.Value_string
              (_960 :: Vector.Vector (RTS.UInt 8)) -> HS.pure _960
            _ -> RTS.pError RTS.FromSystem "22:11--22:37"
                   "Pattern match failure"
     (encU :: Vector.Vector (RTS.UInt 8)) <-
       do (_961 :: PdfValue.Value) <-
            RTS.pIsJust "23:11--23:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "U" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "U") edict)
          case _961 of
            PdfValue.Value_string
              (_962 :: Vector.Vector (RTS.UInt 8)) -> HS.pure _962
            _ -> RTS.pError RTS.FromSystem "23:11--23:37"
                   "Pattern match failure"
     (encP :: HS.Integer) <-
       do (v :: PdfValue.Number) <-
            do (_963 :: PdfValue.Value) <-
                 RTS.pIsJust "26:11--26:26"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "P" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "P") edict)
               case _963 of
                 PdfValue.Value_number (_964 :: PdfValue.Number) -> HS.pure _964
                 _ -> RTS.pError RTS.FromSystem "26:11--26:37"
                        "Pattern match failure"
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
       (do (_968 :: EncryptionDict) <-
             do (enc :: PdfXRef.TrailerDictEncrypt) <-
                  case HS.getField @"encrypt" t of
                    HS.Just (_966 :: PdfXRef.TrailerDictEncrypt) -> HS.pure _966
                    _ -> RTS.pError RTS.FromSystem "71:12--71:28"
                           "Pattern match failure"
                RTS.pErrorMode RTS.Abort
                  (do (__ :: EncryptionDict) <-
                        RTS.pEnter "PdfCrypto.EncryptionDict" (pEncryptionDict enc)
                      HS.pure __)
           HS.pure (MakeContext_encryption _968)))
    (RTS.pEnter "noencryption"
       (do (_969 :: ()) <- HS.pure ()
           HS.pure (MakeContext_noencryption _969)))
 
_V4stmFname ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value -> D.Parser ()
 
_V4stmFname
  (edict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) =
  do (stmF :: Vector.Vector (RTS.UInt 8)) <-
       do (_932 :: PdfValue.Value) <-
            RTS.pIsJust "52:12--52:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StmF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StmF") edict)
          case _932 of
            PdfValue.Value_name (_933 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                          _933
            _ -> RTS.pError RTS.FromSystem "52:12--52:39"
                   "Pattern match failure"
     (strF :: Vector.Vector (RTS.UInt 8)) <-
       do (_934 :: PdfValue.Value) <-
            RTS.pIsJust "53:12--53:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StrF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StrF") edict)
          case _934 of
            PdfValue.Value_name (_935 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                          _935
            _ -> RTS.pError RTS.FromSystem "53:12--53:39"
                   "Pattern match failure"
     (cf :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_936 :: PdfValue.Value) <-
            RTS.pIsJust "54:10--54:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "CF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "CF") edict)
          case _936 of
            PdfValue.Value_dict
              (_937
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _937
            _ -> RTS.pError RTS.FromSystem "54:10--54:35"
                   "Pattern match failure"
     (stmFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_938 :: PdfValue.Value) <-
            RTS.pIsJust "57:16--57:29"
              ("Missing key: "
                 HS.++ HS.show (stmF :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup stmF cf)
          case _938 of
            PdfValue.Value_dict
              (_939
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _939
            _ -> RTS.pError RTS.FromSystem "57:16--57:38"
                   "Pattern match failure"
     do (_940 :: PdfValue.Value) <-
          RTS.pIsJust "58:16--58:36"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "CFM") stmFdict)
        case _940 of
          PdfValue.Value_name (_941 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                        ()
          _ -> RTS.pError RTS.FromSystem "58:16--58:45"
                 "Pattern match failure"
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") stmFdict)
     (strFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_942 :: PdfValue.Value) <-
            RTS.pIsJust "62:16--62:29"
              ("Missing key: "
                 HS.++ HS.show (strF :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup strF cf)
          case _942 of
            PdfValue.Value_dict
              (_943
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _943
            _ -> RTS.pError RTS.FromSystem "62:16--62:38"
                   "Pattern match failure"
     do (_944 :: PdfValue.Value) <-
          RTS.pIsJust "63:16--63:36"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "CFM") strFdict)
        case _944 of
          PdfValue.Value_name (_945 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                        ()
          _ -> RTS.pError RTS.FromSystem "63:16--63:45"
                 "Pattern match failure"
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") strFdict)
 
_ChooseCiph ::
  forall r.
    (RTS.DDL r, RTS.Literal 2 r, RTS.Literal 4 r) =>
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> (r -> D.Parser ())
 
_ChooseCiph
  (edict :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  (v :: r) =
  (RTS.<||)
    (RTS.pEnter "v2RC4"
       (do RTS.pGuard "35:5--35:18" "guard failed"
             (v HS.== (RTS.lit 2 :: r))
           (len :: HS.Integer) <-
             RTS.pEnter "PdfDecl.LookupNat"
               (PdfDecl.pLookupNat (Vector.vecFromRep "Length") edict)
           RTS.pGuard "37:5--37:22" "guard failed"
             (len HS.== (RTS.lit 128 :: HS.Integer))))
    ((RTS.<||)
       (RTS.pEnter "v4RC4"
          (do RTS.pGuard "40:5--40:18" "guard failed"
                (v HS.== (RTS.lit 4 :: r))
              (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                RTS.pEnter "PdfCrypto.V4stmFname" (pV4stmFname edict)
              RTS.pGuard "42:5--42:28" "guard failed"
                (stmFname HS.== Vector.vecFromRep "V2")))
       (RTS.pEnter "v4AES"
          (do RTS.pGuard "45:5--45:18" "guard failed"
                (v HS.== (RTS.lit 4 :: r))
              (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                RTS.pEnter "PdfCrypto.V4stmFname" (pV4stmFname edict)
              RTS.pGuard "47:5--47:31" "guard failed"
                (stmFname HS.== Vector.vecFromRep "AESV2"))))
 
_EncryptionDict :: PdfXRef.TrailerDictEncrypt -> D.Parser ()
 
_EncryptionDict (enc :: PdfXRef.TrailerDictEncrypt) =
  do (edict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_953 :: PdfValue.Value) <-
            RTS.pEnter "PdfDecl.ResolveValRef"
              (PdfDecl.pResolveValRef (HS.getField @"eref" enc))
          case _953 of
            PdfValue.Value_dict
              (_954
                 :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                              _954
            _ -> RTS.pError RTS.FromSystem "7:13--7:43" "Pattern match failure"
     (encFilter :: Vector.Vector (RTS.UInt 8)) <-
       do (_955 :: PdfValue.Value) <-
            RTS.pIsJust "11:16--11:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Filter" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Filter") edict)
          case _955 of
            PdfValue.Value_name (_956 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                          _956
            _ -> RTS.pError RTS.FromSystem "11:16--11:45"
                   "Pattern match failure"
     RTS.pGuard "12:3--12:33" "guard failed"
       (encFilter HS.== Vector.vecFromRep "Standard")
     (RTS.<||)
       (do (_957 :: PdfValue.Value) <-
             RTS.pIsJust "14:29--14:52"
               ("Missing key: "
                  HS.++ HS.show
                          (Vector.vecFromRep "SubFilter" :: Vector.Vector (RTS.UInt 8)))
               (Map.lookup (Vector.vecFromRep "SubFilter") edict)
           case _957 of
             PdfValue.Value_name (_958 :: Vector.Vector (RTS.UInt 8)) -> HS.pure
                                                                           ()
             _ -> RTS.pError RTS.FromSystem "14:29--14:61"
                    "Pattern match failure")
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
     do (_959 :: PdfValue.Value) <-
          RTS.pIsJust "22:11--22:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "O" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "O") edict)
        case _959 of
          PdfValue.Value_string
            (_960 :: Vector.Vector (RTS.UInt 8)) -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "22:11--22:37"
                 "Pattern match failure"
     do (_961 :: PdfValue.Value) <-
          RTS.pIsJust "23:11--23:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "U" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "U") edict)
        case _961 of
          PdfValue.Value_string
            (_962 :: Vector.Vector (RTS.UInt 8)) -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "23:11--23:37"
                 "Pattern match failure"
     do (_963 :: PdfValue.Value) <-
          RTS.pIsJust "26:11--26:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "P" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "P") edict)
        case _963 of
          PdfValue.Value_number (_964 :: PdfValue.Number) -> HS.pure ()
          _ -> RTS.pError RTS.FromSystem "26:11--26:37"
                 "Pattern match failure"
     RTS.pEnter "PdfCrypto._ChooseCiph"
       (_ChooseCiph @HS.Integer edict encV)
 
_MakeContext :: PdfXRef.TrailerDict -> D.Parser ()
 
_MakeContext (t :: PdfXRef.TrailerDict) =
  (RTS.<||)
    (RTS.pEnter "encryption"
       (do (enc :: PdfXRef.TrailerDictEncrypt) <-
             case HS.getField @"encrypt" t of
               HS.Just (_966 :: PdfXRef.TrailerDictEncrypt) -> HS.pure _966
               _ -> RTS.pError RTS.FromSystem "71:12--71:28"
                      "Pattern match failure"
           RTS.pErrorMode RTS.Abort
             (RTS.pEnter "PdfCrypto._EncryptionDict" (_EncryptionDict enc))))
    (RTS.pEnter "noencryption" (HS.pure ()))