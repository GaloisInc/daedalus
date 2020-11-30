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
 
instance HS.HasField "ciph" EncryptionDict ChooseCiph where
  getField (EncryptionDict _ _ _ _ _ _ _ x) = x
 
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
            RTS.pIsJust "49:12--49:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StmF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StmF") edict)
          RTS.pIsJust "49:12--49:39" "Expected `name`"
            (HS.getField @"name" _0)
     (strF :: b) <-
       do (_1 :: c) <-
            RTS.pIsJust "50:12--50:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StrF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StrF") edict)
          RTS.pIsJust "50:12--50:39" "Expected `name`"
            (HS.getField @"name" _1)
     (cf :: Map.Map b o) <-
       do (_2 :: c) <-
            RTS.pIsJust "51:10--51:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "CF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "CF") edict)
          RTS.pIsJust "51:10--51:35" "Expected `dict`"
            (HS.getField @"dict" _2)
     (stmFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_3 :: o) <-
            RTS.pIsJust "54:16--54:29"
              ("Missing key: " HS.++ HS.show (stmF :: b))
              (Map.lookup stmF cf)
          RTS.pIsJust "54:16--54:38" "Expected `dict`"
            (HS.getField @"dict" _3)
     (stmFname :: Vector.Vector (RTS.UInt 8)) <-
       do (_4 :: PdfValue.Value) <-
            RTS.pIsJust "55:16--55:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "CFM") stmFdict)
          RTS.pIsJust "55:16--55:45" "Expected `name`"
            (HS.getField @"name" _4)
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") stmFdict)
     (strFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_5 :: o) <-
            RTS.pIsJust "59:16--59:29"
              ("Missing key: " HS.++ HS.show (strF :: b))
              (Map.lookup strF cf)
          RTS.pIsJust "59:16--59:38" "Expected `dict`"
            (HS.getField @"dict" _5)
     do (_6 :: PdfValue.Value) <-
          RTS.pIsJust "60:16--60:36"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "CFM") strFdict)
        RTS.pIsJust_ "60:16--60:45" "Expected `name`"
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
             do RTS.pGuard "32:5--32:18" "guard failed"
                  (v HS.== (RTS.lit 2 :: u))
                (len :: HS.Integer) <-
                  RTS.pEnter "PdfDecl.LookupNat"
                    (PdfDecl.pLookupNat (Vector.vecFromRep "Length") edict)
                (__ :: ()) <-
                  RTS.pGuard "34:5--34:22" "guard failed"
                    (len HS.== (RTS.lit 128 :: HS.Integer))
                HS.pure __
           HS.pure (ChooseCiph_v2RC4 _7)))
    ((RTS.<||)
       (RTS.pEnter "v4RC4"
          (do (_8 :: ()) <-
                do RTS.pGuard "37:5--37:18" "guard failed"
                     (v HS.== (RTS.lit 4 :: u))
                   (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                     RTS.pEnter "PdfCrypto.V4stmFname"
                       (pV4stmFname @(Vector.Vector (RTS.UInt 8)) @PdfValue.Value
                          @PdfValue.Value
                          edict)
                   (__ :: ()) <-
                     RTS.pGuard "39:5--39:28" "guard failed"
                       (stmFname HS.== Vector.vecFromRep "V2")
                   HS.pure __
              HS.pure (ChooseCiph_v4RC4 _8)))
       (RTS.pEnter "v4AES"
          (do (_9 :: ()) <-
                do RTS.pGuard "42:5--42:18" "guard failed"
                     (v HS.== (RTS.lit 4 :: u))
                   (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                     RTS.pEnter "PdfCrypto.V4stmFname"
                       (pV4stmFname @(Vector.Vector (RTS.UInt 8)) @PdfValue.Value
                          @PdfValue.Value
                          edict)
                   (__ :: ()) <-
                     RTS.pGuard "44:5--44:31" "guard failed"
                       (stmFname HS.== Vector.vecFromRep "AESV2")
                   HS.pure __
              HS.pure (ChooseCiph_v4AES _9))))
 
pEncryptionDict :: PdfValue.Ref -> D.Parser EncryptionDict
 
pEncryptionDict (eref :: PdfValue.Ref) =
  do (edict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_10 :: PdfValue.Value) <-
            RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef eref)
          RTS.pIsJust "6:13--6:39" "Expected `dict`"
            (HS.getField @"dict" _10)
     (encFilter :: Vector.Vector (RTS.UInt 8)) <-
       do (_11 :: PdfValue.Value) <-
            RTS.pIsJust "8:16--8:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Filter" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Filter") edict)
          RTS.pIsJust "8:16--8:45" "Expected `name`"
            (HS.getField @"name" _11)
     RTS.pGuard "9:3--9:33" "guard failed"
       (encFilter HS.== Vector.vecFromRep "Standard")
     (encSubFilter :: HS.Maybe (Vector.Vector (RTS.UInt 8))) <-
       RTS.pOptional (RTS.<||) HS.Just
         (do (_12 :: PdfValue.Value) <-
               RTS.pIsJust "11:29--11:52"
                 ("Missing key: "
                    HS.++ HS.show
                            (Vector.vecFromRep "SubFilter" :: Vector.Vector (RTS.UInt 8)))
                 (Map.lookup (Vector.vecFromRep "SubFilter") edict)
             RTS.pIsJust "11:29--11:61" "Expected `name`"
               (HS.getField @"name" _12))
     (encV :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "V") edict)
     (encR :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "R") edict)
     RTS.pGuard "17:3--17:47" "guard failed"
       ((encR HS.== (RTS.lit 3 :: HS.Integer))
          HS.|| ((encV HS.== (RTS.lit 4 :: HS.Integer))
                   HS.&& (encR HS.== (RTS.lit 4 :: HS.Integer))))
     (encO :: Vector.Vector (RTS.UInt 8)) <-
       do (_13 :: PdfValue.Value) <-
            RTS.pIsJust "19:11--19:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "O" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "O") edict)
          RTS.pIsJust "19:11--19:37" "Expected `string`"
            (HS.getField @"string" _13)
     (encU :: Vector.Vector (RTS.UInt 8)) <-
       do (_14 :: PdfValue.Value) <-
            RTS.pIsJust "20:11--20:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "U" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "U") edict)
          RTS.pIsJust "20:11--20:37" "Expected `string`"
            (HS.getField @"string" _14)
     (encP :: HS.Integer) <-
       do (v :: PdfValue.Number) <-
            do (_15 :: PdfValue.Value) <-
                 RTS.pIsJust "23:11--23:26"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "P" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "P") edict)
               RTS.pIsJust "23:11--23:37" "Expected `number`"
                 (HS.getField @"number" _15)
          (__ :: HS.Integer) <- HS.pure (HS.getField @"num" v)
          HS.pure __
     (ciph :: ChooseCiph) <-
       RTS.pEnter "PdfCrypto.ChooseCiph"
         (pChooseCiph @HS.Integer edict encV)
     HS.pure
       (EncryptionDict encFilter encSubFilter encV encR encO encU encP
          ciph)
 
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
       (do RTS.pGuard "32:5--32:18" "guard failed"
             (v HS.== (RTS.lit 2 :: u))
           (len :: HS.Integer) <-
             RTS.pEnter "PdfDecl.LookupNat"
               (PdfDecl.pLookupNat (Vector.vecFromRep "Length") edict)
           RTS.pGuard "34:5--34:22" "guard failed"
             (len HS.== (RTS.lit 128 :: HS.Integer))))
    ((RTS.<||)
       (RTS.pEnter "v4RC4"
          (do RTS.pGuard "37:5--37:18" "guard failed"
                (v HS.== (RTS.lit 4 :: u))
              (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                RTS.pEnter "PdfCrypto.V4stmFname"
                  (pV4stmFname @(Vector.Vector (RTS.UInt 8)) @PdfValue.Value
                     @PdfValue.Value
                     edict)
              RTS.pGuard "39:5--39:28" "guard failed"
                (stmFname HS.== Vector.vecFromRep "V2")))
       (RTS.pEnter "v4AES"
          (do RTS.pGuard "42:5--42:18" "guard failed"
                (v HS.== (RTS.lit 4 :: u))
              (stmFname :: Vector.Vector (RTS.UInt 8)) <-
                RTS.pEnter "PdfCrypto.V4stmFname"
                  (pV4stmFname @(Vector.Vector (RTS.UInt 8)) @PdfValue.Value
                     @PdfValue.Value
                     edict)
              RTS.pGuard "44:5--44:31" "guard failed"
                (stmFname HS.== Vector.vecFromRep "AESV2"))))
 
_EncryptionDict :: PdfValue.Ref -> D.Parser ()
 
_EncryptionDict (eref :: PdfValue.Ref) =
  do (edict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_10 :: PdfValue.Value) <-
            RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef eref)
          RTS.pIsJust "6:13--6:39" "Expected `dict`"
            (HS.getField @"dict" _10)
     (encFilter :: Vector.Vector (RTS.UInt 8)) <-
       do (_11 :: PdfValue.Value) <-
            RTS.pIsJust "8:16--8:36"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Filter" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Filter") edict)
          RTS.pIsJust "8:16--8:45" "Expected `name`"
            (HS.getField @"name" _11)
     RTS.pGuard "9:3--9:33" "guard failed"
       (encFilter HS.== Vector.vecFromRep "Standard")
     (RTS.<||)
       (do (_12 :: PdfValue.Value) <-
             RTS.pIsJust "11:29--11:52"
               ("Missing key: "
                  HS.++ HS.show
                          (Vector.vecFromRep "SubFilter" :: Vector.Vector (RTS.UInt 8)))
               (Map.lookup (Vector.vecFromRep "SubFilter") edict)
           RTS.pIsJust_ "11:29--11:61" "Expected `name`"
             (HS.getField @"name" _12))
       (HS.pure ())
     (encV :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "V") edict)
     (encR :: HS.Integer) <-
       RTS.pEnter "PdfDecl.LookupNat"
         (PdfDecl.pLookupNat (Vector.vecFromRep "R") edict)
     RTS.pGuard "17:3--17:47" "guard failed"
       ((encR HS.== (RTS.lit 3 :: HS.Integer))
          HS.|| ((encV HS.== (RTS.lit 4 :: HS.Integer))
                   HS.&& (encR HS.== (RTS.lit 4 :: HS.Integer))))
     do (_13 :: PdfValue.Value) <-
          RTS.pIsJust "19:11--19:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "O" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "O") edict)
        RTS.pIsJust_ "19:11--19:37" "Expected `string`"
          (HS.getField @"string" _13)
     do (_14 :: PdfValue.Value) <-
          RTS.pIsJust "20:11--20:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "U" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "U") edict)
        RTS.pIsJust_ "20:11--20:37" "Expected `string`"
          (HS.getField @"string" _14)
     do (_15 :: PdfValue.Value) <-
          RTS.pIsJust "23:11--23:26"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "P" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "P") edict)
        RTS.pIsJust_ "23:11--23:37" "Expected `number`"
          (HS.getField @"number" _15)
     RTS.pEnter "PdfCrypto._ChooseCiph"
       (_ChooseCiph @HS.Integer edict encV)
 
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
            RTS.pIsJust "49:12--49:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StmF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StmF") edict)
          RTS.pIsJust "49:12--49:39" "Expected `name`"
            (HS.getField @"name" _0)
     (strF :: b) <-
       do (_1 :: c) <-
            RTS.pIsJust "50:12--50:30"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "StrF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "StrF") edict)
          RTS.pIsJust "50:12--50:39" "Expected `name`"
            (HS.getField @"name" _1)
     (cf :: Map.Map b o) <-
       do (_2 :: c) <-
            RTS.pIsJust "51:10--51:26"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "CF" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "CF") edict)
          RTS.pIsJust "51:10--51:35" "Expected `dict`"
            (HS.getField @"dict" _2)
     (stmFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_3 :: o) <-
            RTS.pIsJust "54:16--54:29"
              ("Missing key: " HS.++ HS.show (stmF :: b))
              (Map.lookup stmF cf)
          RTS.pIsJust "54:16--54:38" "Expected `dict`"
            (HS.getField @"dict" _3)
     do (_4 :: PdfValue.Value) <-
          RTS.pIsJust "55:16--55:36"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "CFM") stmFdict)
        RTS.pIsJust_ "55:16--55:45" "Expected `name`"
          (HS.getField @"name" _4)
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") stmFdict)
     (strFdict
        :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       do (_5 :: o) <-
            RTS.pIsJust "59:16--59:29"
              ("Missing key: " HS.++ HS.show (strF :: b))
              (Map.lookup strF cf)
          RTS.pIsJust "59:16--59:38" "Expected `dict`"
            (HS.getField @"dict" _5)
     do (_6 :: PdfValue.Value) <-
          RTS.pIsJust "60:16--60:36"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "CFM" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "CFM") strFdict)
        RTS.pIsJust_ "60:16--60:45" "Expected `name`"
          (HS.getField @"name" _6)
     RTS.pEnter "PdfDecl._LookupNat"
       (PdfDecl._LookupNat (Vector.vecFromRep "Length") strFdict)