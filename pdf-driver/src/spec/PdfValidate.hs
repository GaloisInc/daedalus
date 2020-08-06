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
module PdfValidate where
 
import qualified Primitives.Validate as D
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
 
 
pAny :: forall a. RTS.DDL a => a -> D.Parser ()
 
pAny (v :: a) = HS.pure ()
 
pAtLeast :: forall b. RTS.DDL b => b -> (b -> D.Parser ())
 
pAtLeast (x :: b) (y :: b) =
  RTS.pGuard "49:22--49:27" "guard failed" (x HS.<= y)
 
pCheckASCII ::
      PdfValue.Value -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckASCII (v :: PdfValue.Value) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "91:5--91:15" "Expected `string`"
         (HS.getField @"string" v)
     HS.pure __
 
pCheckDate ::
      PdfValue.Value -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckDate (v :: PdfValue.Value) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "83:5--83:15" "Expected `string`"
         (HS.getField @"string" v)
     HS.pure __
 
pIsValidated ::
      HS.Integer
        -> (HS.Integer -> (Vector.Vector (RTS.UInt 8) -> D.Parser HS.Bool))
 
pIsValidated (obj :: HS.Integer) (gen :: HS.Integer)
  (ty :: Vector.Vector (RTS.UInt 8)) =
  D.primIsValidated obj gen ty :: D.Parser HS.Bool
 
pStartValidating ::
      HS.Integer
        -> (HS.Integer -> (Vector.Vector (RTS.UInt 8) -> D.Parser ()))
 
pStartValidating (obj :: HS.Integer) (gen :: HS.Integer)
  (ty :: Vector.Vector (RTS.UInt 8)) =
  D.primStartValidating obj gen ty :: D.Parser ()
 
_StartValidating ::
      HS.Integer
        -> (HS.Integer -> (Vector.Vector (RTS.UInt 8) -> D.Parser ()))
 
_StartValidating (obj :: HS.Integer) (gen :: HS.Integer)
  (ty :: Vector.Vector (RTS.UInt 8)) =
  RTS.pEnter "PdfValidate.StartValidating"
    (pStartValidating obj gen ty)
 
pCheckRef ::
      Vector.Vector (RTS.UInt 8)
        -> ((PdfValue.Value -> D.Parser ())
              -> (PdfValue.Ref -> D.Parser ()))
 
pCheckRef (ty :: Vector.Vector (RTS.UInt 8))
  (pP :: (PdfValue.Value -> D.Parser ()))
  (r :: PdfValue.Ref) =
  RTS.pErrorMode RTS.Fail
    (do (__ :: ()) <-
          RTS.pEnter "PdfDecl.Default"
            (PdfDecl.pDefault @() ()
               (do (done :: HS.Bool) <-
                     RTS.pEnter "PdfValidate.IsValidated"
                       (pIsValidated (HS.getField @"gen" r) (HS.getField @"obj" r) ty)
                   RTS.pGuard "38:5--38:17" "guard failed" (HS.not done)
                   RTS.pErrorMode RTS.Abort
                     (do RTS.pEnter "PdfValidate._StartValidating"
                           (_StartValidating (HS.getField @"gen" r) (HS.getField @"obj" r) ty)
                         (v :: PdfValue.Value) <-
                           RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
                         (__ :: ()) <- RTS.pEnter "P" (pP v)
                         HS.pure __)))
        HS.pure __)
 
pCheckText ::
      PdfValue.Value -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckText (v :: PdfValue.Value) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "87:5--87:15" "Expected `string`"
         (HS.getField @"string" v)
     HS.pure __
 
pCheckValue ::
      Vector.Vector (RTS.UInt 8)
        -> ((PdfValue.Value -> D.Parser ())
              -> (PdfValue.Value -> D.Parser ()))
 
pCheckValue (ty :: Vector.Vector (RTS.UInt 8))
  (pP :: (PdfValue.Value -> D.Parser ()))
  (v :: PdfValue.Value) =
  RTS.pErrorMode RTS.Fail
    (do (__ :: ()) <-
          (RTS.<||)
            (do (r :: PdfValue.Ref) <-
                  RTS.pIsJust "29:13--29:20" "Expected `ref`" (HS.getField @"ref" v)
                RTS.pErrorMode RTS.Abort
                  (do (__ :: ()) <-
                        RTS.pEnter "PdfValidate.CheckRef" (pCheckRef ty pP r)
                      HS.pure __))
            (RTS.pEnter "P" (pP v))
        HS.pure __)
 
pEqNumber :: PdfValue.Number -> (PdfValue.Number -> D.Parser ())
 
pEqNumber (x :: PdfValue.Number) (y :: PdfValue.Number) =
  do RTS.pGuard "96:5--96:18" "guard failed"
       (HS.getField @"num" x HS.== HS.getField @"num" y)
     (__ :: ()) <-
       RTS.pGuard "96:21--96:34" "guard failed"
         (HS.getField @"exp" x HS.== HS.getField @"exp" y)
     HS.pure __
 
pIs :: forall b. RTS.DDL b => b -> (b -> D.Parser ())
 
pIs (x :: b) (y :: b) =
  do (__ :: ()) <-
       RTS.pGuard "48:16--48:21" "guard failed" (x HS.== y)
     HS.pure __
 
pPdfIndirect ::
      Vector.Vector (RTS.UInt 8)
        -> ((PdfValue.Value -> D.Parser ())
              -> (PdfValue.Value -> D.Parser ()))
 
pPdfIndirect (ty :: Vector.Vector (RTS.UInt 8))
  (pP :: (PdfValue.Value -> D.Parser ()))
  (v :: PdfValue.Value) =
  do (r :: PdfValue.Ref) <-
       RTS.pIsJust "63:8--63:15" "Expected `ref`" (HS.getField @"ref" v)
     (__ :: ()) <- RTS.pEnter "PdfValidate.CheckRef" (pCheckRef ty pP r)
     HS.pure __
 
pPdfInteger :: PdfValue.Value -> D.Parser HS.Integer
 
pPdfInteger (v :: PdfValue.Value) =
  do (n :: PdfValue.Number) <-
       RTS.pIsJust "76:8--76:18" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pGuard "77:3--77:12" "guard failed"
       (HS.getField @"exp" n HS.== (RTS.lit 0 :: HS.Integer))
     (__ :: HS.Integer) <- HS.pure (HS.getField @"num" n)
     HS.pure __
 
pPdfName ::
  forall e.
    RTS.DDL e =>
      (Vector.Vector (RTS.UInt 8) -> D.Parser e)
        -> (PdfValue.Value -> D.Parser e)
 
pPdfName (pP :: (Vector.Vector (RTS.UInt 8) -> D.Parser e))
  (v :: PdfValue.Value) =
  do (x :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "53:8--53:16" "Expected `name`" (HS.getField @"name" v)
     (__ :: e) <- RTS.pEnter "P" (pP x)
     HS.pure __
 
pPdfType ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> (Vector.Vector (RTS.UInt 8) -> D.Parser ())
 
pPdfType (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  (expect :: Vector.Vector (RTS.UInt 8)) =
  do (actual :: PdfValue.Value) <-
       RTS.pIsJust "58:14--58:28"
         ("Missing key: "
            HS.++ HS.show
                    (Vector.vecFromRep "Type" :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup (Vector.vecFromRep "Type") d)
     (__ :: ()) <-
       RTS.pEnter "PdfValidate.PdfName"
         (pPdfName @() (pIs @(Vector.Vector (RTS.UInt 8)) expect) actual)
     HS.pure __
 
_Any :: forall a. RTS.DDL a => D.Parser ()
 
_Any = HS.pure ()
 
_AtLeast :: forall b. RTS.DDL b => b -> (b -> D.Parser ())
 
_AtLeast (x :: b) (y :: b) =
  RTS.pGuard "49:22--49:27" "guard failed" (x HS.<= y)
 
_CheckASCII :: PdfValue.Value -> D.Parser ()
 
_CheckASCII (v :: PdfValue.Value) =
  RTS.pIsJust_ "91:5--91:15" "Expected `string`"
    (HS.getField @"string" v)
 
_CheckDate :: PdfValue.Value -> D.Parser ()
 
_CheckDate (v :: PdfValue.Value) =
  RTS.pIsJust_ "83:5--83:15" "Expected `string`"
    (HS.getField @"string" v)
 
_CheckRef ::
      Vector.Vector (RTS.UInt 8)
        -> ((PdfValue.Value -> D.Parser ())
              -> (PdfValue.Ref -> D.Parser ()))
 
_CheckRef (ty :: Vector.Vector (RTS.UInt 8))
  (pP :: (PdfValue.Value -> D.Parser ()))
  (r :: PdfValue.Ref) =
  RTS.pErrorMode RTS.Fail
    (RTS.pEnter "PdfDecl._Default"
       (PdfDecl._Default @()
          (do (done :: HS.Bool) <-
                RTS.pEnter "PdfValidate.IsValidated"
                  (pIsValidated (HS.getField @"gen" r) (HS.getField @"obj" r) ty)
              RTS.pGuard "38:5--38:17" "guard failed" (HS.not done)
              RTS.pErrorMode RTS.Abort
                (do RTS.pEnter "PdfValidate._StartValidating"
                      (_StartValidating (HS.getField @"gen" r) (HS.getField @"obj" r) ty)
                    (v :: PdfValue.Value) <-
                      RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
                    RTS.pEnter "P" (pP v)))))
 
_CheckText :: PdfValue.Value -> D.Parser ()
 
_CheckText (v :: PdfValue.Value) =
  RTS.pIsJust_ "87:5--87:15" "Expected `string`"
    (HS.getField @"string" v)
 
_CheckValue ::
      Vector.Vector (RTS.UInt 8)
        -> ((PdfValue.Value -> D.Parser ())
              -> (PdfValue.Value -> D.Parser ()))
 
_CheckValue (ty :: Vector.Vector (RTS.UInt 8))
  (pP :: (PdfValue.Value -> D.Parser ()))
  (v :: PdfValue.Value) =
  RTS.pErrorMode RTS.Fail
    ((RTS.<||)
       (do (r :: PdfValue.Ref) <-
             RTS.pIsJust "29:13--29:20" "Expected `ref`" (HS.getField @"ref" v)
           RTS.pErrorMode RTS.Abort
             (RTS.pEnter "PdfValidate._CheckRef" (_CheckRef ty pP r)))
       (RTS.pEnter "P" (pP v)))
 
_EqNumber :: PdfValue.Number -> (PdfValue.Number -> D.Parser ())
 
_EqNumber (x :: PdfValue.Number) (y :: PdfValue.Number) =
  do RTS.pGuard "96:5--96:18" "guard failed"
       (HS.getField @"num" x HS.== HS.getField @"num" y)
     RTS.pGuard "96:21--96:34" "guard failed"
       (HS.getField @"exp" x HS.== HS.getField @"exp" y)
 
_Is :: forall b. RTS.DDL b => b -> (b -> D.Parser ())
 
_Is (x :: b) (y :: b) =
  RTS.pGuard "48:16--48:21" "guard failed" (x HS.== y)
 
_IsValidated ::
      HS.Integer
        -> (HS.Integer -> (Vector.Vector (RTS.UInt 8) -> D.Parser ()))
 
_IsValidated (obj :: HS.Integer) (gen :: HS.Integer)
  (ty :: Vector.Vector (RTS.UInt 8)) =
  do HS.void
       (RTS.pEnter "PdfValidate.IsValidated" (pIsValidated obj gen ty))
     HS.pure ()
 
_PdfIndirect ::
      Vector.Vector (RTS.UInt 8)
        -> ((PdfValue.Value -> D.Parser ())
              -> (PdfValue.Value -> D.Parser ()))
 
_PdfIndirect (ty :: Vector.Vector (RTS.UInt 8))
  (pP :: (PdfValue.Value -> D.Parser ()))
  (v :: PdfValue.Value) =
  do (r :: PdfValue.Ref) <-
       RTS.pIsJust "63:8--63:15" "Expected `ref`" (HS.getField @"ref" v)
     RTS.pEnter "PdfValidate._CheckRef" (_CheckRef ty pP r)
 
_PdfInteger :: PdfValue.Value -> D.Parser ()
 
_PdfInteger (v :: PdfValue.Value) =
  do (n :: PdfValue.Number) <-
       RTS.pIsJust "76:8--76:18" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pGuard "77:3--77:12" "guard failed"
       (HS.getField @"exp" n HS.== (RTS.lit 0 :: HS.Integer))
 
_PdfName ::
  forall e.
    RTS.DDL e =>
      (Vector.Vector (RTS.UInt 8) -> D.Parser e)
        -> (PdfValue.Value -> D.Parser ())
 
_PdfName (pP :: (Vector.Vector (RTS.UInt 8) -> D.Parser e))
  (v :: PdfValue.Value) =
  do (x :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "53:8--53:16" "Expected `name`" (HS.getField @"name" v)
     HS.void (RTS.pEnter "P" (pP x))
     HS.pure ()
 
_PdfType ::
      Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value
        -> (Vector.Vector (RTS.UInt 8) -> D.Parser ())
 
_PdfType (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value)
  (expect :: Vector.Vector (RTS.UInt 8)) =
  do (actual :: PdfValue.Value) <-
       RTS.pIsJust "58:14--58:28"
         ("Missing key: "
            HS.++ HS.show
                    (Vector.vecFromRep "Type" :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup (Vector.vecFromRep "Type") d)
     RTS.pEnter "PdfValidate._PdfName"
       (_PdfName @() (pIs @(Vector.Vector (RTS.UInt 8)) expect) actual)