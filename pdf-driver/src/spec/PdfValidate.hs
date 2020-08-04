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
 
pCheckASCII ::
      PdfValue.Value -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckASCII (v :: PdfValue.Value) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "72:5--72:15" "Expected `string`"
         (HS.getField @"string" v)
     HS.pure __
 
pCheckDate ::
      PdfValue.Value -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckDate (v :: PdfValue.Value) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "64:5--64:15" "Expected `string`"
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
       RTS.pIsJust "68:5--68:15" "Expected `string`"
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
  do RTS.pGuard "77:5--77:18" "guard failed"
       (HS.getField @"num" x HS.== HS.getField @"num" y)
     (__ :: ()) <-
       RTS.pGuard "77:21--77:34" "guard failed"
         (HS.getField @"exp" x HS.== HS.getField @"exp" y)
     HS.pure __
 
pInteger ::
      (HS.Integer -> D.Parser HS.Integer)
        -> (PdfValue.Value -> D.Parser HS.Integer)
 
pInteger (pP :: (HS.Integer -> D.Parser HS.Integer))
  (v :: PdfValue.Value) =
  do (n :: PdfValue.Number) <-
       RTS.pIsJust "57:8--57:18" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pGuard "58:3--58:12" "guard failed"
       (HS.getField @"exp" n HS.== (RTS.lit 0 :: HS.Integer))
     (__ :: HS.Integer) <- RTS.pEnter "P" (pP (HS.getField @"num" n))
     HS.pure __
 
_Any :: forall a. RTS.DDL a => D.Parser ()
 
_Any = HS.pure ()
 
_CheckASCII :: PdfValue.Value -> D.Parser ()
 
_CheckASCII (v :: PdfValue.Value) =
  RTS.pIsJust_ "72:5--72:15" "Expected `string`"
    (HS.getField @"string" v)
 
_CheckDate :: PdfValue.Value -> D.Parser ()
 
_CheckDate (v :: PdfValue.Value) =
  RTS.pIsJust_ "64:5--64:15" "Expected `string`"
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
  RTS.pIsJust_ "68:5--68:15" "Expected `string`"
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
  do RTS.pGuard "77:5--77:18" "guard failed"
       (HS.getField @"num" x HS.== HS.getField @"num" y)
     RTS.pGuard "77:21--77:34" "guard failed"
       (HS.getField @"exp" x HS.== HS.getField @"exp" y)
 
_Integer ::
      (HS.Integer -> D.Parser HS.Integer)
        -> (PdfValue.Value -> D.Parser ())
 
_Integer (pP :: (HS.Integer -> D.Parser HS.Integer))
  (v :: PdfValue.Value) =
  do (n :: PdfValue.Number) <-
       RTS.pIsJust "57:8--57:18" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pGuard "58:3--58:12" "guard failed"
       (HS.getField @"exp" n HS.== (RTS.lit 0 :: HS.Integer))
     HS.void (RTS.pEnter "P" (pP (HS.getField @"num" n)))
     HS.pure ()
 
_IsValidated ::
      HS.Integer
        -> (HS.Integer -> (Vector.Vector (RTS.UInt 8) -> D.Parser ()))
 
_IsValidated (obj :: HS.Integer) (gen :: HS.Integer)
  (ty :: Vector.Vector (RTS.UInt 8)) =
  do HS.void
       (RTS.pEnter "PdfValidate.IsValidated" (pIsValidated obj gen ty))
     HS.pure ()