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
 
 
pCheckASCII ::
      PdfValue.Value -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckASCII (v :: PdfValue.Value) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "67:5--67:15" "Expected `string`"
         (HS.getField @"string" v)
     HS.pure __
 
pCheckDate ::
      PdfValue.Value -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckDate (v :: PdfValue.Value) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "59:5--59:15" "Expected `string`"
         (HS.getField @"string" v)
     HS.pure __
 
pCheckInteger :: PdfValue.Value -> D.Parser HS.Integer
 
pCheckInteger (v :: PdfValue.Value) =
  do (n :: PdfValue.Number) <-
       RTS.pIsJust "53:8--53:18" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pGuard "54:3--54:12" "guard failed"
       (HS.getField @"exp" n HS.== (RTS.lit 0 :: HS.Integer))
     (__ :: HS.Integer) <- HS.pure (HS.getField @"num" n)
     HS.pure __
 
pCheckRectangle :: PdfValue.Value -> D.Parser ()
 
pCheckRectangle (v :: PdfValue.Value) = HS.pure ()
 
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
    (RTS.pEnter "PdfDecl.Default"
       (PdfDecl.pDefault @() ()
          (do (done :: HS.Bool) <-
                RTS.pEnter "PdfValidate.IsValidated"
                  (pIsValidated (HS.getField @"gen" r) (HS.getField @"obj" r) ty)
              RTS.pGuard "40:5--40:17" "guard failed" (HS.not done)
              RTS.pErrorMode RTS.Abort
                (do RTS.pEnter "PdfValidate._StartValidating"
                      (_StartValidating (HS.getField @"gen" r) (HS.getField @"obj" r) ty)
                    (v :: PdfValue.Value) <-
                      RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
                    (__ :: ()) <- RTS.pEnter "P" (pP v)
                    HS.pure __))))
 
pCheckText ::
      PdfValue.Value -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckText (v :: PdfValue.Value) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "63:5--63:15" "Expected `string`"
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
    ((RTS.<||)
       (do (r :: PdfValue.Ref) <-
             RTS.pIsJust "31:13--31:20" "Expected `ref`" (HS.getField @"ref" v)
           RTS.pErrorMode RTS.Abort
             (do (__ :: ()) <-
                   RTS.pEnter "PdfValidate.CheckRef" (pCheckRef ty pP r)
                 HS.pure __))
       (RTS.pEnter "P" (pP v)))
 
pEqNumber :: PdfValue.Number -> (PdfValue.Number -> D.Parser ())
 
pEqNumber (x :: PdfValue.Number) (y :: PdfValue.Number) =
  do RTS.pGuard "75:5--75:18" "guard failed"
       (HS.getField @"num" x HS.== HS.getField @"num" y)
     (__ :: ()) <-
       RTS.pGuard "75:21--75:34" "guard failed"
         (HS.getField @"exp" x HS.== HS.getField @"exp" y)
     HS.pure __
 
_CheckASCII :: PdfValue.Value -> D.Parser ()
 
_CheckASCII (v :: PdfValue.Value) =
  RTS.pIsJust_ "67:5--67:15" "Expected `string`"
    (HS.getField @"string" v)
 
_CheckDate :: PdfValue.Value -> D.Parser ()
 
_CheckDate (v :: PdfValue.Value) =
  RTS.pIsJust_ "59:5--59:15" "Expected `string`"
    (HS.getField @"string" v)
 
_CheckInteger :: PdfValue.Value -> D.Parser ()
 
_CheckInteger (v :: PdfValue.Value) =
  do (n :: PdfValue.Number) <-
       RTS.pIsJust "53:8--53:18" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pGuard "54:3--54:12" "guard failed"
       (HS.getField @"exp" n HS.== (RTS.lit 0 :: HS.Integer))
 
_CheckRectangle :: D.Parser ()
 
_CheckRectangle = HS.pure ()
 
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
              RTS.pGuard "40:5--40:17" "guard failed" (HS.not done)
              RTS.pErrorMode RTS.Abort
                (do RTS.pEnter "PdfValidate._StartValidating"
                      (_StartValidating (HS.getField @"gen" r) (HS.getField @"obj" r) ty)
                    (v :: PdfValue.Value) <-
                      RTS.pEnter "PdfDecl.ResolveValRef" (PdfDecl.pResolveValRef r)
                    RTS.pEnter "P" (pP v)))))
 
_CheckText :: PdfValue.Value -> D.Parser ()
 
_CheckText (v :: PdfValue.Value) =
  RTS.pIsJust_ "63:5--63:15" "Expected `string`"
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
             RTS.pIsJust "31:13--31:20" "Expected `ref`" (HS.getField @"ref" v)
           RTS.pErrorMode RTS.Abort
             (RTS.pEnter "PdfValidate._CheckRef" (_CheckRef ty pP r)))
       (RTS.pEnter "P" (pP v)))
 
_EqNumber :: PdfValue.Number -> (PdfValue.Number -> D.Parser ())
 
_EqNumber (x :: PdfValue.Number) (y :: PdfValue.Number) =
  do RTS.pGuard "75:5--75:18" "guard failed"
       (HS.getField @"num" x HS.== HS.getField @"num" y)
     RTS.pGuard "75:21--75:34" "guard failed"
       (HS.getField @"exp" x HS.== HS.getField @"exp" y)
 
_IsValidated ::
      HS.Integer
        -> (HS.Integer -> (Vector.Vector (RTS.UInt 8) -> D.Parser ()))
 
_IsValidated (obj :: HS.Integer) (gen :: HS.Integer)
  (ty :: Vector.Vector (RTS.UInt 8)) =
  do HS.void
       (RTS.pEnter "PdfValidate.IsValidated" (pIsValidated obj gen ty))
     HS.pure ()