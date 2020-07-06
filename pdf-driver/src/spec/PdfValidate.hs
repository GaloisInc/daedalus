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
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Vector as Vector
import qualified RTS.Map as Map
 
 
pCheckASCII ::
  forall a.
    (RTS.DDL a,
     RTS.HasUnion a "string" (Vector.Vector (RTS.UInt 8))) =>
      a -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckASCII (v :: a) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "18:5--18:15" "Expected `string`"
         (HS.getField @"string" v)
     HS.pure __
 
pCheckDate ::
  forall a.
    (RTS.DDL a,
     RTS.HasUnion a "string" (Vector.Vector (RTS.UInt 8))) =>
      a -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckDate (v :: a) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "10:5--10:15" "Expected `string`"
         (HS.getField @"string" v)
     HS.pure __
 
pCheckInteger ::
  forall a b f.
    (RTS.DDL a, RTS.DDL b, RTS.DDL f, RTS.HasUnion a "number" b,
     RTS.HasStruct b "exp" f, RTS.Literal 0 f,
     RTS.HasStruct b "num" HS.Integer) =>
      a -> D.Parser HS.Integer
 
pCheckInteger (v :: a) =
  do (n :: b) <-
       RTS.pIsJust "5:10--5:20" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pGuard "5:23--5:32" "guard failed"
       (HS.getField @"exp" n HS.== (RTS.lit 0 :: f))
     (__ :: HS.Integer) <- HS.pure (HS.getField @"num" n)
     HS.pure __
 
pCheckRectangle :: forall a. RTS.DDL a => a -> D.Parser ()
 
pCheckRectangle (v :: a) = HS.pure ()
 
pCheckText ::
  forall a.
    (RTS.DDL a,
     RTS.HasUnion a "string" (Vector.Vector (RTS.UInt 8))) =>
      a -> D.Parser (Vector.Vector (RTS.UInt 8))
 
pCheckText (v :: a) =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pIsJust "14:5--14:15" "Expected `string`"
         (HS.getField @"string" v)
     HS.pure __
 
pEqNumber ::
  forall a d g.
    (RTS.DDL a, RTS.DDL d, RTS.DDL g, RTS.HasStruct a "num" d,
     RTS.HasStruct a "exp" g) =>
      a -> (a -> D.Parser ())
 
pEqNumber (x :: a) (y :: a) =
  do RTS.pGuard "26:5--26:18" "guard failed"
       (HS.getField @"num" x HS.== HS.getField @"num" y)
     (__ :: ()) <-
       RTS.pGuard "26:21--26:34" "guard failed"
         (HS.getField @"exp" x HS.== HS.getField @"exp" y)
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
 
_CheckASCII ::
  forall a.
    (RTS.DDL a,
     RTS.HasUnion a "string" (Vector.Vector (RTS.UInt 8))) =>
      a -> D.Parser ()
 
_CheckASCII (v :: a) =
  RTS.pIsJust_ "18:5--18:15" "Expected `string`"
    (HS.getField @"string" v)
 
_CheckDate ::
  forall a.
    (RTS.DDL a,
     RTS.HasUnion a "string" (Vector.Vector (RTS.UInt 8))) =>
      a -> D.Parser ()
 
_CheckDate (v :: a) =
  RTS.pIsJust_ "10:5--10:15" "Expected `string`"
    (HS.getField @"string" v)
 
_CheckInteger ::
  forall a b f.
    (RTS.DDL a, RTS.DDL b, RTS.DDL f, RTS.HasUnion a "number" b,
     RTS.HasStruct b "exp" f, RTS.Literal 0 f,
     RTS.HasStruct b "num" HS.Integer) =>
      a -> D.Parser ()
 
_CheckInteger (v :: a) =
  do (n :: b) <-
       RTS.pIsJust "5:10--5:20" "Expected `number`"
         (HS.getField @"number" v)
     RTS.pGuard "5:23--5:32" "guard failed"
       (HS.getField @"exp" n HS.== (RTS.lit 0 :: f))
 
_CheckRectangle :: forall a. RTS.DDL a => D.Parser ()
 
_CheckRectangle = HS.pure ()
 
_CheckText ::
  forall a.
    (RTS.DDL a,
     RTS.HasUnion a "string" (Vector.Vector (RTS.UInt 8))) =>
      a -> D.Parser ()
 
_CheckText (v :: a) =
  RTS.pIsJust_ "14:5--14:15" "Expected `string`"
    (HS.getField @"string" v)
 
_EqNumber ::
  forall a d g.
    (RTS.DDL a, RTS.DDL d, RTS.DDL g, RTS.HasStruct a "num" d,
     RTS.HasStruct a "exp" g) =>
      a -> (a -> D.Parser ())
 
_EqNumber (x :: a) (y :: a) =
  do RTS.pGuard "26:5--26:18" "guard failed"
       (HS.getField @"num" x HS.== HS.getField @"num" y)
     RTS.pGuard "26:21--26:34" "guard failed"
       (HS.getField @"exp" x HS.== HS.getField @"exp" y)
 
_IsValidated ::
      HS.Integer
        -> (HS.Integer -> (Vector.Vector (RTS.UInt 8) -> D.Parser ()))
 
_IsValidated (obj :: HS.Integer) (gen :: HS.Integer)
  (ty :: Vector.Vector (RTS.UInt 8)) =
  do HS.void
       (RTS.pEnter "PdfValidate.IsValidated" (pIsValidated obj gen ty))
     HS.pure ()
 
_StartValidating ::
      HS.Integer
        -> (HS.Integer -> (Vector.Vector (RTS.UInt 8) -> D.Parser ()))
 
_StartValidating (obj :: HS.Integer) (gen :: HS.Integer)
  (ty :: Vector.Vector (RTS.UInt 8)) =
  RTS.pEnter "PdfValidate.StartValidating"
    (pStartValidating obj gen ty)