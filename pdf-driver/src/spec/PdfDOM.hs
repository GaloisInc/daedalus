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
module PdfDOM where
 
import qualified PdfMonad as D
import qualified PdfDecl
import qualified PdfValidate
import qualified PdfValue
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Input as RTS
import qualified RTS.Map as Map
import qualified RTS.Vector as Vector
 
 
pPdfPageObject :: PdfValue.Ref -> (PdfValue.Value -> D.Parser ())
 
pPdfPageObject (parent :: PdfValue.Ref) (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "54:8--54:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Page"))
     (__ :: ()) <-
       do (_2 :: HS.Bool) <-
            do (_1 :: PdfValue.Ref) <-
                 do (_0 :: PdfValue.Value) <-
                      RTS.pIsJust "58:10--58:26"
                        ("Missing key: "
                           HS.++ HS.show
                                   (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
                        (Map.lookup (Vector.vecFromRep "Parent") d)
                    RTS.pIsJust "58:10--58:33" "Expected `ref`" (HS.getField @"ref" _0)
               HS.pure (_1 HS.== parent)
          RTS.pEnter "PdfValue.Guard" (PdfValue.pGuard _2)
     HS.pure __
 
pPdfPageTreeNode ::
      PdfValue.Ref
        -> (HS.Maybe PdfValue.Ref -> (PdfValue.Value -> D.Parser ()))
 
pPdfPageTreeNode (self :: PdfValue.Ref)
  (parent :: HS.Maybe PdfValue.Ref)
  (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "26:8--26:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Pages"))
     do (_5 :: HS.Bool) <-
          do (_4 :: HS.Integer) <-
               do (_3 :: PdfValue.Value) <-
                    RTS.pIsJust "30:22--30:37"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "Count" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "Count") d)
                  RTS.pEnter "PdfValidate.PdfInteger" (PdfValidate.pPdfInteger _3)
             HS.pure ((RTS.lit 0 :: HS.Integer) HS.<= _4)
        RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _5)
     (RTS.<||)
       (do (p :: PdfValue.Ref) <-
             RTS.pIsJust "34:10--34:23" "Expected `Just`" parent
           RTS.pErrorMode RTS.Abort
             (do (_8 :: HS.Bool) <-
                   do (_7 :: PdfValue.Ref) <-
                        do (_6 :: PdfValue.Value) <-
                             RTS.pIsJust "36:12--36:28"
                               ("Missing key: "
                                  HS.++ HS.show
                                          (Vector.vecFromRep "Parent"
                                             :: Vector.Vector (RTS.UInt 8)))
                               (Map.lookup (Vector.vecFromRep "Parent") d)
                           RTS.pIsJust "36:12--36:35" "Expected `ref`" (HS.getField @"ref" _6)
                      HS.pure (_7 HS.== p)
                 RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _8)))
       (HS.pure ())
     (__ :: ()) <-
       do (kids :: Vector.Vector PdfValue.Value) <-
            do (_9 :: PdfValue.Value) <-
                 RTS.pIsJust "40:14--40:28"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "Kids") d)
               RTS.pIsJust "40:14--40:37" "Expected `array`"
                 (HS.getField @"array" _9)
          (__ :: ()) <-
            RTS.loopFoldM
              (\(s :: ()) (v :: PdfValue.Value) ->
                 do (kid :: PdfValue.Ref) <-
                      RTS.pIsJust "42:14--42:21" "Expected `ref`" (HS.getField @"ref" v)
                    (__ :: ()) <-
                      (RTS.<||)
                        (do (__ :: ()) <-
                              RTS.pEnter "PdfValidate.CheckRef"
                                (PdfValidate.pCheckRef (Vector.vecFromRep "PageTreeNode")
                                   (pPdfPageTreeNode kid (HS.Just self))
                                   kid)
                            HS.pure __)
                        (do (__ :: ()) <-
                              RTS.pEnter "PdfValidate.CheckRef"
                                (PdfValidate.pCheckRef (Vector.vecFromRep "PageObject")
                                   (pPdfPageObject self)
                                   kid)
                            HS.pure __)
                    HS.pure __)
              ()
              kids
          HS.pure __
     HS.pure __
 
pPdfCatalog :: PdfValue.Value -> D.Parser ()
 
pPdfCatalog (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "13:8--13:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Catalog"))
     (__ :: ()) <-
       do (ref :: PdfValue.Ref) <-
            do (_10 :: PdfValue.Value) <-
                 RTS.pIsJust "17:12--17:27"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "Pages" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "Pages") d)
               RTS.pIsJust "17:12--17:34" "Expected `ref`"
                 (HS.getField @"ref" _10)
          (__ :: ()) <-
            RTS.pEnter "PdfValidate.CheckRef"
              (PdfValidate.pCheckRef (Vector.vecFromRep "PageTreeNodeRoot")
                 (pPdfPageTreeNode ref (HS.Nothing :: HS.Maybe PdfValue.Ref))
                 ref)
          HS.pure __
     HS.pure __
 
pPdfTrailer ::
  forall a.
    (RTS.DDL a, RTS.HasStruct a "root" (HS.Maybe PdfValue.Ref)) =>
      a -> D.Parser ()
 
pPdfTrailer (t :: a) =
  do (_11 :: PdfValue.Ref) <-
       RTS.pIsJust "7:34--7:47" "Expected `Just`" (HS.getField @"root" t)
     RTS.pEnter "PdfValidate.CheckRef"
       (PdfValidate.pCheckRef (Vector.vecFromRep "Catalog") pPdfCatalog
          _11)
 
_PdfCatalog :: PdfValue.Value -> D.Parser ()
 
_PdfCatalog (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "13:8--13:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Catalog"))
     (ref :: PdfValue.Ref) <-
       do (_10 :: PdfValue.Value) <-
            RTS.pIsJust "17:12--17:27"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Pages" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Pages") d)
          RTS.pIsJust "17:12--17:34" "Expected `ref`"
            (HS.getField @"ref" _10)
     RTS.pEnter "PdfValidate._CheckRef"
       (PdfValidate._CheckRef (Vector.vecFromRep "PageTreeNodeRoot")
          (pPdfPageTreeNode ref (HS.Nothing :: HS.Maybe PdfValue.Ref))
          ref)
 
_PdfPageObject :: PdfValue.Ref -> (PdfValue.Value -> D.Parser ())
 
_PdfPageObject (parent :: PdfValue.Ref) (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "54:8--54:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Page"))
     (_2 :: HS.Bool) <-
       do (_1 :: PdfValue.Ref) <-
            do (_0 :: PdfValue.Value) <-
                 RTS.pIsJust "58:10--58:26"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "Parent") d)
               RTS.pIsJust "58:10--58:33" "Expected `ref`" (HS.getField @"ref" _0)
          HS.pure (_1 HS.== parent)
     RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _2)
 
_PdfPageTreeNode ::
      PdfValue.Ref
        -> (HS.Maybe PdfValue.Ref -> (PdfValue.Value -> D.Parser ()))
 
_PdfPageTreeNode (self :: PdfValue.Ref)
  (parent :: HS.Maybe PdfValue.Ref)
  (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "26:8--26:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Pages"))
     do (_5 :: HS.Bool) <-
          do (_4 :: HS.Integer) <-
               do (_3 :: PdfValue.Value) <-
                    RTS.pIsJust "30:22--30:37"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "Count" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "Count") d)
                  RTS.pEnter "PdfValidate.PdfInteger" (PdfValidate.pPdfInteger _3)
             HS.pure ((RTS.lit 0 :: HS.Integer) HS.<= _4)
        RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _5)
     (RTS.<||)
       (do (p :: PdfValue.Ref) <-
             RTS.pIsJust "34:10--34:23" "Expected `Just`" parent
           RTS.pErrorMode RTS.Abort
             (do (_8 :: HS.Bool) <-
                   do (_7 :: PdfValue.Ref) <-
                        do (_6 :: PdfValue.Value) <-
                             RTS.pIsJust "36:12--36:28"
                               ("Missing key: "
                                  HS.++ HS.show
                                          (Vector.vecFromRep "Parent"
                                             :: Vector.Vector (RTS.UInt 8)))
                               (Map.lookup (Vector.vecFromRep "Parent") d)
                           RTS.pIsJust "36:12--36:35" "Expected `ref`" (HS.getField @"ref" _6)
                      HS.pure (_7 HS.== p)
                 RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _8)))
       (HS.pure ())
     (kids :: Vector.Vector PdfValue.Value) <-
       do (_9 :: PdfValue.Value) <-
            RTS.pIsJust "40:14--40:28"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Kids") d)
          RTS.pIsJust "40:14--40:37" "Expected `array`"
            (HS.getField @"array" _9)
     RTS.loopFoldM
       (\(s :: ()) (v :: PdfValue.Value) ->
          do (kid :: PdfValue.Ref) <-
               RTS.pIsJust "42:14--42:21" "Expected `ref`" (HS.getField @"ref" v)
             (RTS.<||)
               (RTS.pEnter "PdfValidate._CheckRef"
                  (PdfValidate._CheckRef (Vector.vecFromRep "PageTreeNode")
                     (pPdfPageTreeNode kid (HS.Just self))
                     kid))
               (RTS.pEnter "PdfValidate._CheckRef"
                  (PdfValidate._CheckRef (Vector.vecFromRep "PageObject")
                     (pPdfPageObject self)
                     kid)))
       ()
       kids
 
_PdfTrailer ::
  forall a.
    (RTS.DDL a, RTS.HasStruct a "root" (HS.Maybe PdfValue.Ref)) =>
      a -> D.Parser ()
 
_PdfTrailer (t :: a) =
  do (_11 :: PdfValue.Ref) <-
       RTS.pIsJust "7:34--7:47" "Expected `Just`" (HS.getField @"root" t)
     RTS.pEnter "PdfValidate._CheckRef"
       (PdfValidate._CheckRef (Vector.vecFromRep "Catalog") pPdfCatalog
          _11)