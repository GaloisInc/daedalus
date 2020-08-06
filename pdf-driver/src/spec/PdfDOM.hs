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
import qualified PdfValidate
import qualified PdfValue
import qualified PdfDecl
import qualified PdfXRef
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
       RTS.pIsJust "61:8--61:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Page"))
     (__ :: ()) <-
       do (val :: PdfValue.Value) <-
            RTS.pIsJust "65:12--65:28"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Parent") d)
          (ref :: PdfValue.Ref) <-
            RTS.pIsJust "66:12--66:21" "Expected `ref`"
              (HS.getField @"ref" val)
          (__ :: ()) <-
            RTS.pGuard "67:5--67:17" "guard failed" (parent HS.== ref)
          HS.pure __
     HS.pure __
 
pPdfPageTreeNode ::
      PdfValue.Ref
        -> (HS.Maybe PdfValue.Ref -> (PdfValue.Value -> D.Parser ()))
 
pPdfPageTreeNode (self :: PdfValue.Ref)
  (parent :: HS.Maybe PdfValue.Ref)
  (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "27:8--27:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Pages"))
     do (countV :: PdfValue.Value) <-
          RTS.pIsJust "31:15--31:30"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "Count" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "Count") d)
        (count :: HS.Integer) <-
          RTS.pEnter "PdfValidate.PdfInteger"
            (PdfValidate.pPdfInteger countV)
        RTS.pGuard "33:5--33:14" "guard failed"
          ((RTS.lit 0 :: HS.Integer) HS.<= count)
     RTS.pEnter "PdfDecl._Default"
       (PdfDecl._Default @()
          (do (p :: PdfValue.Ref) <-
                RTS.pIsJust "38:10--38:23" "Expected `Just`" parent
              RTS.pErrorMode RTS.Abort
                (do (val :: PdfValue.Value) <-
                      RTS.pIsJust "40:12--40:28"
                        ("Missing key: "
                           HS.++ HS.show
                                   (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
                        (Map.lookup (Vector.vecFromRep "Parent") d)
                    (ref :: PdfValue.Ref) <-
                      RTS.pIsJust "41:12--41:21" "Expected `ref`"
                        (HS.getField @"ref" val)
                    RTS.pGuard "42:5--42:12" "guard failed" (p HS.== ref))))
     (__ :: ()) <-
       do (kidsV :: PdfValue.Value) <-
            RTS.pIsJust "46:14--46:28"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Kids") d)
          (kids :: Vector.Vector PdfValue.Value) <-
            RTS.pIsJust "47:14--47:27" "Expected `array`"
              (HS.getField @"array" kidsV)
          (__ :: ()) <-
            RTS.loopFoldM
              (\(s :: ()) (v :: PdfValue.Value) ->
                 do (kid :: PdfValue.Ref) <-
                      RTS.pIsJust "49:14--49:21" "Expected `ref`" (HS.getField @"ref" v)
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
       RTS.pIsJust "15:8--15:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Catalog"))
     (__ :: ()) <-
       do (pages :: PdfValue.Value) <-
            RTS.pIsJust "19:14--19:29"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Pages" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Pages") d)
          (ref :: PdfValue.Ref) <-
            RTS.pIsJust "20:14--20:25" "Expected `ref`"
              (HS.getField @"ref" pages)
          (__ :: ()) <-
            RTS.pEnter "PdfValidate.CheckRef"
              (PdfValidate.pCheckRef (Vector.vecFromRep "PageTreeNodeRoot")
                 (pPdfPageTreeNode ref (HS.Nothing :: HS.Maybe PdfValue.Ref))
                 ref)
          HS.pure __
     HS.pure __
 
pDOMTrailer :: PdfXRef.TrailerDict -> D.Parser ()
 
pDOMTrailer (t :: PdfXRef.TrailerDict) =
  do (ref :: PdfValue.Ref) <-
       RTS.pIsJust "8:10--8:23" "Expected `Just`" (HS.getField @"root" t)
     (__ :: ()) <-
       RTS.pEnter "PdfValidate.CheckRef"
         (PdfValidate.pCheckRef (Vector.vecFromRep "Catalog") pPdfCatalog
            ref)
     HS.pure __
 
_DOMTrailer :: PdfXRef.TrailerDict -> D.Parser ()
 
_DOMTrailer (t :: PdfXRef.TrailerDict) =
  do (ref :: PdfValue.Ref) <-
       RTS.pIsJust "8:10--8:23" "Expected `Just`" (HS.getField @"root" t)
     RTS.pEnter "PdfValidate._CheckRef"
       (PdfValidate._CheckRef (Vector.vecFromRep "Catalog") pPdfCatalog
          ref)
 
_PdfCatalog :: PdfValue.Value -> D.Parser ()
 
_PdfCatalog (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "15:8--15:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Catalog"))
     (pages :: PdfValue.Value) <-
       RTS.pIsJust "19:14--19:29"
         ("Missing key: "
            HS.++ HS.show
                    (Vector.vecFromRep "Pages" :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup (Vector.vecFromRep "Pages") d)
     (ref :: PdfValue.Ref) <-
       RTS.pIsJust "20:14--20:25" "Expected `ref`"
         (HS.getField @"ref" pages)
     RTS.pEnter "PdfValidate._CheckRef"
       (PdfValidate._CheckRef (Vector.vecFromRep "PageTreeNodeRoot")
          (pPdfPageTreeNode ref (HS.Nothing :: HS.Maybe PdfValue.Ref))
          ref)
 
_PdfPageObject :: PdfValue.Ref -> (PdfValue.Value -> D.Parser ())
 
_PdfPageObject (parent :: PdfValue.Ref) (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "61:8--61:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Page"))
     (val :: PdfValue.Value) <-
       RTS.pIsJust "65:12--65:28"
         ("Missing key: "
            HS.++ HS.show
                    (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup (Vector.vecFromRep "Parent") d)
     (ref :: PdfValue.Ref) <-
       RTS.pIsJust "66:12--66:21" "Expected `ref`"
         (HS.getField @"ref" val)
     RTS.pGuard "67:5--67:17" "guard failed" (parent HS.== ref)
 
_PdfPageTreeNode ::
      PdfValue.Ref
        -> (HS.Maybe PdfValue.Ref -> (PdfValue.Value -> D.Parser ()))
 
_PdfPageTreeNode (self :: PdfValue.Ref)
  (parent :: HS.Maybe PdfValue.Ref)
  (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "27:8--27:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Pages"))
     do (countV :: PdfValue.Value) <-
          RTS.pIsJust "31:15--31:30"
            ("Missing key: "
               HS.++ HS.show
                       (Vector.vecFromRep "Count" :: Vector.Vector (RTS.UInt 8)))
            (Map.lookup (Vector.vecFromRep "Count") d)
        (count :: HS.Integer) <-
          RTS.pEnter "PdfValidate.PdfInteger"
            (PdfValidate.pPdfInteger countV)
        RTS.pGuard "33:5--33:14" "guard failed"
          ((RTS.lit 0 :: HS.Integer) HS.<= count)
     RTS.pEnter "PdfDecl._Default"
       (PdfDecl._Default @()
          (do (p :: PdfValue.Ref) <-
                RTS.pIsJust "38:10--38:23" "Expected `Just`" parent
              RTS.pErrorMode RTS.Abort
                (do (val :: PdfValue.Value) <-
                      RTS.pIsJust "40:12--40:28"
                        ("Missing key: "
                           HS.++ HS.show
                                   (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
                        (Map.lookup (Vector.vecFromRep "Parent") d)
                    (ref :: PdfValue.Ref) <-
                      RTS.pIsJust "41:12--41:21" "Expected `ref`"
                        (HS.getField @"ref" val)
                    RTS.pGuard "42:5--42:12" "guard failed" (p HS.== ref))))
     (kidsV :: PdfValue.Value) <-
       RTS.pIsJust "46:14--46:28"
         ("Missing key: "
            HS.++ HS.show
                    (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
         (Map.lookup (Vector.vecFromRep "Kids") d)
     (kids :: Vector.Vector PdfValue.Value) <-
       RTS.pIsJust "47:14--47:27" "Expected `array`"
         (HS.getField @"array" kidsV)
     RTS.loopFoldM
       (\(s :: ()) (v :: PdfValue.Value) ->
          do (kid :: PdfValue.Ref) <-
               RTS.pIsJust "49:14--49:21" "Expected `ref`" (HS.getField @"ref" v)
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