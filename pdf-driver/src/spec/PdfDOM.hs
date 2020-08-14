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
       RTS.pIsJust "56:8--56:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Page"))
     (__ :: ()) <-
       do (ref :: PdfValue.Ref) <-
            do (_0 :: PdfValue.Value) <-
                 RTS.pIsJust "60:12--60:28"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "Parent") d)
               RTS.pIsJust "60:12--60:35" "Expected `ref`" (HS.getField @"ref" _0)
          (__ :: ()) <-
            RTS.pEnter "PdfValue.Guard" (PdfValue.pGuard (parent HS.== ref))
          HS.pure __
     HS.pure __
 
pPdfPageTreeNode ::
      PdfValue.Ref
        -> (HS.Maybe PdfValue.Ref -> (PdfValue.Value -> D.Parser ()))
 
pPdfPageTreeNode (self :: PdfValue.Ref)
  (parent :: HS.Maybe PdfValue.Ref)
  (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "25:8--25:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Pages"))
     do (i :: HS.Integer) <-
          do (_1 :: PdfValue.Value) <-
               RTS.pIsJust "29:22--29:37"
                 ("Missing key: "
                    HS.++ HS.show
                            (Vector.vecFromRep "Count" :: Vector.Vector (RTS.UInt 8)))
                 (Map.lookup (Vector.vecFromRep "Count") d)
             RTS.pEnter "PdfValidate.PdfInteger" (PdfValidate.pPdfInteger _1)
        RTS.pEnter "PdfValue._Guard"
          (PdfValue._Guard ((RTS.lit 0 :: HS.Integer) HS.<= i))
     RTS.pEnter "PdfDecl._Default"
       (PdfDecl._Default @()
          (do (p :: PdfValue.Ref) <-
                RTS.pIsJust "35:10--35:23" "Expected `Just`" parent
              RTS.pErrorMode RTS.Abort
                (do (ref :: PdfValue.Ref) <-
                      do (_2 :: PdfValue.Value) <-
                           RTS.pIsJust "37:12--37:28"
                             ("Missing key: "
                                HS.++ HS.show
                                        (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
                             (Map.lookup (Vector.vecFromRep "Parent") d)
                         RTS.pIsJust "37:12--37:35" "Expected `ref`" (HS.getField @"ref" _2)
                    RTS.pEnter "PdfValue._Guard" (PdfValue._Guard (ref HS.== p)))))
     (__ :: ()) <-
       do (kids :: Vector.Vector PdfValue.Value) <-
            do (_3 :: PdfValue.Value) <-
                 RTS.pIsJust "42:14--42:28"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "Kids") d)
               RTS.pIsJust "42:14--42:37" "Expected `array`"
                 (HS.getField @"array" _3)
          (__ :: ()) <-
            RTS.loopFoldM
              (\(s :: ()) (v :: PdfValue.Value) ->
                 do (kid :: PdfValue.Ref) <-
                      RTS.pIsJust "44:14--44:21" "Expected `ref`" (HS.getField @"ref" v)
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
       RTS.pIsJust "14:8--14:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Catalog"))
     (__ :: ()) <-
       do (ref :: PdfValue.Ref) <-
            do (_4 :: PdfValue.Value) <-
                 RTS.pIsJust "18:12--18:27"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "Pages" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "Pages") d)
               RTS.pIsJust "18:12--18:34" "Expected `ref`" (HS.getField @"ref" _4)
          (__ :: ()) <-
            RTS.pEnter "PdfValidate.CheckRef"
              (PdfValidate.pCheckRef (Vector.vecFromRep "PageTreeNodeRoot")
                 (pPdfPageTreeNode ref (HS.Nothing :: HS.Maybe PdfValue.Ref))
                 ref)
          HS.pure __
     HS.pure __
 
pPdfTrailer :: PdfXRef.TrailerDict -> D.Parser ()
 
pPdfTrailer (t :: PdfXRef.TrailerDict) =
  do (_5 :: PdfValue.Ref) <-
       RTS.pIsJust "8:34--8:47" "Expected `Just`" (HS.getField @"root" t)
     RTS.pEnter "PdfValidate.CheckRef"
       (PdfValidate.pCheckRef (Vector.vecFromRep "Catalog") pPdfCatalog
          _5)
 
_PdfCatalog :: PdfValue.Value -> D.Parser ()
 
_PdfCatalog (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "14:8--14:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Catalog"))
     (ref :: PdfValue.Ref) <-
       do (_4 :: PdfValue.Value) <-
            RTS.pIsJust "18:12--18:27"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Pages" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Pages") d)
          RTS.pIsJust "18:12--18:34" "Expected `ref`" (HS.getField @"ref" _4)
     RTS.pEnter "PdfValidate._CheckRef"
       (PdfValidate._CheckRef (Vector.vecFromRep "PageTreeNodeRoot")
          (pPdfPageTreeNode ref (HS.Nothing :: HS.Maybe PdfValue.Ref))
          ref)
 
_PdfPageObject :: PdfValue.Ref -> (PdfValue.Value -> D.Parser ())
 
_PdfPageObject (parent :: PdfValue.Ref) (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "56:8--56:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Page"))
     (ref :: PdfValue.Ref) <-
       do (_0 :: PdfValue.Value) <-
            RTS.pIsJust "60:12--60:28"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Parent") d)
          RTS.pIsJust "60:12--60:35" "Expected `ref`" (HS.getField @"ref" _0)
     RTS.pEnter "PdfValue._Guard" (PdfValue._Guard (parent HS.== ref))
 
_PdfPageTreeNode ::
      PdfValue.Ref
        -> (HS.Maybe PdfValue.Ref -> (PdfValue.Value -> D.Parser ()))
 
_PdfPageTreeNode (self :: PdfValue.Ref)
  (parent :: HS.Maybe PdfValue.Ref)
  (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       RTS.pIsJust "25:8--25:16" "Expected `dict`" (HS.getField @"dict" v)
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Pages"))
     do (i :: HS.Integer) <-
          do (_1 :: PdfValue.Value) <-
               RTS.pIsJust "29:22--29:37"
                 ("Missing key: "
                    HS.++ HS.show
                            (Vector.vecFromRep "Count" :: Vector.Vector (RTS.UInt 8)))
                 (Map.lookup (Vector.vecFromRep "Count") d)
             RTS.pEnter "PdfValidate.PdfInteger" (PdfValidate.pPdfInteger _1)
        RTS.pEnter "PdfValue._Guard"
          (PdfValue._Guard ((RTS.lit 0 :: HS.Integer) HS.<= i))
     RTS.pEnter "PdfDecl._Default"
       (PdfDecl._Default @()
          (do (p :: PdfValue.Ref) <-
                RTS.pIsJust "35:10--35:23" "Expected `Just`" parent
              RTS.pErrorMode RTS.Abort
                (do (ref :: PdfValue.Ref) <-
                      do (_2 :: PdfValue.Value) <-
                           RTS.pIsJust "37:12--37:28"
                             ("Missing key: "
                                HS.++ HS.show
                                        (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
                             (Map.lookup (Vector.vecFromRep "Parent") d)
                         RTS.pIsJust "37:12--37:35" "Expected `ref`" (HS.getField @"ref" _2)
                    RTS.pEnter "PdfValue._Guard" (PdfValue._Guard (ref HS.== p)))))
     (kids :: Vector.Vector PdfValue.Value) <-
       do (_3 :: PdfValue.Value) <-
            RTS.pIsJust "42:14--42:28"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Kids") d)
          RTS.pIsJust "42:14--42:37" "Expected `array`"
            (HS.getField @"array" _3)
     RTS.loopFoldM
       (\(s :: ()) (v :: PdfValue.Value) ->
          do (kid :: PdfValue.Ref) <-
               RTS.pIsJust "44:14--44:21" "Expected `ref`" (HS.getField @"ref" v)
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
 
_PdfTrailer :: PdfXRef.TrailerDict -> D.Parser ()
 
_PdfTrailer (t :: PdfXRef.TrailerDict) =
  do (_5 :: PdfValue.Ref) <-
       RTS.pIsJust "8:34--8:47" "Expected `Just`" (HS.getField @"root" t)
     RTS.pEnter "PdfValidate._CheckRef"
       (PdfValidate._CheckRef (Vector.vecFromRep "Catalog") pPdfCatalog
          _5)