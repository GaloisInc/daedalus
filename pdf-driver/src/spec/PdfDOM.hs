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
       case v of
         PdfValue.Value_dict
           (_1506
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _1506
         _ -> RTS.pError RTS.FromSystem "54:8--54:16"
                "Pattern match failure"
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Page"))
     (__ :: ()) <-
       do (_1511 :: HS.Bool) <-
            do (_1510 :: PdfValue.Ref) <-
                 do (_1508 :: PdfValue.Value) <-
                      RTS.pIsJust "58:10--58:26"
                        ("Missing key: "
                           HS.++ HS.show
                                   (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
                        (Map.lookup (Vector.vecFromRep "Parent") d)
                    case _1508 of
                      PdfValue.Value_ref (_1509 :: PdfValue.Ref) -> HS.pure _1509
                      _ -> RTS.pError RTS.FromSystem "58:10--58:33"
                             "Pattern match failure"
               HS.pure (_1510 HS.== parent)
          RTS.pEnter "PdfValue.Guard" (PdfValue.pGuard _1511)
     HS.pure __
 
pPdfPageTreeNode ::
      PdfValue.Ref
        -> (HS.Maybe PdfValue.Ref -> (PdfValue.Value -> D.Parser ()))
 
pPdfPageTreeNode (self :: PdfValue.Ref)
  (parent :: HS.Maybe PdfValue.Ref)
  (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case v of
         PdfValue.Value_dict
           (_1512
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _1512
         _ -> RTS.pError RTS.FromSystem "26:8--26:16"
                "Pattern match failure"
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Pages"))
     do (_1515 :: HS.Bool) <-
          do (_1514 :: HS.Integer) <-
               do (_1513 :: PdfValue.Value) <-
                    RTS.pIsJust "30:22--30:37"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "Count" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "Count") d)
                  RTS.pEnter "PdfValidate.PdfInteger" (PdfValidate.pPdfInteger _1513)
             HS.pure ((RTS.lit 0 :: HS.Integer) HS.<= _1514)
        RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _1515)
     (RTS.<||)
       (do (p :: PdfValue.Ref) <-
             case parent of
               HS.Just (_1516 :: PdfValue.Ref) -> HS.pure _1516
               _ -> RTS.pError RTS.FromSystem "34:10--34:23"
                      "Pattern match failure"
           RTS.pErrorMode RTS.Abort
             (do (_1521 :: HS.Bool) <-
                   do (_1520 :: PdfValue.Ref) <-
                        do (_1518 :: PdfValue.Value) <-
                             RTS.pIsJust "36:12--36:28"
                               ("Missing key: "
                                  HS.++ HS.show
                                          (Vector.vecFromRep "Parent"
                                             :: Vector.Vector (RTS.UInt 8)))
                               (Map.lookup (Vector.vecFromRep "Parent") d)
                           case _1518 of
                             PdfValue.Value_ref (_1519 :: PdfValue.Ref) -> HS.pure _1519
                             _ -> RTS.pError RTS.FromSystem "36:12--36:35"
                                    "Pattern match failure"
                      HS.pure (_1520 HS.== p)
                 RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _1521)))
       (HS.pure ())
     (__ :: ()) <-
       do (kids :: Vector.Vector PdfValue.Value) <-
            do (_1523 :: PdfValue.Value) <-
                 RTS.pIsJust "40:14--40:28"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "Kids") d)
               case _1523 of
                 PdfValue.Value_array
                   (_1524 :: Vector.Vector PdfValue.Value) -> HS.pure _1524
                 _ -> RTS.pError RTS.FromSystem "40:14--40:37"
                        "Pattern match failure"
          (__ :: ()) <-
            RTS.loopFoldM
              (\(s :: ()) (v :: PdfValue.Value) ->
                 do (kid :: PdfValue.Ref) <-
                      case v of
                        PdfValue.Value_ref (_1526 :: PdfValue.Ref) -> HS.pure _1526
                        _ -> RTS.pError RTS.FromSystem "42:14--42:21"
                               "Pattern match failure"
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
       case v of
         PdfValue.Value_dict
           (_1530
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _1530
         _ -> RTS.pError RTS.FromSystem "13:8--13:16"
                "Pattern match failure"
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Catalog"))
     (__ :: ()) <-
       do (ref :: PdfValue.Ref) <-
            do (_1532 :: PdfValue.Value) <-
                 RTS.pIsJust "17:12--17:27"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "Pages" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "Pages") d)
               case _1532 of
                 PdfValue.Value_ref (_1533 :: PdfValue.Ref) -> HS.pure _1533
                 _ -> RTS.pError RTS.FromSystem "17:12--17:34"
                        "Pattern match failure"
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
  do (_1536 :: PdfValue.Ref) <-
       case HS.getField @"root" t of
         HS.Just (_1535 :: PdfValue.Ref) -> HS.pure _1535
         _ -> RTS.pError RTS.FromSystem "7:34--7:47" "Pattern match failure"
     RTS.pEnter "PdfValidate.CheckRef"
       (PdfValidate.pCheckRef (Vector.vecFromRep "Catalog") pPdfCatalog
          _1536)
 
_PdfPageObject :: PdfValue.Ref -> (PdfValue.Value -> D.Parser ())
 
_PdfPageObject (parent :: PdfValue.Ref) (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case v of
         PdfValue.Value_dict
           (_1506
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _1506
         _ -> RTS.pError RTS.FromSystem "54:8--54:16"
                "Pattern match failure"
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Page"))
     (_1511 :: HS.Bool) <-
       do (_1510 :: PdfValue.Ref) <-
            do (_1508 :: PdfValue.Value) <-
                 RTS.pIsJust "58:10--58:26"
                   ("Missing key: "
                      HS.++ HS.show
                              (Vector.vecFromRep "Parent" :: Vector.Vector (RTS.UInt 8)))
                   (Map.lookup (Vector.vecFromRep "Parent") d)
               case _1508 of
                 PdfValue.Value_ref (_1509 :: PdfValue.Ref) -> HS.pure _1509
                 _ -> RTS.pError RTS.FromSystem "58:10--58:33"
                        "Pattern match failure"
          HS.pure (_1510 HS.== parent)
     RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _1511)
 
_PdfPageTreeNode ::
      PdfValue.Ref
        -> (HS.Maybe PdfValue.Ref -> (PdfValue.Value -> D.Parser ()))
 
_PdfPageTreeNode (self :: PdfValue.Ref)
  (parent :: HS.Maybe PdfValue.Ref)
  (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case v of
         PdfValue.Value_dict
           (_1512
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _1512
         _ -> RTS.pError RTS.FromSystem "26:8--26:16"
                "Pattern match failure"
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Pages"))
     do (_1515 :: HS.Bool) <-
          do (_1514 :: HS.Integer) <-
               do (_1513 :: PdfValue.Value) <-
                    RTS.pIsJust "30:22--30:37"
                      ("Missing key: "
                         HS.++ HS.show
                                 (Vector.vecFromRep "Count" :: Vector.Vector (RTS.UInt 8)))
                      (Map.lookup (Vector.vecFromRep "Count") d)
                  RTS.pEnter "PdfValidate.PdfInteger" (PdfValidate.pPdfInteger _1513)
             HS.pure ((RTS.lit 0 :: HS.Integer) HS.<= _1514)
        RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _1515)
     (RTS.<||)
       (do (p :: PdfValue.Ref) <-
             case parent of
               HS.Just (_1516 :: PdfValue.Ref) -> HS.pure _1516
               _ -> RTS.pError RTS.FromSystem "34:10--34:23"
                      "Pattern match failure"
           RTS.pErrorMode RTS.Abort
             (do (_1521 :: HS.Bool) <-
                   do (_1520 :: PdfValue.Ref) <-
                        do (_1518 :: PdfValue.Value) <-
                             RTS.pIsJust "36:12--36:28"
                               ("Missing key: "
                                  HS.++ HS.show
                                          (Vector.vecFromRep "Parent"
                                             :: Vector.Vector (RTS.UInt 8)))
                               (Map.lookup (Vector.vecFromRep "Parent") d)
                           case _1518 of
                             PdfValue.Value_ref (_1519 :: PdfValue.Ref) -> HS.pure _1519
                             _ -> RTS.pError RTS.FromSystem "36:12--36:35"
                                    "Pattern match failure"
                      HS.pure (_1520 HS.== p)
                 RTS.pEnter "PdfValue._Guard" (PdfValue._Guard _1521)))
       (HS.pure ())
     (kids :: Vector.Vector PdfValue.Value) <-
       do (_1523 :: PdfValue.Value) <-
            RTS.pIsJust "40:14--40:28"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Kids" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Kids") d)
          case _1523 of
            PdfValue.Value_array
              (_1524 :: Vector.Vector PdfValue.Value) -> HS.pure _1524
            _ -> RTS.pError RTS.FromSystem "40:14--40:37"
                   "Pattern match failure"
     RTS.loopFoldM
       (\(s :: ()) (v :: PdfValue.Value) ->
          do (kid :: PdfValue.Ref) <-
               case v of
                 PdfValue.Value_ref (_1526 :: PdfValue.Ref) -> HS.pure _1526
                 _ -> RTS.pError RTS.FromSystem "42:14--42:21"
                        "Pattern match failure"
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
 
_PdfCatalog :: PdfValue.Value -> D.Parser ()
 
_PdfCatalog (v :: PdfValue.Value) =
  do (d :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) <-
       case v of
         PdfValue.Value_dict
           (_1530
              :: Map.Map (Vector.Vector (RTS.UInt 8)) PdfValue.Value) -> HS.pure
                                                                           _1530
         _ -> RTS.pError RTS.FromSystem "13:8--13:16"
                "Pattern match failure"
     RTS.pEnter "PdfValidate._PdfType"
       (PdfValidate._PdfType d (Vector.vecFromRep "Catalog"))
     (ref :: PdfValue.Ref) <-
       do (_1532 :: PdfValue.Value) <-
            RTS.pIsJust "17:12--17:27"
              ("Missing key: "
                 HS.++ HS.show
                         (Vector.vecFromRep "Pages" :: Vector.Vector (RTS.UInt 8)))
              (Map.lookup (Vector.vecFromRep "Pages") d)
          case _1532 of
            PdfValue.Value_ref (_1533 :: PdfValue.Ref) -> HS.pure _1533
            _ -> RTS.pError RTS.FromSystem "17:12--17:34"
                   "Pattern match failure"
     RTS.pEnter "PdfValidate._CheckRef"
       (PdfValidate._CheckRef (Vector.vecFromRep "PageTreeNodeRoot")
          (pPdfPageTreeNode ref (HS.Nothing :: HS.Maybe PdfValue.Ref))
          ref)
 
_PdfTrailer ::
  forall a.
    (RTS.DDL a, RTS.HasStruct a "root" (HS.Maybe PdfValue.Ref)) =>
      a -> D.Parser ()
 
_PdfTrailer (t :: a) =
  do (_1536 :: PdfValue.Ref) <-
       case HS.getField @"root" t of
         HS.Just (_1535 :: PdfValue.Ref) -> HS.pure _1535
         _ -> RTS.pError RTS.FromSystem "7:34--7:47" "Pattern match failure"
     RTS.pEnter "PdfValidate._CheckRef"
       (PdfValidate._CheckRef (Vector.vecFromRep "Catalog") pPdfCatalog
          _1536)