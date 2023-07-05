{-# Language OverloadedStrings #-}
{-# Language ImplicitParams #-}

module Daedalus.Core.NoBitdata (noBitdata) where

import           Control.Arrow                   ((***))
import           Data.Bifunctor                  (first)
import           Data.Foldable                   (foldlM, foldrM)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (maybeToList)
import           Data.Text                       (Text)

import qualified Daedalus.BDD                    as BDD
import           Daedalus.Core
import           Daedalus.Core.Bitdata           (bdCase)
import           Daedalus.Core.TraverseUserTypes (mapUserTypes)
import           Daedalus.Core.Type              (sizeType, typeOf)
import           Daedalus.GUID                   (HasGUID, invalidGUID)
import           Daedalus.PP                     (showPP)
import           Daedalus.Panic                  (panic)
import           Daedalus.Rec                    (forgetRecs)

type CoerctionFuns = Map TName (FName, FName)

noBitdata :: HasGUID m => Module -> m Module
noBitdata mo =
  do let ts = map (fmap noBitdataTDecl) (mTypes mo)
     (cfuns, newFuns) <-
       let ?tdecls = Map.fromList [ (tName d, d) | d <- forgetRecs (mTypes mo) ]
       in foldlM mkFun mempty (forgetRecs (mTypes mo))

     let gfs = map (fmap (noBitdataG cfuns)) (mGFuns mo)
         ffs = map (fmap (noBitdataE cfuns)) (mFFuns mo)
         bfs = map (fmap (noBitdataB cfuns)) (mBFuns mo)
         
     -- This maintains the ordering, the newFuns are orderd, and the
     -- only deps are in the old funs
         ffs' = reverse newFuns ++ ffs
         mo'  = mo { mTypes = ts
                   , mFFuns = ffs'
                   , mBFuns = bfs
                   , mGFuns = gfs
                   }
     pure (mapUserTypes forgetBD mo')
  where
    mkFun (seen, acc) TDecl { tName = tn, tDef = TBitdata univ bdef } = do
      fns <- makeCoercionFuns seen tn univ bdef
      let seen' = Map.insert tn (fName *** fName $ fns) seen
          acc'  = fst fns : snd fns : acc
      pure (seen', acc')
    mkFun acc _ = pure acc

    -- TNames occur in the decls, and in UsertTypes (only) so this is
    -- enough to ensure we ha
    forgetBD ut = ut { utName = (utName ut) { tnameBD = False }}

noBitdataTDecl :: TDecl -> TDecl
noBitdataTDecl td =
  td { tName = (tName td) { tnameBD = False }
     , tDef = noBitdataTDef (tDef td)
     }

noBitdataTDef :: TDef -> TDef
noBitdataTDef tdef =
  case tdef of
    TBitdata _bdd bdef ->
      let ctor = case bdef of
            BDUnion  {} -> TUnion
            BDStruct {} -> TStruct
      in ctor (getFields bdef)
    _ -> tdef

-- noBitdataBFun :: HasGUID m => Fun ByteSet -> m (Fun Expr)
-- noBitdataBFun fu =
--   do x <- freshNameSys (TUInt (TSize 8))
--      def <- traverse (flip desugarByteSet (Var x)) (fDef fu)
--      pure fu { fParams = x : fParams fu
--              , fDef = def
--              }

noBitdataG :: CoerctionFuns -> Grammar -> Grammar
noBitdataG cfuns g =
  gebMapChildrenG (noBitdataG cfuns) (noBitdataE cfuns) (noBitdataB cfuns) g

noBitdataE :: CoerctionFuns -> Expr -> Expr
noBitdataE cfuns e =
  case mapChildrenE (noBitdataE cfuns) e of
    Ap1 (CoerceTo ty@(TUser ut)) e' 
      | tnameBD (utName ut) -> bitsToTy cfuns ty e'

    Ap1 (CoerceTo ty) e'
      | TUser ut <- typeOf e'
      , tnameBD (utName ut) -> tyToBits cfuns ty e'

    e' -> e'

noBitdataB :: CoerctionFuns -> ByteSet -> ByteSet
noBitdataB cfuns e =
  ebMapChildrenB (noBitdataE cfuns) (noBitdataB cfuns) e

-- -----------------------------------------------------------------------------
-- Functions to do the actual coercion

-- | Returns functions for coersing to/from bitdata from uints.  Note
-- that only one function will be well-formed.
bitsToTy, tyToBits :: CoerctionFuns -> Type -> Expr -> Expr
bitsToTy _seen ty x | typeOf x == ty = x
bitsToTy seen ty x =
  case ty of
    TUser ut
      | Just (tofn, _fromfn) <- Map.lookup (utName ut) seen -> callF tofn [x]
      | otherwise -> panic "Missing bitdata coercion (to) function" [showPP (utName ut)]
    _ -> coerceTo ty x

tyToBits _seen ty x | typeOf x == ty = x
tyToBits seen ty x =
  case typeOf x of
    TUser ut
      | Just (_tofn, fromfn) <- Map.lookup (utName ut) seen -> callF fromfn [x]
      | otherwise -> panic "Missing bitdata coercion (from) function" [showPP (utName ut)]
    _ -> coerceTo ty x

-- | To be called after the check (inserted by the DDL2Core pass)
makeCoercionFuns :: (HasGUID m, ?tdecls :: Map TName TDecl) =>
                    CoerctionFuns -> TName -> BDD.Pat -> BitdataDef ->
                    m (Fun Expr, Fun Expr)
makeCoercionFuns seen tn univ bdef = do
  to <- makeCoercionFun "to" tn bdef bitTy bdTy (\flds e -> pure (mkToStruct flds e)) mkToUnion
  from <- makeCoercionFun "from" tn bdef bdTy bitTy (\flds e -> pure (mkFromStruct flds e)) (\flds x -> pure (mkFromUnion flds x))
  pure (to, from)
  where
    mkToStruct flds n =
      Struct ut [ (l, mkToField fld ty n)
                | fld@BDField { bdFieldType = BDData l ty } <- flds
                ]

    mkToField fld ty n =
      let offe = intL (fromIntegral (bdOffset fld)) sizeType
          shifted
            | bdOffset fld == 0 = n
            | otherwise         = rShift n offe
          ty' = tWord' (bdWidth fld)
      in bitsToTy seen ty (coerceTo ty' shifted)

    -- Just cat everything together
    mkFromStruct flds n = foldl1 cat (map (mkFromField n) flds)

    mkFromField n fld =
      let ty' = tWord' (bdWidth fld)
      in case bdFieldType fld of
        BDWild      -> intL 0 ty'
        BDTag i     -> intL i ty'
        -- Note that he type doesn't change, just the defn., so we can
        -- reuse ty here.
        BDData l ty -> tyToBits seen ty' (selStruct ty l n)

    mkToUnion flds x = do
      let flds' = [ (l, inUnion ut l $ bitsToTy seen ty (Var x))
                  | (l, ty) <- flds ]
          maskVals = bdCase tn flds' Nothing

          mkOne (mask, alts) m_rest = do
            n <- freshNameSys bitTy
            let pats = map (first PNum) alts ++
                       [ (PAny, rest) | rest <- maybeToList m_rest ]
                cases =
                  coreLet n (bitAnd (Var x) (intL mask bitTy)) (coreCase n pats)
            pure (Just cases)

      r <- foldrM mkOne Nothing maskVals
      case r of
        Nothing -> panic "Empty bitdata union" []
        Just r' -> pure r'

    mkFromUnion flds x =
      let flds' = [ (PCon l, tyToBits seen bitTy (fromUnion ty l (Var x)))
                  | (l, ty) <- flds ]
      in ECase (Case x flds')

    tWord' = tWord . fromIntegral
    bitTy = tWord' (BDD.width univ)
    bdTy  = TUser ut
    ut    = UserType { utName    = tn
                     , utNumArgs = []
                     , utTyArgs  = []
                     }

makeCoercionFun :: (HasGUID m, ?tdecls :: Map TName TDecl) =>
                   Text -> TName -> BitdataDef ->Type -> Type ->
                   ([BDField] -> Expr -> m Expr) ->
                   ([(Label, Type)] -> Name -> m Expr) ->
                   m (Fun Expr)
makeCoercionFun pfx tn bdef argTy retTy mkS mkU = do
  n  <- freshName n0
  fn <- freshFName fn0

  def <- case bdef of
           BDStruct flds -> mkS flds (Var n)
           BDUnion  flds -> mkU flds n

  pure Fun { fName    = fn
           , fParams  = [n]
           , fDef     = Def def
           , fIsEntry = False
           , fAnnot   = []
           }
  where
    n0 = Name { nameId = invalidGUID
              , nameText = Just "from"
              , nameType = argTy
              }

    fname = pfx <> tnameText tn

    fn0 = FName { fnameId     = invalidGUID
                , fnameText   = fname
                , fnamePublic = False
                , fnameType   = retTy
                , fnameMod    = tnameMod tn
                }
