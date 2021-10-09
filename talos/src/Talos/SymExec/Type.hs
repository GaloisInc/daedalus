
--------------------------------------------------------------------------------
-- Types
--
-- Types are embedded direclty (without renaming), hence this is non-monadic

module Talos.SymExec.Type where

-- import Data.Map (Map)
import           Control.Monad.Reader
import           Data.Set                        (Set)
import qualified Data.Set                        as Set

import           SimpleSMT                       (SExpr)
import qualified SimpleSMT                       as S

import           Daedalus.Core                   hiding (freshName)
import           Daedalus.Core.Free
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic

-- import Talos.Strategy.Monad
import           Talos.Analysis.Slice
import           Talos.SymExec.SolverT
import           Talos.SymExec.StdLib
  
symExecTName :: TName -> String
symExecTName = tnameToSMTName

labelToField :: TName -> Label -> String
labelToField n l = symExecTName n ++ "-" ++ show (pp l)

typeNameToCtor :: TName -> String
typeNameToCtor n = "mk-" ++ symExecTName n

-- | Construct SMT solver datatype declaration based on DaeDaLus type
-- declarations.

tranclTypeDefs :: Module -> Set TName -> [Rec SMTTypeDef]
tranclTypeDefs md roots0 = go [] roots0 (reverse (mTypes md))
  where
    go acc _ [] = acc
    go acc roots (NonRec td : rest) 
      | tName td `Set.member` roots =
        go (NonRec (tdeclToSMTTypeDef td) : acc) (Set.union roots (freeTCons td)) rest
      | otherwise                   = go acc roots rest
      
    go acc roots (MutRec tds : rest) 
      | any (\td -> tName td `Set.member` roots) tds = 
        go (MutRec (map tdeclToSMTTypeDef tds) : acc) (Set.unions (roots : map freeTCons tds)) rest
      | otherwise                   = go acc roots rest


-- we already have recs and it is ordered, so we don't need to calculate a fixpoint
sliceToSMTTypeDefs :: Module -> Slice -> [Rec SMTTypeDef]
sliceToSMTTypeDefs md = tranclTypeDefs md . freeTCons

tdeclToSMTTypeDef :: TDecl -> SMTTypeDef      
tdeclToSMTTypeDef td@(TDecl { tTParamKNumber = _ : _ }) =
  panic "Unsupported type decl (number params)" [showPP td]

tdeclToSMTTypeDef td@(TDecl { tTParamKValue = _ : _ }) =
  panic "Unsupported type decl (type params)" [showPP td]

tdeclToSMTTypeDef TDecl { tName = name, tTParamKNumber = [], tTParamKValue = [], tDef = td } =
  SMTTypeDef { stdName = name
             , stdBody = sflds
             }
  where
    sflds = case td of
              TStruct flds -> [(typeNameToCtor name, map mkOneS flds) ]
              TUnion flds  -> map mkOneU flds
    mkOneS (l, t) = (lblToFld l, symExecTy t)
    mkOneU (l, t) = (lblToFld l, [ ("get-" ++ lblToFld l, symExecTy t) ])
    lblToFld = labelToField name
      
defineSliceTypeDefs :: (MonadIO m, HasGUID m) => Module -> Slice -> SolverT m ()
defineSliceTypeDefs md sl = mapM_ defineSMTTypeDefs (sliceToSMTTypeDefs md sl)

symExecTy :: Type -> SExpr
symExecTy = go 
  where
    go ty' =
      let unimplemented = panic "Unsupported type" [showPP ty']
      in case ty' of
        TStream         -> unimplemented
        TUInt (TSize 8) -> tByte -- prettier in the output
        TUInt (TSize n) -> S.tBits n
        TUInt {}        -> panic "Non-constant bit size" [ showPP ty' ]
        TSInt (TSize n) -> S.tBits n
        TSInt {}        -> panic "Non-constant bit size" [ showPP ty' ]
        TInteger        -> S.tInt
        TBool           -> S.tBool
        TUnit           -> tUnit
        TArray t'       -> tArrayWithLength (go t')
        TMaybe t'       -> tMaybe (go t')
        TMap kt vt      -> tMap (go kt) (go vt)
        TBuilder elTy   -> tArrayWithLength (go elTy)
        TIterator (TArray elTy) -> tArrayIter (go elTy)
        TIterator {}    -> unimplemented -- map iterators
        TUser (UserType { utName = n, utNumArgs = [], utTyArgs = [] }) -> S.const (symExecTName n)
        TUser _ut       -> panic "Saw type with arguments" [showPP ty']
        TParam _x       -> panic "Saw type variable" [showPP ty']

typeDefault :: Type -> SExpr
typeDefault = go
  where
    go ty =
      let unimplemented = panic "Unsupported type" [showPP ty]
      in case ty of
        TStream         -> unimplemented
        TUInt (TSize n) -> sBitVec (fromIntegral n) 0
        TUInt {}        -> unimplemented
        TSInt (TSize n) -> sBitVec (fromIntegral n) 0
        TSInt {}        -> unimplemented
        TInteger        -> S.int 0
        TBool           -> S.bool False
        TUnit           -> sUnit
        TArray t'       -> sEmptyL (symExecTy t') (typeDefault t')
        TMaybe t'       -> sNothing (symExecTy t')
        TMap {}         -> unimplemented
        TBuilder elTy   -> go (TArray elTy) -- probably not needed?
        TIterator (TArray _elTy) -> unimplemented
        TIterator {}    -> unimplemented -- map iterators
        TUser UserType { utName = n, utNumArgs = [], utTyArgs = [] } ->
               S.const (typeNameToDefault n)
        TUser {}        -> unimplemented
        TParam {}       -> unimplemented
