{-# Language GeneralizedNewtypeDeriving, DefaultSignatures, TypeFamilies #-}

module Daedalus.Core.SpecialiseType (specialiseTypes) where

-- Instantiates polymorphic instances of types to be monomorphic.
-- Note that functions are already monomorphic, so we just need to
-- find instances of usertypes with arguments and construct
-- specialised versions
--
-- We do this in 2 phases: a linear pass over the functions, and then
-- a fixpoint over the types (processing a type may introduce further
-- specialised types)

import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import MonadLib

import Daedalus.GUID
import Daedalus.Panic
import Daedalus.Rec

import Daedalus.Core
import Daedalus.Core.Free

-- -----------------------------------------------------------------------------
-- Top level function

specialiseTypes :: HasGUID m => Module -> m  Module
specialiseTypes m = fst <$> runStateT initSpecTyState (getSpecTyM (specTy m))

-- -----------------------------------------------------------------------------
-- Monad

data SpecTyState =
  SpecTyState { stsSeenTypes :: Map UserType UserType
              , stsWorklist  :: Set UserType -- ^ subset of (dom seenTypes)
              }

initSpecTyState :: SpecTyState
initSpecTyState = SpecTyState { stsSeenTypes = mempty, stsWorklist = mempty }

newtype SpecTyM m a =
  SpecTyM { getSpecTyM :: StateT SpecTyState m a }
  deriving (Functor, Applicative, Monad, HasGUID)
  
-- Base case, the argument is normalised (this should not be called
-- directly, use specTy on UserTypes)
processUserType :: HasGUID m => UserType -> SpecTyM m UserType
processUserType ut = do
  m_ut' <- Map.lookup ut . stsSeenTypes <$> SpecTyM get 
  case m_ut' of
    Just ut' -> pure ut'
    Nothing  -> do
      n' <- freshTName (utName ut)
      let ut' = UserType { utName = n', utNumArgs = [], utTyArgs = [] }
      SpecTyM $ sets_ (\s -> s { stsSeenTypes = Map.insert ut ut' (stsSeenTypes s)
                               , stsWorklist  = Set.insert ut (stsWorklist s)
                               })
      pure ut'

-- Gets the next element of the worklist
getNextWL :: Monad m => SpecTyM m (Maybe (UserType, UserType))
getNextWL = SpecTyM (sets go)
  where
    go s
      | Just (ut, wl) <- Set.minView (stsWorklist s)
      , Just ut'      <- Map.lookup ut (stsSeenTypes s) =
          (Just (ut, ut'), s { stsWorklist = wl })
      | otherwise = (Nothing, s)
    
-- -----------------------------------------------------------------------------
-- Worker functions (second phase)

specTyDecls :: HasGUID m => [TDecl] -> SpecTyM m [TDecl]
specTyDecls polyTys = go []
  where
    go acc = do
      m_next <- getNextWL
      case m_next of
        Nothing -> pure acc
        Just (ut, ut') | Just td <- Map.lookup (utName ut) tyMap -> do
              let td' = instantiateTDecl ut ut' td
              td'' <- specTy td'
              go (td'' : acc)

        Just {} -> panic "Missing instantiation" []
        
    tyMap = Map.fromList [ (tName td, td) | td <- polyTys ]

instantiateTDecl :: UserType -> UserType -> TDecl -> TDecl
instantiateTDecl orig new td =
  TDecl { tName = utName new
        , tTParamKNumber = []
        , tTParamKValue  = []
        , tDef = def'
        }
  where
    def' = case tDef td of
             TStruct ls -> TStruct (goLabeled ls)
             TUnion  ls -> TUnion  (goLabeled ls)
             
    goTy ty =
      case ty of
        TStream   -> ty
        TUInt tsz -> TUInt (goSize tsz)
        TSInt tsz -> TSInt (goSize tsz)
        TInteger  -> ty
        TBool     -> ty
        TUnit     -> ty
        TArray ty' -> TArray (goTy ty')
        TMaybe ty' -> TMaybe (goTy ty')
        TMap dTy rTy -> TMap (goTy dTy) (goTy rTy)
        TBuilder ty' -> TBuilder (goTy ty')
        TIterator ty' -> TIterator (goTy ty')
        TUser ut      -> TUser (ut { utNumArgs = map goSize (utNumArgs ut)
                                   , utTyArgs  = map goTy (utTyArgs ut)
                                   })
        TParam p
          | Just ty' <- Map.lookup p tenv -> ty'
          | otherwise -> panic "Missing type param" []

    goSize (TSizeParam p) 
      | Just n <- Map.lookup p nenv = n -- _should_ be a size? 
      | otherwise = panic "Missing type param" []
    goSize sz = sz

    goLabeled = map (\(l, t) -> (l, goTy t))

    nenv = Map.fromList (zip (tTParamKNumber td) (utNumArgs orig))
    tenv = Map.fromList (zip (tTParamKValue  td) (utTyArgs orig))
    
    
  

-- -----------------------------------------------------------------------------
-- Worker functions (first phase)

class SpecTy a where
  specTy :: HasGUID m => a -> SpecTyM m a

  default specTy :: (Traversable f, SpecTy b, HasGUID m, a ~ f b) => a -> SpecTyM m a
  specTy v = traverse specTy v

instance SpecTy a => SpecTy [a] where {- default -}
instance SpecTy a => SpecTy (Maybe a) where {- default -}
instance SpecTy a => SpecTy (Case a) where {- default -}
instance SpecTy a => SpecTy (FunDef a) where

instance SpecTy a => SpecTy (Fun a) where
  specTy fn =
    Fun <$> specTy (fName fn) <*> specTy (fParams fn) <*> specTy (fDef fn)

  
instance SpecTy Name where
  specTy n = do
    n' <- specTy (nameType n)
    pure (n { nameType = n' })

instance SpecTy FName where
  specTy n = do
    n' <- specTy (fnameType n)
    pure (n { fnameType = n' })

instance SpecTy Module where
  specTy m = do
    ffs  <- traverse specTy (mFFuns m)
    bfs  <- traverse specTy (mBFuns m)
    gfs  <- traverse specTy (mGFuns m)
    tys' <- traverse specTy tys
    
    newTys <- specTyDecls pTys
    
    let mTys = topoOrder (\td -> (tName td, freeTCons td)) (newTys ++ tys')

    pure $ m { mFFuns = ffs, mBFuns = bfs, mGFuns = gfs, mTypes = mTys }
      where
        (pTys, tys) = partition isParamTy allTys        
        allTys = forgetRecs (mTypes m)
        isParamTy td = not (null (tTParamKNumber td) && null (tTParamKValue td))
  
instance SpecTy Match where
  specTy m =
    case m of
      MatchByte e  -> MatchByte  <$> specTy e
      MatchBytes e -> MatchBytes <$> specTy e
      MatchEnd     -> pure m
      
instance SpecTy Grammar where
  specTy gram = 
    case gram of -- FIXME: use childrenG
      Pure e -> Pure <$> specTy e
      GetStream -> pure gram
      SetStream e -> SetStream <$> specTy e
      Match s m -> Match s <$> specTy m
      Fail es ty m_e -> Fail es <$> specTy ty <*> specTy m_e
      Do_ l r -> Do_ <$> specTy l <*> specTy r
      Do x l r -> Do <$> specTy x <*> specTy l <*> specTy r
      Let x l r -> Let <$> specTy x <*> specTy l <*> specTy r

      OrBiased l r -> OrBiased <$> specTy l <*> specTy r
      OrUnbiased l r -> OrUnbiased <$> specTy l <*> specTy r
      Call f args -> Call <$> specTy f <*> specTy args
      Annot a g'  -> Annot a <$> specTy g'
      GCase c     -> GCase <$> specTy c

instance SpecTy ByteSet where
  specTy bs =
    case bs of
      SetAny        -> pure bs
      SetSingle e   -> SetSingle <$> specTy e
      SetRange e e' -> SetRange <$> specTy e <*> specTy e'
      SetComplement bs' -> SetComplement <$> specTy bs'
      SetUnion bs' bs'' -> SetUnion <$> specTy bs' <*> specTy bs''
      SetIntersection bs' bs'' -> SetIntersection <$> specTy bs' <*> specTy bs''
      SetLet x e bs'           -> SetLet <$> specTy x <*> specTy e <*> specTy bs'
      SetCall f es             -> SetCall <$> specTy f <*> specTy es
      SetCase c                -> SetCase <$> specTy c

instance SpecTy Expr where
  specTy e =
    case e of
      Var n -> Var <$> specTy n
      PureLet x e' e'' -> PureLet <$> specTy x <*> specTy e' <*> specTy e''
      Struct ut ls     -> Struct  <$> specTy ut <*> mapM (\(l, e') -> (,) l <$> specTy e') ls
      ECase c          -> ECase   <$> specTy c
     
      Ap0 op0          -> Ap0 <$> specTy op0
      Ap1 op1 e'       -> Ap1 <$> specTy op1 <*> specTy e'
      Ap2 op2 e' e''   -> Ap2 op2 <$> specTy e' <*> specTy e''
      Ap3 op3 e1 e2 e3 -> Ap3 op3 <$> specTy e1 <*> specTy e2 <*> specTy e3
      ApN opN es       -> ApN <$> specTy opN <*> specTy es


instance SpecTy Op0 where
  specTy op0 =
    case op0 of
      Unit      -> pure op0
      IntL n ty -> IntL n <$> specTy ty
      BoolL {}  -> pure op0
      ByteArrayL {} -> pure op0
      NewBuilder ty -> NewBuilder <$> specTy ty
      MapEmpty dTy rTy -> MapEmpty <$> specTy dTy <*> specTy rTy
      ENothing ty      -> ENothing <$> specTy ty

instance SpecTy Op1 where
  specTy op1 =
    case op1 of
      CoerceTo ty -> CoerceTo <$> specTy ty
      SelStruct ty l -> flip SelStruct l <$> specTy ty
      InUnion ut l   -> flip InUnion   l <$> specTy ut
      FromUnion ty l -> flip FromUnion l <$> specTy ty
      _ -> pure op1  

instance SpecTy OpN where
  specTy opN =
    case opN of
      ArrayL ty -> ArrayL <$> specTy ty
      CallF n   -> CallF  <$> specTy n

instance SpecTy Type where
  specTy ty =
    case ty of
      TStream    -> pure ty
      TUInt {}   -> pure ty
      TSInt {}   -> pure ty
      TInteger   -> pure ty
      TBool      -> pure ty
      TUnit      -> pure ty
      TArray ty' -> TArray <$> specTy ty'
      TMaybe ty' -> TMaybe <$> specTy ty'
      TMap tyD tyR -> TMap <$> specTy tyD <*> specTy tyR
      TBuilder ty' -> TBuilder <$> specTy ty'
      TIterator ty' -> TIterator <$> specTy ty'
      TUser ut      -> TUser <$> specTy ut
      TParam {}     -> pure ty
  
instance SpecTy UserType where
  specTy ut =
    case ut of
      UserType { utNumArgs = [], utTyArgs = [] } -> pure ut
      _ -> do 
        targs' <- specTy (utTyArgs ut)
        processUserType (ut { utTyArgs = targs' })

instance SpecTy TDecl where
  specTy td = do
    def' <- specTy (tDef td)
    pure (td { tDef = def' })

instance SpecTy TDef where
  specTy tdef =
    case tdef of
      TStruct fs -> TStruct <$> go fs
      TUnion  fs -> TUnion  <$> go fs
    where
      go = mapM (\(l, e') -> (,) l <$> specTy e')
