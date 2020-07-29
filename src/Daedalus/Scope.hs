{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, OverloadedStrings #-}

module Daedalus.Scope (
  resolveModules, resolveModule, ScopeError(..), prettyScopeError
  ) where

import Data.Functor ( ($>) )
import Data.Set(Set)
import qualified Data.Set as Set

import Data.Graph.SCC(stronglyConnComp)

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Map.Merge.Lazy (preserveMissing, zipWithAMatched, mergeA)
import Control.Exception(Exception(..))

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.Rec
import Daedalus.Panic

import Daedalus.AST
import Daedalus.PrettyError

-- -----------------------------------------------------------------------------
-- Monad and operations 
--
-- This traversal figures out the scope of each variable, and records
-- which variables are used by each rule.  The latter information is
-- used to determine the SCCs.

-- Note localVars shadows moduleVars.  We could unify these, and have
-- moduelVars map to Locals, but it is convenient to separate them as
-- we can then note used module refs.
type ModuleScope = Map Ident ScopedIdent
data Scope = Scope { moduleVars    :: ModuleScope
                   , localVars     :: Set Ident
                   , currentModule :: ModuleName
                   }

data ScopeError = ScopeViolation ModuleName Name
                | DuplicateNames ModuleName (Map Ident [Name])
                  deriving Show

instance PP ScopeError where
  pp (ScopeViolation _mn n) = "Undeclared variable" <+> backticks (pp n)
  pp (DuplicateNames _mn m) = "Duplicate rules:"
                               $$ nest 2 (bullets (map mkOne (Map.toList m)))
    where
      mkOne (k, vs) = pp k <+> "declared at" <+> vcat (map (pp . nameRange) vs)

instance Exception ScopeError where
  displayException = show . pp

prettyScopeError :: ScopeError -> IO String
prettyScopeError e@(ScopeViolation _m n) = prettyError (sourceFrom (nameRange n)) (show (pp e))
prettyScopeError e = pure (show (pp e))

newtype ScopeM a = ScopeM { getScopeM :: ReaderT Scope
                                         (StateT (Set ScopedIdent)
                                          (Except ScopeError)) a
                          }
  deriving (Functor, Applicative, Monad)

runScopeM :: Scope -> ScopeM a -> Either ScopeError (a, Set ScopedIdent)
runScopeM scope (ScopeM m) = runExcept (runStateT (runReaderT m scope) Set.empty)

recordIdentRef :: ScopedIdent -> ScopeM ()
recordIdentRef r = ScopeM $ modify (Set.insert r)

extendLocalScopeIn :: [Name] -> ScopeM a -> ScopeM a
extendLocalScopeIn ids = ScopeM . local extendScope . getScopeM
  where
    extendScope s = s { localVars = foldl extendOneScope (localVars s) ids }
    extendOneScope scope = flip Set.insert scope . nameScopeAsUnknown

makeNameLocal :: Name -> Name
makeNameLocal n = n { nameScope = Local (nameScopeAsUnknown n) }

makeNameModScope :: ModuleName -> Name -> Name
makeNameModScope m n = n { nameScope = ModScope m (nameScopeAsUnknown n) }

getScope :: ScopeM Scope
getScope = ScopeM ask

resolveModules :: [Module] -> Either ScopeError [Module]
resolveModules = go Map.empty []
  where
    go _    acc []       = pure (reverse acc)
    go seen acc (m : ms) =
      do (m1, seen1) <- resolveModule seen m
         go seen1 (m1 : acc) ms


resolveModule ::
  Map ModuleName (Map Ident Name) {- ^ Info about other modules -} ->
  Module ->
  Either ScopeError (Module, Map ModuleName (Map Ident Name))
resolveModule ms m =
  do (s, newDefs) <- moduleScope ms m
     let scope = Scope s Set.empty (moduleName m)
     rs'<- mkRec <$> mapM (runResolve scope) (forgetRecs (moduleRules m))
     let m' = m { moduleRules = rs' }
     pure (m', Map.insert (moduleName m') newDefs ms)
  where
  mkRec = map sccToRec . stronglyConnComp

  runResolve scope r =
    do (r', refs) <- runScopeM scope (resolve r)
       return (r', nameScope (ruleName r'), Set.toList refs)




-- | Figure out the map from idents to resolved names for a given
-- module.  This is slightly more complex than it has to be, as we try
-- to detect all dups, not just the first.
moduleScope ::
  Map ModuleName (Map Ident Name) {- ^ Maps module name to what's in scope -} ->
  Module                          {- ^ Module to process -} ->
  Either ScopeError (ModuleScope, Map Ident Name)
  {- ^ All things in scope of this module, new things defined -}
moduleScope ms m =
  case runState merged Map.empty of
    (s, dups) | Map.null dups -> Right s
    (_, dups)                 -> Left  (DuplicateNames mname dups)
  where
    mname = moduleName m
    rs    = forgetRecs (moduleRules m)

    merged  :: State (Map Ident [Name]) (ModuleScope, Map Ident Name)
    merged  = do defs' <- foldM doMerge Map.empty defs
                 allDs <- foldM doMerge defs' imported
                 return (nameScope <$> allDs, defs')

    doMerge = mergeA preserveMissing preserveMissing (zipWithAMatched matched)
    matched :: Ident -> Name -> Name -> State (Map Ident [Name]) Name
    matched k x y = modify (Map.insertWith (++) k [x, y]) $> x

    imported = [ ms Map.! thingValue i | i <- moduleImports m ]

    -- Makes it easier to detect duplicates
    defs  = [ Map.singleton (nameScopeAsUnknown (ruleName r))
                            (makeNameModScope mname (ruleName r))
            | r <- rs
            ]

-- -----------------------------------------------------------------------------
-- A type class for resolving names

class ResolveNames t where
  resolve :: t -> ScopeM t

instance ResolveNames a => ResolveNames [a] where
  resolve = traverse resolve

instance ResolveNames a => ResolveNames (Maybe a) where
  resolve = traverse resolve

-- This is the base case
instance ResolveNames Name where
  resolve n = do
    scope <- getScope
    ident' <-
      case nameScope n of
        Unknown ident
          | ident `Set.member` localVars scope -> pure (Local ident)
          | Just v <- Map.lookup ident (moduleVars scope) ->
              recordIdentRef v *> pure v
          | otherwise -> ScopeM $ throwError
                                $ ScopeViolation (currentModule scope) n
        _ -> panicRange n "Expecting an Unknown scope." []
    return (n { nameScope = ident' })


instance ResolveNames Rule where
  resolve r = do scope <- getScope
                 let n' = makeNameModScope (currentModule scope) (ruleName r)
                 e' <- extendLocalScopeIn (map paramName (ruleParams r))
                                  (resolve (ruleDef r))
                 ps1 <- mapM resolve (ruleParams r)
                 resT1 <- resolve (ruleResTy r)
                 return (Rule { ruleName   = n'
                              , ruleParams = ps1
                              , ruleResTy  = resT1
                              , ruleDef    = e'
                              , ruleRange  = ruleRange r
                              })

instance ResolveNames Expr where
  resolve (Expr r) = Expr <$> traverse resolve r

instance ResolveNames RuleParam where
  resolve p = do t1 <- resolve (paramType p)
                 pure RuleParam { paramName = makeNameLocal (paramName p)
                                , paramType = t1 }

instance ResolveNames e => ResolveNames (ExprF e) where
  resolve expr =
    case expr of
      ENumber {}      -> pure expr
      EBool {}        -> pure expr
      ENothing {}     -> pure expr
      EJust e         -> EJust <$> resolve e
      EStruct fs      -> EStruct <$> resolveStructFields fs
      EChoiceU c e1 e2-> EChoiceU c <$> resolve e1 <*> resolve e2
      EChoiceT c fs   -> EChoiceT c <$> resolve fs
      EArray es       -> EArray    <$> resolve es
      EApp f es       -> EApp      <$> resolve f <*> resolve es
      EVar x          -> EVar      <$> resolve x
      ETry e          -> ETry      <$> resolve e

      EAnyByte        -> pure expr
      EOptional c e   -> EOptional c <$> resolve e
      EMany c bnds z  -> EMany c   <$> resolve bnds <*> resolve z
      EEnd            -> pure expr
      EOffset         -> pure expr
      ECurrentStream  -> pure expr
      ESetStream x    -> ESetStream <$> resolve x
      EStreamLen x y  -> EStreamLen <$> resolve x <*> resolve y
      EStreamOff x y  -> EStreamOff <$> resolve x <*> resolve y

      EMapEmpty       -> pure expr
      EMapInsert ke ve me -> EMapInsert <$> resolve ke <*> resolve ve <*> resolve me
      EMapLookup ke me -> EMapLookup <$> resolve ke <*> resolve me

      EArrayLength ve  -> EArrayLength <$> resolve ve       
      EArrayIndex  ve ixe -> EArrayIndex <$> resolve ve <*> resolve ixe

      EFor fl mbI y b c -> EFor  <$> resolveFL
                                 <*> pure (makeNameLocal <$> mbI)
                                 <*> pure (makeNameLocal y)
                                 <*> resolve b
                                 <*> extendLocalScopeIn names (resolve c)
        where
        (fnames,resolveFL) =
          case fl of
            FFold x e -> ([x], FFold (makeNameLocal x) <$> resolve e)
            FMap      -> ([],  pure FMap)

        inames = case mbI of
                   Nothing -> []
                   Just i  -> [i]

        names = fnames ++ inames ++ [y]

      EIf be te fe    -> EIf       <$> resolve be <*> resolve te <*> resolve fe

      EHasType sig e t -> EHasType sig <$> resolve e <*> resolve t
      -- XXX: for the moment we don't have top level type names,
      -- but at some point we should probably add them, and then we'd
      -- have to resolve the type as well.

      EQuiet e        -> EQuiet    <$> resolve e
      EPure e         -> EPure     <$> resolve e
      EFail msg       -> EFail     <$> resolve msg

      EBytes _        -> pure expr
      EByte _         -> pure expr
      EInRange e1 e2  -> EInRange  <$> resolve e1 <*> resolve e2
      ETriOp op e1 e2 e3 -> ETriOp op <$> resolve e1
                                      <*> resolve e2
                                      <*> resolve e3
      EBinOp op e1 e2 -> EBinOp op <$> resolve e1 <*> resolve e2
      EUniOp op e     -> EUniOp op <$> resolve e
      ESel e s        -> ESel      <$> resolve e <*> pure s

-- FIXME: Maybe we should make a separate 'Label' scope?
instance ResolveNames e => ResolveNames (UnionField e) where
  resolve (n :> e) = (n :> ) <$> resolve e

instance ResolveNames e => ResolveNames (ManyBounds e) where
  resolve = traverse resolve

instance ResolveNames t => ResolveNames (TypeF t) where
  resolve tf =
    case tf of
      TGrammar t -> TGrammar <$> resolve t
      TStream    -> pure tf
      TByteClass -> pure tf
      TNum {}    -> pure tf
      TUInt t    -> TUInt <$> resolve t
      TSInt t    -> TSInt <$> resolve t
      TInteger   -> pure tf
      TBool      -> pure tf
      TUnit      -> pure tf
      TArray t   -> TArray <$> resolve t
      TMaybe t   -> TMaybe <$> resolve t
      TMap  k v  -> TMap <$> resolve k <*> resolve v

instance ResolveNames t => ResolveNames (Located t) where
  resolve t = traverse resolve t

instance ResolveNames SrcType where
  resolve ty =
    case ty of
      SrcVar x ->
        do scope <- getScope
           newS <- case nameScope x of
                     Unknown i ->
                       case Map.lookup i (moduleVars scope) of
                         Just j -> recordIdentRef j >> pure j
                         Nothing -> pure (Local i)
                     _ -> panic "ResolveNames@SrcType" ["Resolved name"]
           pure (SrcVar x { nameScope = newS })

      SrcType tf -> SrcType <$> resolve tf


resolveStructFields :: ResolveNames e => [StructField e] -> ScopeM [StructField e]
resolveStructFields []       = return []
resolveStructFields (f : fs) = 
  case f of
    COMMIT r -> (COMMIT r :) <$> resolveStructFields fs
    Anon e   -> (:) <$> (Anon <$> resolve e) <*> resolveStructFields fs
    x := e   -> go (:=)  x e
    x :@= e  -> go (:@=) x e
  where
    go ctor x e =
      do f'  <- ctor (makeNameLocal x) <$> resolve e
         fs' <- extendLocalScopeIn [x] (resolveStructFields fs)
         pure (f' : fs')
