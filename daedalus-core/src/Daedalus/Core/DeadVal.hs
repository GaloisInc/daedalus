module Daedalus.Core.DeadVal (deadVal) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes,fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad(mapAndUnzipM)

import Daedalus.GUID (HasGUID)
import Daedalus.Panic (panic)
import Daedalus.PP (pp)
import Daedalus.Core
import Daedalus.Core.Free (freeFVars, freeVars)

-- | Avoid constructing unused grammar results.
-- We generate "matcher" forms of grammars whose results are discarded---
-- these return no semantic result.
deadVal :: HasGUID m => Module -> m Module
deadVal mo = do
  -- We allocate all matching function names before rewriting
  -- any bodies, so calls within recursive groups can always use their matchers.
  matcherNames <- Map.fromList . catMaybes <$> mapM matcherName (mGFuns mo)
  (cleanFuns,initialUses) <- mapAndUnzipM (cleanFun matcherNames) (mGFuns mo)
  let originalFuns = Map.fromList [ (fName f,f) | f <- mGFuns mo ]
  matchers <- generateMatchers matcherNames originalFuns (Set.unions initialUses)
  pure (removeDeadCode mo { mGFuns = cleanFuns ++ matchers })

-- | Remove unused values in a single function.
-- Also returns what matchers were used.
cleanFun :: HasGUID m => Map FName FName -> Fun Grammar -> m (Fun Grammar, Set FName)
cleanFun matcherNames f =
  case fDef f of
    External -> pure (f,mempty)
    Def g -> do
      r <- cleanR matcherNames g
      pure (f { fDef = Def (resGrammar r) }, resMatchers r)

generateMatchers ::
  HasGUID m =>
  Map FName FName         {- ^ Names for matcher functions (original -> matcher) -} ->
  Map FName (Fun Grammar) {- ^ Original function definitions -} ->
  Set FName               {- ^ Non-matcher functions that need matchers -} ->
  m [Fun Grammar]
generateMatchers matcherNames originalFuns = go Set.empty
  where
  go done todo =
    case Set.minView todo of
      Nothing -> pure []
      Just (original,more)
        | original `Set.member` done -> go done more
        | otherwise ->
            case Map.lookup original originalFuns of
              Nothing ->
                panic "generateMatchers"
                  [ "Missing original function:" ++ show (pp original) ]
              Just f ->
                do (matcherFun,new) <- makeMatcher matcherNames f
                   rest <- go (Set.insert original done) (more <> new)
                   pure (matcherFun : rest)

matcherName :: HasGUID m => Fun Grammar -> m (Maybe (FName, FName))
matcherName f
  | fnameType old == TUnit = pure Nothing
  | External <- fDef f     = pure Nothing
  | otherwise = do
      new <- freshFName old
        { fnameText   = fnameText old <> "_"
        , fnamePublic = False
        , fnameType   = TUnit
        }
      pure (Just (old, new))
  where
  old = fName f

makeMatcher ::
  HasGUID m => Map FName FName -> Fun Grammar -> m (Fun Grammar, Set FName)
makeMatcher matcherNames f =
  case fDef f of
    External -> impossible
    Def g
      | fnameType (fName f) == TUnit -> impossible -- sanity
      | otherwise -> do
          r <- noSemR matcherNames g
          pure
            ( f { fName    = matcher
                , fDef     = Def (resGrammar r)
                , fIsEntry = False
                  -- This pass runs before may-fail analysis, so the
                  -- generated function will be annotated later.
                , fMayFail = Unknown
                }
            , resMatchers r
            )
  where
  matcher = fromMaybe impossible (Map.lookup (fName f) matcherNames)

  impossible =
    panic "makeMatcher"
      [ "Matcher requested for ineligible function:" ++ show (pp (fName f)) ]

data Result = Result
  { resGrammar  :: Grammar      -- ^ Updated grammar
  , resFreeVars :: Set Name     -- ^ Free value variables in grammar
  , resMatchers :: Set FName    -- ^ Functions for which we need to make matchers
  , resPure     :: Bool         -- ^ Grammar may be discarded entirely
  }

-- Preserve the value of a grammar, while eliminating unused intermediate values.
cleanR :: HasGUID m => Map FName FName -> Grammar -> m Result
cleanR matcherNames gram =
  case gram of
    Pure {}       -> pure pureResult
    GetStream     -> pure pureResult
    SetStream {}  -> pure same
    Match {}      -> pure same
    Fail {}       -> pure same

    Do_ g1 g2 ->
      binary Do_ <$> noSemR matcherNames g1 <*> cleanR matcherNames g2

    Do x g1 g2 -> do
      r2 <- cleanR matcherNames g2
      r1 <- if x `Set.member` resFreeVars r2
              then cleanR matcherNames g1
              else noSemR matcherNames g1
      let make = if x `Set.member` resFreeVars r2 then Do x else Do_
      pure Result
        { resGrammar = make (resGrammar r1) (resGrammar r2)
        , resFreeVars = resFreeVars r1 <> Set.delete x (resFreeVars r2)
        , resMatchers = resMatchers r1 <> resMatchers r2
        , resPure = resPure r1 && resPure r2
        }

    Let x e g -> do
      r <- cleanR matcherNames g
      pure $
        if x `Set.member` resFreeVars r
          then r { resGrammar = Let x e (resGrammar r)
                 , resFreeVars = freeVars e <>
                                 Set.delete x (resFreeVars r)
                 }
          else r

    OrBiased g1 g2 ->
      binary OrBiased <$> cleanR matcherNames g1 <*> cleanR matcherNames g2

    OrUnbiased g1 g2 ->
      binary OrUnbiased <$> cleanR matcherNames g1 <*> cleanR matcherNames g2

    Call {} -> pure same

    Annot a g -> do
      r <- cleanR matcherNames g
      pure r { resGrammar = gAnnotate [a] (resGrammar r) }

    GCase (Case x as) -> do
      as' <- mapM cleanAlt as
      pure (caseResult x as')
      where
      cleanAlt (p,g) = do r <- cleanR matcherNames g
                          pure (p,r)

    Loop loop -> cleanLoop loop
  where
  cleanLoop loop = do
    r <- cleanR matcherNames (loopClassBody loop)
    let g = Loop (resGrammar r <$ loop)
    pure Result { resGrammar  = g
                , resFreeVars = freeVars g
                , resMatchers = resMatchers r
                , resPure     = False
                }

  same = Result { resGrammar  = gram
                , resFreeVars = freeVars gram
                , resMatchers = mempty
                , resPure     = False
                }
  pureResult = same { resPure = True }

-- Preserve the parsing effects of a grammar, but return unit.
noSemR :: HasGUID m => Map FName FName -> Grammar -> m Result
noSemR matcherNames gram =
  case gram of
    Pure _         -> pure unitResult
    GetStream      -> pure unitResult
    SetStream {}   ->
      pure Result { resGrammar  = gram
                  , resFreeVars = freeVars gram
                  , resMatchers = mempty
                  , resPure     = False
                  }
    Match _ m      -> pure (result (Match SemNo m))
    Fail src _ msg -> pure (result (Fail src TUnit msg))

    Do_ g1 g2 ->
      binary Do_ <$> noSemR matcherNames g1 <*> noSemR matcherNames g2

    Do x g1 g2 -> do
      r2 <- noSemR matcherNames g2
      r1 <- if x `Set.member` resFreeVars r2
              then cleanR matcherNames g1
              else noSemR matcherNames g1
      let make = if x `Set.member` resFreeVars r2 then Do x else Do_
      pure Result
        { resGrammar = make (resGrammar r1) (resGrammar r2)
        , resFreeVars = resFreeVars r1 <> Set.delete x (resFreeVars r2)
        , resMatchers = resMatchers r1 <> resMatchers r2
        , resPure = resPure r1 && resPure r2
        }

    Let x e g -> do
      r <- noSemR matcherNames g
      pure $
        if x `Set.member` resFreeVars r
          then r { resGrammar = Let x e (resGrammar r)
                 , resFreeVars = freeVars e <> Set.delete x (resFreeVars r)
                 }
          else r

    OrBiased g1 g2 ->
      binary OrBiased <$> noSemR matcherNames g1 <*> noSemR matcherNames g2

    OrUnbiased g1 g2 ->
      binary OrUnbiased <$> noSemR matcherNames g1 <*> noSemR matcherNames g2

    Call f es ->
      pure $ case Map.lookup f matcherNames of
        Just matcher -> (result (Call matcher es)) { resMatchers = Set.singleton f }
        Nothing
          | fnameType f == TUnit -> result (Call f es)
          | otherwise -> result (Do_ (Call f es) (Pure unit))

    Annot a g -> do
      r <- noSemR matcherNames g
      pure r { resGrammar = gAnnotate [a] (resGrammar r) }

    GCase (Case x as) -> do
      as' <- mapM noSemAlt as
      pure (caseResult x as')
      where
      noSemAlt (p,g) = do r <- noSemR matcherNames g
                          pure (p,r)

    Loop loop -> noSemLoop matcherNames loop
  where
  result g = Result { resGrammar  = g
                    , resFreeVars = freeVars g
                    , resMatchers = mempty
                    , resPure     = False
                    }

noSemLoop :: HasGUID m => Map FName FName -> LoopClass Grammar -> m Result
noSemLoop matcherNames loop =
  case loop of
    ManyLoop _ backtrack lower upper body -> do
      r <- noSemR matcherNames body
      pure Result
        { resGrammar = Loop (ManyLoop SemNo backtrack lower upper (resGrammar r))
        , resFreeVars = freeVars lower <> freeVars upper <> resFreeVars r
        , resMatchers = resMatchers r
        , resPure = False
        }

    -- A repeat loop uses the body's result to drive the next iteration.
    -- Preserve that computation and discard only the final value.
    RepeatLoop {} -> discardResult <$> cleanR matcherNames (Loop loop)

    -- A map loop's iterations are determined by its input collection, so
    -- when its result is unused it can become a unit-valued fold.
    MorphismLoop (MapMorphism col body) -> do
      r <- noSemR matcherNames body
      if resPure r then pure unitResult else unitFold col r

    MorphismLoop (FoldMorphism state _ col body) -> do
      r <- noSemR matcherNames body
      if resPure r
        then pure unitResult
        else if state `Set.member` resFreeVars r
               then discardResult <$> cleanR matcherNames (Loop loop)
               else unitFold col r
  where
  unitFold col r = do
    state <- freshNameSys TUnit
    let g = Loop (MorphismLoop (FoldMorphism state unit col (resGrammar r)))
    pure Result
      { resGrammar = g
      , resFreeVars = freeVars g
      , resMatchers = resMatchers r
      , resPure = False
      }

unitResult :: Result
unitResult =
  Result { resGrammar  = Pure unit
         , resFreeVars = mempty
         , resMatchers = mempty
         , resPure     = True
         }

binary :: (Grammar -> Grammar -> Grammar) -> Result ->  Result ->  Result
binary make r1 r2 =
  Result
    { resGrammar  = make (resGrammar r1) (resGrammar r2)
    , resFreeVars = resFreeVars r1 <> resFreeVars r2
    , resMatchers = resMatchers r1 <> resMatchers r2
    , resPure     = resPure r1 && resPure r2
    }

discardResult :: Result -> Result
discardResult r =
  r { resGrammar = Do_ (resGrammar r) (Pure unit),
      resFreeVars = resFreeVars r }

caseResult :: Name -> [(Pattern,Result)] -> Result
caseResult x as =
  Result
    { resGrammar  = GCase (Case x [ (p,resGrammar r) | (p,r) <- as ])
    , resFreeVars = Set.insert x (Set.unions [ resFreeVars r | (_,r) <- as ])
    , resMatchers = Set.unions [ resMatchers r | (_,r) <- as ]
    , resPure     = all (resPure . snd) as
    }

-- Matcher generation changes the call graph, so some of the original
-- functions retained by specialization may no longer be reachable.
removeDeadCode :: Module -> Module
removeDeadCode mo =
  mo { mFFuns = keep (mFFuns mo)
     , mBFuns = keep (mBFuns mo)
     , mGFuns = keep (mGFuns mo)
     }
  where
  allFuns = [ (fName f, fIsEntry f, freeFVars f) | f <- mFFuns mo ] ++
            [ (fName f, fIsEntry f, freeFVars f) | f <- mBFuns mo ] ++
            [ (fName f, fIsEntry f, freeFVars f) | f <- mGFuns mo ]

  dependencies = Map.fromList [ (f,ds) | (f,_,ds) <- allFuns ]
  entries = Set.fromList [ f | (f,True,_) <- allFuns ]
  reachable = closure dependencies entries

  keep fs = filter (\f -> fName f `Set.member` reachable) fs

closure :: Map FName (Set FName) -> Set FName -> Set FName
closure dependencies = go mempty
  where
  go done todo =
    case Set.minView todo of
      Nothing -> done
      Just (f,more)
        | f `Set.member` done -> go done more
        | otherwise ->
            let new = Map.findWithDefault mempty f dependencies
            in go (Set.insert f done) (more <> new)
