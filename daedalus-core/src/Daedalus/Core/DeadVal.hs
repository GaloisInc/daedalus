module Daedalus.Core.DeadVal (deadVal) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

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
  let (cleanFuns,initialUses) = unzip (map (cleanFun matcherNames) (mGFuns mo))
      originalFuns = Map.fromList [ (fName f,f) | f <- mGFuns mo ]
      initial = Set.unions initialUses
      matchers = generateMatchers matcherNames originalFuns initial
  pure (removeDeadCode mo { mGFuns = cleanFuns ++ matchers })

cleanFun :: Map FName FName -> Fun Grammar -> (Fun Grammar, Set FName)
cleanFun matcherNames f =
  case fDef f of
    External -> (f,mempty)
    Def g ->
      let r = cleanR matcherNames g
      in (f { fDef = Def (resGrammar r) }, resMatchers r)

generateMatchers ::
  Map FName FName         {- ^ Names for matcher functions (original -> matcher) -} ->
  Map FName (Fun Grammar) {- ^ Original function definitions -} ->
  Set FName               {- ^ Non-matcher functions that need matchers -} ->
  [Fun Grammar]
generateMatchers matcherNames originalFuns = go Set.empty
  where
  go done todo =
    case Set.minView todo of
      Nothing -> []
      Just (original,more)
        | original `Set.member` done -> go done more
        | otherwise ->
            case Map.lookup original originalFuns of
              Nothing ->
                panic "generateMatchers"
                  [ "Missing original function:" ++ show (pp original) ]
              Just f ->
                let (matcherFun,new) = makeMatcher matcherNames f
                in matcherFun : go (Set.insert original done) (more <> new)

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
  Map FName FName -> Fun Grammar -> (Fun Grammar, Set FName)
makeMatcher matcherNames f =
  case fDef f of
    External -> impossible
    Def g
      | fnameType (fName f) == TUnit -> impossible
      | otherwise ->
          let r = noSemR matcherNames g
          in ( f { fName    = matcher
                 , fDef     = Def (resGrammar r)
                 , fIsEntry = False
                   -- This pass runs before may-fail analysis, so the
                   -- generated function will be annotated later.
                 , fMayFail = Unknown
                 }
             , resMatchers r
             )
  where
  matcher =
    case Map.lookup (fName f) matcherNames of
      Just name -> name
      Nothing -> impossible

  impossible =
    panic "makeMatcher"
      [ "Matcher requested for ineligible function:" ++ show (pp (fName f)) ]

data Result = Result
  { resGrammar  :: Grammar      -- ^ Updated grammar
  , resFreeVars :: Set Name     -- ^ Free value variables in grammar
  , resMatchers :: Set FName    -- ^ Functions for which we need to make matchers
  }

-- Preserve the value of a grammar, while eliminating unused intermediate values.
cleanR :: Map FName FName -> Grammar -> Result
cleanR matcherNames gram =
  case gram of
    Pure {}       -> same
    GetStream     -> same
    SetStream {}  -> same
    Match {}      -> same
    Fail {}       -> same

    Do_ g1 g2 -> binary Do_ (noSemR matcherNames g1) (cleanR matcherNames g2)

    Do x g1 g2 ->
      let r2 = cleanR matcherNames g2
          finish r1 mk =
            Result
              { resGrammar = mk (resGrammar r1) (resGrammar r2)
              , resFreeVars = resFreeVars r1 <>
                              Set.delete x (resFreeVars r2)
              , resMatchers = resMatchers r1 <> resMatchers r2
              }
      in if x `Set.member` resFreeVars r2
           then finish (cleanR matcherNames g1) (Do x)
           else finish (noSemR matcherNames g1) Do_

    Let x e g ->
      let r = cleanR matcherNames g
      in if x `Set.member` resFreeVars r
           then r { resGrammar = Let x e (resGrammar r)
                  , resFreeVars = freeVars e <>
                                  Set.delete x (resFreeVars r)
                  }
           else r

    OrBiased g1 g2 ->
      binary OrBiased (cleanR matcherNames g1) (cleanR matcherNames g2)

    OrUnbiased g1 g2 ->
      binary OrUnbiased (cleanR matcherNames g1) (cleanR matcherNames g2)

    Call {} -> same

    Annot a g ->
      let r = cleanR matcherNames g
      in r { resGrammar = Annot a (resGrammar r) }

    GCase (Case x as) -> caseResult x [ (p, cleanR matcherNames g) | (p,g) <- as ]

    Loop lc -> loopResult Loop (cleanR matcherNames) lc
  where
  same = Result { resGrammar  = gram
                , resFreeVars = freeVars gram
                , resMatchers = mempty
                }

-- Preserve the parsing effects of a grammar, but return unit.
noSemR :: Map FName FName -> Grammar -> Result
noSemR matcherNames gram =
  case gram of
    Pure _         -> unitResult
    GetStream      -> unitResult
    SetStream {}   ->
      Result { resGrammar  = gram
             , resFreeVars = freeVars gram
             , resMatchers = mempty
             }
    Match _ m      -> result (Match SemNo m)
    Fail src _ msg -> result (Fail src TUnit msg)

    Do_ g1 g2 -> binary Do_ (noSemR matcherNames g1) (noSemR matcherNames g2)

    Do x g1 g2 ->
      let r2 = noSemR matcherNames g2
          finish r1 mk =
            Result
              { resGrammar = mk (resGrammar r1) (resGrammar r2)
              , resFreeVars = resFreeVars r1 <> Set.delete x (resFreeVars r2)
              , resMatchers = resMatchers r1 <> resMatchers r2
              }
      in if x `Set.member` resFreeVars r2
           then finish (cleanR matcherNames g1) (Do x)
           else finish (noSemR matcherNames g1) Do_

    Let x e g ->
      let r = noSemR matcherNames g
      in if x `Set.member` resFreeVars r
           then r { resGrammar = Let x e (resGrammar r)
                  , resFreeVars = freeVars e <> Set.delete x (resFreeVars r)
                  }
           else r

    OrBiased g1 g2 ->
      binary OrBiased (noSemR matcherNames g1) (noSemR matcherNames g2)

    OrUnbiased g1 g2 ->
      binary OrUnbiased (noSemR matcherNames g1) (noSemR matcherNames g2)

    Call f es ->
      case Map.lookup f matcherNames of
        Just matcher -> (result (Call matcher es)) { resMatchers = Set.singleton f }
        Nothing
          | fnameType f == TUnit -> result (Call f es)
          | otherwise -> result (Do_ (Call f es) (Pure unit))

    Annot a g ->
      let r = noSemR matcherNames g
      in r { resGrammar = Annot a (resGrammar r) }

    GCase (Case x as) -> caseResult x [ (p, noSemR matcherNames g) | (p,g) <- as ]

    Loop (ManyLoop _ backtrack lower upper body) ->
      let r = noSemR matcherNames body
      in Result
           { resGrammar = Loop (ManyLoop SemNo backtrack lower upper (resGrammar r))
           , resFreeVars = freeVars lower <> freeVars upper <> resFreeVars r
           , resMatchers = resMatchers r
           }

    -- Repeat and morphism loops use the body's result to drive the next
    -- iteration.  Preserve that computation and discard only the final value.
    Loop lc -> loopResult (\lc' -> Do_ (Loop lc') (Pure unit)) (cleanR matcherNames) lc
  where
  unitResult = Result { resGrammar  = Pure unit
                      , resFreeVars = mempty
                      , resMatchers = mempty
                      }
  result g = Result { resGrammar  = g
                    , resFreeVars = freeVars g
                    , resMatchers = mempty
                    }

binary ::
  (Grammar -> Grammar -> Grammar) ->
  Result ->
  Result ->
  Result
binary make r1 r2 =
  Result
    { resGrammar  = make (resGrammar r1) (resGrammar r2)
    , resFreeVars = resFreeVars r1 <> resFreeVars r2
    , resMatchers = resMatchers r1 <> resMatchers r2
    }

caseResult :: Name -> [(Pattern,Result)] -> Result
caseResult x as =
  Result
    { resGrammar  = GCase (Case x [ (p,resGrammar r) | (p,r) <- as ])
    , resFreeVars = Set.insert x (Set.unions [ resFreeVars r | (_,r) <- as ])
    , resMatchers = Set.unions [ resMatchers r | (_,r) <- as ]
    }

loopResult ::
  (LoopClass Grammar -> Grammar) ->
  (Grammar -> Result) ->
  LoopClass Grammar ->
  Result
loopResult make transform lc =
  let (lc',used) =
        case lc of
          -- Note that this case is not used by `noSemR`
          ManyLoop sem backtrack lower upper body ->
            let r = transform body
            in ( ManyLoop sem backtrack lower upper (resGrammar r)
               , resMatchers r
               )
          RepeatLoop backtrack x e body ->
            let r = transform body
            in ( RepeatLoop backtrack x e (resGrammar r), resMatchers r )
          MorphismLoop lm ->
            case lm of
              FoldMorphism x e col body ->
                let r = transform body
                in ( MorphismLoop
                       (FoldMorphism x e col (resGrammar r))
                   , resMatchers r
                   )
              MapMorphism col body ->
                let r = transform body
                in ( MorphismLoop (MapMorphism col (resGrammar r))
                   , resMatchers r
                   )
      g = make lc'
  in Result { resGrammar  = g
            , resFreeVars = freeVars g
            , resMatchers = used
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
