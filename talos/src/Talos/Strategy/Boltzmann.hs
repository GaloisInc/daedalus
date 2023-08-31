{-# LANGUAGE LambdaCase #-}

module Talos.Strategy.Boltzmann (
  printRandomPath,
  chooseTemperature,
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Functor
import Data.Functor.Identity
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import System.Random

import qualified Data.List as List
import qualified Data.Map as M
import qualified Data.Set as S

import Daedalus.Core.Basics
import Daedalus.Core.Decl
import Daedalus.Core.Grammar
import Daedalus.GUID
import Daedalus.Rec

import Talos.Analysis (summarise, absEnvTys)
import Talos.Analysis.AbsEnv (AbsEnvTy(AbsEnvTy))
import Talos.Analysis.Slice (assertionsFID)
import Talos.Strategy.Boltzmann.Polynomial
import Talos.Strategy.Boltzmann.Types
import Talos.Strategy.Boltzmann.Util
import Talos.Strategy.Monad
import qualified Talos.Strategy.Boltzmann.Newton as Newton

import Talos.SymExec.Path

printRandomPath :: Module -> FName -> String -> IO ()
printRandomPath md mainRule absEnvNm = ioStrategyM md absEnvNm $ do
    wp <- getBoltzmannWeight mainRule
    sp <- randomSelectedPath wp
    liftIO . print . pp $ sp

chooseTemperature :: Double -> Module -> Map FName WeightedPath
chooseTemperature temperature md = M.mapMaybe pathFor ge where
    ge = mkGrammarEnv md
    gfs = summarizeGrammars ge
    weights = M.insert Temperature temperature
            . M.mapKeysMonotonic Recursive
            $ solveEquations temperature gfs
    pathFor = \case
        Fun { fDef = Def grammar } -> Just (grammarChoices gfs weights grammar)
        _ -> Nothing

ioStrategyM :: Module -> String -> StrategyM a -> IO ()
ioStrategyM md absEnvNm act = case lookup absEnvNm absEnvTys of
    Just (AbsEnvTy env) -> do
        gen <- newStdGen
        void . runStrategyM act $ emptyStrategyMState gen summaries weights md guid
        where
        (summaries, guid) = summarise env md firstValidGUID
        weights = chooseTemperature 0.49 md
    Nothing -> fail $ "Couldn't find abstraction environment " ++ absEnvNm

type GrammarEnv = Map FName (Fun Grammar)

mkGrammarEnv :: Module -> GrammarEnv
mkGrammarEnv md = M.fromList [(fName fun, fun) | fun <- mGFuns md]

lookupGrammar :: FName -> GrammarEnv -> Maybe Grammar
lookupGrammar nm ge = do
    Fun { fDef = Def grammar } <- M.lookup nm ge
    pure grammar

neighborhood :: GrammarEnv -> FName -> Set FName
neighborhood ge nm = foldMap calls (lookupGrammar nm ge)

calls :: Grammar -> Set FName
calls grammar = case grammar of
  Pure _ -> S.empty
  GetStream -> S.empty
  SetStream _ -> S.empty
  Match _ _ -> S.empty
  Fail _ _ _ -> S.empty
  Do_ g g' -> calls g `S.union` calls g'
  Do _ g g' -> calls g `S.union` calls g'
  Let _ _ g -> calls g
  OrBiased g g' -> calls g `S.union` calls g'
  OrUnbiased g g' -> calls g `S.union` calls g'
  Call fnm _ -> S.singleton fnm
  Annot _ g -> calls g
  GCase c -> foldMap calls c

-- | In the literature, 'Temperature' is the @x@ variable in the polynomial, while @'Recursive' nm@ is part of a recursive specification.
data HotName
    = Temperature
    | Recursive FName
    deriving (Eq, Ord)

data GrammarGF
    = StraightLine (Polynomial Int HotName) -- | For the purposes of generating functions, @StraightLine p@ is the polynomial @\x -> x*p(x)@. But for size analysis, factors of @x@ don't pile up within code that doesn't do any branching.
    | Failing String -- | For the purposes of generating functions, this is the polynomial @\x -> 0@. But for size analysis, we gather up reasons for failure.
    | Branching (Polynomial Int HotName)
    deriving (Eq, Ord)

instance Num GrammarGF where
    fromInteger = Branching . fromInteger

    Failing s + Failing s' = Failing (s <> " or " <> s')
    gf + gf' = Branching (gfPolynomial gf + gfPolynomial gf')

    Failing s * Failing s' = Failing (s <> " and " <> s')
    Failing s * _ = Failing s
    _ * Failing s = Failing s
    StraightLine p * StraightLine p' = StraightLine (p * p')
    StraightLine p * gf = StraightLine (p * gfPolynomial gf)
    gf * StraightLine p = StraightLine (gfPolynomial gf * p)
    gf * gf' = Branching (gfPolynomial gf * gfPolynomial gf')

    negate = gfUnimplemented "negate"
    abs = gfUnimplemented "abs"
    signum = gfUnimplemented "signum"

gfUnimplemented :: HasCallStack => String -> a
gfUnimplemented = unimplemented . ("GrammarGF::"++)

gfPolynomial :: GrammarGF -> Polynomial Int HotName
gfPolynomial (StraightLine poly) = variable Temperature * poly
gfPolynomial (Failing _) = zero
gfPolynomial (Branching poly) = poly

summarizeGrammar :: Map FName GrammarGF -> Grammar -> GrammarGF
summarizeGrammar scope = go where
    go = \case
        Pure _ -> 1
        GetStream -> 1
        SetStream _ -> 1
        Match _ _ -> StraightLine 1
        Fail _ _ _ -> Branching zero
        Do_ g g' -> go g * go g'
        Do _ g g' -> go g * go g'
        Let _ _ g -> go g
        OrBiased g g' -> go g + go g'
        OrUnbiased g g' -> go g + go g'
        Call f _ -> M.findWithDefault (Branching (variable (Recursive f))) f scope
        Annot _ g -> go g
        GCase (Case _ pats) -> sum . map (go . snd) $ pats

summarizeLookupGrammar :: GrammarEnv -> Map FName GrammarGF -> FName -> GrammarGF
summarizeLookupGrammar ge scope nm = case lookupGrammar nm ge of
    Just grammar -> summarizeGrammar scope grammar
    _ -> Failing (show (pp nm) <> " not defined in this module")

summarizeGrammars :: GrammarEnv -> Map FName GrammarGF
summarizeGrammars ge = List.foldl' go M.empty $
    topoOrder (\nm -> (nm, neighborhood ge nm)) (M.keys ge)
    where
    go scope rec = M.union
        (M.fromList [(nm, summarizeLookupGrammar ge scope nm) | nm <- recToList rec])
        scope

newtonPoly :: Double -> GrammarGF -> Polynomial Double FName
newtonPoly temperature = evalPoly unHotname . bimap fromIntegral id . gfPolynomial where
    unHotname = \case
        Temperature -> Right temperature
        Recursive nm -> Left nm

solveEquations :: Double -> Map FName GrammarGF -> Map FName Double
solveEquations temperature eqns = Newton.fixedPoint 1e-6 (newtonPoly temperature <$> eqns)

grammarChoices :: Map FName GrammarGF -> Map HotName Double -> Grammar -> WeightedPath
grammarChoices gfs weights = go where
    go = \case
        -- TODO: surely not all of these are SelectedHole
        Pure{} -> SelectedHole
        GetStream{} -> SelectedHole
        SetStream{} -> SelectedHole
        Match{} -> SelectedHole
        Fail{} -> SelectedHole
        Do_ g g' -> SelectedDo (go g) (go g')
        Do _nm g g' -> SelectedDo (go g) (go g')
        Let _nm _rhs g -> go g
        Choice _ gs -> SelectedChoice (WeightedChoice (weightFor <$> gs))
        Call fn _args -> SelectedCall (FName1 fn)
        Annot _note g -> go g
        GCase c -> SelectedCase . WeightedCase $ weightFor <$> c
    weightFor g = Weighted
        { weight = fromJust . evalConstant weights . lmap fromIntegral . gfPolynomial . summarizeGrammar gfs $ g
        , choice = go g
        }

randomSelectedPath :: LiftStrategyM m => WeightedPath -> m SelectedPath
randomSelectedPath = \case
    SelectedHole -> pure SelectedHole
    SelectedDo p p' -> liftA2 SelectedDo (randomSelectedPath p) (randomSelectedPath p')
    SelectedChoice (WeightedChoice ps) -> do
        (i, p) <- categorical ps
        SelectedChoice . PathIndex i <$> randomSelectedPath p
    SelectedCall (FName1 fn) -> do
        wp <- getBoltzmannWeight fn
        SelectedCall . CallInstantiation assertionsFID <$> randomSelectedPath wp
    SelectedCase (WeightedCase (Case _ ps)) -> do
        (_, p) <- categorical (snd <$> ps)
        SelectedCase . Identity <$> randomSelectedPath p
    _ -> undefined

categorical :: LiftStrategyM m => [Weighted a] -> m (Int, a)
categorical [] = error "can't sample from an empty categorical distribution"
categorical categories = do
    let ws = weight <$> categories
    n <- randR (0, sum ws)
    let mix = findIndex (n<) (scanl1 (+) ws)
        (ix, selectIx) = case mix of
            Nothing -> (length categories, last)
            Just i -> (i, (!!i))
    pure (ix, choice (selectIx categories))

instance PP HotName where
    pp = \case
        Temperature -> text "x"
        Recursive f -> pp f <> parens (pp Temperature)

instance PP GrammarGF where
    pp = \case
        Failing s -> text ("0<" <> s <> ">")
        gf -> pp (gfPolynomial gf)
