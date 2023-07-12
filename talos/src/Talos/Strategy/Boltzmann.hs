{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyCase #-}

module Talos.Strategy.Boltzmann (
  printGeneratingFunctions,
  ) where

import Data.Foldable
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Void
import System.Random

import qualified Data.List as List
import qualified Data.Map as M
import qualified Data.Set as S

import Daedalus.Core.Basics
import Daedalus.Core.Decl
import Daedalus.Core.Grammar
import Daedalus.Rec

import Talos.Strategy.Boltzmann.Polynomial
import Talos.Strategy.Boltzmann.Util
import Talos.SymExec.Path
import qualified Talos.Strategy.Boltzmann.Newton as Newton

printGeneratingFunctions :: Module -> FName -> IO ()
printGeneratingFunctions md mainRule = do
    print (ppGFs gfs)
    putStrLn . showPP . grammarChoices gfs weights' $ top
    randomGrammar ge gfs temperature $ top
    where
    temperature = 0.49
    ge = mkGrammarEnv md
    gfs = summarizeGrammars ge (county ge mainRule)
    weights = solveEquations temperature gfs
    weights' = M.insert Temperature temperature . M.mapKeysMonotonic Recursive $ weights
    top = fromJust $ lookupGrammar mainRule ge

type GrammarEnv = Map FName (Fun Grammar)

mkGrammarEnv :: Module -> GrammarEnv
mkGrammarEnv md = M.fromList [(fName fun, fun) | fun <- mGFuns md]

lookupGrammar :: FName -> GrammarEnv -> Maybe Grammar
lookupGrammar nm ge = do
    Fun { fDef = Def grammar } <- M.lookup nm ge
    pure grammar

county :: GrammarEnv -> FName -> Set FName
county ge = go S.empty . S.singleton where
  go seen frontier
    | S.null frontier = seen
    | otherwise = go seen' frontier'
    where
    seen' = S.union seen frontier
    frontier' = S.unions [neighborhood ge nm | nm <- S.toList frontier] S.\\ seen'

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
  GCase{} -> undefined

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

summarizeGrammars :: GrammarEnv -> Set FName -> Map FName GrammarGF
summarizeGrammars ge fullScope = List.foldl' go M.empty $
    topoOrder (\nm -> (nm, neighborhood ge nm)) (S.toList fullScope)
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

data Weighted a = Weighted
    { weight :: Double
    , choice :: a
    } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance PP a => PP (Weighted a) where
    pp w = pp (choice w) <+> parens (text "weight" <+> pp (weight w))

data WeightedChoice a = WeightedChoice { wc, wc' :: Weighted a }
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance PP a => PP (WeightedChoice a) where
    pp choice = pp (wc choice) <> text "/" <> pp (wc' choice)

newtype WeightedCase a = WeightedCase (Case (Weighted a))
    deriving (Functor, Foldable, Traversable, PP)

data FName1 a = FName1 FName deriving (Functor, Foldable, Traversable)

instance PP (FName1 a) where ppPrec _ (FName1 fn) = space <> pp fn

type WeightedPath = SelectedPathF WeightedChoice WeightedCase FName1 Void

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
        OrBiased g g' -> SelectedChoice (WeightedChoice (weightFor g) (weightFor g'))
        OrUnbiased g g' -> go (OrBiased g g')
        Call fn _args -> SelectedCall (FName1 fn)
        Annot _note g -> go g
        GCase c -> SelectedCase . WeightedCase $ weightFor <$> c
    weightFor g = Weighted
        { weight = fromJust . evalConstant weights . lmap fromIntegral . gfPolynomial . summarizeGrammar gfs $ g
        , choice = go g
        }

randomGrammar :: GrammarEnv -> Map FName GrammarGF -> Double -> Grammar -> IO ()
randomGrammar env scope temperature = go where
    go = \case
        Pure _ -> putStrLn "pure -> pure"
        GetStream -> putStrLn "getstream -> getstream"
        SetStream _ -> putStrLn "setstream -> setstream"
        Match _ _ -> putStrLn "match -> match"
        Fail _ _ _ -> putStrLn "YIKES! fail -> fail"
        Do_ g g' -> go g >> go g'
        Do _ g g' -> go g >> go g'
        Let _ _ g -> go g
        OrBiased g g' -> do
            let w  = weightFor g
                w' = weightFor g'
            putStrLn $ "weight " ++ show w ++ " for " ++ pps g
            putStrLn $ "weight " ++ show w' ++ " for " ++ pps g'
            n <- randomRIO (0, w+w')
            print n
            putStrLn $ "biased or -> chose " ++ if n < w then "first branch" else "second branch"
            if n < w then go g else go g'
        OrUnbiased g g' -> do
            let w  = weightFor g
                w' = weightFor g'
            putStrLn $ "weight " ++ show w ++ " for " ++ pps g
            putStrLn $ "weight " ++ show w' ++ " for " ++ pps g'
            n <- randomRIO (0, w+w')
            print n
            putStrLn $ "unbiased or -> chose " ++ if n < w then "first branch" else "second branch"
            if n < w then go g else go g'
        Call f _ -> case lookupGrammar f env of
            Just g -> go g
            Nothing -> putStrLn $ "YIKES! call " <> show (pp f) <> " -> call " <> show (pp f)
        Annot _ g -> go g
        GCase (Case _ pats) -> do
            let ws = map (weightFor . snd) pats
            for_ (zip ws pats) $ \(w, (_, g)) -> do
                putStrLn $ "weight " ++ show w ++ " for " ++ pps g
            n <- randomRIO (0, sum ws)
            print n
            putStrLn $ "gcase -> chose " ++ show (findIndex (n<) (scanl1 (+) ws))
            go . snd $ case findIndex (n<) (scanl1 (+) ws) of
                Nothing -> last pats
                Just i -> pats !! i

    pps = show . pp . unannotated
    weightFor = fromJust . evalConstant valuations . newtonPoly temperature . summarizeGrammar scope
    valuations = Newton.fixedPoint 1e-6 (newtonPoly temperature <$> scope)

unannotated :: Grammar -> Grammar
unannotated = \case
    Annot _ g -> unannotated g
    Do_ g g' -> Do_ (unannotated g) (unannotated g')
    Do nm g g' -> Do nm (unannotated g) (unannotated g')
    Let nm e g -> Let nm e (unannotated g)
    OrBiased g g' -> OrBiased (unannotated g) (unannotated g')
    OrUnbiased g g' -> OrUnbiased (unannotated g) (unannotated g')
    GCase (Case nm pats) -> GCase (Case nm [(pat, unannotated g) | (pat, g) <- pats])
    g -> g

instance PP HotName where
    pp = \case
        Temperature -> text "x"
        Recursive f -> pp f <> parens (pp Temperature)

instance PP GrammarGF where
    pp = \case
        Failing s -> text ("0<" <> s <> ">")
        gf -> pp (gfPolynomial gf)

ppGFs :: Map FName GrammarGF -> Doc
ppGFs gfs = vcat
  [ pp (Recursive nm) <+> text "=" <+> pp spec
  | (nm, spec) <- M.toList gfs
  ]
