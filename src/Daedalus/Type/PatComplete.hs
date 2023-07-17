{-# Language GADTs, ScopedTypeVariables #-}
{-# Language PatternGuards, OverloadedStrings #-}

-- Pattern completeness for case expressions

module Daedalus.Type.PatComplete ( patComplete
                                 , CaseSummary(..)
                                 , PatternCompleteness(..)
                                 , summariseCase
                                 ) where

import Data.Maybe (isNothing)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Daedalus.Panic
import Daedalus.PP
import Daedalus.Rec
import Daedalus.SourceRange
import Daedalus.Type.AST
import Daedalus.Type.Traverse

-- FIXME: Move?

-- The sets of examined patterns are non empty
data CaseSummary =
  UnionCase TCTyName  -- ^ Type name
            [Label] -- ^ Labels (explicitly) examined
            [Label] -- ^ Labels missing
  | MaybeCase [Maybe ()] -- ^ Included cases
  | NumberCase Type [Integer] -- ^ Included cases
  | BoolCase [Bool] -- ^ Included cases

data PatternCompleteness = Incomplete | Complete
  deriving (Show, Eq)

missingConPatterns :: Map TCTyName TCTyDecl -> [TCPat] ->
                      (PatternCompleteness, CaseSummary)
missingConPatterns declTys allPats
  | Set.null missing = (Complete, summary)
  | otherwise        = (Incomplete, summary)
  where
    summary = UnionCase tyName caseLabels (Set.toList missing)
    missing =   Set.fromList allLabels `Set.difference` Set.fromList caseLabels
    (allLabels,_) = unzip allTyCtors
    (tyName, allTyCtors)
      | TCon tyN _ : _ <- typs
      , Just TCTyDecl { tctyDef = TCTyUnion cs } <- Map.lookup tyN declTys 
      = (tyN, cs)
      | otherwise = panic "Expecting a known tycon" (map (show . pp) typs)
      
    (typs, caseLabels) = unzip $ map extractLbl allPats
    extractLbl (TCConPat typ lbl _) = (typ, lbl)
    extractLbl pat = panic "Saw a non-constructor pattern" [show (pp pat)]

missingMaybePatterns :: Map TCTyName TCTyDecl -> [TCPat] ->
                        (PatternCompleteness, CaseSummary)                        
missingMaybePatterns _declTys allPats
  | _ : _ <- Set.toList missing = (Incomplete, summary)
  | otherwise                   = (Complete, summary)
  where
    summary = MaybeCase caseLabels
    missing =   Set.fromList allLabels `Set.difference` Set.fromList caseLabels
    allLabels = [Nothing, Just ()]      
    caseLabels = map extractLbl allPats
    extractLbl (TCJustPat    {}) = Just ()
    extractLbl (TCNothingPat {}) = Nothing
    extractLbl pat = panic "Saw a non-maybe pattern" [show (pp pat)]

missingBoolPatterns :: Map TCTyName TCTyDecl -> [TCPat] ->
                       (PatternCompleteness, CaseSummary)
missingBoolPatterns _declTys allPats
  | _ : _ <- Set.toList missing = (Incomplete, summary)
  | otherwise                   = (Complete, summary)
  where
    summary = BoolCase caseLabels
    missing =   Set.fromList allLabels `Set.difference` Set.fromList caseLabels
    allLabels = [True, False]
    caseLabels = map extractLbl allPats
    extractLbl (TCBoolPat b) = b
    extractLbl pat = panic "Saw a non-bool pattern" [show (pp pat)]

-- A bit hacky, mainly works for small n
missingNumPatterns :: Map TCTyName TCTyDecl -> [TCPat] ->
                      (PatternCompleteness, CaseSummary)
missingNumPatterns _declTys allPats
  | Just n <- tySize (head typs)
  , caseCount < n    = (Incomplete, summary)
  -- Can't figure out a finite size
  | Nothing <- tySize (head typs) = (Incomplete, summary)
  | otherwise        = (Complete, summary)
  where
    summary = NumberCase (head typs) caseNums
    caseCount = Set.size (Set.fromList caseNums)
 
    tySize :: Type -> Maybe Int
    tySize ty =
      case ty of
        Type (TByteClass)            -> Just 256
        Type (TUInt (Type (TNum n))) -> Just (2 ^ n)
        Type (TSInt (Type (TNum n))) -> Just (2 ^ n)
        _                            -> Nothing
      
    (typs, caseNums) = unzip $ map extractLbl allPats
    extractLbl (TCNumPat typ n _) = (typ, n)
    extractLbl pat = panic "Saw a non-constructor pattern" [show (pp pat)]

summariseCase ::
  PP a => Map TCTyName TCTyDecl -> TC a k -> (PatternCompleteness, CaseSummary)
summariseCase declTys tc =
  case texprValue tc of
    -- FIXME: Maybe have the class of case in the ctor?
    TCCase _v alts m_def ->
      case firstPat of
        TCConPat {}     -> upd $ missingConPatterns   declTys allPats
        TCNumPat {}     -> upd $ missingNumPatterns   declTys allPats
        TCBoolPat {}    -> upd $ missingBoolPatterns  declTys allPats        
        TCNothingPat {} -> upd $ missingMaybePatterns declTys allPats
        TCJustPat {}    -> upd $ missingMaybePatterns declTys allPats
        _               -> panic "Unexpected pattern" [show (pp firstPat)]
      where
        upd x | isNothing m_def = x
              | otherwise       = (Complete, snd x)
        allPats = concatMap tcAltPatterns alts
        firstPat = head (tcAltPatterns (NE.head alts))
        
    _ -> panic "Expected a TCCase" [show (pp tc)]
        
--------------------------------------------------------------------------------
-- Module-level checks

missingPatterns ::
  forall a k. (PP a, HasRange a) => Map TCTyName TCTyDecl -> TC a k ->
  [ (SourceRange, CaseSummary ) ]
missingPatterns declTys = go
  where
    go :: forall k'. TC a k' -> [ (SourceRange, CaseSummary) ]
    go tc = case texprValue tc of
      TCCase v alts _
        | kindOf (typeOf tc) == KValue ->
          [ (range tc, s) | (Incomplete, s) <- [summariseCase declTys tc]] ++
          go v ++
          concatMap (go . tcAltBody) alts
      tcf -> foldMapTCF go tcf

patComplete :: Map TCTyName TCTyDecl ->
               [Rec (TCDecl SourceRange)] ->
               [ (Name, SourceRange, CaseSummary) ]
patComplete declTys decls = concatMap go allDecls
  where
    go TCDecl { tcDeclName = fn, tcDeclDef = Defined def } =
      [ (fn, sr, mp) | (sr, mp) <- missingPatterns declTys def ]
    go _ = []
    allDecls = forgetRecs decls
               
--------------------------------------------------------------------------------
-- Instances

instance PP CaseSummary where
  pp x =
    case x of
      UnionCase tyN present missing -> "Case over" <+> pp tyN <+> "includes"
                                       <+> commaSep (map pp present)
                                       <+> "and omits" <+> commaSep (map pp missing)
      MaybeCase ms -> "Case over maybe includes"
                      <+> commaSep [ if isNothing y then "nothing" else "just"
                                   | y <- ms ]
      NumberCase ty present -> "Case over" <+> pp ty <+> "includes"
                               <+> commaSep (map pp present)
      BoolCase present -> "Caes over bool inclues"
                          <+> commaSep (map pp present)
      
instance PP PatternCompleteness where
  pp Incomplete = "incomplete"
  pp Complete   = "complete"
