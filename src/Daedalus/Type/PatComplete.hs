{-# Language GADTs, ScopedTypeVariables #-}
{-# Language PatternGuards, OverloadedStrings #-}

-- Pattern completeness for case expressions

module Daedalus.Type.PatComplete ( patComplete
                                 , missingPatternsTC
                                 , MissingPatterns(..)
                                 ) where

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

-- Note: we are guaranteed at least one pattern is present.
data MissingPatterns =
  MissingConstructors [Label]
  | MissingMaybe (Maybe ())
  | MissingNumbers
  | MissingBool Bool

missingConPatterns :: Map TCTyName TCTyDecl -> [TCPat] ->
                      Maybe MissingPatterns
missingConPatterns declTys allPats
  | Set.null missing = Nothing
  | otherwise        = Just (MissingConstructors $ Set.toList missing)
  where
    missing =   Set.fromList allLabels `Set.difference` Set.fromList caseLabels
    allLabels = fst (unzip allTyCtors)
    allTyCtors
      | TCon tyN _ : _ <- typs
      , Just TCTyDecl { tctyDef = TCTyUnion cs } <- Map.lookup tyN declTys 
      = cs
      | otherwise = panic "Expecting a known tycon" (map (show . pp) typs)
      
    (typs, caseLabels) = unzip $ map extractLbl allPats
    extractLbl (TCConPat typ lbl _) = (typ, lbl)
    extractLbl pat = panic "Saw a non-constructor pattern" [show (pp pat)]

missingMaybePatterns :: Map TCTyName TCTyDecl -> [TCPat] ->
                        Maybe MissingPatterns
missingMaybePatterns _declTys allPats
  | c : _ <- Set.toList missing = Just (MissingMaybe c)
  | otherwise                   = Nothing
  where
    missing =   Set.fromList allLabels `Set.difference` Set.fromList caseLabels
    allLabels = [Nothing, Just ()]      
    caseLabels = map extractLbl allPats
    extractLbl (TCJustPat    {}) = Just ()
    extractLbl (TCNothingPat {}) = Nothing
    extractLbl pat = panic "Saw a non-maybe pattern" [show (pp pat)]

missingBoolPatterns :: Map TCTyName TCTyDecl -> [TCPat] ->
                        Maybe MissingPatterns
missingBoolPatterns _declTys allPats
  | b : _ <- Set.toList missing = Just (MissingBool b)
  | otherwise                   = Nothing
  where
    missing =   Set.fromList allLabels `Set.difference` Set.fromList caseLabels
    allLabels = [True, False]
    caseLabels = map extractLbl allPats
    extractLbl (TCBoolPat b) = b
    extractLbl pat = panic "Saw a non-bool pattern" [show (pp pat)]

-- A bit hacky, mainly works for small n
missingNumPatterns :: Map TCTyName TCTyDecl -> [TCPat] ->
                      Maybe MissingPatterns
missingNumPatterns _declTys allPats
  | Just n <- tySize (head typs)
  , caseCount < n    = Just MissingNumbers
  -- Can't figure out a finite size
  | Nothing <- tySize (head typs) = Just MissingNumbers
  | otherwise        = Nothing
  where
    caseCount = Set.size (Set.fromList caseNums)

    tySize :: Type -> Maybe Int
    tySize ty =
      case ty of
        Type (TByteClass)            -> Just 256
        Type (TUInt (Type (TNum n))) -> Just (2 ^ n)
        Type (TSInt (Type (TNum n))) -> Just (2 ^ n)
        _                            -> Nothing
      
    (typs, caseNums) = unzip $ map extractLbl allPats
    extractLbl (TCNumPat typ n) = (typ, n)
    extractLbl pat = panic "Saw a non-constructor pattern" [show (pp pat)]

missingPatternsTC :: Map TCTyName TCTyDecl -> TC a k -> Maybe MissingPatterns
missingPatternsTC declTys tc =
  case texprValue tc of
    TCCase _v _alts (Just _) -> Nothing
    -- FIXME: Maybe have the class of case in the ctor?
    TCCase _v alts _ ->
      case firstPat of
        TCConPat {}     -> missingConPatterns   declTys allPats
        TCNumPat {}     -> missingNumPatterns   declTys allPats
        TCBoolPat {}    -> missingBoolPatterns  declTys allPats        
        TCNothingPat {} -> missingMaybePatterns declTys allPats
        TCJustPat {}    -> missingMaybePatterns declTys allPats
        _               -> panic "Unexpected pattern" [show (pp firstPat)]
      where
        allPats = concatMap tcAltPatterns alts
        firstPat = head (tcAltPatterns (NE.head alts))
        
    _ -> panic "Expected a TCCase" [show (pp tc)]
        
--------------------------------------------------------------------------------
-- Module-level checks

missingPatterns :: forall a k. HasRange a => Map TCTyName TCTyDecl -> TC a k ->
                   [ (SourceRange, MissingPatterns) ]
missingPatterns declTys = go
  where
    go :: forall k'. TC a k' -> [ (SourceRange, MissingPatterns) ]
    go tc = case texprValue tc of
      TCCase v alts _
        | kindOf (typeOf tc) == KValue ->
          maybe [] (\r -> [(range tc, r)]) (missingPatternsTC declTys tc) ++
          go v ++
          concatMap (go . tcAltBody) alts
      tcf -> foldMapTCF go tcf

patComplete :: Map TCTyName TCTyDecl ->
               [Rec (TCDecl SourceRange)] ->
               [ (Name, SourceRange, MissingPatterns) ]
patComplete declTys decls = concatMap go allDecls
  where
    go TCDecl { tcDeclName = fn, tcDeclDef = Defined def } =
      [ (fn, sr, mp) | (sr, mp) <- missingPatterns declTys def ]
    go _ = []
    allDecls = forgetRecs decls
               
--------------------------------------------------------------------------------
-- Instances

instance PP MissingPatterns where
  pp x =
    case x of
      MissingConstructors cs -> "Missing constructors:" <+> commaSep (map pp cs)
      MissingMaybe v -> "Missing case: " <+> maybe "nothing" (const "just") v
      MissingNumbers -> "Missing numbers"
      MissingBool b  -> "Missing bool:" <+> pp b
      

