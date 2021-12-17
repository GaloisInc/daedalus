{-# Language TupleSections, GeneralizedNewtypeDeriving #-}
{-# Language BlockArguments #-}
module Daedalus.Core.Determinize (determinizeModule) where

import Debug.Trace(trace)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import MonadLib
import Data.List(partition)
import Data.Graph.SCC(stronglyConnComp)
import Data.Graph(SCC(..))

import Daedalus.Panic(panic)
import Daedalus.PP(pp)
import Daedalus.Rec(Rec(..))
import Daedalus.GUID(HasGUID, GUID, firstValidGUID)

import Daedalus.Core.Free(FreeVars(freeFVars))
import Daedalus.Core.Subst

import Daedalus.Core.Decl
import Daedalus.Core.Expr
import Daedalus.Core.ByteSet
import Daedalus.Core.Grammar
import Daedalus.Core.Basics
-- import Daedalus.Type.AST (Arg(GrammarArg))

determinizeModule :: Module -> Module
determinizeModule m  =
  let newGram = map (fmap detGram) (mGFuns m) in
  m {mGFuns = newGram}

detGram :: Grammar -> Grammar
detGram g =
  case g of
    Pure _      -> g
    GetStream   -> g
    SetStream _ -> g
    Match _ _   -> g
    Fail {}     -> g
    Do_ g1 g2     -> Do_     (detGram g1) (detGram g2)
    Do name g1 g2 -> Do name (detGram g1) (detGram g2)
    Let name e g1 -> Let name e (detGram g1)
    OrBiased {}    -> g
    OrUnbiased _ _ -> detOrUnbiased g
    Call {}  -> g
    Annot {} -> g
    GCase {} -> g


data ZGrammar =
    ZTop
  | ZDo_ Grammar
  | ZDo Name Grammar
  | ZYesSem

type ZipGrammar = [ ZGrammar ]

detOrUnbiased :: Grammar -> Grammar
detOrUnbiased g =
  let orLst = listOrUnbiased g in
  let derLst = tryDet orLst in
  let linDerLst = do
        linLst <- derLst
        traverse (\ (a, b, c) -> do { d <- charListFromByteSet a ; return (d, b, c) }) linLst
        -- sequenceA (map (\ (a, b, c) -> do { d <- charListFromByteSet a ; return (d, b, c) })
  in
  case linDerLst of
    Nothing  -> g
    Just a ->
      if checkNonOverlapping a
      then translateToCase a
      else g

  where
  listOrUnbiased :: Grammar -> [Grammar]
  listOrUnbiased g =
    case g of
      OrUnbiased g1 g2 -> listOrUnbiased g1 ++ listOrUnbiased g2
      _                -> [g]

  tryDet :: [Grammar] -> Maybe [(ByteSet, Sem, Maybe ZipGrammar)]
  tryDet lst =
    mapM closure lst

  closure :: Grammar -> Maybe (ByteSet, Sem, Maybe ZipGrammar)
  closure g =
    case g of
      Let {}    -> Nothing
      Do_ g1 g2 ->
        case g1 of
          Match {} -> deriveMatch g1 (Just [ZDo_ g2])
          _        -> Nothing
      Do name g1 g2 ->
        case g1 of
          Match {} -> deriveMatch g1 (Just [ZDo name g2])
          _        -> Nothing
      Match {} -> deriveMatch g Nothing
      _        -> Nothing

  deriveMatch :: Grammar -> Maybe ZipGrammar -> Maybe (ByteSet, Sem, Maybe ZipGrammar)
  deriveMatch (Match sem match) gCont =
    case match of
      MatchByte b ->
        case b of
          SetAny -> Just (b, sem, gCont)
          SetSingle (Ap0 (IntL s (TUInt (TSize 8)))) ->
            trace (show (pp s)) $ Just (b, sem, gCont)
          _ -> Nothing
      _ -> Nothing
  deriveMatch _ _ = error "function should be applied to Match"

  charListFromByteSet :: ByteSet -> Maybe (Set Integer)
  charListFromByteSet b =
    case b of
      SetAny -> Just (foldr (\ i s -> Set.insert i s) Set.empty [0 .. 255])
      SetSingle (Ap0 (IntL s (TUInt (TSize 8)))) -> Just (Set.singleton s)
      SetRange (Ap0 (IntL s1 (TUInt (TSize 8)))) (Ap0 (IntL s2 (TUInt (TSize 8)))) ->
        if s1 <= s2 && s1 >=0 && s2 <= 255
        then Just (foldr (\ i s -> Set.insert i s) Set.empty [s1 .. s2])
        else Nothing
      _      -> Nothing

  checkNonOverlapping :: [(Set Integer, Sem, Maybe ZipGrammar)] -> Bool
  checkNonOverlapping [] = True
  checkNonOverlapping ((s, _, _) : rs) =
    if checkOnTail rs
    then checkNonOverlapping rs
    else False
    where
    checkOnTail [] = True
    checkOnTail ((t, _, _) : rest) = if Set.disjoint s t then checkOnTail rest else False


  translateToCase :: [(Set Integer, Sem, Maybe ZipGrammar)] -> Grammar
  translateToCase lst =
    let name = Name { nameId = firstValidGUID ,
                      nameText = Nothing,
                      nameType = TUInt (TSize 8)
                    } in
    Do name (Match SemYes (MatchByte SetAny))
     (GCase
       (Case
         name
        (concatMap (\ (s, sem, g1) -> map (\ b -> buildCase (b, sem, g1)) (Set.toList s)) lst)))

    where
    buildCase :: (Integer, Sem, Maybe ZipGrammar) -> (Pattern, Grammar)
    buildCase (c, SemYes, Nothing) = (PNum c, Pure (Ap0 (IntL c (TUInt (TSize 8)))))
    buildCase (c, SemNo, Nothing)  = (PNum c, Pure (Ap0 Unit))
    buildCase (c, s,     Just g1) = (PNum c, buildContinuation c s g1)

    buildContinuation :: Integer -> Sem -> ZipGrammar -> Grammar
    buildContinuation _ _ [] = Pure (Ap0 Unit)
    buildContinuation c SemYes [ZDo_ g2] =
      let name = Name { nameId = firstValidGUID ,
                        nameText = Nothing,
                        nameType = TUInt (TSize 8)
                      } in
      Let name (Ap0 (IntL c (TUInt (TSize 8)))) g2
    buildContinuation _c SemNo [ZDo_ g2] = g2
    buildContinuation c SemYes [ZDo name g2] =
      Let name (Ap0 (IntL c (TUInt (TSize 8)))) g2
    buildContinuation _c SemNo [ZDo _name g2] = g2
    buildContinuation _ _ _ = Pure (Ap0 Unit)
