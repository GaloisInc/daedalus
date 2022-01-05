{-# Language TupleSections, GeneralizedNewtypeDeriving #-}
{-# Language BlockArguments #-}
module Daedalus.Core.Determinize (determinizeModule) where

import Debug.Trace(trace)

import Data.Text (pack)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import MonadLib
import Data.List(partition)
import Data.Word(Word8)
import Data.ByteString(ByteString, uncons, empty)
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
import Daedalus.Core.Semantics.Grammar (evalByteSet)
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
    OrBiased {}   -> detOr g
    OrUnbiased {} -> detOr g
    Call {}  -> g
    Annot {} -> g
    GCase (Case name lst) ->
      GCase (Case name (map (\ (pat, g1) -> (pat, detGram g1)) lst))



{- Zipped Grammar -}
data ZMatch =
    ZMatchByte ByteSet
  | ZMatchBytes (Maybe ByteString) ByteString
  | ZMatchEnd

data ZGrammar =
    ZBot
  | ZMatch Sem
  | ZDo_ Grammar
  | ZDo Name Grammar
  | ZAnnot Annot

type PathGrammar = [ ZGrammar ]

data ZipGrammar =
    ZipNode{ focus :: Grammar, path :: PathGrammar }
  | ZipLeaf{ zmatch :: ZMatch, path :: PathGrammar }

emptyPathGrammar :: PathGrammar
emptyPathGrammar = []

mkPathGrammar :: ZGrammar -> PathGrammar -> PathGrammar
mkPathGrammar n z = n : z

initZipGrammar :: Grammar -> ZipGrammar
initZipGrammar gram = ZipNode {focus = gram, path = emptyPathGrammar}

mkZipGrammar :: Grammar -> PathGrammar -> ZipGrammar
mkZipGrammar g p = ZipNode g p

goLeft :: ZipGrammar -> ZipGrammar
goLeft (ZipNode {focus = foc, path = pth}) =
  case foc of
    Do_ g1 g2     -> ZipNode {focus = g1, path = mkPathGrammar (ZDo_ g2) pth }
    Do name g1 g2 -> ZipNode {focus = g1, path = mkPathGrammar (ZDo name g2) pth }
    Annot ann g1  -> ZipNode {focus = g1, path = mkPathGrammar (ZAnnot ann) pth}
    _ -> error "should not happen"
goLeft (ZipLeaf {}) = error "should not happen"

{- END of Zipped Grammar -}

data NextChar =
    NCByteSet ByteSet
  | NCWord8 Word8

detOr :: Grammar -> Grammar
detOr grammar =
  let orLst = getListOr grammar in
  let derLst = tryDeterminizeListOr orLst in
  let linDerLst = do
        linLst <- derLst
        forM linLst (\ (a, c) -> do { d <- charListFromByteSet a ; return (d, c) })
  in
  case linDerLst of
    Nothing  -> grammar
    Just a -> if checkNonOverlapping a
              then translateToCase a
              else grammar

  where
  getListOr :: Grammar -> [Grammar]
  getListOr g =
    case g of
      OrUnbiased g1 g2 -> getListOr g1 ++ getListOr g2
      OrBiased   g1 g2 -> getListOr g1 ++ getListOr g2
      _                -> [g]

  tryDeterminizeListOr :: [Grammar] -> Maybe [(NextChar, ZipGrammar)]
  tryDeterminizeListOr lst =
    mapM deriveOneByte lst

  deriveOneByte :: Grammar -> Maybe (NextChar, ZipGrammar)
  deriveOneByte gram =
    deriveGo (initZipGrammar gram)
    where
    deriveGo :: ZipGrammar -> Maybe (NextChar, ZipGrammar)
    deriveGo gr@(ZipNode {focus = g}) =
      case g of
        Let {}    -> Nothing
        Do_ {} -> deriveGo (goLeft gr)
        Do {} -> deriveGo (goLeft gr)
        Match {} -> deriveMatch gr
        Annot {} -> deriveGo (goLeft gr)
        _        -> Nothing
    deriveGo (ZipLeaf {zmatch = zm, path = pth}) =
      case zm of
        ZMatchByte _ -> error "TODO should move up"
        ZMatchBytes prev next ->
          case uncons next of
            Nothing -> error "TODO should move up"
            Just (w, rest) ->
              Just (NCWord8 w, ZipLeaf { zmatch = ZMatchBytes prev rest, path = pth})
        ZMatchEnd -> error "TODO END"


  deriveMatch :: ZipGrammar -> Maybe (NextChar, ZipGrammar)
  deriveMatch (ZipNode { focus = Match sem match, path = pth}) =
    let newPath = mkPathGrammar (ZMatch sem) pth in
    case match of
      MatchByte b ->
        case b of
          SetAny ->
            let x = ZipLeaf{ zmatch = ZMatchByte b, path = newPath} in
            Just (NCByteSet b, x)
          SetSingle (Ap0 (IntL _s (TUInt (TSize 8)))) ->
            let x = ZipLeaf{ zmatch = ZMatchByte b, path = newPath} in
            Just (NCByteSet b, x)
          _ -> Nothing
      MatchBytes b ->
        case b of
          Ap0 (ByteArrayL bs) ->
            case uncons bs of
              Nothing -> error "TODO should move up"
              Just (w, rest) ->
                Just (NCWord8 w, ZipLeaf { zmatch = ZMatchBytes (Just empty) rest, path = newPath})
          _ -> Nothing
      _ -> Nothing
  deriveMatch _ = error "function should be applied to Match"

  charListFromByteSet :: NextChar -> Maybe (Set Integer)
  charListFromByteSet b =
    case b of
      NCWord8 w -> Just (Set.singleton $ toInteger w)
      NCByteSet SetAny -> Just (foldr (\ i s -> Set.insert i s) Set.empty [0 .. 255])
      NCByteSet (SetSingle (Ap0 (IntL s (TUInt (TSize 8))))) -> Just (Set.singleton s)
      NCByteSet (SetRange (Ap0 (IntL s1 (TUInt (TSize 8)))) (Ap0 (IntL s2 (TUInt (TSize 8))))) ->
        if s1 <= s2 && s1 >=0 && s2 <= 255
        then Just (foldr (\ i s -> Set.insert i s) Set.empty [s1 .. s2])
        else Nothing
      _      -> Nothing

  checkNonOverlapping :: [(Set Integer, ZipGrammar)] -> Bool
  checkNonOverlapping [] = True
  checkNonOverlapping ((s, _) : rs) =
    if checkOnTail rs
    then checkNonOverlapping rs
    else False

    where
    checkOnTail [] = True
    checkOnTail ((t, _) : rest) = if Set.disjoint s t then checkOnTail rest else False


  translateToCase :: [(Set Integer, ZipGrammar)] -> Grammar
  translateToCase lst =
    let name = Name { nameId = firstValidGUID ,
                      nameText = Nothing,
                      nameType = TUInt (TSize 8)
                    } in
    Annot (SrcAnnot $ Data.Text.pack "DETERMINIZE HERE") $
      Do name (Match SemYes (MatchByte SetAny))
      (GCase
        (Case
          name
          (concatMap (\ (s, g1) -> map (\ b -> buildCase (b, g1)) (Set.toList s)) lst)))

    where
    buildCase :: (Integer, ZipGrammar) -> (Pattern, Grammar)
    --buildCase (c, SemYes, Nothing) = (PNum c, Pure (Ap0 (IntL c (TUInt (TSize 8)))))
    --buildCase (c, SemNo , Nothing) = (PNum c, Pure (Ap0 Unit))
    buildCase (c, g1) = (PNum c, buildLeaf c g1)

    buildLeaf :: Integer -> ZipGrammar -> Grammar
    buildLeaf c (ZipLeaf{ zmatch = ZMatchByte _, path = ZMatch SemYes : pth}) =
      let newBuilt = Pure (Ap0 (IntL c (TUInt (TSize 8)))) in
      buildUp (mkZipGrammar newBuilt pth)
    buildLeaf _c (ZipLeaf{ zmatch = ZMatchByte _, path = ZMatch SemNo : pth}) =
      let newBuilt = Pure (Ap0 Unit) in
      buildUp (mkZipGrammar newBuilt pth)
    buildLeaf _ _ = error "Should not happen"


    buildUp :: ZipGrammar -> Grammar
    buildUp (ZipNode {focus = built, path = pth}) =
      case pth of
        [] -> built
        (ZDo_ g2 : z) ->
          {-let name = Name { nameId = firstValidGUID,
                              nameText = Nothing,
                              nameType = TUInt (TSize 8)
                            } in -}
          let newBuilt = Do_ built g2 in
       -- let newBuilt = Let name (Ap0 (IntL c (TUInt (TSize 8)))) g2 in
          buildUp (mkZipGrammar newBuilt z)
        (ZDo name g2 : z) ->
          let newBuilt = Do name built g2 in
          buildUp (mkZipGrammar newBuilt z)
        (ZAnnot ann : z) ->
          buildUp (mkZipGrammar (Annot ann built) z)
        _  -> error "case not handled"
    buildUp (ZipLeaf {}) = error "broken invariant"
