{-# Language TupleSections, GeneralizedNewtypeDeriving #-}
{-# Language BlockArguments #-}
module Daedalus.Core.Determinize2 (determinizeModule2) where

import Debug.Trace(trace)

import Data.Text (pack)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import MonadLib
import Data.Word(Word8)
import Data.Maybe(fromJust)
import Data.ByteString(ByteString, uncons, null)

--import Daedalus.Panic(panic)
--import Daedalus.PP(pp)
import Daedalus.GUID(GUID, firstValidGUID, succGUID)

import Daedalus.Core.Decl
import Daedalus.Core.Expr
import Daedalus.Core.ByteSet
import Daedalus.Core.Grammar
    ( Grammar(..), Match(MatchByte, MatchBytes), Sem(..), ErrorSource (ErrorFromSystem)  )
import Daedalus.Core.Basics
import Daedalus.Core.Type (typeOf)
-- import Daedalus.Type.AST (Arg(GrammarArg))

determinizeModule2 :: Module -> Module
determinizeModule2 m  =
  let newGram = map (fmap (detGram m)) (mGFuns m) in
  m {mGFuns = newGram}

detGram :: Module -> Grammar -> Grammar
detGram modl gram =
  detGo gram

  where
  detGo g = case g of
    Pure _      -> g
    GetStream   -> g
    SetStream _ -> g
    Match _ _   -> g
    Fail {}     -> g
    Do_ g1 g2     -> Do_     (detGo g1) (detGo g2)
    Do name g1 g2 -> Do name (detGo g1) (detGo g2)
    Let name e g1 -> Let name e (detGo g1)
    OrBiased {}   -> detOr modl g
    OrUnbiased {} -> detOr modl g
    Call {} -> g
    Annot {} -> g
    GCase (Case name lst) ->
      GCase (Case name (map (\ (pat, g1) -> (pat, detGo g1)) lst))

getGrammarDef :: Module -> FName -> Maybe Grammar
getGrammarDef modl name =
  goGetGrammar (mGFuns modl)
  where
  goGetGrammar [] = error "cannot find grammar"
  goGetGrammar ( Fun {fName = n1, fDef = fdef} : rest ) =
    if fnameId n1 == fnameId name
    then case fdef of
           Def g2 -> Just g2
           External -> Nothing
    else goGetGrammar rest

getByteSetDef :: Module -> FName -> Maybe ByteSet
getByteSetDef modl name =
  goGetByteSet (mBFuns modl)
  where
  goGetByteSet [] = error "cannot find grammar"
  goGetByteSet ( Fun {fName = n1, fDef = fdef} : rest ) =
    if fnameId n1 == fnameId name
    then case fdef of
           Def g2 -> Just g2
           External -> Nothing
    else goGetByteSet rest



{- Zipped Grammar -}
data ZMatch =
    ZMatchByte ByteSet
  | ZMatchBytes ([Word8], ByteString) ByteString -- the pair is the zipper of a ByteString
                                                 -- the last ByteString is the original one in the grammar
  | ZMatchEnd

data ZGrammar =
    ZMatch Sem
  | ZDo_ Grammar
  | ZDo_2 Grammar
  | ZDo Name Grammar
  | ZDo2 Name Grammar
  | ZOr
  -- | ZAnnot Annot

type PathGrammar = [ ZGrammar ]

data ZipGrammar =
    ZipNode { focus :: Grammar, path :: PathGrammar }
  | ZipLeaf { zmatch :: ZMatch, path :: PathGrammar }

emptyPathGrammar :: PathGrammar
emptyPathGrammar = []

mkPathGrammar :: ZGrammar -> PathGrammar -> PathGrammar
mkPathGrammar n z = n : z

initZipGrammar :: Grammar -> ZipGrammar
initZipGrammar gram = ZipNode {focus = gram, path = emptyPathGrammar}

mkZipGrammar :: Grammar -> PathGrammar -> ZipGrammar
mkZipGrammar g p = ZipNode g p

fromCharSetToSet :: Module -> CharSet -> Maybe (Set Integer)
fromCharSetToSet modl b =
  go b
  where
  go bc =
    let allBytes = (foldr (\ i s -> Set.insert i s) Set.empty [0 .. 255]) in
    case bc of
      CWord8 w -> Just (Set.singleton $ toInteger w)
      CByteSet SetAny -> Just allBytes
      CByteSet (SetSingle (Ap0 (IntL s  (TUInt (TSize 8))))) -> Just (Set.singleton s)
      CByteSet (SetRange  (Ap0 (IntL s1 (TUInt (TSize 8)))) (Ap0 (IntL s2 (TUInt (TSize 8))))) ->
        if s1 <= s2 && s1 >=0 && s2 <= 255
        then Just (foldr (\ i s -> Set.insert i s) Set.empty [s1 .. s2])
        else Nothing
      CByteSet (SetComplement bs1) -> do
        s1 <- go (CByteSet bs1)
        return $ Set.difference allBytes s1
      CByteSet (SetUnion bs1 bs2) -> do
        s1 <- go (CByteSet bs1)
        s2 <- go (CByteSet bs2)
        Just (Set.union s1 s2)
      CByteSet (SetCall name []) -> do
        s1 <- getByteSetDef modl name
        go (CByteSet s1)
      _      -> Nothing

data BackTree =
    Alt   BackTree BackTree
  | BTake Int CharSet BackTree
  | BCut  BackTree
  | Done  Grammar
  | Next  Int Grammar PathGrammar
  | FailInput
  | FailCut



genGUID :: Int -> GUID
genGUID n =
  go n (firstValidGUID)
  where
  go i r =
    if i <= 0 then r else go (i-1) (succGUID r)

mkVarGrammar :: Int -> Grammar
mkVarGrammar n =
  let guid = genGUID n in
  let name = Name { nameId = guid,
                    nameText = Nothing,
                    nameType = TUInt (TSize 8)
                  } in
  Pure (Var name)

-- der down
der :: Int -> Grammar -> PathGrammar -> BackTree
der n g k =
  case g of
    Pure v         -> next n (Pure v) k
    Match _ (MatchByte b) -> BTake n (CByteSet b) (eps n (mkVarGrammar n) k)
    OrBiased g1 g2 -> Alt (der n g1 (ZOr : k)) (der n g2 (ZOr : k))
    Do m g1 g2     -> der n g1 (ZDo m g2 : k)
    Do_ g1 g2      -> der n g1 (ZDo_  g2 : k)
    _ -> error "unhandled case"

-- der up
next :: Int -> Grammar -> PathGrammar -> BackTree
next _ g [] = Done g
next n g (k1: k) =
  case k1 of
    ZDo  m g2 -> der  n g2 (ZDo2 m g : k)
    ZDo_   g2 -> der  n g2 (ZDo_2  g : k)
    ZDo2 m g1 -> next n (Do m g1 g) k
    ZDo_2  g1 -> next n (Do_  g1 g) k
    ZOr       -> BCut (next n g k)
    _ -> error "unhandled case"

-- epsilon up
eps :: Int -> Grammar -> PathGrammar -> BackTree
eps _ g [] = Done g
eps n g (k1: k) =
  case k1 of
    ZDo  m g2  -> epsdo n g2 (ZDo2 m g : k)
    ZDo_   g2  -> epsdo n g2 (ZDo_2  g : k)
    ZDo2 m g1 -> eps   n (Do m g1 g) k
    ZDo_2  g1 -> eps   n (Do_  g1 g) k
    ZOr       -> BCut (eps n g k)
    _ -> error "unhandled case"

-- epsilon down
epsdo :: Int -> Grammar -> PathGrammar -> BackTree
epsdo n g k =
  case g of
    Pure v -> eps n (Pure v) k
    Match _ (MatchByte _b) -> Next n g k
    OrBiased _g1 _g2       -> Next n g k
    Do _ _g1 _g2           -> Next n g k
    Do_  _g1 _g2           -> Next n g k
    _ -> error "unhandled case"


dummy :: BackTree -> Int
dummy _t = 0

-- take a backtree at step n and return the backtree at step n+1
derivStep :: BackTree -> BackTree
derivStep t = error "not implemented"

isUnambiguous :: BackTree -> Bool
isUnambiguous t = case t of
  Alt bt bt' -> error "unhandled case"
  BTake _ _ _ -> error "unhandled case"
  BCut bt -> error "unhandled case"
  Done gram -> error "unhandled case"
  Next _ _ _ -> error "unhandled case"


trimi :: Module -> [(Int, Integer)] -> BackTree -> BackTree
trimi modl inp t =
  go t
  where
  go tr =
    case tr of
      Alt t1 t2 -> Alt (go t1) (go t2)
      BTake n b t1 ->
        let i = fromJust $ lookup n inp in
        if Set.member i (fromJust $ fromCharSetToSet modl b)
        then
          BTake n b (go t1)
        else FailInput
      BCut t1    -> BCut (go t1)
      Done gram  -> Done gram
      Next n g k -> Next n g k
      FailInput  -> FailInput
      FailCut    -> FailCut
      -- _ -> error ""

data TreeInfo =
    BFail
  | BOne
  | BDunno

trimfail :: BackTree -> TreeInfo
trimfail tr = case tr of
  Alt t1 t2 ->
    let i1 = trimfail t1
        i2 = trimfail t2
    in
    let f = case (i2, i2) of
              (BFail, BFail)  -> BFail
              (BFail, BOne)   -> BOne
              (BFail, BDunno) -> BDunno
              (BOne,  BFail)   -> BOne
              (BOne,  BOne)    -> BDunno
              (BOne,  BDunno)  -> BDunno
              (BDunno, _)     -> BDunno
    in f
  BTake n cs t1 -> trimfail t1
  BCut t1       -> trimfail t1
  Done g1     -> BOne
  Next n g1 zgs -> BOne
  FailInput     -> BFail
  FailCut   -> BFail



{- END of Zipped Grammar -}

data CharSet =
    CByteSet ByteSet
  | CWord8 Word8

data Deriv =
    DerivStart      [ZipGrammar]
  | DerivUnResolved [(Set Integer, [ZipGrammar])]
  | DerivResolved   [(Integer, Resolution)]

data Resolution =
    YesResolved   ZipGrammar
  | NoResolved    Deriv

detOr :: Module -> Grammar -> Grammar
detOr modl grammar =
  let t = der 0 grammar emptyPathGrammar in
  trace (show $ (dummy t)) $
  grammar