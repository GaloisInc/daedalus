{-# Language TupleSections, GeneralizedNewtypeDeriving #-}
{-# Language BlockArguments #-}
module Daedalus.Core.Determinize (determinizeModule) where

import Debug.Trace(trace)

import Data.Text (pack)
import Data.Set (Set)
import qualified Data.Set as Set
import MonadLib
import Data.Word(Word8)
import Data.ByteString(ByteString, uncons, null, pack)

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

determinizeModule :: Module -> Module
determinizeModule m  =
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
  | ZLet Name Expr
  | ZAnnot Annot

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

goLeft :: ZipGrammar -> ZipGrammar
goLeft (ZipNode {focus = foc, path = pth}) =
  case foc of
    Do_ g1 g2     -> ZipNode { focus = g1, path = mkPathGrammar (ZDo_ g2) pth }
    Do name g1 g2 -> ZipNode { focus = g1, path = mkPathGrammar (ZDo name g2) pth }
    Annot ann g1  -> ZipNode { focus = g1, path = mkPathGrammar (ZAnnot ann) pth }
    Let name e g1 -> ZipNode { focus = g1, path = mkPathGrammar (ZLet name e) pth }
    _ -> error "should not happen"
goLeft (ZipLeaf {}) = error "should not happen"

goUpRight :: ZipGrammar -> Maybe ZipGrammar
goUpRight (ZipNode {focus = foc, path = pth}) =
  case pth of
    [] -> Nothing
    (ZDo_ g2 : z) ->
      let newPath = ZDo_2 foc : z in
      Just (mkZipGrammar g2 newPath)
    (ZDo_2 g1 : z) ->
      goUpRight (mkZipGrammar (Do_ g1 foc) z)
    (ZDo name g2 : z) ->
        let newPath = ZDo2 name foc : z in
      Just (mkZipGrammar g2 newPath)
    (ZDo2 name g1 : z) ->
      goUpRight (mkZipGrammar (Do name g1 foc) z)
    (ZAnnot ann : z) ->
      goUpRight (mkZipGrammar (Annot ann foc) z)
    (ZLet name e : z) ->
      goUpRight (mkZipGrammar (Let name e foc) z)
    _  -> error "case not handled"
goUpRight (ZipLeaf{}) = error "cannot be a leaf"


goNextLeaf :: Integer -> ZipGrammar -> Maybe ZipGrammar
goNextLeaf c (ZipLeaf{ zmatch = ZMatchByte _, path = ZMatch sem : pth}) =
  case sem of
    SemYes ->
      let newBuilt = Pure (Ap0 (IntL c (TUInt (TSize 8)))) in
      goUpRight (mkZipGrammar newBuilt pth)
    SemNo ->
      let newBuilt = Pure (Ap0 Unit) in
      goUpRight (mkZipGrammar newBuilt pth)
goNextLeaf _c (ZipLeaf{ zmatch = zm@(ZMatchBytes (_, rest) orig), path = fpth@(ZMatch sem : pth)}) =
  if not (Data.ByteString.null rest)
  then Just $ ZipLeaf {zmatch = zm, path = fpth }
  else
    case sem of
      SemYes ->
        let pureMatch = Pure (Ap0 (ByteArrayL orig))
        in
        goUpRight (mkZipGrammar pureMatch pth)
      SemNo ->
        let newBuilt = Pure (Ap0 Unit) in
        goUpRight (mkZipGrammar newBuilt pth)
goNextLeaf _ _ = error "Should not happen"

buildUp :: ZipGrammar -> Grammar
buildUp (ZipNode {focus = built, path = pth}) =
  case pth of
    [] -> built
    (ZDo_ g2 : z) ->
      let newBuilt = Do_ built g2 in
      buildUp (mkZipGrammar newBuilt z)
    (ZDo_2 g1 : z) ->
      let newBuilt = Do_ g1 built in
      buildUp (mkZipGrammar newBuilt z)
    (ZDo name g2 : z) ->
      let newBuilt = Do name built g2 in
      buildUp (mkZipGrammar newBuilt z)
    (ZDo2 name g1 : z) ->
      let newBuilt = Do name g1 built in
      buildUp (mkZipGrammar newBuilt z)
    (ZLet name e : z) ->
      let newBuilt = Let name e built in
      buildUp (mkZipGrammar newBuilt z)
    (ZAnnot ann : z) ->
      buildUp (mkZipGrammar (Annot ann built) z)
    _  -> error "case not handled"
buildUp (ZipLeaf {}) = error "broken invariant"

{- END of Zipped Grammar -}

data CharSet =
    CByteSet ByteSet
  | CWord8 Word8

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



data Deriv =
    DerivStart      [ZipGrammar]
  | DerivUnResolved [(Set Integer, [ZipGrammar])] -- disjoing set of integers in list
  | DerivResolved   [(Integer, Resolution)]

data Resolution =
    YesResolved   ZipGrammar
  | FailResolved [ZipGrammar]
  | NoResolved    Deriv

checkUnambiguousOrDone :: Deriv -> Bool
checkUnambiguousOrDone der =
  case der of
    DerivStart {} -> False
    DerivUnResolved {} -> False
    DerivResolved lst ->
      foldr
      (\ (_a, r) b -> b && case r of
                             YesResolved {} -> True
                             NoResolved d -> checkUnambiguousOrDone d
                             FailResolved _ -> True -- that's a little weird, but it basically
                                                  -- means there is no more work to do
      ) True lst

gLLkDEPTH :: Int
gLLkDEPTH = 10

detOr :: Module -> Grammar -> Grammar
detOr modl grammar =
  let ty = typeOf grammar in
  let orLst = getListOr grammar in
  repeatStep ty (DerivStart $ map initZipGrammar orLst) 0

  where
  stepDerivFactor :: [ZipGrammar] -> Maybe Deriv
  stepDerivFactor orLst =
    let derLst = deriveOneByteOnList orLst in
    let linDerLst = do
          linLst <- derLst
          forM linLst
            (\ (a, c) -> do { d <- fromCharSetToSet modl a ; return (d, c) })
    in
    case linDerLst of
      Nothing -> Nothing
      Just a ->
        let f = factorize a in
        Just $ DerivUnResolved (map (\ (c, l) -> (c, l)) f)

  mapStep :: Deriv -> Maybe Deriv
  mapStep (DerivStart orLst) = stepDerivFactor orLst
  mapStep (DerivUnResolved opts) = do
    der <- forM opts
      (\ (c, orLst) ->
            if checkUnambiguousList orLst
            then Just (map (\ x -> (x, YesResolved (head orLst))) (Set.toList c))
            else
              forM (Set.toList c)
              (\ x -> do
                 let mepsLst = forM orLst (\ z -> goNextLeaf x z)
                 case mepsLst of
                   Nothing -> return (x, FailResolved orLst)
                   Just epsLst ->
                     let mDerLst = stepDerivFactor epsLst in
                     case mDerLst of
                       Nothing -> return (x, FailResolved orLst)
                       Just derLst -> return (x, NoResolved derLst)
              )
      )
    return $ DerivResolved (concat der)

  {-
   do
    der <- forM opts
        (\ (c, orLst) ->
              if checkUnambiguousList orLst
              then Just (map (\ x -> (x, YesResolved (head orLst))) (Set.toList c))
              else do
                pairs <- forM (Set.toList c)
                  (\ x -> do
                      newNextList <- forM orLst (\ zg -> goNextLeaf x zg) -- eps transition
                      return (x, newNextList)) in
                pairAfterStep <- forM pairs
                    (\ (x, lst) -> do
                        dzg <- stepDerivFactor lst -- deriv a char
                        return (x, dzg)
                    )
                return $ map (\ (x, tr) -> (x, NoResolved tr)) pairAfterStep
        )
    return $ DerivResolved (concat der) -}
  mapStep (DerivResolved opts) = do
      der <- forM opts
        (\ x@(c, l) ->
          case l of
            YesResolved _  -> Just x
            FailResolved _ -> Just x
            NoResolved d ->
              do n <- mapStep d
                 return (c, NoResolved n)
        )
      return $ DerivResolved der


  repeatStep :: Type -> Deriv -> Int -> Grammar
  repeatStep ty der depth =
    if depth > gLLkDEPTH
    then grammar
    else
      let mder1 = mapStep der in
      case mder1 of
        Nothing -> grammar
        Just der1 ->
          if checkUnambiguousOrDone der1
          then translateToCaseDeriv ty der1
          else repeatStep ty der1 (depth + 1)


  getListOr :: Grammar -> [Grammar]
  getListOr g =
    case g of
      OrUnbiased g1 g2 -> getListOr g1 ++ getListOr g2
      OrBiased   g1 g2 -> getListOr g1 ++ getListOr g2
      Call name []  ->
        case getGrammarDef modl name of
          Nothing -> [ g ]
          Just gram1 -> getListOr gram1
      _                -> [g]

  deriveOneByteOnList :: [ZipGrammar] -> Maybe [(CharSet, ZipGrammar)]
  deriveOneByteOnList lst =
    let t = mapM deriveOneByte lst in
    fmap concat t

  deriveOneByte :: ZipGrammar -> Maybe [(CharSet, ZipGrammar)]
  deriveOneByte gram =
    deriveGo gram
    where
    deriveGo :: ZipGrammar -> Maybe [(CharSet, ZipGrammar)]
    deriveGo gr@(ZipNode {focus = g}) =
      case g of
        Do_ {}   -> deriveGo (goLeft gr)
        Do {}    -> deriveGo (goLeft gr)
        Match {} -> fmap (\ x -> [x]) $ deriveMatch gr
        Annot {} -> deriveGo (goLeft gr)
        Call name []  ->
          case getGrammarDef modl name of
            Nothing -> Nothing
            Just gram1 -> deriveGo (gr {focus = gram1 })
        OrBiased _g1 _g2 -> Nothing
          -- do d1 <- deriveGo (gr {focus = g1}) -- WARNING Should be Zor
          --   d2 <- deriveGo (gr {focus = g2})
          --   return (d1 ++ d2)
        Let _ _ _g   -> deriveGo (goLeft gr)
          -- trace "LET" $ deriveGo (goLeft gr) -- WARNING INCORRECT
          -- trace "LET" $
          -- Nothing
        _        -> Nothing
    deriveGo (ZipLeaf {zmatch = zm, path = pth}) =
      case zm of
        ZMatchByte _          -> error "TODO should move up"
        ZMatchBytes (prev, next) orig ->
          case uncons next of
            Nothing -> error "TODO should move up"
            Just (w, rest) ->
              Just [(CWord8 w, ZipLeaf { zmatch = ZMatchBytes (w : prev, rest) orig, path = pth}) ]
        ZMatchEnd -> error "TODO END"

  convArrayToByteString :: [Expr] -> ByteString
  convArrayToByteString lst =
    Data.ByteString.pack (map f lst)
    where
    f (Ap0 (IntL n (TUInt (TSize 8)))) =
      if 0 <=n && n <= 255
      then fromInteger n
      else error "error converting Integer to Word8"
    f _ = error "error converting Expr to Word8"


  deriveMatch :: ZipGrammar -> Maybe (CharSet, ZipGrammar)
  deriveMatch (ZipNode { focus = Match sem match, path = pth}) =
    let newPath = mkPathGrammar (ZMatch sem) pth in
    case match of
      MatchByte b ->
        let x = ZipLeaf{ zmatch = ZMatchByte b, path = newPath} in
        Just (CByteSet b, x)
      MatchBytes b ->
        case b of
          Ap0 (ByteArrayL bs) ->
            case uncons bs of
              Nothing -> error "TODO should move up"
              Just (w, rest) ->
                Just (CWord8 w, ZipLeaf { zmatch = ZMatchBytes ([w], rest) bs, path = newPath})
          Ap1 ArrayLen _ -> trace ("AP1 ArrayLen")  $ Nothing
          Ap1 _ _ -> trace ("AP1")  $ Nothing
          Ap0 _ -> trace ("AP0")  $ Nothing
          ApN (ArrayL (TUInt (TSize 8))) arr  ->
            let bs = convArrayToByteString arr in
            case uncons bs of
              Nothing -> error "TODO should move up"
              Just (w, rest) ->
                Just (CWord8 w, ZipLeaf { zmatch = ZMatchBytes ([w], rest) bs, path = newPath})
          _ -> Nothing
      _ -> Nothing -- TODO: replace this with an error, or look into it
  deriveMatch _ = error "function should be applied to Match"

  factorize :: [(Set Integer, ZipGrammar)] -> [(Set Integer, [ZipGrammar])]
  factorize lst =
    factorizeGo lst []
    where
      factorizeGo :: [(Set Integer, ZipGrammar)] -> [(Set Integer, [ZipGrammar])] -> [(Set Integer, [ZipGrammar])]
      factorizeGo [] acc = acc
      factorizeGo ((s, g) : rs) acc =
        let newAcc = insertInResult (s,g) acc in
        factorizeGo rs newAcc

      insertInResult :: (Set Integer, ZipGrammar) -> [(Set Integer, [ZipGrammar])] -> [(Set Integer, [ZipGrammar])]
      insertInResult   (s, g) [] = [(s,[g])]
      insertInResult x@(s, g) ((y@(s1, gs)) : rest) =
        let inter = Set.intersection s s1 in
        if Set.null inter
        then y : insertInResult x rest
        else
          let diff1 = Set.difference s s1 in
          let diff2 = Set.difference s1 s in
          case (Set.null diff1, Set.null diff2) of
            (False, False) ->
              -- NNNNNNNNN
              --     OOOOOOOOOO
              let notNew = (diff2, gs) in
              let new = (inter, gs ++ [g]) in
              notNew : new : insertInResult (diff1, g) rest
            (False, True) ->
              -- NNNNNNNNN
              --    OOOOO
              let new = (inter, gs ++ [g]) in
              new : insertInResult (diff1, g) rest
            (True, False) ->
              --  NNNNN
              --  OOOOOOOOO
              let notNew = (diff2, gs) in
              let new = (inter, gs ++ [g]) in
              notNew : new : rest
            (True, True) ->
              --  NNNNN
              --  OOOOO
              let new = (inter, gs ++ [g]) in
              new : rest

  checkUnambiguousList :: [ZipGrammar] -> Bool
  checkUnambiguousList opts =
    case length opts of
      0 -> error "impossible"
      1 -> True
      _ -> False

  translateToCaseDeriv :: Type -> Deriv -> Grammar
  translateToCaseDeriv ty der =
    let guid1 = firstValidGUID in
    Annot (SrcAnnot $ Data.Text.pack "DETERMINIZE HERE") $
      translateToCase ty guid1 der

  translateToCase :: Type -> GUID -> Deriv -> Grammar
  translateToCase _ _ (DerivStart {})         = error "impossible"
  translateToCase _ _ (DerivUnResolved {})    = error "impossible"
  translateToCase ty guid (DerivResolved lst) =
    translateList ty guid lst

  translateList :: Type -> GUID -> [(Integer, Resolution)] -> Grammar
  translateList ty guid lst =
    let name = Name { nameId = guid,
                      nameText = Nothing,
                      nameType = TUInt (TSize 8)
                    } in
    Do name (Match SemYes (MatchByte SetAny))
      (GCase (Case name
        ((map
            (\ (c, g1) ->
                case g1 of
                  YesResolved zg -> buildCase (c, zg)
                  FailResolved zg ->
                    let newOr = buildOr c zg
                    in (PNum c, newOr)
                  NoResolved der ->
                    let newG1 = translateToCase ty (succGUID guid) der
                    in (PNum c, newG1))
            lst
          ) ++ [ (PAny, Fail ErrorFromSystem ty Nothing) ]
        )))

    where
    buildCase :: (Integer, ZipGrammar) -> (Pattern, Grammar)
    buildCase (c, g1) = (PNum c, buildLeaf c g1)

    buildOr :: Integer -> [ ZipGrammar ] -> Grammar
    buildOr c zgs =
      go zgs
      where
      go [] = error ""
      go [x] = buildLeaf c x
      go (x : xs) = OrBiased (buildLeaf c x) (go xs)


    -- TODO: is this not redundant with goNextLeaf ????
    buildLeaf :: Integer -> ZipGrammar -> Grammar
    buildLeaf c (ZipLeaf{ zmatch = ZMatchByte _, path = ZMatch sem : pth}) =
      case sem of
        SemYes ->
          let newBuilt = Pure (Ap0 (IntL c (TUInt (TSize 8)))) in
          buildUp (mkZipGrammar newBuilt pth)
        SemNo ->
          let newBuilt = Pure (Ap0 Unit) in
           buildUp (mkZipGrammar newBuilt pth)
    buildLeaf c (ZipLeaf{ zmatch = ZMatchBytes (zpast, rest) orig, path = ZMatch sem : pth}) =
      if not (c == (toInteger $ head zpast))
      then error "broken invariant"
      else
        let pureMatch = Pure (Ap0 (ByteArrayL orig)) in
        if Data.ByteString.null rest
        then
          case sem of
            SemYes ->
              let newBuilt = pureMatch in
              buildUp (mkZipGrammar newBuilt pth)
            SemNo ->
              let newBuilt = Pure (Ap0 Unit) in
              buildUp (mkZipGrammar newBuilt pth)
        else
          let matchRest = Match SemNo (MatchBytes (Ap0 (ByteArrayL rest))) in
          case sem of
            SemYes ->
              let newBuilt = Do_ matchRest pureMatch in
              buildUp (mkZipGrammar newBuilt pth)
            SemNo ->
              let newBuilt = matchRest in
              buildUp (mkZipGrammar newBuilt pth)
    buildLeaf _ _ = error "Should not happen"

