{-# Language TupleSections, GeneralizedNewtypeDeriving #-}
{-# Language BlockArguments #-}
module Daedalus.Core.Determinize (determinizeModule) where

-- import Debug.Trace(trace)

import Data.Text (pack)
import Data.Set (Set)
import qualified Data.Set as Set
import MonadLib
import Data.Word(Word8)
import Data.ByteString(ByteString, uncons, pack, null)

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
  | ZMatchBytes [Word8] ByteString
  | ZMatchEnd

data ZGrammar =
    ZMatch Sem
  | ZDo_ Grammar
  | ZDo_2 Grammar
  | ZDo Name Grammar
  | ZDo2 Name Grammar
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
    Do_ g1 g2     -> ZipNode {focus = g1, path = mkPathGrammar (ZDo_ g2) pth }
    Do name g1 g2 -> ZipNode {focus = g1, path = mkPathGrammar (ZDo name g2) pth }
    Annot ann g1  -> ZipNode {focus = g1, path = mkPathGrammar (ZAnnot ann) pth}
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
goNextLeaf _c (ZipLeaf{ zmatch = ZMatchBytes zpast rest, path = fpth@(ZMatch sem : pth)}) =
  if not (Data.ByteString.null rest)
  then Just $ ZipLeaf {zmatch = ZMatchBytes zpast rest, path = fpth }
  else
    case sem of
      SemYes ->
        let str = (Data.ByteString.pack (reverse zpast)) :: ByteString in
        let pureMatch = Pure (Ap0 (ByteArrayL str))
        in
        goUpRight (mkZipGrammar pureMatch pth)
      SemNo ->
        let newBuilt = Pure (Ap0 Unit) in
        goUpRight (mkZipGrammar newBuilt pth)
goNextLeaf _ _ = error "Should not happen"


{- END of Zipped Grammar -}

data CharSet =
    NCByteSet ByteSet
  | NCWord8 Word8

data Deriv =
    DerivStart      [ZipGrammar]
  | DerivUnResolved [(Set Integer, [ZipGrammar])]
  | DerivResolved   [(Integer, Resolution)]

data Resolution =
    YesResolved   ZipGrammar
  | NoResolved    Deriv

detOr :: Grammar -> Grammar
detOr grammar =
  let ty = typeOf grammar in
  let orLst = getListOr grammar in
  iterateDerivFactorize ty (DerivStart $ map initZipGrammar orLst) 0

  where
  stepDerivFactorize :: [ZipGrammar] -> Maybe Deriv
  stepDerivFactorize orLst =
    let derLst = tryDeterminizeListOr orLst in
    let linDerLst = do
          linLst <- derLst
          forM linLst
            (\ (a, c) -> do { d <- charListFromByteSet a ; return (d, c) })
    in
    case linDerLst of
      Nothing  -> Nothing
      Just a ->
        let f = factorize a in
        Just $ DerivUnResolved (map (\ (c, l) -> (c, l)) f)

  mapDeriv :: Deriv -> Maybe Deriv
  mapDeriv (DerivStart orLst) =
    stepDerivFactorize orLst
  mapDeriv (DerivUnResolved opts) = do
    der <- forM opts
        (\ (c, orLst) ->
            do
              if checkUnambiguousList orLst
              then Just (map (\ x -> (x, YesResolved (head orLst))) (Set.toList c))
              else do
                pairs <-
                  forM (Set.toList c)
                  (\ x -> do
                      newNextList <- forM orLst (\ zg -> goNextLeaf x zg)
                      return (x, newNextList))
                pairAfterStep <- forM pairs
                    (\ (x, lst) -> do
                        dzg <- stepDerivFactorize lst
                        return (x, dzg)
                    )
                return $ map (\ (x, tr) -> (x, NoResolved tr)) pairAfterStep
        )
    return $ DerivResolved (concat der)
  mapDeriv (DerivResolved opts) =
    do
      der <- forM opts
        (\ x@(c, l) ->
          case l of
            YesResolved _ -> Just x
            NoResolved d ->
              do n <- mapDeriv d
                 return (c, NoResolved n)
        )
      return $ DerivResolved der


  iterateDerivFactorize :: Type -> Deriv -> Int ->  Grammar
  iterateDerivFactorize ty der depth =
    if depth > 10
    then grammar
    else
      let der1 = mapDeriv der in
      case der1 of
        Nothing -> grammar
        Just a ->
          if checkUnambiguousDeriv a
          then translateToCaseDeriv ty a
          else iterateDerivFactorize ty a (depth + 1)


  getListOr :: Grammar -> [Grammar]
  getListOr g =
    case g of
      OrUnbiased g1 g2 -> getListOr g1 ++ getListOr g2
      OrBiased   g1 g2 -> getListOr g1 ++ getListOr g2
      _                -> [g]

  tryDeterminizeListOr :: [ZipGrammar] -> Maybe [(CharSet, ZipGrammar)]
  tryDeterminizeListOr lst =
    mapM deriveOneByte lst

  deriveOneByte :: ZipGrammar -> Maybe (CharSet, ZipGrammar)
  deriveOneByte gram =
    deriveGo gram
    where
    deriveGo :: ZipGrammar -> Maybe (CharSet, ZipGrammar)
    deriveGo gr@(ZipNode {focus = g}) =
      case g of
        Let {}   -> Nothing
        Do_ {}   -> deriveGo (goLeft gr)
        Do {}    -> deriveGo (goLeft gr)
        Match {} -> deriveMatch gr
        Annot {} -> deriveGo (goLeft gr)
        _        -> Nothing
    deriveGo (ZipLeaf {zmatch = zm, path = pth}) =
      case zm of
        ZMatchByte _          -> error "TODO should move up"
        ZMatchBytes prev next ->
          case uncons next of
            Nothing -> error "TODO should move up"
            Just (w, rest) ->
              Just (NCWord8 w, ZipLeaf { zmatch = ZMatchBytes (w : prev) rest, path = pth})
        ZMatchEnd -> error "TODO END"


  deriveMatch :: ZipGrammar -> Maybe (CharSet, ZipGrammar)
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
                Just (NCWord8 w, ZipLeaf { zmatch = ZMatchBytes [w] rest, path = newPath})
          _ -> Nothing
      _ -> Nothing
  deriveMatch _ = error "function should be applied to Match"

  charListFromByteSet :: CharSet -> Maybe (Set Integer)
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

  checkUnambiguousDeriv :: Deriv -> Bool
  checkUnambiguousDeriv der =
    case der of
      DerivStart {} -> False
      DerivUnResolved {} -> False
      DerivResolved lst ->
        foldr (\ (_a, r) b -> b && case r of
                               YesResolved {} -> True
                               NoResolved d -> checkUnambiguousDeriv d
                              ) True lst

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
                  NoResolved der ->
                    let newG1 = translateToCase ty (succGUID guid) der
                    in (PNum c, newG1)) lst)
           ++ [ (PAny, Fail ErrorFromSystem ty Nothing) ]
          )))

    where
    buildCase :: (Integer, ZipGrammar) -> (Pattern, Grammar)
    buildCase (c, g1) = (PNum c, buildLeaf c g1)

    buildLeaf :: Integer -> ZipGrammar -> Grammar
    buildLeaf c (ZipLeaf{ zmatch = ZMatchByte _, path = ZMatch sem : pth}) =
      case sem of
        SemYes ->
          let newBuilt = Pure (Ap0 (IntL c (TUInt (TSize 8)))) in
          buildUp (mkZipGrammar newBuilt pth)
        SemNo ->
          let newBuilt = Pure (Ap0 Unit) in
           buildUp (mkZipGrammar newBuilt pth)
    buildLeaf c (ZipLeaf{ zmatch = ZMatchBytes zpast rest, path = ZMatch sem : pth}) =
      if not (c == (toInteger $ head zpast))
      then error "broken invariant"
      else
        if Data.ByteString.null rest
        then
          case sem of
            SemYes ->
              let newBuilt = Pure (Ap0 (ByteArrayL $ Data.ByteString.pack (reverse zpast))) in
              buildUp (mkZipGrammar newBuilt pth)
            SemNo ->
              let newBuilt = Pure (Ap0 Unit) in
              buildUp (mkZipGrammar newBuilt pth)
        else
          let matchRest = Match sem (MatchBytes (Ap0 (ByteArrayL rest))) in
          case sem of
            SemYes ->
              let guid1 = firstValidGUID in
              let guid2 = succGUID guid1 in
              let namePrev = Name {
                    nameId = guid1,
                    nameText = Nothing,
                    nameType = TArray (TUInt (TSize 8))
                  } in
              let nameNext = Name {
                    nameId = guid2,
                    nameText = Nothing,
                    nameType = TArray (TUInt (TSize 8))
                  } in
              let newBuilt =
                    Let namePrev (Ap0 (ByteArrayL $ Data.ByteString.pack (reverse zpast)))
                      (Do nameNext matchRest
                          (Pure
                            (Ap1
                              Concat
                              (ApN (ArrayL (TArray (TUInt (TSize 8))))
                                [Var namePrev, Var nameNext])))) in
              buildUp (mkZipGrammar newBuilt pth)
            SemNo ->
              let newBuilt = matchRest in
              buildUp (mkZipGrammar newBuilt pth)
    buildLeaf _ _ = error "Should not happen"


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
        (ZAnnot ann : z) ->
          buildUp (mkZipGrammar (Annot ann built) z)
        _  -> error "case not handled"
    buildUp (ZipLeaf {}) = error "broken invariant"
