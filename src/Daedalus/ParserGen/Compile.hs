{-# Language GADTs, DataKinds, ExistentialQuantification, RecordWildCards #-}

module Daedalus.ParserGen.Compile ( module Daedalus.ParserGen.Compile ) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (isInfixOf)

import Daedalus.AST (Name(..))
import Daedalus.Normalise.AST
import Daedalus.Type.AST (WithSem(..))
import Daedalus.AST (ManyBounds(..), Commit(..))

--import Debug.Trace

import qualified Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action
import Daedalus.ParserGen.Aut

------- 1 Allocate States

allocate :: a -> Int -> Int -> ((a, [Int]), Int)
allocate p start num =
  let newlast = start + num
  in ((p, [(start+1) .. newlast]), newlast)

nameToName :: NName -> PAST.NName
nameToName n =
  PAST.NName {PAST.nName = nName n} --, PAST.nType = nType v}


idCExpr :: NCExpr -> PAST.NCExpr
idCExpr cexpr =
  case cexpr of
    NSetAny -> PAST.NSetAny
    NSetSingle e1 -> PAST.NSetSingle (idVExpr e1)
    NSetComplement e1 -> PAST.NSetComplement (idCExpr e1)
    NSetUnion  le -> PAST.NSetUnion (map idCExpr le)
    NSetOneOf bs -> PAST.NSetOneOf bs
    NSetRange e1 e2 -> PAST.NSetRange (idVExpr e1) (idVExpr e2)
    NCCall n lst -> PAST.NCCall (nameToName n) (map idVExpr lst)
    _ -> error ("TODO: " ++ show cexpr)

idVExpr :: NVExpr -> PAST.NVExpr
idVExpr vexpr =
  case vexpr of
    NCoerce ty1 ty2 e1 -> PAST.NCoerce ty1 ty2 (idVExpr e1)
    NNumber i t -> PAST.NNumber i t
    NBool b -> PAST.NBool b
    NNothing t -> PAST.NNothing t
    NJust e -> PAST.NJust (idVExpr e)
    NByte w -> PAST.NByte w
    NStruct s t -> PAST.NStruct (map (\ (s',e) -> (s', idVExpr e)) s) t
    NUnit -> PAST.NUnit
    NByteArray ba -> PAST.NByteArray ba
    NArray     l t -> PAST.NArray (map idVExpr l) t
    NMapEmpty t -> PAST.NMapEmpty t
    NIn lbl e lst -> PAST.NIn lbl (idVExpr e) lst
    NBinOp op e1 e2 _ -> PAST.NBinOp op (idVExpr e1) (idVExpr e2)
    NUniOp op e1 -> PAST.NUniOp op (idVExpr e1)
    NSelStruct e n t -> PAST.NSelStruct (idVExpr e) n t
    NIf e1 e2 e3 -> PAST.NIf (idVExpr e1) (idVExpr e2) (idVExpr e3)
    NVCall n lst -> PAST.NVCall (nameToName n) (map idVExpr lst)
    NVFor n e1 Nothing n1 e2 e3 -> PAST.NVFor (nameToName n) (idVExpr e1) (nameToName n1) (idVExpr e2) (idVExpr e3)
    NVar v ->
      let av = nameToName v
      in PAST.NVar av
    _ -> error ("TODO: " ++ show vexpr)

convertManyBounds :: ManyBounds NVExpr -> ManyBounds PAST.NVExpr
convertManyBounds b = fmap idVExpr b

allocGExpr :: Int -> NGExpr -> (PAST.NGExpr, Int)
allocGExpr n gexpr =
  case gexpr of
    NGPure e ->
      let ae = idVExpr e
      in allocate (PAST.NGPure ae) n 2
    NLabel l g ->
      let (ag, n1) = allocGram n g
      in allocate (PAST.NLabel l ag) n1 0
    NGuard e ->
      let ae = idVExpr e
      in allocate (PAST.NGuard ae) n 2
    NCurrnetStream ->
      allocate (PAST.NCurrnetStream) n 2
    NSetStream e1 ->
      let ae1 = idVExpr e1
      in allocate (PAST.NSetStream ae1) n 2
    NStreamLen ws e1 e2 ->
      let ae1 = idVExpr e1
          ae2 = idVExpr e2
      in allocate (PAST.NStreamLen ws ae1 ae2) n 2
    NStreamOff ws e1 e2 ->
      let ae1 = idVExpr e1
          ae2 = idVExpr e2
      in allocate (PAST.NStreamOff ws ae1 ae2) n 2
    NGetByte ws ->
      allocate (PAST.NGetByte ws) n 2
    NMatch ws cexpr ->
      let acexpr = idCExpr cexpr
      in allocate (PAST.NMatch ws acexpr) n 2
    NMatchBytes ws vexpr ->
      let ae = idVExpr vexpr
      in allocate (PAST.NMatchBytes ws ae) n 2
    NChoice c lst t ->
      let step g (ags, n') = let (ag, n1) = allocGram n' g
                             in (ag:ags, n1)
          (alst, ns) = foldr step ([], n) lst
      in allocate (PAST.NChoice c alst t) ns 2
    NOptional c e1 ->
      let (ae1, n1) = allocGram n e1
      in allocate (PAST.NOptional c ae1) n1 2
    NMany ws c bounds e1 ->
      let (ae1, n1) = allocGram n e1
      in allocate (PAST.NMany ws c (convertManyBounds bounds) ae1) n1 7
    NEnd -> allocate (PAST.NEnd) n 2
    NOffset -> allocate (PAST.NOffset) n 2
    NMapLookup ws e1 e2 ->
      let ae1 = idVExpr e1
          ae2 = idVExpr e2
      in allocate (PAST.NMapLookup ws ae1 ae2) n 2
    NMapInsert ws e1 e2 e3 ->
      let ae1 = idVExpr e1
          ae2 = idVExpr e2
          ae3 = idVExpr e3
      in allocate (PAST.NMapInsert ws ae1 ae2 ae3) n 2
    NCoerceCheck ws t1 t2 e1 ->
      let ae1 = idVExpr e1
      in allocate (PAST.NCoerceCheck ws t1 t2 ae1) n 2
    NSelUnion ws e1 lbl ty ->
      let ae1 = idVExpr e1
      in allocate (PAST.NSelUnion ws ae1 lbl ty) n 2
    NSelJust ws e1 ty ->
      let ae1 = idVExpr e1
      in allocate (PAST.NSelJust ws ae1 ty) n 2
    NGFor nname1 e1 Nothing nname2 e2 gram ->
      let (agram, n1) = allocGram n gram
          ae1 = idVExpr e1
          ae2 = idVExpr e2
      in allocate (PAST.NGFor (nameToName nname1) ae1 (nameToName nname2) ae2 agram) n1 3
    NGMap Nothing nname1 e1 gram ->
      let (agram, n1) = allocGram n gram
          ae1 = idVExpr e1
      in allocate (PAST.NGMap (nameToName nname1) ae1 agram) n1 3
    NGCall nname le ->
      let ale = map idVExpr le
      in allocate (PAST.NGCall (nameToName nname) ale) n 2
    NGErrorMode c e1 ->
      let (ae1, n1) = allocGram n e1
      in allocate (PAST.NGErrorMode c ae1) n1 2
    x -> error ("TODO:"++ show x)

allocGram :: Int -> NGrammar -> (PAST.NGrammar, Int)
allocGram n gram =
  let allocBindList n1 gr =
        case gr of
          NBind mname gexpr gram1 ->
            let (agexpr, n2) = allocGExpr n1 gexpr
                (agram1, n3) = allocBindList n2 gram1
                aname = case mname of
                          Nothing -> Nothing
                          Just name -> Just (nameToName name)
            in allocate (PAST.NBind aname agexpr agram1) n3 2
          NPure vexpr ->
            let avexpr = idVExpr vexpr
            in allocate (PAST.NPure avexpr) n1 2
  in
    -- Two states are reserved for the top-level of binds and they are
    -- placed at the end of the list
    let ((ag, lst), n1) = allocBindList (n+2) gram in
    ((ag, lst++[n+1, n+2]), n1)

allocDeclBody :: Int -> NDeclBody -> (PAST.NDeclBody, Int)
allocDeclBody n d =
  case d of
    NCDecl e ->
      let ae = idCExpr e
      in allocate (PAST.NCDecl ae) n 2
    NVDecl e ->
      let ae = idVExpr e
      in allocate (PAST.NVDecl ae) n 2
    NGDecl e ->
      let (ae, n1) = allocGram n e
      in allocate (PAST.NGDecl ae) n1 2
    NExtern -> allocate (PAST.NExtern) n 2

globalStartState :: State
globalStartState = 1

globalFinalState :: State
globalFinalState = 0


allocStates :: [NDecl] -> PAST.GblAlloc
allocStates decls =
  fst $ foldr f (Map.empty, globalStartState) decls
  where
    f d (lst, n) =
      let (abody, n1) = allocDeclBody n (nDeclDef d)
          iname = nDeclName d
          dd = PAST.NDecl { PAST.nDeclName=iname,
                            PAST.nDeclParams=map nameToName (nDeclParams d),
                            PAST.nDeclType=nDeclType d,
                            PAST.nDeclDef=abody }
          moreStates = 2
          ad :: PAST.NDecl
          (ad, lastn) = allocate dd n1 moreStates
      in  (Map.insert iname ad lst, lastn)


systemToFunctions :: Map.Map Name PAST.NDecl -> PAST.GblFuns
systemToFunctions m =
  Map.map buildFunSpec (Map.filter onlyValue m)
  where onlyValue (a, _) =
          case fst (PAST.nDeclDef a) of
            PAST.NCDecl _ -> True
            PAST.NVDecl _ -> True
            PAST.NGDecl _ -> False
            PAST.NExtern  -> True -- error ("TODO: NExtern not handled yet")
        buildFunSpec (a, _) =
          case fst (PAST.nDeclDef a) of
            PAST.NCDecl v -> (PAST.nDeclParams a, PAST.ClassExpression v)
            PAST.NVDecl v -> (PAST.nDeclParams a, PAST.ValueExpression v)
            _ -> error "can only be a Value Declaration or Class Declaration"


type GblGrammar = Map.Map Name PAST.NDecl

systemToGrammars :: Map.Map Name PAST.NDecl -> GblGrammar
systemToGrammars m =
  Map.filter f m
  where f (a,_) = case fst (PAST.nDeclDef a) of
                PAST.NCDecl _ -> False
                PAST.NVDecl _ -> False
                PAST.NGDecl _ -> True
                PAST.NExtern  -> True -- error ("TODO: NExtern not handled yet")


--- 2. Build the Automaton

genGExpr :: GblGrammar -> PAST.NGExpr -> Aut
genGExpr gbl (e, st) =
  case e of
    PAST.NGPure e1 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [(n1, UniChoice (SAct (EvalPure e1), n2))]) n2
    PAST.NLabel _ g -> -- TODO: ignore the Label case
      genGram gbl g
    PAST.NGuard e1 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (SAct (Guard e1), n2)) ]) n2
    PAST.NCurrnetStream ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct YesSem (CurrentStream), n2)) ]) n2
    PAST.NSetStream e1 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct YesSem (SetStream e1), n2)) ]) n2
    PAST.NStreamLen ws e1 e2 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct ws (StreamLen e1 e2), n2)) ]) n2
    PAST.NStreamOff ws e1 e2 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct ws (StreamOff e1 e2), n2)) ]) n2
    PAST.NGetByte s ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct s (IGetByte), n2)) ]) n2
    PAST.NMatch s e1 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct s (ClssAct e1), n2)) ]) n2
    PAST.NMatchBytes s e1 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [(n1, UniChoice (IAct s (IMatchBytes e1), n2))]) n2
    PAST.NChoice c lst _ty ->
      let lstITF = map (\ expr -> dsAut $ genGram gbl expr) lst
          n1 = st !! 0
          n2 = st !! 1
      in
        case c of
          Commit ->
            let transIn  = foldr (\ (i1,_t1,_f1) accTr -> (EpsA, i1) : accTr) [] lstITF
                transOut = foldr (\ (_i1,_t1,f1) accTr -> (f1, UniChoice (BAct (CutBiasAlt n2), n2)) : accTr) [] lstITF
                transBdy = foldr (\ (_i1,t1,_f1) accTr -> unionTr t1 accTr) emptyTr lstITF
            in mkAut n1 (unionTr (mkTr1 (n1, SeqChoice transIn n2)) (unionTr (mkTr transOut) transBdy)) n2
          Backtrack ->
            let transIn  = foldr (\ (i1,_t1,_f1) accTr -> (EpsA, i1) : accTr) [] lstITF
                transOut = foldr (\ (_i1,_t1,f1) accTr -> (f1, UniChoice (EpsA, n2)) : accTr) [] lstITF
                transBdy = foldr (\ (_i1,t1,_f1) accTr -> unionTr t1 accTr) emptyTr lstITF
            in mkAut n1 (unionTr (mkTr1 (n1, ParChoice transIn)) (unionTr (mkTr transOut) transBdy)) n2
    PAST.NOptional c e1 ->
      let (i1, t1, f1) = dsAut $ genGram gbl e1
          n1 = st !! 0
          n2 = st !! 1
          trans =
            case c of
              Commit ->
                [ (n1, SeqChoice [(EpsA, i1), (BAct (CutBiasAlt n2), n2)] n2),
                  (f1, UniChoice (BAct (CutBiasAlt n2), n2))
                ]
              Backtrack ->
                [ (n1, ParChoice [(EpsA, i1), (EpsA, n2)]),
                  (f1, UniChoice (EpsA, n2))
                ]
      in mkAut n1 (unionTr (mkTr trans) t1) n2
    PAST.NMany s c bounds e1 ->
      let (i1, t1, f1) = dsAut $ genGram gbl e1
          n1 = st !! 0
          n2 = st !! 1
          n3 = st !! 2
          n4 = st !! 3
          n5 = st !! 4
          n6 = st !! 5
          n7 = st !! 6
          loopTrans =
            case c of
              Backtrack -> -- Not sure this case is possible or tested at the moment
                [ (n1, UniChoice (CAct (BoundSetup bounds), n2)),
                  (n2, UniChoice (SAct (ManyFreshList s), n3)),
                  (n3, UniChoice (CAct (BoundIsMore), i1)),
                  (n3, UniChoice (CAct (BoundCheckSuccess), n7)),
                  (f1, UniChoice (SAct (ManyAppend s), n4)),
                  (n4, UniChoice (CAct (BoundIncr), n3))
                ]
              Commit ->
                [ (n1, UniChoice (CAct (BoundSetup bounds), n2)),
                  (n2, UniChoice (SAct (ManyFreshList s), n3)),
                  (n3, SeqChoice [(CAct (BoundIsMore), i1),
                                  (CAct (BoundCheckSuccess), n6)] n7),
                  (f1, UniChoice (SAct (ManyAppend s), n4)),
                  (n4, UniChoice (CAct (BoundIncr), n5)),
                  (n5, UniChoice (BAct (CutBiasAlt n7), n3)),
                  (n6, UniChoice (BAct (CutBiasAlt n7), n7))
                ]
      in mkAut n1 (unionTr (mkTr loopTrans) t1) n7
    PAST.NEnd ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct NoSem IEnd, n2)) ]) n2
    PAST.NOffset ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct YesSem IOffset, n2)) ]) n2
    PAST.NMapLookup ws e1 e2 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (SAct (MapLookup ws e1 e2), n2)) ]) n2
    PAST.NMapInsert ws e1 e2 e3 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (SAct (MapInsert ws e1 e2 e3), n2)) ]) n2
    PAST.NCoerceCheck ws t1 t2 e1 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [ (n1, UniChoice (SAct (CoerceCheck ws t1 t2 e1), n2)) ]) n2
    PAST.NSelUnion ws e1 lbl _ty ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [(n1, UniChoice (SAct (SelUnion ws e1 lbl), n2))]) n2
    PAST.NSelJust ws e1 _ty ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [(n1, UniChoice (SAct (SelJust ws e1), n2))]) n2
    PAST.NGFor nname1 e1 nname2 e2 gram ->
      let n1 = st !! 0
          n2 = st !! 1
          n3 = st !! 2
          (i1, t1, f1) = dsAut $ genGram gbl gram
          trans = mkTr
            [ (n1, UniChoice (CAct (ForInit nname1 e1 nname2 e2), n2))
            , (n2, UniChoice (EpsA, i1))
            , (f1, UniChoice (CAct (ForNext), n2))
            , (n2, UniChoice (CAct (ForEnd), n3))
            ]
      in mkAut n1 (unionTr trans t1) n3
    PAST.NGMap nname1 e1 gram ->
      let n1 = st !! 0
          n2 = st !! 1
          n3 = st !! 2
          (i1, t1, f1) = dsAut $ genGram gbl gram
          trans = mkTr
            [ (n1, UniChoice (CAct (MapInit nname1 e1), n2))
            , (n2, UniChoice (EpsA, i1))
            , (f1, UniChoice (CAct (MapNext), n2))
            , (n2, UniChoice (CAct (MapEnd), n3))
            ]
      in mkAut n1 (unionTr trans t1) n3
    PAST.NGCall nname le ->
      let name = PAST.nName nname
          (_, annot) =
            let x = Map.lookup name gbl in
              case x of
                Nothing -> error ("Name not found in gbl grammar: " ++ show name)
                Just r -> r
          n1 = st !! 0
          n2 = st !! 1
          call = annot !! 0
          ret  = annot !! 1
          trans = mkTr
            [ (n1, UniChoice (CAct (Push nname le n2), call))
            , (ret, UniChoice (CAct (Pop n2), n2))
            ]
      in mkAut n1 trans n2
    PAST.NGErrorMode c e1 ->
      let n1 = st !! 0
          n2 = st !! 1
          (i1, t1, f1) = dsAut $ genGram gbl e1
          trans =
            case c of
              Commit ->
                [ (n1, UniChoice (BAct (CutLocal), i1)),
                  (f1, UniChoice (EpsA, n2))
                ]
              Backtrack -> error "not handled in ErrorMode"
      in mkAut n1 (unionTr (mkTr trans) t1) n2


genGram :: GblGrammar -> PAST.NGrammar -> Aut
genGram gbl (e, st) =
  let
    genForBindList :: PAST.NGrammar -> Aut
    genForBindList (expr, sts) =
        case expr of
          PAST.NBind mname e1 e2 ->
            let (i1,t1,f1) = dsAut $ genGExpr gbl e1
                (i2,t2,f2) = dsAut $ genForBindList e2
                n1 = sts !! 0
                n2 = sts !! 1
                trans = mkTr
                  [ (n1, UniChoice (EpsA, i1))
                  , (f1, UniChoice (SAct (EnvStore mname), i2))
                  , (f2, UniChoice (EpsA, n2))
                  ]
            in mkAut n1 (unionTr trans (unionTr t1 t2)) n2
          PAST.NPure e1 ->
            let n1 = sts !! 0
                n2 = sts !! 1
            in mkAut n1 (mkTr [(n1, UniChoice (SAct (ReturnBind e1), n2))]) n2
  in
    let (i1,t1,f1) = dsAut $ genForBindList (e, st)
        n1 = st !! 2
        n2 = st !! 3
        trans = mkTr [ (n1, UniChoice (SAct EnvFresh, i1)), (f1, UniChoice (EpsA, n2))]
    in mkAut n1 (unionTr trans t1) n2


genDeclBody :: GblGrammar -> PAST.NDeclBody -> Aut
genDeclBody gbl (e, st) =
  case e of
    PAST.NCDecl _e1 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 emptyTr n2
    PAST.NVDecl _e1 ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 emptyTr n2
    PAST.NGDecl e1 ->
      let (i1,t1,f1) = dsAut $ genGram gbl e1
          n1 = st !! 0
          n2 = st !! 1
          trans = mkTr [ (n1, UniChoice (EpsA, i1)), (f1, UniChoice (EpsA, n2)) ]
      in mkAut n1 (unionTr trans t1) n2
    PAST.NExtern ->
      let n1 = st !! 0
          n2 = st !! 1
      in mkAut n1 (mkTr [(n1, UniChoice (EpsA, n2))]) n2

genDecl :: GblGrammar -> PAST.NDecl -> Aut
genDecl gbl (e, st) =
  let (i1,t1,f1) = dsAut $ genDeclBody gbl (PAST.nDeclDef e)
      n1 = st !! 0
      n2 = st !! 1
      trans = mkTr
        [ (n1, UniChoice (CAct (ActivateFrame (PAST.nDeclParams e)), i1))
        , (f1, UniChoice (CAct (DeactivateReady), n2))
        ]
  in mkAut n1 (unionTr trans t1) n2

buildAut :: [NDecl] -> (PAST.GblFuns, Aut)
buildAut decls =
  (systemToFunctions allocDecls,
   mkAut globalStartState (unionTr trans table) globalFinalState
  )
  where
    allocDecls = allocStates decls
    allocGrammar = systemToGrammars allocDecls
    (_, mainInfo) = fromJust $ Map.foldrWithKey
      (\ k a b ->
         case b of
           Just res -> Just res
           Nothing ->
             let ident = name2Text k in
               -- TODO: this infixOf test should be removed
               if isInfixOf "Main" (show ident) then Just (k, a) else Nothing
      ) Nothing allocDecls
    mainAnnots = snd mainInfo
    startState = mainAnnots !! 0
    finalState = mainAnnots !! 1
    mainName = PAST.NName {PAST.nName = PAST.nDeclName $ fst mainInfo}
    f a tr =
      let (_i, t, _f) = dsAut $ genDecl allocGrammar a in unionTr t tr
    table = foldr f emptyTr allocDecls
    trans = mkTr
      [ (globalStartState, UniChoice (CAct (Push mainName [] globalFinalState), startState))
      , (finalState,       UniChoice (CAct (Pop globalFinalState), globalFinalState))
      ]
