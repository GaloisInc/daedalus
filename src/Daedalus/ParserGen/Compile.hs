{-# Language GADTs, DataKinds, ExistentialQuantification, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# language FlexibleContexts #-}

module Daedalus.ParserGen.Compile where

-- import Debug.Trace

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NonEmpty

import qualified Control.Monad.State as ST

import Daedalus.Type.AST
import Daedalus.Type.Traverse

import qualified Daedalus.ParserGen.AST as PAST
import Daedalus.ParserGen.Action
import Daedalus.ParserGen.Aut


-------- Global states

cSTART_STATE :: State
cSTART_STATE = 1

cFINAL_STATE :: State
cFINAL_STATE = 0

------- 1 Allocate States

allocate :: a -> Int -> Int -> ((a, [Int]), Int)
allocate p start num =
  let newlast = start + num
  in ((p, [(start+1) .. newlast]), newlast)


idCExpr :: Show a => TC a Class -> TC (a, PAST.Annot) Class
idCExpr cexpr =
  annotExpr ((texprAnnot cexpr), PAST.emptyAnnot) subexpr
  where
    subexpr =
      case texprValue cexpr of
        TCSetAny -> TCSetAny
        TCSetSingle e1 -> TCSetSingle (idVExpr e1)
        TCSetComplement e1 -> TCSetComplement (idCExpr e1)
        TCSetUnion  le -> TCSetUnion (map idCExpr le)
        TCSetOneOf bs -> TCSetOneOf bs
        TCSetRange e1 e2 -> TCSetRange (idVExpr e1) (idVExpr e2)
        TCCall n [] lst -> TCCall n [] (map subArg lst)
        TCCall _ _ _    -> error "Saw a function call with non-empty type args"
        _ -> error ("TODO: " ++ show cexpr)

    subArg arg =
      case arg of
        ValArg a -> ValArg $ idVExpr a
        _ -> error "value argument expected"

idVExpr :: Show a => TC a Value -> TC (a, PAST.Annot) Value
idVExpr vexpr =
  annotExpr ((texprAnnot vexpr), PAST.emptyAnnot) subexpr
  where
    subexpr =
      case texprValue vexpr of
        TCCoerce ty1 ty2 e1 -> TCCoerce ty1 ty2 (idVExpr e1)
        TCLiteral lit t -> TCLiteral lit t
        TCNothing t -> TCNothing t
        TCJust e -> TCJust (idVExpr e)
        TCStruct s t -> TCStruct (map (\ (s',e) -> (s', idVExpr e)) s) t
        TCUnit -> TCUnit
        TCArray lst t -> TCArray (map idVExpr lst) t
        TCArrayLength e -> TCArrayLength (idVExpr e)
        TCMapEmpty t -> TCMapEmpty t
        TCIn lbl e lst -> TCIn lbl (idVExpr e) lst
        TCBinOp op e1 e2 t -> TCBinOp op (idVExpr e1) (idVExpr e2) t
        TCUniOp op e1 -> TCUniOp op (idVExpr e1)
        TCSelStruct e n t -> TCSelStruct (idVExpr e) n t
        TCIf e1 e2 e3 -> TCIf (idVExpr e1) (idVExpr e2) (idVExpr e3)
        TCCall n [] lst -> TCCall n [] (map subArg lst)
        TCCall _ _ _   -> error "Saw a function call with non-empty type args"
        -- TCFor n e1 Nothing n1 e2 e3 -> TCVFor n (idVExpr e1) n1 (idVExpr e2) (idVExpr e3)
        TCFor lp ->
          case loopFlav lp of
            LoopMap -> TCFor $
              Loop{ loopFlav = LoopMap
                  , loopKName = loopKName lp
                  , loopElName = loopElName lp
                  , loopCol = idVExpr (loopCol lp)
                  , loopBody = idVExpr (loopBody lp)
                  , loopType = loopType lp
                  }
            Fold n1 e1 ->
              TCFor $
              Loop{ loopFlav = Fold n1 (idVExpr e1)
                  , loopKName = loopKName lp
                  , loopElName = loopElName lp
                  , loopCol = idVExpr (loopCol lp)
                  , loopBody = idVExpr (loopBody lp)
                  , loopType = loopType lp
                  }

        TCVar v -> TCVar v
        _ -> error ("TODO: " ++ show vexpr)

    subArg arg =
      case arg of
        ValArg a -> ValArg $ idVExpr a
        _ -> error "value argument expected"


convertManyBounds :: Show a => ManyBounds (TC a Value) -> ManyBounds (TC (a, PAST.Annot) Value)
convertManyBounds b = fmap idVExpr b


getByteArray :: Show a => TC a Value -> Maybe [Word8]
getByteArray e =
  case texprValue e of
    TCLiteral (LBytes w) _ -> Just (BS.unpack w)
    TCArray arr _ ->
      foldr ( \ a b ->
                case b of
                  Nothing -> Nothing
                  Just acc ->
                    case texprValue a of
                      TCLiteral (LNumber n) _ ->
                        if (0 <= n && n <= 255)
                        then Just $ ((toEnum . fromEnum) n) : acc
                        else Nothing
                      _ -> Nothing
            ) (Just []) arr
    _ -> Nothing


allocGExpr :: Show a => Int -> PAST.Contx -> TC a Grammar -> (TC (a, PAST.Annot) Grammar, Int)
allocGExpr n ctx gexpr =
  (annotExpr ((texprAnnot gexpr), PAST.mkAnnot states ctx) subexpr, subexpr_n)
  where
    ((subexpr, states), subexpr_n) =
      case texprValue gexpr of
        TCPure e ->
          let ae = idVExpr e
          in allocate (TCPure ae) n 2
        TCLabel l g ->
          let (ag, n1) = allocGram n ctx g
          in allocate (TCLabel l ag) n1 0
        TCGuard e ->
          let ae = idVExpr e
          in allocate (TCGuard ae) n 2
        TCCurrentStream ->
          allocate (TCCurrentStream) n 2
        TCSetStream e1 ->
          let ae1 = idVExpr e1
          in allocate (TCSetStream ae1) n 2
        TCStreamLen ws e1 e2 ->
          let ae1 = idVExpr e1
              ae2 = idVExpr e2
          in allocate (TCStreamLen ws ae1 ae2) n 2
        TCStreamOff ws e1 e2 ->
          let ae1 = idVExpr e1
              ae2 = idVExpr e2
          in allocate (TCStreamOff ws ae1 ae2) n 2
        TCGetByte ws ->
          allocate (TCGetByte ws) n 2
        TCMatch ws cexpr ->
          let acexpr = idCExpr cexpr
          in
            let r = allocate (TCMatch ws acexpr) n 2
            in r

        TCMatchBytes ws vexpr ->
          let ae = idVExpr vexpr
          in
            case getByteArray ae of
              Nothing -> allocate (TCMatchBytes ws ae) n 2
              Just str -> allocate (TCMatchBytes ws ae) n (2 * (length str) + 2)

        TCChoice c lst t ->
          let step g (ags, n') = let (ag, n1) = allocGram n' ctx g
                                 in (ag:ags, n1)
              (alst, ns) = foldr step ([], n) lst
          in allocate (TCChoice c alst t) ns 2
        TCOptional c e1 ->
          let (ae1, n1) = allocGram n ctx e1
          in allocate (TCOptional c ae1) n1 2
        TCMany ws c bounds e1 ->
          let (ae1, n1) = allocGram n ctx e1
          in allocate (TCMany ws c (convertManyBounds bounds) ae1) n1 8
        TCEnd -> allocate (TCEnd) n 2
        TCOffset -> allocate (TCOffset) n 2
        TCMapLookup ws e1 e2 ->
          let ae1 = idVExpr e1
              ae2 = idVExpr e2
          in allocate (TCMapLookup ws ae1 ae2) n 2
        TCMapInsert ws e1 e2 e3 ->
          let ae1 = idVExpr e1
              ae2 = idVExpr e2
              ae3 = idVExpr e3
          in allocate (TCMapInsert ws ae1 ae2 ae3) n 2
        TCCoerceCheck ws t1 t2 e1 ->
          let ae1 = idVExpr e1
          in allocate (TCCoerceCheck ws t1 t2 ae1) n 2

        TCFor lp ->
          case loopFlav lp of
            Fold x e ->
              let gram = loopBody lp
                  e1 = e
                  e2 = loopCol lp
                  (agram, n1) = allocGram n ctx gram
                  ae1 = idVExpr e1
                  ae2 = idVExpr e2
                  forContent = Loop
                      { loopFlav = Fold x ae1
                      , loopKName = loopKName lp
                      , loopElName = loopElName lp
                      , loopCol = ae2
                      , loopBody = agram
                      , loopType = loopType lp
                      }
              in
                allocate (TCFor forContent) n1 5

            LoopMap ->
              let gram = loopBody lp
                  e1 = loopCol lp
                  (agram, n1) = allocGram n ctx gram
                  ae1 = idVExpr e1
                  forContent = Loop
                      { loopFlav = LoopMap
                      , loopKName = loopKName lp
                      , loopElName = loopElName lp
                      , loopCol = ae1
                      , loopBody = agram
                      , loopType = loopType lp
                      }
              in allocate (TCFor forContent) n1 5
        TCCall name [] le ->
          let ale = map subArg le
          in allocate (TCCall name [] ale) n 2
        TCCall _ _ _   -> error "Saw a function call with non-empty type args"
        TCErrorMode c e1 ->
          let (ae1, n1) = allocGram n ctx e1
          in allocate (TCErrorMode c ae1) n1 2
        TCFail e1 t ->
          let ae1 = maybe Nothing (\ e -> Just $ idVExpr e) e1
          in allocate (TCFail ae1 t) n 2
        TCCase e1 lpat mdpat ->
          let ae1 = idVExpr e1 in
          let stepPat (ags, n') g@(TCAlt { tcAltBody = body }) =
                let (abody, n1) = allocGram n' ctx body
                in (g{ tcAltBody = abody} : ags, n1)
          in

          let (alpat, ns1) = foldl stepPat ([], n) (NonEmpty.toList lpat) in
          let (amdpat, ns2) =
                case mdpat of
                  Nothing -> (Nothing, ns1)
                  Just dpat ->
                    let (adpat, ndpat) = allocGram ns1 ctx dpat
                    in (Just adpat, ndpat)

          in allocate (TCCase ae1 (NonEmpty.fromList (reverse alpat)) amdpat) ns2 4
          -- let n = length lpat + (maybe 0 (\ n -> 1) dpat)

        x -> error ("TODO: " ++ show x)


    subArg arg =
      case arg of
        ValArg a -> ValArg $ idVExpr a
        _ -> error "value argument expected"


allocGram :: forall a. Show a => Int -> PAST.Contx -> TC a Grammar -> (TC (a, PAST.Annot) Grammar, Int)
allocGram n ctx gram =
  let
    allocBindList :: Int -> TC a Grammar -> (TC (a, PAST.Annot) Grammar, Int)
    allocBindList n1 gr =
      case texprValue gr of
        TCDo _ _ _ -> allocDo n1 gr
        TCPure _ -> allocDo n1 gr
        _ -> allocGExpr n1 ctx gr

    allocDo :: Int -> TC a Grammar -> (TC (a, PAST.Annot) Grammar, Int)
    allocDo n1 gr =
      let
        ((subexpr, states), subexpr_n) =
          case texprValue gr of
            TCDo name gexpr gram1 ->
              let (agexpr, n2) = allocGram n1 ctx gexpr
                  (agram1, n3) = allocDo n2 gram1
              in allocate (TCDo name agexpr agram1) n3 2
            TCPure vexpr ->
              let avexpr = idVExpr vexpr
              in allocate (TCPure avexpr) n1 2
            _ ->
              let (ag,n2) = allocGExpr (n1+2) ctx gr
                  subannot = texprAnnot ag
              in ((texprValue ag, (PAST.annotStates $ snd subannot)++[n1+1, n1+2]), n2)
      in
      (annotExpr ((texprAnnot gr), PAST.mkAnnot states ctx) subexpr, subexpr_n)


  in
    -- Two states are reserved for the top-level of binds and they are
    -- placed at the end of the list
    let (ag, n1) = allocBindList (n+2) gram
        subannot = texprAnnot ag
    in
      (annotExpr (fst subannot, PAST.mkAnnot ((PAST.annotStates $ snd subannot)++[n+1, n+2]) ctx) (texprValue ag), n1)


allocDecl :: Show a => Int -> TCDecl a -> (TCDecl (a, PAST.Annot), Int)
allocDecl n _decl@(TCDecl {..}) =
  let
    mkBody :: forall a k. Show a => Context k -> TCDeclDef a k -> (TCDeclDef (a, PAST.Annot) k, PAST.Annot, Int)
    mkBody AClass (Defined tc) =
      let ae = idCExpr tc
          ((aae, lst), lastSt) = allocate ae n 2
      in (Defined aae, PAST.statesToAnnot lst, lastSt)
    mkBody AValue (Defined tc) =
      let ae = idVExpr tc
          ((aae, lst), lastSt) = allocate ae n 2
      in (Defined aae, PAST.statesToAnnot lst, lastSt)
    mkBody AGrammar (Defined tc) =
      let
        ctx = (PAST.nameToContx tcDeclName)
        (ae, n1) = allocGram n ctx tc
        ((aae, lst), lastSt) = allocate ae n1 2
      in (Defined aae, PAST.mkAnnot lst ctx, lastSt)
    mkBody _ (ExternDecl _) =
      error "TCExtern not handled"

    (ne, annot, lastState) = mkBody tcDeclCtxt tcDeclDef
    eAnnot =
      TCDecl
      { tcDeclName = tcDeclName
      , tcDeclTyParams = tcDeclTyParams
      , tcDeclCtrs = tcDeclCtrs
      , tcDeclParams = tcDeclParams
      , tcDeclDef = ne
      , tcDeclCtxt = tcDeclCtxt
      , tcDeclAnnot = (tcDeclAnnot, annot)
      }
  in (eAnnot, lastState)

allocTCModule :: Show a => Int -> TCModule a -> (TCModule (a, PAST.Annot), Int)
allocTCModule n _tc@(TCModule{..}) =
  let (n1, atc) = foldl fRec (n,[]) tcModuleDecls
      -- annotTCModule :: TCModule (a, PAST.Annot)
      annotTCModule =
        TCModule
        { tcModuleName = tcModuleName
        , tcModuleImports = tcModuleImports
        , tcModuleTypes = tcModuleTypes
        , tcModuleDecls = reverse atc
        }

  in (annotTCModule, n1)
  where
    fRec (currN, acc) decl =
      case decl of
        NonRec d ->
          let (adecl, n1) =  allocDecl currN d
          in (n1, NonRec adecl : acc)

        MutRec ds -> -- erasing the dependency analysis
          let (n1, ads) = foldl f (currN, []) ds
          in (n1, (MutRec $ reverse ads) : acc)
    f (currN, acc) d =
      let (adecl, n1) = allocDecl currN d
      in (n1, adecl : acc)



allocStates :: [TCModule SourceRange] -> PAST.GblAlloc
allocStates decls =
  let (_lastSt, lstMod) =
        foldl (
        \ (b,acc) a ->
          let (allocMod, st) = allocTCModule b a in (st, allocMod : acc))
        (cSTART_STATE, []) decls
  in
    foldl fRec Map.empty lstMod
  where
    fRec m (TCModule{..}) =
      let newM = foldl fDecl m tcModuleDecls
      in newM

      where
        -- the fields of `TCModule` are carried down and wrapped up
        -- over each `TCDecl` in the module in other word it
        -- duplicates the info of `TCModule` over each TCDecl
        fDecl localM decl =
          case decl of
            NonRec d ->
              let uniModule =
                    TCModule { tcModuleName = tcModuleName
                             , tcModuleImports = tcModuleImports
                             , tcModuleTypes = tcModuleTypes
                             , tcModuleDecls = [ NonRec d ]
                             }
              in  (Map.insert (tcDeclName d) uniModule localM)
            MutRec ds ->
              let mres = foldl f localM ds
              in mres

        f localM decl =
          let uniModule =
                TCModule { tcModuleName = tcModuleName
                         , tcModuleImports = tcModuleImports
                         , tcModuleTypes = tcModuleTypes
                         , tcModuleDecls = [ NonRec decl ]
                         }
          in
            Map.insert (tcDeclName decl) uniModule localM


getSourceRangeDecl :: TCModule (SourceRange, b) -> SourceRange
getSourceRangeDecl (TCModule {..}) =
  case tcModuleDecls of
    [ NonRec (TCDecl {..}) ] -> fst tcDeclAnnot
    _ -> error "there should be exactly one decl"



collectTC :: forall k. TC (SourceRange, PAST.Annot) k -> ST.State (Map.Map State (SourceRange, PAST.Contx)) (TC (SourceRange, PAST.Annot) k)
collectTC tce =
   let
     onAnnots :: (SourceRange, PAST.Annot) -> ST.State (Map.Map State (SourceRange, PAST.Contx)) (SourceRange, PAST.Annot)
     onAnnots anns =
       let
         a = snd anns
         srcRng = fst anns
       in
       do
         acc <- ST.get
         ST.put (foldr (\ q m -> Map.insert q ((srcRng, fromJust $ PAST.annotContx a)) m) acc (PAST.annotStates a))
         return anns
   in
       traverseTC onAnnots (collectTC) tce

collectDecls :: PAST.GblAlloc -> Map.Map State (SourceRange, PAST.Contx)
collectDecls gbl =
  let
    collectDecl :: TCModule (SourceRange, PAST.Annot) -> Map.Map State (SourceRange, PAST.Contx)
    collectDecl modl =
      case modl of
        (TCModule{ tcModuleDecls = [ NonRec (TCDecl {tcDeclDef = (Defined d), tcDeclAnnot = (srcRng,a)}) ] }) ->
          let sub = snd (ST.runState (collectTC d) Map.empty)
              top = foldr (\q m -> Map.insert q (srcRng, fromJust $ PAST.annotContx a) m) Map.empty (PAST.annotStates a)
          in Map.union top sub
        (TCModule{ tcModuleDecls = [ NonRec (TCDecl {tcDeclDef = (ExternDecl _)}) ] }) ->
          Map.empty
        _ -> error "broken invariant on the list of decls"
  in
    Map.foldr (\ modl m -> Map.union (collectDecl modl) m) Map.empty gbl


paramToName :: Param -> Name
paramToName param =
  case param of
    ValParam v -> tcName v
    ClassParam v -> tcName v
    GrammarParam v -> tcName v

argToValue :: Arg a -> TC a Value
argToValue param =
  case param of
    ValArg v -> v
    ClassArg _ -> error "Class is not a Value"
    GrammarArg _ -> error "Grammar is not a Value"


systemToFunctions :: PAST.GblAlloc -> PAST.GblFuns
systemToFunctions m =
  Map.map buildFunSpec
  (Map.filter onlyValue m)
  where
    onlyValue :: TCModule (SourceRange, PAST.Annot) -> Bool
    onlyValue t =
      case t of
        (TCModule {tcModuleDecls = [ NonRec (TCDecl {tcDeclCtxt = AClass, tcDeclDef = Defined _}) ]}) -> True
        (TCModule {tcModuleDecls = [ NonRec (TCDecl {tcDeclCtxt = AValue, tcDeclDef = Defined _}) ]}) -> True
        (TCModule {tcModuleDecls = [ NonRec (TCDecl {tcDeclCtxt = AGrammar, tcDeclDef = Defined _}) ]}) -> False
        (TCModule {tcModuleDecls = [ NonRec (TCDecl {tcDeclCtxt = AGrammar, tcDeclDef = ExternDecl _}) ]}) -> True
        (TCModule {tcModuleDecls = _}) -> error "broken invariant: more than one declaration"

    buildFunSpec :: TCModule (SourceRange, PAST.Annot) -> ([Name], PAST.CorV)
    buildFunSpec t =
      case t of
        (TCModule {tcModuleDecls = [ NonRec decl@(TCDecl {tcDeclCtxt = AClass, tcDeclDef = Defined v}) ]}) ->
          (map paramToName (tcDeclParams decl), PAST.ClassExpr v)
        (TCModule {tcModuleDecls = [ NonRec decl@(TCDecl {tcDeclCtxt = AValue, tcDeclDef = Defined v}) ]}) ->
          (map paramToName (tcDeclParams decl), PAST.ValueExpr v)
        (TCModule {tcModuleDecls = _}) ->
          error "broken invariant: more than one declaration"


systemToGrammars :: PAST.GblAlloc -> PAST.GblGrammar
systemToGrammars m =
  Map.map g (Map.filter f m)
  where
    f t =
      case t of
        (TCModule {tcModuleDecls = [ NonRec (TCDecl {tcDeclCtxt = AGrammar, tcDeclDef = Defined _}) ]}) ->
          True
        _ -> False

    g :: TCModule (SourceRange, PAST.Annot) -> TCDecl (SourceRange, PAST.Annot)
    g (TCModule {tcModuleDecls = [ NonRec decl@(TCDecl {tcDeclCtxt = AGrammar, tcDeclDef = Defined _}) ]}) =
      decl
    g _ = error "broken invariant"



--- 2. Build the Automaton

genGExpr :: PAST.GblGrammar -> TC (SourceRange, PAST.Annot) Grammar -> MapAut
genGExpr gbl e =
  let ann = snd (texprAnnot e)
      getS i = PAST.getState ann i in
  case texprValue e of
    TCPure e1 ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [(n1, UniChoice (SAct (EvalPure e1), n2))]) n2
    TCLabel _ g -> -- TODO: ignore the Label case
      genGram gbl g
    TCGuard e1 ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (SAct (Guard e1), n2)) ]) n2
    TCCurrentStream ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct (GetStream), n2)) ]) n2
    TCSetStream e1 ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct (SetStream e1), n2)) ]) n2
    TCStreamLen s e1 e2 ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct (StreamLen s e1 e2), n2)) ]) n2
    TCStreamOff s e1 e2 ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct (StreamOff s e1 e2), n2)) ]) n2
    TCGetByte s ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct (IGetByte s), n2)) ]) n2
    TCMatch s e1 ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct (ClssAct s e1), n2)) ]) n2
    TCMatchBytes s e1 ->
      case getByteArray e1 of
        Nothing ->
          let n1 = getS 0
              n2 = getS 1
          in mkAut n1 (mkTr [(n1, UniChoice (IAct (IMatchBytes s e1), n2))]) n2
        Just w ->
          mkAut (getS 0) (mkTr (createTrans 0)) stSemEnd
          where
            strLen = length w
            stSemStart = getS (2*strLen)
            stSemEnd =   getS (2*strLen + 1)
            createTrans index =
              if index >= strLen
              then
                case s of
                  YesSem -> [(stSemStart, UniChoice (SAct (EvalPure e1), stSemEnd))]
                  NoSem  ->
                    let eunit = TC (TCAnnot { tcAnnot = texprAnnot e, tcAnnotExpr = TCUnit}) in
                    [(stSemStart, UniChoice (SAct (EvalPure eunit), stSemEnd))]
              else
                let ebyte = TC (TCAnnot { tcAnnot = texprAnnot e
                                        , tcAnnotExpr = TCLiteral (LByte (w !! index)) tByte })
                    eSetSingle = TC (TCAnnot { tcAnnot = texprAnnot e, tcAnnotExpr = TCSetSingle ebyte})
                    iact = ClssAct NoSem eSetSingle
                    n0 = getS (2 * index)
                    n1 = getS (2 * index + 1)
                    n2 = getS (2 * (index + 1))
                    tr = [ (n0, UniChoice (IAct iact, n1))
                         , (n1, UniChoice (SAct DropOneOut, n2))
                         ]
                in tr ++ createTrans (index+1)

    TCChoice c lst _ty ->
      let lstITF = map (\ expr -> dsAut $ genGram gbl expr) lst
          n1 = getS 0
          n2 = getS 1
          transIn  = foldr (\ (i1,_t1,_f1,_) accTr -> (EpsA, i1) : accTr) [] lstITF
          transBdy = foldr (\ (_i1,t1,_f1,_) accTr -> unionTr t1 accTr) emptyTr lstITF
          pops     = foldr (\ (_,_,_, p1) accPop -> unionPopTrans p1 accPop) emptyPopTrans lstITF
      in
        case c of
          Commit ->
            let transOut = foldr (\ (_i1,_t1,f1,_) accTr -> (f1, UniChoice (BAct (CutBiasAlt n2), n2)) : accTr) [] lstITF
            in mkAutWithPop n1 (unionTr (mkTr1 (n1, SeqChoice transIn n2)) (unionTr (mkTr transOut) transBdy)) n2 pops
          Backtrack ->
            let transOut = foldr (\ (_i1,_t1,f1,_) accTr -> (f1, UniChoice (EpsA, n2)) : accTr) [] lstITF
            in mkAutWithPop n1 (unionTr (mkTr1 (n1, ParChoice transIn)) (unionTr (mkTr transOut) transBdy)) n2 pops
    TCOptional c e1 ->
      let (i1, t1, f1, pops) = dsAut $ genGram gbl e1
          n1 = getS 0
          n2 = getS 1
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
      in mkAutWithPop n1 (unionTr (mkTr trans) t1) n2 pops
    TCMany s c bounds e1 ->
      let (i1, t1, f1, pops) = dsAut $ genGram gbl e1
          n1 = getS 0
          n2 = getS 1
          n3 = getS 2
          n4 = getS 3
          n5 = getS 4
          n6 = getS 5
          n7 = getS 6
          n8 = getS 7
          loopTrans =
            case c of
              Backtrack -> -- Not sure this case is possible or tested at the moment
                [ (n1, UniChoice (CAct (BoundSetup bounds), n2)),
                  (n2, UniChoice (SAct (ManyFreshList s), n3)),
                  (n3, ParChoice [(CAct (BoundIsMore), i1),
                                  (CAct (BoundCheckSuccess), n7)]),
                  (f1, UniChoice (SAct (ManyAppend s), n4)),
                  (n4, UniChoice (CAct (BoundIncr), n3)),
                  (n7, UniChoice (SAct (ManyReturn), n8))
                ]
              Commit ->
                [ (n1, UniChoice (CAct (BoundSetup bounds), n2)),
                  (n2, UniChoice (SAct (ManyFreshList s), n3)),
                  (n3, SeqChoice [(CAct (BoundIsMore), i1),
                                  (CAct (BoundCheckSuccess), n6)] n8),
                  (f1, UniChoice (SAct (ManyAppend s), n4)),
                  (n4, UniChoice (CAct (BoundIncr), n5)),
                  (n5, UniChoice (BAct (CutBiasAlt n8), n3)),
                  (n6, UniChoice (BAct (CutBiasAlt n8), n7)),
                  (n7, UniChoice (SAct (ManyReturn), n8))
                ]
      in mkAutWithPop n1 (unionTr (mkTr loopTrans) t1) n8 pops
    TCEnd ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct IEnd, n2)) ]) n2
    TCOffset ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (IAct IOffset, n2)) ]) n2
    TCMapLookup ws e1 e2 ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (SAct (MapLookup ws e1 e2), n2)) ]) n2
    TCMapInsert ws e1 e2 e3 ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (SAct (MapInsert ws e1 e2 e3), n2)) ]) n2
    TCCoerceCheck ws t1 t2 e1 ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [ (n1, UniChoice (SAct (CoerceCheck ws t1 t2 e1), n2)) ]) n2
    TCFor lp ->
      case loopFlav lp of
        Fold name1 e1 ->
          let nname1 = tcName name1
              nname2 = tcName (loopElName lp)
              e2 = loopCol lp
              gram = loopBody lp
              n1 = getS 0
              n2 = getS 1
              n3 = getS 2
              n4 = getS 3
              n5 = getS 4
              infoN = n5
              (i1, t1, f1, pops) = dsAut $ genGram gbl gram
              trans = mkTr
                [ (n1, UniChoice (CAct (ForInit nname1 e1 nname2 e2), n2))
                , (n2, SeqChoice [ (CAct (ForHasMore), i1), (CAct (ForEnd), n4) ] infoN)
                , (f1, UniChoice (CAct (ForNext), n3))
                , (n3, UniChoice (BAct (CutBiasAlt infoN), n2))
                , (n4, UniChoice (BAct (CutBiasAlt infoN), n5))
                ]
          in mkAutWithPop n1 (unionTr trans t1) n5 pops
        LoopMap ->
          let nname1 = tcName (loopElName lp)
              e1 = loopCol lp
              gram = loopBody lp
              n1 = getS 0
              n2 = getS 1
              n3 = getS 2
              n4 = getS 3
              n5 = getS 4
              infoN = n5
              (i1, t1, f1, pops) = dsAut $ genGram gbl gram
              trans = mkTr
                [ (n1, UniChoice (CAct (MapInit nname1 e1), n2))
                , (n2, SeqChoice [ (CAct (MapHasMore), i1), (CAct (MapEnd), n4) ] infoN)
                , (f1, UniChoice (CAct (MapNext), n3))
                , (n3, UniChoice (BAct (CutBiasAlt infoN), n2))
                , (n4, UniChoice (BAct (CutBiasAlt infoN), n5))
                ]
          in mkAutWithPop n1 (unionTr trans t1) n5 pops
    TCCall name _ le ->
      let nname = tcName name
          (_, annot) =
            let x = Map.lookup nname gbl in
              case x of
                Nothing -> error ("Name not found in gbl grammar: " ++ show nname)
                Just r -> tcDeclAnnot r
          n1 = getS 0
          n2 = getS 1
          call = PAST.getState annot 0
          ret  = PAST.getState annot 1
          trans = mkTr
            [ (n1, UniChoice (CAct (Push nname (map argToValue le) n2), call))
            -- , (ret, UniChoice (CAct (Pop n2), n2))
            ]
      in mkAutWithPop n1 trans n2 (addPopTrans ret n2 emptyPopTrans)
    TCErrorMode c e1 ->
      let n1 = getS 0
          n2 = getS 1
          (i1, t1, f1, pops) = dsAut $ genGram gbl e1
          trans =
            case c of
              Commit ->
                [ (n1, UniChoice (BAct (CutLocal), i1)),
                  (f1, UniChoice (EpsA, n2))
                ]
              Backtrack -> error "not handled in ErrorMode"
      in mkAutWithPop n1 (unionTr (mkTr trans) t1) n2 pops
    TCFail e1 _ ->
      let n1 = getS 0
          n2 = getS 1
      in mkAut n1 (mkTr [(n1, UniChoice (BAct (FailAction e1), n2))]) n2

    TCCase e1 lpat dpat ->
      let lstITF =
            map
            (\ _pat@(TCAlt{tcAltPatterns = pcase, tcAltBody = bdy}) ->
                (Just pcase, dsAut $ genGram gbl bdy)
            ) (NonEmpty.toList lpat)
            ++
            (case dpat of
               Nothing -> []
               Just bdy -> [ (Nothing, dsAut $ genGram gbl bdy) ]
            )

          n1 = getS 0
          n2 = getS 1
          n3 = getS 2
          n4 = getS 3
          infoN = n4
          transIn  = foldr (\ (mpcase, (i1,_t1,_f1,_)) accTr ->
                              (maybe
                                ((CAct (CaseTry Nothing), i1) : accTr)
                                (\pcase -> (CAct (CaseTry (Just pcase)), i1) : accTr)
                                mpcase
                              )
                           )
                     [] lstITF
          transBdy = foldr (\ (_mpcase, (_i1,t1,_f1,_)) accTr -> unionTr t1 accTr) emptyTr lstITF
          pops     = foldr (\ (_mpcase, (_,_,_, p1)) accPop -> unionPopTrans p1 accPop) emptyPopTrans lstITF
      in
        let transOut = foldr (\ (_mpcase, (_i1,_t1,f1,_)) accTr ->
                                (f1, UniChoice (BAct (CutBiasAlt infoN), n3)) : accTr) [] lstITF
        in mkAutWithPop n1
           (unionTr
             (mkTr [ (n1, UniChoice (CAct (CaseCall e1), n2))
                   , (n2, SeqChoice transIn infoN)
                   , (n3, UniChoice (CAct CaseEnd, n4))
                   ])
             (unionTr (mkTr transOut) transBdy)
           )
           n4 pops

    x -> error ("Case not handled: " ++ show x)

data WhatGram = DoSeq | NonDoSeq

{- To see how the states are allocated, see  `allocGram` function.
   There are three cases
   * 1) an expression is not Do/Pure then just call `genGExpr`
   * 2) an expression has a Do/Pure
     * a) if it ends with Pure then end with `ReturnBind`
     * b) otherwise it ends with `ReturnLast`

   TODO: 2 b) is where a tail call optimization could happen
-}
genGram :: PAST.GblGrammar -> TC (SourceRange, PAST.Annot) Grammar -> MapAut
genGram gbl e =
  let
    genForBindList :: TC (SourceRange, PAST.Annot) Grammar -> (MapAut, WhatGram)
    genForBindList expr =
      case texprValue expr of
        TCDo _ _ _ -> (genForDo expr, DoSeq)
        TCPure _   -> (genForDo expr, DoSeq)
        _ -> (genGExpr gbl expr, NonDoSeq)

    genForDo :: TC (SourceRange, PAST.Annot) Grammar -> MapAut
    genForDo expr =
      let ann = snd $ texprAnnot expr in
      case texprValue expr of
        TCDo mname e1 e2 ->
          let name = maybe Nothing (\x -> Just $ tcName x) mname
              (i1,t1,f1,pops1) = dsAut $ genGram gbl e1
              (i2,t2,f2,pops2) = dsAut $ genForDo e2
              n1 = PAST.getState ann 0
              n2 = PAST.getState ann 1
              trans = mkTr
                [ (n1, UniChoice (EpsA, i1))
                , (f1, UniChoice (SAct (EnvStore name), i2))
                , (f2, UniChoice (EpsA, n2))
                ]
          in mkAutWithPop n1 (unionTr trans (unionTr t1 t2)) n2 (unionPopTrans pops1 pops2)
        TCPure e1 ->
          let n1 = PAST.getState ann 0
              n2 = PAST.getState ann 1
          in mkAut n1 (mkTr [(n1, UniChoice (SAct (ReturnBind e1), n2))]) n2
        _x ->
          let
              (i1,t1,f1, pops) = dsAut $ genGExpr gbl expr
              n1 = PAST.getState ann (PAST.getStatesLength ann - 2)
              n2 = PAST.getState ann (PAST.getStatesLength ann - 1)
              trans = mkTr
                [ (n1, UniChoice (EpsA, i1))
                , (f1, UniChoice (SAct ReturnLast, n2))
                ]
          in mkAutWithPop n1 (unionTr trans t1) n2 pops

  in
    let (aut, wg) = genForBindList e
        (i1,t1,f1,pops) = dsAut $ aut
        ann = snd $ texprAnnot e
        getS i = PAST.getState ann i
        n1 = getS (PAST.getStatesLength ann - 2)
        n2 = getS (PAST.getStatesLength ann - 1)
        trans =
          case wg of
            DoSeq -> mkTr [ (n1, UniChoice (SAct EnvFresh, i1)), (f1, UniChoice (EpsA, n2))]
            NonDoSeq -> mkTr [ (n1, UniChoice (EpsA, i1)), (f1, UniChoice (EpsA, n2))]
    in mkAutWithPop n1 (unionTr trans t1) n2 pops


genDecl :: PAST.GblGrammar -> TCDecl (SourceRange, PAST.Annot) -> MapAut
genDecl gbl (TCDecl {tcDeclCtxt = AGrammar, tcDeclDef = Defined e, tcDeclAnnot = (_,ann), tcDeclParams = ps}) =
  let (i1,t1,f1,pops) = dsAut $ genGram gbl e
      n1 = PAST.getState ann 0
      n2 = PAST.getState ann 1
      trans = mkTr
        [ (n1, UniChoice (CAct (ActivateFrame (map paramToName ps)), i1))
        , (f1, UniChoice (CAct (DeactivateReady), n2))
        ]
  in mkAutWithPop n1 (unionTr trans t1) n2 pops
genDecl _gbl _ = error "broken invariant"


getTCModuleAnnot :: TCModule (a, PAST.Annot) -> PAST.Annot
getTCModuleAnnot (TCModule {tcModuleDecls = [ NonRec (TCDecl {tcDeclAnnot = annot})]}) = snd annot
getTCModuleAnnot _ = error "broken invariant"

getTCModuleName :: TCModule (a, PAST.Annot) -> Name
getTCModuleName (TCModule {tcModuleDecls = [ NonRec (TCDecl {tcDeclCtxt = AGrammar, tcDeclDef = Defined _, tcDeclName = name})]}) = name
getTCModuleName _ = error "broken invariant"


getTCModuleDecl :: TCModule (a, PAST.Annot) -> TCDecl (a, PAST.Annot)
getTCModuleDecl (TCModule {tcModuleDecls = [ NonRec decl@(TCDecl {tcDeclCtxt = AGrammar, tcDeclDef = Defined _})]}) = decl
getTCModuleDecl _ = error "broken invariant"

buildMapAut :: [TCModule SourceRange] -> MapAut
buildMapAut decls =
  let
    aut =
      mkAutWithPop cSTART_STATE (unionTr mainTrans table) cFINAL_STATE (unionPopTrans mainPop pops)
  in
  let
    aut1 = aut { stateMapping = Map.insert cSTART_STATE (mainSourceRange, mainName) stateInfo }
  in
  let
    aut2 = addGblFunsAut (systemToFunctions allocDecls) aut1
  in
    aut2
  where
    allocDecls = allocStates decls
    allocGrammar = systemToGrammars allocDecls
    (mainFullName, mainSourceRange, mainInfo) =
      let
        searchMain =
          Map.foldrWithKey
          (\ k a b ->
             case b of
               Just res -> Just res
               Nothing ->
                 let ident = PAST.name2Text k in
                   -- TODO: this infixOf test should be removed
                 if isInfixOf "Main" (show ident)
                 then Just (k, getSourceRangeDecl a, a)
                 else Nothing
          ) Nothing allocDecls
      in
        case searchMain of
          Nothing -> error "Missing Main rule"
          Just x -> x
    mainAnnots = getTCModuleAnnot mainInfo
    startState = PAST.getState mainAnnots 0
    finalState = PAST.getState mainAnnots 1
    mainName = mainFullName
    lstDecls = Map.foldr (\ decl acc -> decl : acc) [] allocGrammar
    f a (trAccu,popsAccu) =
      let (_i, t, _f, p1) = dsAut $ genDecl allocGrammar a in (unionTr t trAccu, unionPopTrans p1 popsAccu)
    (table, pops) = foldr f (emptyTr, emptyPopTrans) lstDecls

    stateInfo = collectDecls allocDecls
    mainTrans = mkTr
      [ (cSTART_STATE, UniChoice (CAct (Push mainName [] cFINAL_STATE), startState))
      ]
    mainPop = addPopTrans finalState cFINAL_STATE emptyPopTrans

buildArrayAut :: [TCModule SourceRange] -> ArrayAut
buildArrayAut decls =
  let
    aut = buildMapAut decls
  in
    -- This trace is to show the amount of non-determinism generated by Pop instructions
    -- trace (show (foldr (\ a b -> case a of
    --                  UniChoice _ -> max b 1
    --                  SeqChoice lst _ -> let n = length lst in (trace $ show n) $ if n >= b then trace (show n) $ n else b
    --                  ParChoice lst -> let n = length lst in (trace $ show n) $ if n >= b then trace (show n) $ n else b
    --              ) 0 (transition aut))) $

    -- trace (Map.foldrWithKey (\ k a b -> show k ++ "," ++ show a ++ "\n" ++ b) "" (popTrans aut)) $

    convertToArrayAut aut
