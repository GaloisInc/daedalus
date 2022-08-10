{-# Language TemplateHaskell, ConstraintKinds, ImplicitParams #-}
{-# Language RankNTypes, BlockArguments #-}
module Daedalus.VM.Backend.Haskell
  ( compileModule
  , Config(..)
  , defaultConfig
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Data.Functor.Identity as RTS
import qualified Daedalus.RTS as RTS

import qualified Daedalus.TH as TH
import Daedalus.Panic(panic)
import Daedalus.PP(pp)
import Daedalus.Rec(forgetRecs)
import qualified Daedalus.BDD as BDD
import qualified Daedalus.Core as Core
import qualified Daedalus.Core.Type as Core
import Daedalus.Core.TH.Names
import Daedalus.Core.TH.Type
import Daedalus.Core.TH.TypeDecls
import Daedalus.Core.TH.Ops
import qualified Daedalus.Core.Bitdata as BD
import Daedalus.VM

import Debug.Trace

data Config = Config
  { userMonad      :: Maybe TH.TypeQ
  , userPrimitives :: Map FName ([TH.ExpQ] -> TH.ExpQ)
  }

defaultConfig :: Config
defaultConfig = Config
  { userMonad = Nothing
  , userPrimitives = mempty
  }

-- | Make a function with the given list of arguments and result.
funT :: [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
funT args res = foldr addArg res args
  where
  addArg x y = [t| $x -> $y |]

type HasConfig = (?config :: Config)

type HasTypes  = (?tdecls :: Map Core.TName Core.TDecl)

compileVMT :: VMT -> TH.TypeQ
compileVMT vmt =
  case vmt of
    TSem t    -> compileMonoType t
    TThreadId -> [t| Int |]


--------------------------------------------------------------------------------
compileModule :: Config -> Module -> TH.DecsQ
compileModule cfg m =
  do tys <- compileTDecls (mTypes m)
     let ?config = cfg
     let ?tdecls = Map.fromList [ (Core.tName d, d) | d <- forgetRecs (mTypes m) ]
     fus <- mapM compileFun (mFuns m)
     pure (tys ++ concat fus)


--------------------------------------------------------------------------------

compileFun :: (HasConfig, HasTypes) => VMFun -> TH.DecsQ
compileFun fun =
  do args     <- traverse newArg srcArgs

     (funTy,contArgs) <-
        if vmfPure fun then pure (PureFun, []) else
        case vmfCaptures fun of
          NoCapture -> pure (NonCapturingParser (),[])
          _ -> do rtyName  <- TH.newName "r"
                  noK  <- TH.newName "noK"
                  yesK <- TH.newName "yesK"
                  let noP  = TH.varP noK
                      yesP = TH.varP yesK
                  pure ( CapturingParser () rtyName (TH.varE noK) (TH.varE yesK)
                       , [ noP, yesP ]
                       )
     let def = compileDef (vmfName fun) funTy (vmfDef fun) (map snd args)
     decl <- TH.funD (fnameName (vmfName fun))
                  [ TH.clause (map fst args ++ contArgs) (TH.normalB def) [] ]

     sig <- TH.sigD (fnameName (vmfName fun)) (sigTy funTy)
     pure [sig,decl]

  where
  srcTs   = map (compileVMT . getType) srcArgs
  valResT = compileMonoType (Core.typeOf (vmfName fun))
  sigTy funTy =

    case funTy of
      PureFun ->
        funT srcTs
        valResT

      NonCapturingParser {} ->
        funT srcTs
        case userMonad ?config of
          Nothing -> [t| RTS.DParser $valResT |]
          Just m  -> [t| RTS.DParserM $m $valResT |]

      CapturingParser _ rtyName _ _ ->
        TH.forallT [TH.plainTV rtyName] (TH.cxt []) $
        funT srcTs
        case userMonad ?config of
          Nothing -> [t| RTS.CParser $rty $valResT |]
          Just m  -> [t| RTS.CParserM $rty $m $valResT |]
        where rty = TH.varT rtyName



  srcArgs =
    case vmfDef fun of
      VMExtern bs -> bs
      VMDef d -> case Map.lookup (vmfEntry d) (vmfBlocks d) of
                   Just b -> blockArgs b
                   Nothing -> panic "compileFun" ["Missing entry"]

compileDef ::
  (HasConfig, HasTypes) => FName -> FunTy () -> VMFDef -> [TH.ExpQ] -> TH.ExpQ
compileDef nm ty def args =
  case def of
    VMExtern {} ->
      case Map.lookup nm (userPrimitives ?config) of
        Just d -> d args
        Nothing -> panic "compileDef" ["Missing primitve"]
    VMDef d ->
      TH.letE [ compileBlock ty b | b <- Map.elems (vmfBlocks d) ]
              (TH.appsE (TH.varE (labelName (vmfEntry d)) : args))



compileBlock :: (HasConfig, HasTypes) => FunTy () -> Block -> TH.DecQ
compileBlock ty b =
  do args <- traverse newArg (blockArgs b)

     (funTy,sArgs) <-
        case ty of
          PureFun -> pure (PureFun, [])
          NonCapturingParser {} ->
            do (spat,sexp) <- newArg' "s" [t| RTS.ParserErrorState |]
               pure (NonCapturingParser sexp, [ spat ])
          CapturingParser _ rty noK yesK ->
            do let m = case userMonad ?config of
                         Nothing -> [t| RTS.Identity |]
                         Just mt -> mt
               (spat,sexp) <- newArg' "s" [t| RTS.ThreadState $(TH.varT rty) $m |]
               pure (CapturingParser sexp rty noK yesK, [ spat ])

     let ?localVars = mempty
         ?blockArgs = Map.fromList (zip (blockArgs b) (map snd args))
         ?funTy     = funTy

     let def = compileInstrs (blockInstrs b)
             $ compileCInstr (blockTerm b)

     TH.funD (labelName (blockName b))
       [ TH.clause (map fst args ++ sArgs) (TH.normalB def) [] ]




newArg :: BA -> TH.Q (TH.PatQ, TH.ExpQ)
newArg ba = newArg' (show (pp ba)) (compileVMT (getType ba))

newArg' :: String -> TH.TypeQ -> TH.Q (TH.PatQ, TH.ExpQ)
newArg' str' ty =
  do nm <- TH.newName str
     let pat  = TH.sigP (TH.bangP (TH.varP nm)) ty
         expr = TH.varE nm
     pure (pat,expr)
  where
  str = zEsc str'




--------------------------------------------------------------------------------
type ExprEnv =
  ( ?blockArgs    :: Map BA TH.ExpQ     -- block arguments
  , ?localVars    :: Map BV TH.ExpQ     -- "copy" to be inlined
  )

addLocal :: ExprEnv => BV -> TH.ExpQ -> (ExprEnv => a) -> a
addLocal x e k =
  let ?localVars = Map.insert x e ?localVars
  in k

compileE :: ExprEnv => E -> TH.ExpQ
compileE expr =
  case expr of
    EUnit         -> compileOp0 Core.Unit
    ENum i t      -> compileOp0 (Core.IntL i t)
    EBool b       -> compileOp0 (Core.BoolL b)
    EFloat d t    -> compileOp0 (Core.FloatL d t)
    EMapEmpty k v -> compileOp0 (Core.MapEmpty k v)
    ENothing t    -> compileOp0 (Core.ENothing t)

    EBlockArg ba  ->
      case Map.lookup ba ?blockArgs of
        Just e -> e
        Nothing -> panic "compileE" ["Missing argument", show (pp ba)]

    EVar bv ->
      case Map.lookup bv ?localVars of
        Just e  -> e
        Nothing -> panic "compileE" ["Missing local variable", show (pp bv)]

compilePrim :: (HasTypes,ExprEnv) => PrimName -> [E] -> TH.ExpQ
compilePrim prim es =
  let args = map compileE es
  in
  case prim of

    StructCon ut
      | not (Core.tnameBD nm) ->
          [| $(TH.appsE (TH.conE (structConName nm) : args)) |]

      | otherwise -> mkBDCon nm args
      where
      nm = Core.utName ut

    NewBuilder t   -> compileOp0 (Core.NewBuilder t)
    Integer i      -> compileOp0 (Core.IntL i Core.TInteger)
    ByteArray bs   -> compileOp0 (Core.ByteArrayL bs)

    Op1 op1 ->
      case args of
        [e] -> compileOp1 op1 (getSemType (head es)) e
        _   -> panic "compilePrim" ["Op1 arity mismatch"]


    Op2 op2 ->
      case args of
        [e1,e2] -> compileOp2 op2 e1 e2
        _       -> panic "compilePrim" ["Op2 arity mismatch"]

    Op3 op3 ->
      case args of
        [e1,e2,e3] -> compileOp3 op3 e1 e2 e3
        _          -> panic "compilePrim" ["Op3 arity mismatch"]

    OpN opN -> compileOpN doFun opN args
      where doFun = panic "compileOpN" ["Unexpcetd function call"]

mkBDCon :: HasTypes => Core.TName -> [TH.ExpQ] -> TH.ExpQ
mkBDCon nm args =
  [| RTS.fromBits ($val :: $(compileMonoType (Core.tWord (toInteger bw))))
        :: $(TH.conT (dataName nm)) |]

  where

  val = case zipWith doF labs args of
          [] -> [| RTS.UInt base |]
          vs -> let allVs = if base == 0 then vs else [| RTS.UInt base |] : vs
                in foldr1 (\a b -> [| RTS.bitOr $a $b |]) allVs

  base = BD.bdStructConBase fs
  Core.TFlavStruct labs = Core.tnameFlav nm

  (bw,fs) = case Map.lookup nm ?tdecls of
              Just d ->
                 case Core.tDef d of
                   Core.TBitdata w (Core.BDStruct fs') -> (BDD.width w, fs')
                   _ -> bad "Not a bitdata struct"
              Nothing -> bad "Misisng bitdata declaration"

  fiMap = Map.fromList [ (l, (Core.bdOffset f, toInteger (Core.bdWidth f)))
                       | f <- fs, Core.BDData l _ <- [Core.bdFieldType f] ]

  doF l e =
    case Map.lookup l fiMap of
      Just (o,w) ->
        [| RTS.convert (RTS.toBits $e :: $(compileMonoType (Core.tWord w)))
             `RTS.shiftl` RTS.UInt o
        |]
      Nothing    -> bad ("Missing constructor " ++ show (pp l))


  bad msg = panic "mkBDCon" [msg, "in bitdata " ++ show (pp nm)]


--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
data FunTy s =
    PureFun
  | NonCapturingParser s -- error state
  | CapturingParser s -- thread state
                    TH.Name    -- the type of the "final" result of the parser
                    TH.ExpQ   -- no cont
                    TH.ExpQ   -- yes cont


type BlockEnv =
  ( HasConfig
  , HasTypes
  , ExprEnv
  , ?funTy :: FunTy TH.ExpQ
  )

stateArgs :: BlockEnv => [TH.ExpQ]
stateArgs = case ?funTy of
              NonCapturingParser e    -> [e]
              CapturingParser e _ _ _ -> [e]
              PureFun                 -> []

getErrorState :: BlockEnv => TH.ExpQ
getErrorState =
  case ?funTy of
    NonCapturingParser e      -> e
    CapturingParser e _ _ _   -> [| RTS.thrErrors $e |]
    PureFun                   -> panic "getErrorState" ["Pure parser"]

updErrorState :: BlockEnv => (BlockEnv => a) -> (TH.ExpQ -> TH.ExpQ) -> a
updErrorState k f =
  let ?funTy =
        case ?funTy of
          NonCapturingParser e -> NonCapturingParser (f e)
          CapturingParser e ty noK yesK ->
            CapturingParser
              [| RTS.thrUpdateErrors (\x -> $(f [|x|])) $e |] ty noK yesK
          PureFun -> panic "updErrorState" ["Pure parser"]
  in k

getThreadState :: BlockEnv => TH.ExpQ
getThreadState =
  case ?funTy of
    CapturingParser e _ _ _ -> e
    _  -> panic "getThreadState" ["Not a capturing parser"]

withThrErrorState ::
  BlockEnv => (TH.ExpQ -> (TH.ExpQ -> TH.ExpQ) -> TH.ExpQ) -> TH.ExpQ
withThrErrorState k =
  case ?funTy of
    CapturingParser e t no yes ->
      [| let s = $e
         in $(let ?funTy = CapturingParser [|s|] t no yes
              in k [|RTS.thrErrors s|]
                   (\s1 -> [| RTS.thrUpdateErrors (const $s1) s |])
             )
      |]
    _ -> panic "withThrErrorState" ["Not a capturing parser"]


updThreadState :: BlockEnv => (BlockEnv => a) -> (TH.ExpQ -> TH.ExpQ) -> a
updThreadState k f =
  let ?funTy =
        case ?funTy of
          CapturingParser e ty noK yesK -> CapturingParser (f e) ty noK yesK
          _ -> panic "updThreadState" ["Not a capturing parser"]
  in k

compileInstrs :: BlockEnv => [Instr] -> (BlockEnv => TH.ExpQ) -> TH.ExpQ
compileInstrs is k =
  case is of
    []       -> k
    i : more -> compileInstr i (compileInstrs more k)

compileInstr :: BlockEnv => Instr -> (BlockEnv => TH.ExpQ) -> TH.ExpQ
compileInstr i k =
  case i of

    Say x -> [| trace x $k |]

    CallPrim x p es ->
      [| let v = $(compilePrim p es)
         in $(addLocal x [| v |] k)
      |]

    Let x e -> addLocal x (compileE e) k

    Free {} -> k

    NoteFail src loc inp msg ->
      let txtLoc = Text.pack loc
      in updErrorState k \s ->
            [| RTS.vmNoteFail
                 $(case src of
                     Core.ErrorFromUser   -> [| RTS.FromUser   |]
                     Core.ErrorFromSystem -> [| RTS.FromSystem |]
                  )
                  txtLoc
                 $(compileE inp)
                 $(compileE msg)
                 $s
            |]

    PushDebug c t ->
      updErrorState k \s ->
         case c of
           DebugCall     -> [| RTS.vmPushDebugCall t $s |]
           DebugTailCall -> [| RTS.vmPushDebugTail t $s |]

    PopDebug ->
      updErrorState k \s -> [| RTS.vmPopDebug $s |]

    Spawn x c -> [| case RTS.vmSpawn $code $getThreadState of
                      (tid,s1) -> $( addLocal x [| tid |]
                                   $ updThreadState k \_ -> [| s1 |]
                                   )
                 |]

      where code = [| \n s -> $(doJump [ [| n |] ] [ [| s |] ] c) |]

    Output e  -> updThreadState k \s -> [| RTS.vmOutput $(compileE e) $s |]
    Notify e  -> updThreadState k \s -> [| RTS.vmNotify $(compileE e) $s |]


-- XXX: strictness
compileCInstr :: BlockEnv => CInstr -> TH.ExpQ
compileCInstr cinstr =
  case cinstr of
    ReturnPure a      -> compileE a

    ReturnNo ->
      case ?funTy of
        NonCapturingParser {} ->
          let val = [| (Nothing, $getErrorState) |]
          in case userMonad ?config of
               Nothing -> val
               Just {} -> [| pure $val |] -- do we need type sig here?

        CapturingParser _ _ noK _ -> [| $noK $getThreadState |]
        PureFun {} -> panic "compileCInstr" ["ReturnNo in pure"]

    ReturnYes a inp ->
      case ?funTy of
        NonCapturingParser {} ->
          let val = [| ( Just ($(compileE a), $(compileE inp))
                       , $getErrorState) |]
          in case userMonad ?config of
              Nothing -> val
              Just {} -> [| pure $val |]

        CapturingParser _ _ _ yesK -> [| $yesK $(compileE a) $(compileE inp)
                                             $getThreadState |]
        PureFun {} -> panic "compileCInstr" ["ReturnNo i  pure"]

    Jump jp -> doJump [] stateArgs jp

    JumpIf e (JumpCase ch)
      | Core.TUser ut <- getSemType e
      , let bdname = Core.utName ut, Core.tnameBD bdname -> doBDCase bdname
      | otherwise -> TH.caseE (compileE e) $
                        map doAlt rhss ++
                        case mbDflt of
                           Nothing -> []
                           Just d  -> [mkAlt [p| _ |] d ]

      where
      (rhss1,mbDflt) =
        case Map.lookup Core.PAny ch of
          Nothing -> (Map.toList ch, Nothing)
          Just d  -> (Map.toList (Map.delete Core.PAny ch), Just (toRHS d))

      toRHS j = doJump [] stateArgs (jumpTarget j)

      rhss = [ (p, toRHS j) | (p,j) <- rhss1 ]

      numP  n       = TH.conP 'RTS.UInt [ TH.litP (TH.integerL n) ]
      mkAlt p rhs   = TH.match p (TH.normalB rhs) []

      doAlt (p,rhs) = mkAlt (doPat p) rhs

      doPat p =
        case p of
          Core.PBool b   -> if b then [p| True |] else [p| False |]
          Core.PNothing  -> [p| Nothing |]
          Core.PJust     -> [p| Just {} |]
          Core.PNum i    -> let n = TH.litP (TH.integerL i)
                            in case getSemType e of
                                 Core.TInteger -> n
                                 Core.TUInt {} -> [p| RTS.UInt $n |]
                                 Core.TSInt {} -> [p| RTS.SInt $n |]
                                 t -> panic "doPat"
                                        ["Unexpected type for numeric pattern"
                                        , show (pp t) ]
          Core.PBytes {} -> panic "compileCInstr" ["PBytes"]
          Core.PCon l    -> flip TH.recP []
                            case getSemType e of
                              Core.TUser ut -> unionConName (Core.utName ut) l
                              _ -> panic "compileCInstr" ["ConP not UserT"]
          Core.PAny      -> [p| _ |]

      doBDCase bdname =
        let opts  = BD.bdCase bdname
                      [ (l,r) | (Core.PCon l, r) <- rhss ]
                      mbDflt

            doChoices val cs orElse =
              TH.caseE val (
                 [ mkAlt (numP i) rhs | (i,rhs) <- cs ] ++
                 [ mkAlt [p| _ |] orElse ]
              )

            doOpt :: TH.ExpQ -> (Integer, [(Integer, TH.ExpQ)]) ->
                                TH.ExpQ -> TH.ExpQ
            doOpt val (mask,choices) orElse
              | mask == 0 =
                case lookup 0 choices of
                  Just r -> r
                  Nothing -> panic "doOpt" ["Nothing"]
              | otherwise =
                  doChoices [| $val `RTS.bitAnd` UInt mask |] choices orElse

        in [| let x = RTS.toBits $(compileE e)
              in $(foldr (doOpt [|x|]) [| error "Unreachable" |] opts)
            |]




    Yield -> [| RTS.vmYield $getThreadState |]

    CallPure f jp es ->
      doJump [ doCall f (map compileE es) ] stateArgs jp

    Call f how no yes es ->
      case how of
        NoCapture ->
          case ?funTy of

            NonCapturingParser {} ->
              dToC (doCall f (map compileE es ++ stateArgs))
                   (\s1     -> doJump []     [s1] no)
                   (\a i s1 -> doJump [a, i] [s1] yes)

            CapturingParser {} ->
              withThrErrorState \getS setS ->
                dToC (doCall f (map compileE es ++ [getS]))
                     (\s1     -> doJump []     [setS s1] no)
                     (\a i s1 -> doJump [a, i] [setS s1] yes)

            PureFun -> panic "compileCInstr" ["Called non-capturing from pure"]


        Capture ->
          doCall f $
            map compileE es ++
            [ [| \s -> $(doJump [] [ [| s |] ] no) |]

            , [| \val inp s ->
                    $(doJump [ [| val |], [| inp |] ] [[|s|]] yes) |]
            ] ++
            stateArgs

        Unknown -> panic "compileCInstr" ["Unknown call"]

    TailCall f how es ->
      case how of
        NoCapture ->
          case ?funTy of
            NonCapturingParser {} -> doCall f (map compileE es ++ stateArgs)
            CapturingParser _ _ noK yesK ->
              withThrErrorState \getS setS ->
              dToC (doCall f (map compileE es ++ [getS]))
                   (\s1 -> [| $noK $(setS s1) |])
                   (\a i s1 -> [| $yesK $a $i $(setS s1) |])
            PureFun -> panic "compileCInstr" ["Tail call parser from pure"]

        Capture ->
          doCall f $
            map compileE es ++
            case ?funTy of
              CapturingParser _ _ no yes -> [ no, yes ]
              _ -> panic "compileCInstr"
                            [ "Call to capturing from non-capturing" ]
            ++ stateArgs
        Unknown -> panic "compileCInstr" ["Unknown tail call"]

dToC :: HasConfig =>
  TH.ExpQ ->
  (TH.ExpQ -> TH.ExpQ) ->
  (TH.ExpQ -> TH.ExpQ -> TH.ExpQ -> TH.ExpQ) ->
  TH.ExpQ
dToC e no yes =
  case userMonad ?config of
    Nothing -> doCase e
    Just _ ->
      [| do v <- $e
            $(doCase [|v|])
      |]
  where
  doCase d =
    [| case $d of
         (Nothing,    s1) -> $(no                  [| s1 |])
         (Just (a,i), s1) -> $(yes [| a |] [| i |] [| s1 |])
    |]


labelName :: Label -> TH.Name
labelName (Label txt n) =
  TH.mkName ('l' : zEsc (Text.unpack txt) ++ "_" ++ show n)

doCall :: FName -> [TH.ExpQ] -> TH.ExpQ
doCall f es = TH.appsE (TH.varE (fnameName f) : es)

doJump :: ExprEnv => [TH.ExpQ] -> [TH.ExpQ] -> JumpPoint -> TH.ExpQ
doJump before after jp =
  TH.appsE ( TH.varE (labelName (jLabel jp))
           : before
          ++ map compileE (jArgs jp)
          ++ after
           )




