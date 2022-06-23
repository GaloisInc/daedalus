{-# Language TemplateHaskell, ConstraintKinds, ImplicitParams #-}
{-# Language RankNTypes, BlockArguments #-}
module Daedalus.VM.Backend.Haskell where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(maybeToList)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Lib as TH
import qualified Data.Text as Text
import Debug.Trace(trace)

import qualified RTS.ParserVM as RTS
import qualified RTS.ParserAPI as RTS

import Daedalus.Panic(panic)
import Daedalus.PP(pp)
import qualified Daedalus.Core as Core
import Daedalus.Core.TH.Names
import Daedalus.Core.TH.Type
import Daedalus.Core.TH.Ops
import Daedalus.VM

data Config = Config
  { userMonad      :: TH.TypeQ
  , userPrimitives :: Map FName ([TH.ExpQ] -> TH.ExpQ)
  }

-- | Make a function with the given list of arguments and result.
funT :: [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
funT args res = foldr addArg res args
  where
  addArg x y = [t| $x -> $y |]

type HasConfig = (?config :: Config)

userMonadT :: HasConfig => TH.TypeQ
userMonadT = userMonad ?config

codeT :: HasConfig => TH.TypeQ -> TH.TypeQ
codeT r = [t| RTS.ThreadState $r $userMonadT |]

-- | The type of a capturing parser
cParserT :: HasConfig => [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ -> TH.TypeQ
cParserT as b r = funT as [t| RTS.CParser $r $userMonadT $b |]

-- | The type of a block in a capturing parser
cParserBlockT :: HasConfig => [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
cParserBlockT as r = funT as (codeT r)

-- | The type of a non-capturing parser.
dParserT :: HasConfig => [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
dParserT as b = funT as [t| RTS.DParser $userMonadT $b |]


--------------------------------------------------------------------------------

type BlockEnv =
  ( ?blockArgs    :: Map BA TH.ExpQ     -- block arguments
  , ?localVars    :: Map BV TH.ExpQ     -- "copy" to be inlined
  , ?errorState   :: Maybe TH.ExpQ      -- the error state, for parsers
  )
  -- XXX: add thread state, for capturing parsers

compileE :: BlockEnv => E -> TH.ExpQ
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
        Just e -> e
        Nothing -> panic "compileE" ["Missing local variable", show (pp bv)]

compilePrim :: BlockEnv => PrimName -> [E] -> TH.ExpQ
compilePrim prim es =
  let args = map compileE es
  in
  case prim of

    StructCon ut ->
      [| $(TH.appsE (TH.conE (structConName (Core.utName ut)) : args))
         :: $(compileMonoType (Core.TUser ut)) |]

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
--------------------------------------------------------------------------------

getErrorState :: BlockEnv => TH.ExpQ
getErrorState =
  case ?errorState of
    Just e  -> e
    Nothing -> panic "getErrorState" ["Missing error state"]



updErrorState :: BlockEnv => (BlockEnv => a) -> (TH.ExpQ -> TH.ExpQ) -> a
updErrorState k f =
  let ?errorState = Just (f getErrorState)
  in k

compileInstr :: BlockEnv => Instr -> (BlockEnv => TH.ExpQ) -> TH.ExpQ
compileInstr i k =
  case i of
    Say x -> [| trace x $k |]

    CallPrim x p es ->
      [| let v = $(compilePrim p es)
         in $(let ?localVars = Map.insert x [| v |] ?localVars
              in k)
      |]

    Let x e -> let ?localVars = Map.insert x (compileE e) ?localVars
               in k

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

    Spawn {} -> undefined
    Output {} -> undefined
    Notify {} -> undefined

-- XXX: user monad
compileCInstrSimple :: BlockEnv => CInstr -> TH.ExpQ
compileCInstrSimple cinstr =
  case cinstr of
    ReturnNo          -> [| (Nothing, $getErrorState) |]
    ReturnYes a inp   -> [| ( Just ($(compileE a), $(compileE inp))
                            , $getErrorState
                            )
                          |]
    ReturnPure a      -> compileE a
    Jump jp           -> doJump stateArgs jp

    JumpIf e (JumpCase ch) -> TH.caseE (compileE e) alts
      where
      alts = map doAlt (Map.toList (Map.delete Core.PAny ch)) ++ dflt

      dflt  = case Map.lookup Core.PAny ch of
                Just r  -> [ doAlt (Core.PAny, r) ]
                Nothing -> []

      doAlt (p,rhs) =
        TH.match (doPat p) (TH.normalB (doJump stateArgs (jumpTarget rhs))) []

      -- XXX: bitdata
      doPat p =
        case p of
          Core.PBool b   -> TH.conP (if b then 'True else 'False) []
          Core.PNothing  -> TH.conP 'Nothing []
          Core.PJust     -> TH.conP 'Just [TH.wildP]
          Core.PNum i    -> TH.litP (TH.IntegerL i)
          Core.PBytes {} -> panic "compileCInstrSimple" ["PBytes"]
          Core.PCon l    -> flip TH.recP []
                            case getSemType e of
                              Core.TUser ut -> unionConName (Core.utName ut) l
                              _ -> panic "compileCInstrSimple" ["ConP not UserT"]
          Core.PAny      -> TH.wildP
 


  -- Yield

    -- force thunk?
    CallPure f jp es ->
      doJump
        (stateArgs ++ [ doCall f (map compileE es) ])
        jp

    -- XXX: if the call is to external primitive needs to be monadic
    Call f _cap no yes es ->
      [| case $(doCall f (stateArgs ++ map compileE es)) of
           (Nothing, s1)       -> $(doJump [ [|s1|] ] no)
           (Just (a,inp), s1)  -> $(doJump [ [|s1|], [|a|], [|inp|] ] yes)
      |]


    TailCall f _cap es -> doCall f (stateArgs ++ map compileE es)


labelName :: Label -> TH.Name
labelName (Label txt n) = TH.mkName ('l' : Text.unpack txt ++ "_" ++ show n)


stateArgs :: BlockEnv => [TH.ExpQ]
stateArgs = maybeToList ?errorState

doCall :: FName -> [TH.ExpQ] -> TH.ExpQ
doCall f es = TH.appsE (TH.varE (fnameName f) : es)

doJump :: BlockEnv => [TH.ExpQ] -> JumpPoint -> TH.ExpQ
doJump extra jp =
  TH.appsE ( TH.varE (labelName (jLabel jp))
           : extra
          ++ map compileE (jArgs jp)
           )




