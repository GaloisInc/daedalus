{-# Language BlockArguments, LambdaCase, ImportQualifiedPost, GeneralizedNewtypeDeriving, ParallelListComp #-}
module Daedalus.VM.Semantics where

import Daedalus.Core qualified as Src

import Control.Monad.Trans.Cont (Cont, cont, runCont)
import Control.Monad (foldM)
import Daedalus.Core.Semantics.Expr (evalOp1, evalOp2, evalOp3)
import Daedalus.Core.Semantics.Expr qualified as Src
import Daedalus.Panic ( panic )
import Daedalus.Rec ( forgetRecs )
import Daedalus.Value qualified as V
import Daedalus.VM
import Daedalus.PP (pp)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Debug.Trace (trace)
import GHC.Float (double2Float)

-----------------------------------------------------------------------

data Env = Env {
    envArgs :: IntMap V.Value,
    envVars :: IntMap V.Value
}

newEnv ::
  [BA]      {- ^ argument names  -} ->
  [V.Value] {- ^ argument values -} ->
  Env
newEnv bas vs = Env{
  envArgs = IntMap.fromList [(k,v) | BA k _ _ <- bas | v <- vs],
  envVars = IntMap.empty}

extendEnv :: BV -> V.Value -> Env -> Env
extendEnv (BV i _) v e =
  let m = IntMap.insert i v (envVars e)
  in m `seq` e{envVars = m}

envBA :: Env -> BA -> V.Value
envBA env (BA i _ _) = envArgs env IntMap.! i

envBV :: Env -> BV -> V.Value
envBV env (BV i _  ) = envVars env IntMap.! i

-----------------------------------------------------------------------

type ThreadId = Int

vThreadId :: ThreadId -> V.Value
vThreadId = V.VInteger . toInteger

fromThreadId :: V.Value -> ThreadId
fromThreadId = \case
  V.VInteger i  -> fromInteger i
  _             -> panic "fromThreadId" ["bad thread ID"]

-----------------------------------------------------------------------

-- | Observable effects of a single parser thread execution
data Result
  -- Outcomes
  = Success V.Value
  | Failure

  -- Concurrency
  | SpawnResult (Bool -> Result) (ThreadId -> Result)
  | NotifyResult ThreadId Result

  -- External interaction
  | Primitive Src.FName [V.Value] (Maybe (V.Value, V.Value) -> Result)

  -- Debugging support
  | SayResult String Result
  | PopResult Result
  | PushResult DebugCall Text Result
  | Note Src.ErrorSource String V.Value V.Value Result -- ^ input, message

newtype M a = M (Cont Result a)
  deriving (Functor, Applicative, Monad)

yield :: ((a -> Result) -> Result) -> M a
yield f = M (cont f)

yield_ :: (Result -> Result) -> M ()
yield_ f = M (cont \k -> f (k ()))

runM :: M a -> (a -> Result) -> Result
runM (M m) = runCont m

-----------------------------------------------------------------------

moduleToValues :: Module -> [V.Value] -> [V.Value]
moduleToValues m args = resultToValues (semModule m args)

resultToValues :: Result -> [V.Value]
resultToValues = resultToValues' IntSet.empty IntMap.empty 

resultToValues' :: IntSet -> IntMap (Bool -> Result) -> Result -> [V.Value]
resultToValues' notifies threads = \case
  NotifyResult n r      -> resultToValues' (IntSet.insert n notifies) threads r
  SpawnResult thread k  -> resultToValues' notifies threads' (k threadId)
    where
      threadId = IntMap.size threads
      threads' = IntMap.insert threadId thread threads

  Primitive fn arg _    -> panic "resultToValues" ["primitives not supported", show (pp fn), show (map pp arg)]

  SayResult s r         -> trace s (resultToValues' notifies threads r)
  PopResult r           -> resultToValues' notifies threads r
  PushResult _ _ r      -> resultToValues' notifies threads r
  Note _ _ _ _ r        -> resultToValues' notifies threads r
  
  Success va            -> va : resume
  Failure               -> resume
  where
    resume =
      case IntMap.maxViewWithKey threads of
        Nothing -> []
        Just ((tid,k),threads') ->
          resultToValues' notifies' threads' (k (IntSet.member tid notifies))
          where
            notifies' = IntSet.delete tid notifies

-----------------------------------------------------------------------
-- Semantics of VM ----------------------------------------------------
-----------------------------------------------------------------------

semModule :: Module -> [V.Value] -> Result
semModule m args = runM (semVMFBody tDecls fns entryBody args) k
  where
    tDecls = Map.fromList [(Src.tName t, t) | t <- forgetRecs (mTypes m)]
    fns = Map.fromList [(vmfName f, f) | f <- mFuns m]

    entryBody : _ = [body | f <- mFuns m, vmfIsEntry f, VMDef body <- [vmfDef f]]
                 ++ panic "semModule" ["no entry point"]

    k (Yes x _) = Success x
    k No        = Failure
    k (Pure x)  = Success x

-- | Result of running a single VM function
data VMFResult
  = Yes V.Value V.Value -- ^ parser function succeeded
  | Pure V.Value        -- ^ pure function result
  | No                  -- ^ parser failed

semVMFBody :: Map Src.TName Src.TDecl -> Map FName VMFun -> VMFBody -> [V.Value] -> M VMFResult
semVMFBody tDecls fns vmfBody fnArgs =
  yield \k -> runM (semVMFBodyK tDecls fns vmfBody fnArgs k) k

semVMFBodyK ::
  Map Src.TName Src.TDecl {- ^ type declarations     -} ->
  Map FName VMFun         {- ^ function declarations -} ->
  VMFBody                 {- ^ current function body -} ->
  [V.Value]               {- ^ function arguments    -} ->
  (VMFResult -> Result)   {- ^ function continuation -} ->
  M VMFResult
semVMFBodyK tDecls fns vmfBody fnArgs k = goto (vmfEntry vmfBody) fnArgs
  where
    goto :: Label -> [V.Value] -> M VMFResult
    goto label args = semBlock args (vmfBlocks vmfBody Map.! label)

    call :: FName -> [V.Value] -> M VMFResult
    call fn args =
      let f = fns Map.! fn in
      case vmfDef f of
        VMDef body -> semVMFBody tDecls fns body args
        VMExtern{} ->
         do r <- yield (Primitive fn args)
            pure case r of
              Nothing    -> No
              Just (x,y) -> Yes x y

    semJumpPoint :: Env -> JumpPoint -> [V.Value] -> M VMFResult
    semJumpPoint env jp xs =
      goto (jLabel jp) (xs <> (semE env <$> jArgs jp))

    semBlock :: [V.Value] -> Block -> M VMFResult
    semBlock args block =
     do env <- foldM semInstr (newEnv (blockArgs block) args) (blockInstrs block)
        semCInstr env (blockTerm block)
    
    semCInstr :: Env -> CInstr -> M VMFResult
    semCInstr env term =
      let next = semJumpPoint env in
      case term of
        -- Jumps
        Jump jp       -> next jp []
        JumpIf e jc   -> next (jumpTarget (patternMatch (semE env e) jc)) []

        -- Finished
        Yield         -> yield (const Failure)
        ReturnNo      -> pure No
        ReturnYes x y -> pure (Yes (semE env x) (semE env y))
        ReturnPure x  -> pure (Pure (semE env x))

        -- Calls
        TailCall fn _ es -> call fn (semE env <$> es)

        CallPure fn jp es ->
         do result <- call fn (semE env <$> es)
            case result of
              Pure va -> next jp [va]
              _       -> panic "semVMFBody" ["pure function returned impure result"]

        Call fn _ jpN jpY es ->
         do result <- call fn (semE env <$> es)
            case result of
              Yes x y -> next jpY [x,y]
              No      -> next jpN []
              Pure{}  -> panic "semVMFBody" ["parser returned pure result"]

    semInstr :: Env -> Instr -> M Env
    semInstr env = \case
      Say str -> env <$ yield_ (SayResult str)

      Output{} -> panic "semInstr" ["unexpected output instruction"]

      Notify e -> env <$ yield_ (NotifyResult (fromThreadId (semE env e)))

      CallPrim bv primName es ->
       do let r = semPrim tDecls primName [(semE env e, getType e) | e <- es]
          pure (extendEnv bv r env)

      Spawn bv c ->
       do r <- yield (SpawnResult \flag -> runM (semJumpPoint env c [V.VBool flag]) k)
          pure (extendEnv bv (vThreadId r) env)

      NoteFail errorSource text msg input ->
        env <$ yield_ (Note errorSource text (semE env msg) (semE env input))

      Let bv e -> pure (extendEnv bv (semE env e) env)

      -- ignored; we're relying on the Haskell GC
      Free{} -> pure env

      PushDebug debugCall text -> env <$ yield_ (PushResult debugCall text)

      PopDebug -> env <$ yield_ PopResult

patternMatch :: V.Value -> JumpChoice -> JumpWithFree
patternMatch v (JumpCase jc) =
  case Map.lookup (valueToPattern v) jc of
    Just jf -> jf
    Nothing -> Map.findWithDefault (panic "patternMatch" ["incomplete patterns"]) PAny jc

valueToPattern :: V.Value -> Pattern
valueToPattern = \case
  V.VBool b        -> PBool b
  V.VMaybe Nothing -> PNothing
  V.VMaybe Just{}  -> PJust
  V.VInteger i     -> PNum i
  V.VUnionElem l _ -> PCon l
  V.VBDUnion bdu rep
    | l:_ <- [l | l <- V.bduCases bdu, V.bduMatches bdu l rep] -> PCon l
  _ -> PAny

semE :: Env -> E -> V.Value
semE env = \case
  ENum n (Src.TUInt (Src.TSize w)) -> V.vUInt  (fromInteger w) n
  ENum n (Src.TSInt (Src.TSize w)) -> V.vSInt' (fromInteger w) n
  ENum n Src.TFloat     -> V.vFloat (fromInteger n)
  ENum n Src.TDouble    -> V.vDouble (fromInteger n)
  ENum{}                -> panic "semE" ["number literal at unsupported type"]
  EBool b               -> V.VBool b
  EFloat x Src.TDouble  -> V.vDouble x
  EFloat x Src.TFloat   -> V.vFloat (double2Float x)
  EFloat{}              -> panic "semE" ["floating literal at unsupported type"]
  EMapEmpty{}           -> V.VMap Map.empty
  ENothing{}            -> V.VMaybe Nothing
  EBlockArg ba          -> envBA env ba
  EVar bv               -> envBV env bv
  EUnit                 -> V.vUnit

semPrim :: Map Src.TName Src.TDecl -> PrimName -> [(V.Value, VMT)] -> V.Value
semPrim tDecls prim vs =
  case (prim, vs) of
    (StructCon ut, _)               -> semStructCon tDecls ut vs
    (NewBuilder _ty, [])            -> V.vBuilder
    (Integer n, [])                 -> V.VInteger n
    (ByteArray bs, [])              -> V.vByteString bs
    (Op1 op1, [(x, TSem t)])        -> evalOp1 tDecls op1 t x
    (Op2 op2, [(x,_),(y,_)])        -> evalOp2 op2 x y
    (Op3 op3, [(x,_),(y,_),(z,_)])  -> evalOp3 op3 x y z
    (OpN Src.ArrayL{}, _)           -> V.vArray (map fst vs)
    (OpN Src.CallF{}, _)            -> panic "semPrim" ["calls not supported"]
    _                               -> panic "semPrim" ["argument mismatch", show (pp prim)]

semStructCon :: Map Src.TName Src.TDecl -> Src.UserType -> [(V.Value, b)] -> V.Value
semStructCon tDecls ut vs =
  case Src.tDef (tDecls Map.! Src.utName ut) of
    Src.TStruct fields -> V.VStruct [(l,v) | (l,_) <- fields | (v,_) <- vs]
    Src.TBitdata{} | V.TVBDStruct bds <- Src.evalType tDecls (Src.TUser ut) ->
      V.VBDStruct bds (V.bdStruct bds [(l,v) | l <- V.bdFields bds | (v,_) <- vs])
    _ -> panic "semStructCon" ["StructCon at unsupported type"]
