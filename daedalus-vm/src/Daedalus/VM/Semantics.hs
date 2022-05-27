{-# Language BlockArguments, LambdaCase, ImportQualifiedPost, GeneralizedNewtypeDeriving, ParallelListComp, ImplicitParams, ConstraintKinds #-}
module Daedalus.VM.Semantics where

import Daedalus.Core qualified as Src

import Control.Applicative ((<|>))
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

-- | Environment of block variables
data Env = Env {
    envArgs :: IntMap V.Value,
    envVars :: IntMap V.Value
}

-- | Constructs new block environment
newEnv ::
  [BA]      {- ^ argument names  -} ->
  [V.Value] {- ^ argument values -} ->
  Env
newEnv bas vs = Env{
  envArgs = IntMap.fromList [(k,v) | BA k _ _ <- bas | v <- vs],
  envVars = IntMap.empty}

-- | Extend block environment with a new binding
extendEnv :: BV -> V.Value -> Env -> Env
extendEnv (BV i _) v e =
  let m = IntMap.insert i v (envVars e)
  in m `seq` e{envVars = m}

-- | Lookup block argument
envBA :: Env -> BA -> V.Value
envBA env (BA i _ _) = envArgs env IntMap.! i

-- | Lookup block local variable
envBV :: Env -> BV -> V.Value
envBV env (BV i _  ) = envVars env IntMap.! i

-----------------------------------------------------------------------

-- Thread ID used to link spawn and notify commands
type ThreadId = Int

-- | Embed thread IDs into semantic values
vThreadId :: ThreadId -> V.Value
vThreadId = V.VInteger . toInteger

-- | Project thread ID from semantic value
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
  | External Src.FName [V.Value] (Maybe (V.Value, V.Value) -> Result) -- ^ continuation argument: result, input

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

abort :: Result -> M a
abort = M . cont . const

runM :: M a -> (a -> Result) -> Result
runM (M m) = runCont m

-----------------------------------------------------------------------

resultToValues :: Result -> [V.Value]
resultToValues = resultToValues' IntSet.empty IntMap.empty

resultToValues' :: IntSet -> IntMap (Bool -> Result) -> Result -> [V.Value]
resultToValues' notifies threads = \case
  NotifyResult n r      -> resultToValues' (IntSet.insert n notifies) threads r
  SpawnResult thread k  -> resultToValues' notifies threads' (k threadId)
    where
      threadId = IntMap.size threads
      threads' = IntMap.insert threadId thread threads

  External fn arg _     -> panic "resultToValues" ["primitives not supported", show (pp fn), show (map pp arg)]

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

type DeclEnv =
  (?tDecls :: Map Src.TName Src.TDecl,
   ?fDecls :: Map Src.FName VMFDef,
   ?lDecls :: Map Label Block)

semModule :: Module -> Map Src.FName ([V.Value] -> Result)
semModule m =
  let ?tDecls = Map.fromList [(Src.tName t, t) | t <- forgetRecs (mTypes m)]
      ?fDecls = Map.fromList [(vmfName f, vmfDef f) | f <- mFuns m]
      ?lDecls = Map.unions [vmfBlocks body | f <- mFuns m, VMDef body <- [vmfDef f]]
  in Map.fromList [
    (fn, \args -> runM (semVMFDef fn def args) k)
    | f <- mFuns m, vmfIsEntry f, let fn = vmfName f, def <- [vmfDef f]
  ]

  where
    k (Yes x _) = Success x
    k No        = Failure
    k (Pure x)  = Success x

-- | Result of running a single VM function
data FrameResult
  = Yes V.Value V.Value -- ^ parser function succeeded; result, input
  | Pure V.Value        -- ^ pure function finished; result
  | No                  -- ^ parser failed

semVMFDef :: DeclEnv => Src.FName -> VMFDef -> [V.Value] -> M FrameResult
semVMFDef fn def args =
  case def of
    VMDef body -> yield \k -> let ?kFrame = k in runM (semVMFBody body args) k
    VMExtern{} ->
     do r <- yield (External fn args)
        pure case r of
          Nothing    -> No
          Just (x,y) -> Yes x y

semFName :: DeclEnv => FName -> [V.Value] -> M FrameResult
semFName fn = semVMFDef fn (?fDecls Map.! fn)

type Frame = (DeclEnv, ?kFrame :: FrameResult -> Result)

semLabel :: Frame => Label -> [V.Value] -> M FrameResult
semLabel label = semBlock (?lDecls Map.! label)

semBlock :: Frame => Block -> [V.Value] -> M FrameResult
semBlock block args =
 do env <- foldM semInstr (newEnv (blockArgs block) args) (blockInstrs block)
    semCInstr env (blockTerm block)

semVMFBody :: Frame => VMFBody -> [V.Value] -> M FrameResult
semVMFBody = semLabel . vmfEntry

semJumpPoint :: Frame => Env -> JumpPoint -> [V.Value] -> M FrameResult
semJumpPoint env jp xs =
  semLabel (jLabel jp) (xs <> (semE env <$> jArgs jp))

semCInstr :: Frame => Env -> CInstr -> M FrameResult
semCInstr env term =
  let next = semJumpPoint env in
  case term of
    -- Jumps
    Jump jp       -> next jp []
    JumpIf e jc   -> next (jumpTarget (semJumpChoice jc (semE env e))) []

    -- Finished
    Yield         -> abort Failure
    ReturnNo      -> pure No
    ReturnYes x y -> pure (Yes (semE env x) (semE env y))
    ReturnPure x  -> pure (Pure (semE env x))

    -- Calls
    TailCall fn _ es -> semFName fn (semE env <$> es)

    CallPure fn jp es ->
     do result <- semFName fn (semE env <$> es)
        case result of
          Pure va -> next jp [va]
          _       -> panic "semCInstr" ["pure function returned impure result"]

    Call fn _ jpN jpY es ->
     do result <- semFName fn (semE env <$> es)
        case result of
          Yes x y -> next jpY [x,y]
          No      -> next jpN []
          Pure{}  -> panic "semCInstr" ["parser returned pure result"]

semInstr :: Frame => Env -> Instr -> M Env
semInstr env = \case
  Say str -> env <$ yield_ (SayResult str)

  Output{} -> panic "semInstr" ["unexpected output instruction"]

  Notify e -> env <$ yield_ (NotifyResult (fromThreadId (semE env e)))

  CallPrim bv primName es ->
   do let r = semPrimName primName [(semE env e, getType e) | e <- es]
      pure (extendEnv bv r env)

  Spawn bv c ->
   do r <- yield (SpawnResult \flag -> runM (semJumpPoint env c [V.VBool flag]) ?kFrame)
      pure (extendEnv bv (vThreadId r) env)

  NoteFail errorSource text msg input ->
    env <$ yield_ (Note errorSource text (semE env msg) (semE env input))

  Let bv e -> pure (extendEnv bv (semE env e) env)

  -- ignored; we're relying on the Haskell GC
  Free{} -> pure env

  PushDebug debugCall text -> env <$ yield_ (PushResult debugCall text)

  PopDebug -> env <$ yield_ PopResult

-----------------------------------------------------------------------
-- Patterns -----------------------------------------------------------
-----------------------------------------------------------------------

semJumpChoice :: JumpChoice -> V.Value -> JumpWithFree
semJumpChoice (JumpCase jc) v =
  case match <|> Map.lookup PAny jc of
    Just jf -> jf
    Nothing -> panic "semJumpChoice" ["incomplete patterns"]
  where
    match =
     do p <- valuePattern v
        Map.lookup p jc

valuePattern :: V.Value -> Maybe Pattern
valuePattern = \case
  V.VBool b        -> Just (PBool b)
  V.VMaybe Nothing -> Just PNothing
  V.VMaybe Just{}  -> Just PJust
  V.VInteger i     -> Just (PNum i)
  V.VUnionElem l _ -> Just (PCon l)
  V.VBDUnion bdu rep
    | l:_ <- [l | l <- V.bduCases bdu, V.bduMatches bdu l rep] -> Just (PCon l)
  _                -> Nothing

-----------------------------------------------------------------------
-- Expressions --------------------------------------------------------
-----------------------------------------------------------------------

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

semPrimName :: DeclEnv => PrimName -> [(V.Value, VMT)] -> V.Value
semPrimName prim vs =
  case (prim, vs) of
    (StructCon ut, _)               -> semStructCon ut vs
    (NewBuilder _ty, [])            -> V.vBuilder
    (Integer n, [])                 -> V.VInteger n
    (ByteArray bs, [])              -> V.vByteString bs
    (Op1 op1, [(x, TSem t)])        -> evalOp1 ?tDecls op1 t x
    (Op2 op2, [(x,_),(y,_)])        -> evalOp2 op2 x y
    (Op3 op3, [(x,_),(y,_),(z,_)])  -> evalOp3 op3 x y z
    (OpN Src.ArrayL{}, _)           -> V.vArray (map fst vs)
    (OpN Src.CallF{}, _)            -> panic "semPrimName" ["calls not supported"]
    _                               -> panic "semPrimName" ["argument mismatch", show (pp prim)]

semStructCon :: DeclEnv => Src.UserType -> [(V.Value, b)] -> V.Value
semStructCon ut vs =
  case Src.tDef (?tDecls Map.! Src.utName ut) of
    Src.TStruct fields -> V.VStruct [(l,v) | (l,_) <- fields | (v,_) <- vs]
    Src.TBitdata{} | V.TVBDStruct bds <- Src.evalType ?tDecls (Src.TUser ut) ->
      V.VBDStruct bds (V.bdStruct bds [(l,v) | l <- V.bdFields bds | (v,_) <- vs])
    _ -> panic "semStructCon" ["StructCon at unsupported type"]
