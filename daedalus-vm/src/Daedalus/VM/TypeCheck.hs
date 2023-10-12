{-# Language BlockArguments, ImplicitParams, ConstraintKinds #-}
{-# Language OverloadedStrings #-}
module Daedalus.VM.TypeCheck where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad(forM,zipWithM_)

import Daedalus.PP
import qualified Daedalus.Core as Core
import Daedalus.VM

type Error    = Doc
type M        = Either Error
type FunCtx   = (?funSigs :: Map FName [VMT])
type BlockCtx = (?blockSigs :: Map Label BlockSig)

problem :: Doc -> [Doc] -> M a
problem x xs = Left (x $$ nest 2 (vcat xs))

data BlockSig =
  BlockSig
    { blockFlavor   :: BlockType
    , blockArgTypes :: [VMT]
    }

blockSig :: Block -> BlockSig
blockSig b =
  BlockSig
    { blockFlavor   = blockType b
    , blockArgTypes = paramTys (blockArgs b)
    }

paramTys :: [BA] -> [VMT]
paramTys ps = [ ty | BA _ ty _ <- ps ]

funSig :: VMFun -> M [VMT]
funSig fu =
  case vmfDef fu of
    VMDef body    -> funBodySig (vmfName fu) body
    VMExtern args -> pure (paramTys args)

funBodySig :: FName -> VMFBody -> M [VMT]
funBodySig fu bo =
  case Map.lookup (vmfEntry bo) (vmfBlocks bo) of
    Just bl -> Right (blockArgTypes (blockSig bl))
    Nothing ->
      problem "Missing entry block."
         [ "Function:" <+> pp fu
         , "Block:" <+> pp (vmfEntry bo)
         ]

funSigs :: Program -> M (Map FName [VMT])
funSigs prog =
  Map.fromList <$>
  forM allFuns \fu ->
    do sig <- funSig fu
       pure (vmfName fu, sig)
  where
  allFuns = [ fu | m  <- pModules prog, fu <- mFuns m ]

--------------------------------------------------------------------------------

checkInstr :: BlockCtx => [Doc] -> Instr -> M ()
checkInstr loc instr =
  case instr of
    Say _    -> pure ()

    Output _ ->
      problem "Unexpected `Output` instruction." loc

    Notify e -> checkExpr loc e TThreadId

    CallPrim x p es ->
      checkPrim loc p es (getType x)

    Spawn x c ->
      do checkExpr loc (EVar x) TThreadId
         checkJump loc c []

    Let x e ->
      checkExpr loc e (getType x)

    Free {} -> pure ()

    NoteFail _ _ e1 e2 ->
      do checkExpr loc e1 (TSem Core.tByteArray)
         checkExpr loc e2 (TSem Core.tByteArray)

    PushDebug {} -> pure ()

    PopDebug  {} -> pure ()

checkCInstr :: (FunCtx,BlockCtx) => [Doc] -> CInstr -> M ()
checkCInstr loc cinstr =
  case cinstr of
    Jump jp -> checkJump loc jp []
{-
  | JumpIf E (JumpChoice Src.Pattern)
  | Yield
  | ReturnNo
  | ReturnYes E E   -- ^ Result, input
  | ReturnPure E    -- ^ Return from a pure function (no fail cont.)

  | CallPure Src.FName JumpWithFree [E]
    -- ^ The jump point contains information on where to continue after
    -- return and what we need to preserve acrross the call, and what we
    -- should free right after we return from the call.

  | CallNoCapture Src.FName (JumpChoice Bool) [E]
    -- ^ Call a parser that does not capture the continuation.

  | CallCapture Src.FName Closure Closure [E]
    -- ^ Call a function that captures the continuation.
    -- The closures are: no, yes
    -- This differs from NoCapture in that we may use *both* continuations.

  | TailCall Src.FName Captures [E]
    -- ^ Used for both grammars and exprs.
    -- This is basically the same as `Jump` just with the extra
    -- info that we are calling a function (e.g., for stack trace)
-}
    _ -> undefined

checkExpr :: [Doc] -> E -> VMT -> M ()
checkExpr loc e tyExpect
  | tyActual == tyExpect = pure ()
  | otherwise =
    problem "Type mismatch."
      $ loc ++
        [ "Expected:" <+> pp tyExpect
        , "Actual:" <+> pp tyActual
        ]
  where tyActual = getType e

checkExprs :: [Doc] -> [E] -> [VMT] -> M ()
checkExprs loc es ts
  | expectNum == actualNum = zipWithM_ checkArg [ 1 :: Int .. ] (zip es ts)
  | otherwise =
    problem "Arity mismatch"
      $ loc ++
      [ "Expected:" <+> pp expectNum
      , "Actual:" <+> pp actualNum
      ]
  where
  expectNum = length ts
  actualNum = length es
  checkArg n (e,t) = checkExpr (loc ++ ["Argument" <+> pp n]) e t

getBlockSig :: BlockCtx => [Doc] -> Label -> M BlockSig
getBlockSig loc l =
  case Map.lookup l ?blockSigs of
    Just sig -> pure sig
    Nothing  -> problem "Missing block." (loc ++ [ "Block:" <+> pp l])

checkJump :: BlockCtx => [Doc] -> JumpPoint -> [E] -> M ()
checkJump loc jp es =
  do sig <- getBlockSig loc (jLabel jp)
     checkExprs loc (es ++ jArgs jp) (blockArgTypes sig)

checkJumpWithFree :: BlockCtx => [Doc] -> JumpWithFree -> [E] -> M ()
checkJumpWithFree loc jwf = checkJump loc (jumpTarget jwf)

checkPrim :: [Doc] -> PrimName -> [E] -> VMT -> M ()
checkPrim = undefined
