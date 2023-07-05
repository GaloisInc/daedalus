{-# Language OverloadedStrings, BlockArguments #-}
module Daedalus.VM.CaptureAnalysis where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(foldl')

import Daedalus.Panic(panic)
import Daedalus.Rec(topoOrder,Rec(..))
import Daedalus.PP

import Daedalus.VM


captureAnalysis :: Program -> Program
captureAnalysis prog = Program { pModules = map annotateModule ms }
  where
  ms = pModules prog

  info    = fixCaptureInfo
          $ Map.fromList
              [ (vmfName f, captureInfo f) | m <- ms, f <- mFuns m ]


  -- NOTE: we assume that return blocks are not shared across functions with
  -- different capture state!

  -- capturing functions may call non-capturing ones, in which case
  -- the corresponding return block should be *non* capturing!
  findNoCaptureRet b =
    case blockTerm b of
      CallCapture f r1 r2 _
        | NoCapture <- getCaptures info f -> [ jLabel r1, jLabel r2 ]
      CallNoCapture {} -> panic "findNoCaptureRet" ["CallNoCapture"]
      _ -> []

  nonCapture = Set.fromList (concatMap findNoCaptureRet (pAllBlocks prog))
  retBlockCaptureInfo b =
    if blockName b `Set.member` nonCapture then NoCapture else Capture

  ---

  annotateModule m = m { mFuns = map annotateFun (mFuns m) }

  annotateFun f = f { vmfCaptures = me
                    , vmfDef = annotateDef me (vmfDef f)
                    }
    where me = getCaptures info (vmfName f)

  annotateDef me d =
    case d of
      VMExtern {} -> d
      VMDef b -> VMDef b { vmfBlocks = annotateBlock me <$> vmfBlocks b }

  annotateBlock me b =
    b { blockTerm = annotateTerm me (blockTerm b)
      , blockType = case blockType b of
                      NormalBlock -> NormalBlock
                      ThreadBlock -> ThreadBlock
                      ReturnBlock how ->
                        ReturnBlock
                          case how of
                            RetPure  -> RetPure
                            RetNo _  -> RetNo (retBlockCaptureInfo b)
                            RetYes _ -> RetYes (retBlockCaptureInfo b)
      }


  annotateTerm me i =
    case i of
      CallCapture f no yes es ->
        case callSanity f of
          Capture   -> CallCapture f no yes es
          NoCapture -> CallNoCapture f (JumpCase ks) es
            where ks = Map.fromList
                          [ (False, jumpNoFree no)
                          , (True,  jumpNoFree yes)
                          ]
          Unknown   -> panic "captureAnalysis" ["sanity: unknown"]
      TailCall f _ es     -> TailCall f (callSanity f) es
      _                   -> i
    where
    callSanity f =
      case getCaptures info f of
        Unknown -> panic "captureAnalysis" ["unknown"]
        Capture ->
          case me of
            Capture   -> Capture
            Unknown   -> panic "captureInfo" ["call unknown"]
            NoCapture -> panic "captureInfo" ["call capture from no capture"]
        NoCapture -> NoCapture



-- We only mark something as capturing if we have good reason to do so
-- (i.e., we found a concrete instruction that forces capture)
getCaptures :: Map FName CaptureInfo -> FName -> Captures
getCaptures mp f =
  case Map.lookup f mp of
    Just CapturesYes -> Capture
    _                -> NoCapture

-- | Compute a fix-point of the input map.
fixCaptureInfo :: Map FName CaptureInfo -> Map FName CaptureInfo
fixCaptureInfo = foldl' updateKnownGroup Map.empty
               . topoOrder deps
               . Map.toList
  where
  deps (f,i) = (f, case i of
                     CapturesYes   -> Set.empty
                     CapturesIf fs -> fs
               )

updateKnownGroup ::
  Map FName CaptureInfo -> Rec (FName, CaptureInfo) -> Map FName CaptureInfo
updateKnownGroup known rec =
  case rec of
    NonRec (f,i) -> Map.insert f (updateKnown known i) known
    MutRec r ->
      let (fs,is) = unzip r
          this    = Set.fromList fs
          newI    = case mconcat (map (updateKnown known) is) of
                      CapturesYes   -> CapturesYes
                      CapturesIf gs -> CapturesIf (gs `Set.difference` this)
      in foldr (\f -> Map.insert f newI) known fs

-- Update some currently known information based on new facts.
updateKnown :: Map FName CaptureInfo -> CaptureInfo -> CaptureInfo
updateKnown known i =
  case i of
    CapturesYes   -> CapturesYes
    CapturesIf xs -> foldMap lkp xs
      where lkp f = case Map.lookup f known of
                      Nothing -> CapturesIf (Set.singleton f)
                      Just k  -> k


--------------------------------------------------------------------------------
data CaptureInfo = CapturesYes            -- ^ Known to capture
                 | CapturesIf (Set FName) -- ^ Captures if any of these do
                   deriving Eq

capturesNo :: CaptureInfo
capturesNo = CapturesIf Set.empty

instance PP CaptureInfo where
  pp c =
    case c of
      CapturesYes   -> ".spawns"
      CapturesIf xs -> ".spawns-if" <+> commaSep (map pp (Set.toList xs))

instance Semigroup CaptureInfo where
  x <> y = case (x,y) of
            (CapturesIf xs, CapturesIf ys) -> CapturesIf (Set.union xs ys)
            _                              -> CapturesYes

instance Monoid CaptureInfo where
  mempty = capturesNo


class GetCaptureInfo t where
  captureInfo   :: t -> CaptureInfo

instance GetCaptureInfo Instr where
  captureInfo i =
    case i of
      Spawn {} -> CapturesYes
      _        -> capturesNo


instance GetCaptureInfo CInstr where
  captureInfo i =
    case i of
      CallCapture f _ _ _ -> CapturesIf (Set.singleton f)
      CallNoCapture {}    -> panic "captureInfo" ["CallNoCapture"]
      TailCall f c _      -> case c of
                               Unknown -> CapturesIf (Set.singleton f)
                               _       -> panic "captureInfo" ["Not unknown"]
      _                   -> capturesNo

instance GetCaptureInfo Block where
  captureInfo b = captureInfo (blockTerm b) <>
                  foldMap captureInfo (blockInstrs b)

instance GetCaptureInfo VMFun where
  captureInfo b = case vmfDef b of
                    VMExtern {} -> capturesNo
                    VMDef d     -> foldMap captureInfo (vmfBlocks d)
