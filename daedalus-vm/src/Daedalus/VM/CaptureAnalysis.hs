{-# Language OverloadedStrings #-}
module Daedalus.VM.CaptureAnalysis where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)
import Data.List(foldl')

import Daedalus.Rec(topoOrder,Rec(..))
import Daedalus.PP

import Daedalus.Core(FName)
import Daedalus.VM


captureAnalysis :: [Module] -> [Module]
captureAnalysis ms = map annotateModule ms
  where
  info = fixCaptureInfo
       $ Map.fromList
         [ (vmfName f, captureInfo f) | m <- ms, f <- mFuns m ]

  getCaptures f = case Map.lookup f info of
                    Just (CapturesIf xs) | Set.null xs -> NoCapture
                    _ -> Capture  -- conservative

  annotateModule m = m { mFuns = map annotateFun (mFuns m) }
  annotateFun f = f { vmfCaptures = getCaptures (vmfName f) }

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
  captureInfo :: t -> CaptureInfo

instance GetCaptureInfo Instr where
  captureInfo i =
    case i of
      Spawn {} -> CapturesYes
      _        -> capturesNo

instance GetCaptureInfo CInstr where
  captureInfo i =
    case i of
      Call f _ _ _ -> CapturesIf (Set.singleton f)
      TailCall f _ -> CapturesIf (Set.singleton f)
      _            -> capturesNo

instance GetCaptureInfo Block where
  captureInfo b = captureInfo (blockTerm b) <>
                  foldMap captureInfo (blockInstrs b)

instance GetCaptureInfo VMFun where
  captureInfo b = foldMap captureInfo (vmfBlocks b)


