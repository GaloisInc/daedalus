module Daedalus.VM.ThrowsAnalysis where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(foldl')

import Daedalus.Rec(Rec(..))
import Daedalus.Panic(panic)
import Daedalus.PP

import Daedalus.VM


throwsAnalysis :: Program -> Program
throwsAnalysis prog = Program { pModules = map annotateModule ms }
  where
  ms = pModules prog

  info =
    foldl' updateKnownGroup Map.empty
      [ fmap (\f -> (vmfName f, throwsInfo f)) group
      | m <- ms
      , group <- mFuns m
      ]

  annotateModule = mapModuleFuns annotateFun

  annotateFun f = f { vmfThrows = getThrows info (vmfName f) }


getThrows :: Map FName ThrowsInfo -> FName -> Throws
getThrows mp f =
  case Map.lookup f mp of
    Just ThrowsYes -> Throws
    Just (ThrowsIf fs)
      | Set.null fs -> NoThrows
      | otherwise ->
          panic "getThrows"
            [ "Unresolved throw dependencies for " ++ show (pp f)
            , show (map pp (Set.toList fs))
            ]
    Nothing ->
      panic "getThrows"
        [ "Missing throw information for " ++ show (pp f) ]

updateKnownGroup ::
  Map FName ThrowsInfo -> Rec (FName, ThrowsInfo) -> Map FName ThrowsInfo
updateKnownGroup known rec =
  case rec of
    NonRec (f,i) -> Map.insert f (updateKnown known i) known
    MutRec r ->
      let (fs,is) = unzip r
          this    = Set.fromList fs
          newI    = case mconcat (map (updateKnown known) is) of
                      ThrowsYes   -> ThrowsYes
                      ThrowsIf gs -> ThrowsIf (gs `Set.difference` this)
      in foldr (\f -> Map.insert f newI) known fs

updateKnown :: Map FName ThrowsInfo -> ThrowsInfo -> ThrowsInfo
updateKnown known i =
  case i of
    ThrowsYes   -> ThrowsYes
    ThrowsIf xs -> foldMap lkp xs
      where lkp f = case Map.lookup f known of
                      Nothing -> ThrowsIf (Set.singleton f)
                      Just k  -> k


--------------------------------------------------------------------------------
data ThrowsInfo = ThrowsYes
                | ThrowsIf (Set FName)
                  deriving Eq

throwsNo :: ThrowsInfo
throwsNo = ThrowsIf Set.empty

instance PP ThrowsInfo where
  pp c =
    case c of
      ThrowsYes   -> ".throws"
      ThrowsIf xs -> ".throws-if" <+> commaSep (map pp (Set.toList xs))

instance Semigroup ThrowsInfo where
  x <> y = case (x,y) of
            (ThrowsIf xs, ThrowsIf ys) -> ThrowsIf (Set.union xs ys)
            _                           -> ThrowsYes

instance Monoid ThrowsInfo where
  mempty = throwsNo


class GetThrowsInfo t where
  throwsInfo :: t -> ThrowsInfo

instance GetThrowsInfo CInstr where
  throwsInfo i =
    case i of
      Throw _ _ -> ThrowsYes
      TailCall f _ _   -> ThrowsIf (Set.singleton f)
      CallPure f _ _ _ -> ThrowsIf (Set.singleton f)
      CallNoCapture f _ _ _ -> ThrowsIf (Set.singleton f)
      CallCapture f _ _ _ _ -> ThrowsIf (Set.singleton f)
      _                -> throwsNo

instance GetThrowsInfo Block where
  throwsInfo b = throwsInfo (blockTerm b)

instance GetThrowsInfo VMFun where
  throwsInfo f = case vmfDef f of
                   VMExtern {} -> throwsNo
                   VMDef d     -> foldMap throwsInfo (vmfBlocks d)
