{-# Language BlockArguments #-}
module Daedalus.VM.RefCountSane where

import Data.List(find)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad(when)

import Daedalus.PP(PP,showPP,pp,hsep)
import Daedalus.VM.BorrowAnalysis(modeI)
import Daedalus.VM.TypeRep
import Daedalus.VM

data RO       = RO { roBlocks :: Map Label Block
                   , roFuns   :: Map FName (Either [BA] Block)
                      -- ^ args of prims, or entry block for normal functions
                   }

type Error    = [String]
type Count    = Map VMVar VarCount
data VarCount = U | B | O Int deriving (Show,Eq)


checkProgram :: Program -> Maybe Error
checkProgram prog =
  case check of
    Right _  -> Nothing
    Left err -> Just err
  where
  check = mapM_ checkModule (pModules prog)

  checkModule = mapM_ checkVMFun . mFuns
  checkVMFun f = case vmfDef f of
                   VMDef b -> checkBlocks (vmfBlocks b)
                   VMExtern {} -> pure ()
  checkBlocks = mapM_ (checkBlock ro)

  ro = RO { roBlocks = allBlocks
          , roFuns = Map.fromList (map funInfo allFuns)
          }

  funInfo vmf    = ( vmfName vmf
                   , case vmfDef vmf of
                       VMExtern as -> Left as
                       VMDef d  -> Right (vmfBlocks d Map.! vmfEntry d)
                   )
  allFuns        = concatMap mFuns (pModules prog)
  allBlocks      = Map.fromList
                    [ (blockName b, b)
                    | b <- pAllBlocks prog
                    ] -- Map.unions (map entryBoot (pEntries prog) ++
                         --               map vmfBlocks allFuns)



checkBlock :: RO -> Block -> Either Error ()
checkBlock ro block =
  -- trace (unlines $ ("Checking " ++ name)
  --                : [ "  " ++ showPP x ++ ": " ++ show y | (x,y) <- Map.toList count ])
  do checkArgs
     fin <- checkTerm name ro (blockTerm block) =<<
                          checkIs name 1 ro (blockInstrs block) count
     mapM_ checkFin (Map.toList fin)
  where
  count = Map.fromList [ (ArgVar x, case getOwnership x of
                                      Owned     -> O 1
                                      Borrowed  -> B
                                      Unmanaged -> U
                         )
                       | x <- blockArgs block ]

  name = showPP (blockName block)

  checkArgs =
    case blockType block of
      NormalBlock -> pure ()
      b -> mapM_ (ownArg "malformed block")
                 $ drop (extraArgs b) $ blockArgs block

  ownArg loc a
    | typeRepOf a == HasRefs =
      when (getOwnership a == Borrowed)
        $ Left [name, loc, "Closure argument " ++ showPP a ++
                                                  " cannot be borrowed"]
    | otherwise = pure ()

  checkFin (x,c) =
    case c of
      O n | n /= 0 -> Left [name, "Variable " ++ showPP x ++ " was not freed."
                           , "(" ++ show n ++ ")" ]
      _ -> pure ()



newVar :: VMVar -> Count -> Count
newVar x = case typeRepOf x of
             NoRefs  -> id
             HasRefs -> Map.insert x (O 1)

freeVar :: VMVar -> Count -> Either Error Count
freeVar x count =
  case typeRepOf x of
    NoRefs  -> Left ["Free of non-ref var: " ++ showPP x]
    HasRefs ->
      case Map.lookup x count of
        Just (O n)
          | n > 0     -> pure (Map.insert x (O (n-1)) count)
          | otherwise -> Left ["Double free of: " ++ showPP x]
        Just B -> Left ["Free of borrowed variable: " ++ showPP x]
        Just U -> Left ["Free of unmanaged variable: " ++ showPP x]
        Nothing -> Left ["Missing count for " ++ showPP x]

freeVars :: Set VMVar -> Count -> Either Error Count
freeVars = go . Set.toList
  where
  go xs count =
    case xs of
      []      -> pure count
      x : xs' -> go xs' =<< freeVar x count

checkIs :: String -> Int -> RO -> [Instr] -> Count -> Either Error Count
checkIs loc n ro is count =
  case is of
    []       -> pure count
    i : more -> checkIs loc (n+1) ro more =<< checkI (loc ++ ":" ++ show n)
                                                    ro i count

checkI :: String -> RO -> Instr -> Count -> Either Error Count
checkI loc ro i count =
  let mode            = modeI i
      checkArgs args  = checkEs loc args (zipWith const mode args) count
      checkDef x args = newVar (LocalVar x) <$> checkArgs args
  in
  case i of
    Say _           -> checkArgs []
    PushDebug {}    -> checkArgs []
    PopDebug  {}    -> checkArgs []
    Output e        -> checkArgs [e]
    Notify e        -> checkArgs [e]
    CallPrim x _ es -> checkDef x es
    Spawn x l -> newVar (LocalVar x) <$> checkJP loc ro (==ThreadBlock) l count
    NoteFail _ _ ei em -> checkArgs [ei,em]

    Let x e ->
      case eIsVar e of
        Just y
          | typeRepOf y == HasRefs -> pure (newVar (LocalVar x) count)
          | otherwise -> Left ["Copy of non-ref: " ++ showPP y]
        Nothing -> Left ["Copy of non-var: " ++ showPP e]

    Free xs -> freeVars xs count

checkTerm :: String -> RO -> CInstr -> Count -> Either Error Count
checkTerm loc ro ci count =
  --trace (unlines $ ("Check term " ++ loc)
  --               : [ "  " ++ showPP x ++ ": " ++ show y | (x,y) <- Map.toList count ])
  case ci of
    Yield         -> pure count
    ReturnNo      -> pure count
    ReturnYes e i -> checkE "returnYesI" i Owned =<<
                     checkE "returnYesR" e Owned count
    ReturnPure e  -> checkE "returnPure" e Owned count

    Jump jp       -> checkJP loc ro (== NormalBlock) jp count
    JumpIf e ch   ->
      checkJumpChoice loc ro (== NormalBlock) ch =<<
                        checkE (loc ++ ":case") e (Borrowed `ifRefs` e) count
    CallPure f jp es -> checkJumpWithFree retLab ro isRet jp =<< checkArgs f es
    CallNoCapture f ks es ->
      checkJumpChoice retLab ro isRet ks =<< checkArgs f es
    CallCapture f jp1 jp2 es ->
      checkRet jp1 =<< checkRet jp2 =<< checkArgs f es
    TailCall f _ es -> checkArgs f es
  where
  isRet r = case r of
              ReturnBlock _ -> True
              _             -> False

  checkArgs f es =
    case Map.lookup f (roFuns ro) of
      Just inf ->
        case inf of
          Right bl
            | blockType bl == NormalBlock ->
                checkEs (loc ++ ", call to " ++ showPP f)
                         es
                        (map getOwnership (blockArgs bl)) count
            | otherwise -> Left ["Function entry is not normal: " ++ showPP f]
          Left as ->
                checkEs (loc ++ ", call to " ++ showPP f)
                         es
                         (map getOwnership as) count
      Nothing -> Left ["Missing function: " ++ showPP f]

  retLab = loc ++ ", return from call"
  checkRet = checkJP retLab ro isRet

checkEs :: String -> [E] -> [Ownership] -> Count -> Either Error Count
checkEs loc es0 os0 = go es0 os0
  where
  go es os count =
    case (es,os) of
      ([],[])            -> pure count
      (e : es', o : os') -> go es' os' =<< checkE loc e o count
      _                  -> Left [ loc ++ ": Arity mismatch"
                                 , "given: " ++ show (hsep (map pp es0))
                                 , "expected: " ++ show (hsep (map pp os0))
                                 ]


checkE :: String -> E -> Ownership -> Count -> Either Error Count
checkE loc expr mode count =
  case eIsVar expr of
    Nothing -> pure count
    Just x  ->
      case Map.lookup x count of
        Just U -> pure count
        Just (O n)
          | n > 0 ->
            case mode of
              Borrowed  -> Right count
              Owned     -> Right (Map.insert x (O (n-1)) count)
              Unmanaged -> Left [loc,"Unexpected Unmanaged"]
          | otherwise -> Left [loc,"Use of deallocated variable: " ++ showPP x]
        Just B ->
           case mode of
             Borrowed -> pure count
             Owned ->
               Left [loc,"Passing a borrowed variable as owned: " ++ showPP x]
             Unmanaged -> Left [loc,"Unexpected Unmanaged"]
        Nothing
          | typeRepOf x == NoRefs -> pure count
          | otherwise -> Left [loc,"Missing count for vairable: " ++ showPP x]

checkJP ::
  String -> RO -> (BlockType -> Bool) ->
  JumpPoint -> Count -> Either Error Count
checkJP loc ro typeOk jp count =
  case Map.lookup (jLabel jp) (roBlocks ro) of
    Just bl ->
      case blockType bl of
        t | not (typeOk t) -> Left ["Unexpected block type: " ++ showPP t]
          | otherwise -> check (extraArgs t)
      where
      sig     = map getOwnership (blockArgs bl)
      check n = checkEs loc (jArgs jp) (drop n sig) count

    Nothing -> Left ["Jump to missing block: " ++ showPP (jLabel jp)]

checkJumpWithFree ::
  String -> RO -> (BlockType -> Bool) ->
  JumpWithFree -> Count -> Either Error Count
checkJumpWithFree loc ro typeOk jf count =
  checkJP loc ro typeOk (jumpTarget jf) =<< freeVars (freeFirst jf) count


checkJumpChoice ::
  (PP i) => String -> RO -> (BlockType -> Bool) ->
  JumpChoice i -> Count -> Either Error Count
checkJumpChoice loc ro blockOk (JumpCase alts) count =
  do counts <- Map.traverseWithKey
                (\p jf -> let l' = loc ++ ", case " ++ showPP p
                          in checkJumpWithFree l' ro blockOk jf count) alts
     case Map.toList counts of
       [] -> pure count
       (p,m) : more
         | Just (q,m1) <- find ((/= m) . snd) more ->
           Left $ [loc, "Counts in branch " ++ showPP p ++ " and " ++
                                        showPP q ++ " differ."
                  ] ++
                    Map.elems
                    ( let sh x a b =
                            "variable " ++ showPP x ++ " " ++ a ++ " vs. " ++ b
                      in Map.mergeWithKey
                      (\x a b -> if a == b
                                  then Nothing
                                  else Just (sh x (show a) (show b))
                      )
                      (Map.mapWithKey (\x a -> sh x (show a) "_"))
                      (Map.mapWithKey (\x a -> sh x "_" (show a)))
                      m m1
                    )
           -- XXX: report diff
         | otherwise -> pure m


