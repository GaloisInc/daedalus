{-# Language BlockArguments #-}
module Run (runProgram, I, V(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List(foldl')
import Data.List.NonEmpty(NonEmpty(..))

import Value(I(..),primFuns,V(..))
import VM
import PP

import Debug.Trace



-- | Global machine state
data S = S
  { input     :: I                -- ^ Current input
  , bArgs     :: [VMV]            -- ^ Arguments to current block
  , bLocals   :: Map BV VMV       -- ^ Locals for the current block
  , stack     :: [CallFrame]      -- ^ Call stack
  , threads   :: [Thread]         -- ^ Suspended parsing paths.
  , results   :: [(V,I)]          -- ^ Successful results
  , errLoc    :: Maybe Int        -- ^ Location of error to report
  , curT      :: ThreadId         -- ^ Just for debug output
  , nextT     :: ThreadId         -- ^ For allocating thread ids
  } deriving Show



-- | A suspended function, waiting for call to return.
data CallFrame = CallFrame
  { cNo    :: Closure               -- ^ Return here on failure
  , cYes   :: Closure               -- ^ Return here on success
  } deriving Show

data Closure = Closure
  { cLabel :: Label
  , cFree  :: [VMV]
  } deriving Show


-- | A thread identifier
type ThreadId = Int

-- | A closure for suspended parsing paths.
data Thread = Thread
  { tStack    :: [CallFrame]        -- ^ Stack to use when restoring
  , tCode     :: Closure            -- ^ Location to resume from.
  , tInput    :: I                  -- ^ Input to resume with
  , tId       :: ThreadId           -- ^ This thread's ID
  , tFailF    :: Bool               -- ^ A bit of state for sync (see :||)
  } deriving Show


--------------------------------------------------------------------------------
-- Values

-- | Values known to the VM
data VMV = SemV V
         | TID ThreadId
           deriving (Show,Eq)

vToInput :: VMV -> I
vToInput v =
  case v of
    SemV (VInput x) -> x
    _ -> error "type error: expected an input"

vToBool :: VMV -> Bool
vToBool v =
  case v of
    SemV (VBool b) -> b
    _ -> error "type error: expected a bool"

vToTID :: VMV -> ThreadId
vToTID v =
  case v of
    TID t -> t
    _     -> error "type error: expected a thread id"
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Execution

data Code = Code
  { cLabels :: Map Label Block
  , cFuns   :: Map FunLab Label
  , cPrims  :: Map PrimLab ([VMV] -> VMV)
  }

type Exe = Code -> S -> S

execStmts :: [Instr] -> Exe
execStmts is = \p s -> foldl' (\s1 i -> execInstr i p s1) s is

execBlock :: Block -> [VMV] -> Exe
execBlock block vs =
  \p s ->
    let s0 = s { bArgs = vs, bLocals = Map.empty }
        s1 = execStmts (blockInstrs block) p s0
    in execCInstr (blockTerm block) p s1

doJump :: Label -> [VMV] -> Exe
doJump l vs =
  \c s ->
     case Map.lookup l (cLabels c) of
       Just b  -> execBlock b vs c s
       Nothing -> error ("Undefined label: " ++ show l)


--------------------------------------------------------------------------------
-- Instructions


evalE :: E -> S -> VMV
evalE expr =
  case expr of
    EUnit   -> \_ -> SemV VUnit
    EInt i  -> \_ -> SemV (VInt i)
    EChar c -> \_ -> SemV (VChar c)

    EVar bv -> \s ->
      case Map.lookup bv (bLocals s) of
        Just  v -> v
        Nothing -> error ("Undefined local: " ++ show (pp bv))

    EBlockArg ba@(BA x _) -> \s ->
      case splitAt x (bArgs s) of
        (_,a:_) -> a
        _       -> error ("Undefined argument: " ++ show (pp ba))

evalEs :: [E] -> S -> [VMV]
evalEs es s = [ evalE e s | e <- es ]


execInstr :: Instr -> Exe
execInstr instruction =
  case instruction of
    CallPrim f vs x   -> callPrim f vs x
    GetInput x        -> getInput x
    Spawn c x         -> spawn c x

    SetInput v        -> setInput v
    Say s             -> say s
    Output v          -> output v
    Notify v          -> notify v
    NoteFail          -> noteFail



noteFail :: Exe
noteFail = \_ s ->
  let x = loc (input s)
  in case errLoc s of
       Just y | y >= x -> s
       _ -> s { errLoc = Just x }

setLocal :: BV -> VMV -> S -> S
setLocal x v s = s { bLocals = Map.insert x v (bLocals s) }

callPrim :: PrimLab -> [E] -> BV -> Exe
callPrim f es x =
  \c s -> case Map.lookup f (cPrims c) of
            Just fv -> setLocal x (fv (evalEs es s)) s
            Nothing -> error ("Missing primitve: " ++ show f)

getInput :: BV -> Exe
getInput x =
  \_ s -> setLocal x (SemV (VInput (input s))) s

setInput :: E -> Exe
setInput e =
  \_ s -> s { input = vToInput (evalE e s) }

say :: String -> Exe
say x =
  \_ s ->
    let n   = 2 * length (stack s)
        t   = pad 5 ("(" ++ show (curT s) ++ ")")
        n2  = pad 2 (show n)
        pref = t ++ " " ++ n2 ++ " " ++ replicate n '.'
    in trace (pref ++ x) s

  where
  pad a xs = let n = length xs
             in replicate (a - n) ' ' ++ xs

output :: E -> Exe
output e =
  \_ s ->
    case evalE e s of
      SemV v -> s { results = (v,input s) : results s }
      _ -> error "output: not a sematic value"

spawn :: JumpPoint -> BV -> Exe
spawn j x =
  \_ s ->
    let t = Thread { tStack = stack s
                   , tCode  = closure j s
                   , tInput = input s
                   , tId    = i
                   , tFailF = False
                   }
        i = nextT s
    in setLocal x (TID i) s { threads = t : threads s
                            , nextT = i + 1
                            }

notify :: E -> Exe
notify e =
  \_ s ->
    let tid = vToTID (evalE e s)
        upd ts =
          case break ((tid ==) . tId) ts of
            (as, t : bs) -> as ++ t { tFailF = True } : bs
            _ -> error ("notify: unkown thread " ++ show tid)
    in s { threads = upd (threads s) }


--------------------------------------------------------------------------------
-- Control instructions

execCInstr :: CInstr -> Exe
execCInstr c =
  case c of
    Jump v        -> jump v
    JumpIf v t e  -> jumpIf v t e
    Yield         -> yield
    Call f n y vs -> call f n y vs
    TailCall f vs -> tailCall f vs
    ReturnNo      -> returnNo
    ReturnYes e   -> returnYes e


goto :: JumpPoint -> Exe
goto (JumpPoint l es) = \p s -> doJump l (evalEs es s) p s

-- | Unconditional jump
jump :: JumpPoint -> Exe
jump jp = goto jp

-- | Conditional jump
jumpIf :: E -> JumpPoint -> JumpPoint -> Exe
jumpIf e lThen lElse =
  \p s ->
    if vToBool (evalE e s) then goto lThen p s
                           else goto lElse p s

-- | Call a function
call :: FunLab -> JumpPoint -> JumpPoint -> [E] -> Exe
call fun n y es =
  \p s ->
      let s1 = s { stack = CallFrame { cYes   = closure y s
                                     , cNo    = closure n s
                                     }
                         : stack s
                 }
      in case Map.lookup fun (cFuns p) of
           Just l  -> doJump l (evalEs es s) p s1
           Nothing -> error ("Undefined function: " ++ show fun)


tailCall :: FunLab -> [E] -> Exe
tailCall fun es =
  \p s ->
    case Map.lookup fun (cFuns p) of
      Just l  -> doJump l (evalEs es s) p s
      Nothing -> error ("Undefined function: " ++ show fun)



returnNo :: Exe
returnNo =
  \p s ->
    case stack s of
      f : fs -> enter (cNo f) [] p s { stack = fs }
      []     -> error "popFrameNo: []"

returnYes :: E -> Exe
returnYes e =
  \p s ->
    case stack s of
      f : fs -> enter (cYes f) [evalE e s] p s { stack = fs }
      []     -> error "popFrameNo: []"


-- | Resume a suspended thread or stop if there are no others.
yield :: Exe
yield =
  \p s ->
    case threads s of
      t : ts ->
        enter (tCode t) [SemV (VBool (tFailF t))] p s { threads = ts
                                                      , input   = tInput t
                                                      , stack   = tStack t
                                                      , curT    = tId t
                                                      }
      [] -> s


closure :: JumpPoint -> S -> Closure
closure (JumpPoint l es) s = Closure { cLabel = l, cFree = evalEs es s }

enter :: Closure -> [VMV] -> Exe
enter c es = doJump (cLabel c) (es ++ cFree c)

--------------------------------------------------------------------------------


runProgram :: Program -> I -> Either Int (NonEmpty (V,I))
runProgram p x = case reverse (results finS) of
                   [] -> case errLoc finS of
                           Just e -> Left e
                           Nothing -> error "No error and no results."
                   x : xs -> Right (x :| xs)
  where
  finS = doJump (pEntry p) [] code initS

  prims = Map.fromList [ (PL f,liftPrim fv) | (f,fv) <- primFuns ]
  liftPrim f = \vs -> SemV (f [ v | SemV v <- vs ])

  code = Code { cPrims  = prims
              , cLabels = Map.unions ( pBoot p
                                     : map vmfBlocks (Map.elems (pFuns p))
                                     )
              , cFuns   = vmfEntry <$> pFuns p
              }

  initS = S { input    = x
            , bArgs    = []
            , bLocals  = Map.empty
            , stack    = []
            , threads  = []
            , results  = []
            , errLoc   = Nothing
            , curT     = 0
            , nextT    = 1
            }

