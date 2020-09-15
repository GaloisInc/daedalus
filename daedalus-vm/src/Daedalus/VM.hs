{-# Language OverloadedStrings #-}
module Daedalus.VM where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import Data.ByteString(ByteString)

import Daedalus.PP
import Daedalus.Rec

import qualified Daedalus.Core as Src


-- | A program
data Program = Program
  { pModules  :: [Module]

    -- XXX: we probably want to support more than one entry point.
  , pBoot     :: Map Label Block
  , pEntry    :: Label
  }

-- | A module
data Module = Module
  { mName     :: Src.MName
  , mImports  :: [Src.MName]
  , mTypes    :: [Rec Src.TDecl]
  , mFuns     :: [VMFun]
  }

-- | A function
data VMFun = VMFun
  { vmfName     :: Src.FName
  , vmfCaptures :: Captures
  , vmfPure     :: Bool     -- ^ True if this is not a parser
  , vmfEntry    :: Label
  , vmfBlocks   :: Map Label Block
  }

-- | A basic block
data Block = Block
  { blockName     :: Label
  , blockArgs     :: [BA]
  , blockLocalNum :: Int      -- ^ How many locals we define
  , blockInstrs   :: [Instr]
  , blockTerm     :: CInstr
  }

-- | Instructions
data Instr =
    SetInput E
  | Say String
  | Output E
  | Notify E          -- Let this thread know other alternative failed
  | CallPrim PrimName [E] BV
  | GetInput BV
  | Spawn JumpPoint BV
  | NoteFail


-- | Instructions that jump
data CInstr =
    Jump JumpPoint
  | JumpIf E JumpPoint JumpPoint
  | Yield
  | ReturnNo
  | ReturnYes E
  | Call Src.FName Captures JumpPoint JumpPoint [E]
  | TailCall Src.FName Captures [E]  -- ^ Used for both grammars and exprs
  | ReturnPure E            -- ^ Return from a pure function (no fail cont.)

-- | A flag to indicate if a function may capture the continuation.
-- If yes, then the function could return multiple times, and we need to
-- explicitly store the continuation closures.
-- It is always safe, but less efficient, to use 'Capture'
data Captures = Capture | NoCapture


-- | Target of a jump
data JumpPoint = JumpPoint Label [E]


-- | Constants, and acces to the VM state that does not change in a block.
data E =
    EUnit
  | ENum Integer Src.Type
  | EBool Bool
  | EByteArray ByteString
  | EMapEmpty Src.Type Src.Type
  | ENothing Src.Type

  | EBlockArg BA
  | EVar      BV


-- | Types of values in the VM
data VMT =
    TSem Src.Type
  | TThreadId
    deriving (Eq,Ord)




--------------------------------------------------------------------------------
-- Names

data Effect     = MayFail | DoesNotFail
  deriving (Eq,Ord,Show)

data Label      = Label Text Int deriving (Eq,Ord)

data BV        = BV Int VMT           deriving (Eq,Ord)
data BA        = BA Int VMT           deriving (Eq,Ord)

class HasType t where
  getType :: t -> VMT

instance HasType BV where getType (BV _ t) = t
instance HasType BA where getType (BA _ t) = t


data PrimName =
    StructCon Src.UserType
  | NewBuilder Src.Type
  | Op1 Src.Op1
  | Op2 Src.Op2   -- Without `And` and `Or`
  | Op3 Src.Op3   -- Without `PureIf`
  | OpN Src.OpN


--------------------------------------------------------------------------------


ppFun :: Doc -> [Doc] -> Doc
ppFun f ds = f <.> parens (hsep (punctuate comma ds))


instance PP Label where
  pp (Label f i) = "L_" <.> int i <.> "_" <.> pp f

instance PP Instr where
  pp instr =
    case instr of
      CallPrim f vs x  -> pp x <+> "=" <+> ppFun (pp f) (map pp vs)
      GetInput x       -> pp x <+> "=" <+> "input"
      Spawn c x        -> pp x <+> "=" <+> ppFun "spawn" [pp c]
      SetInput e       -> "input" <+> "=" <+> pp e
      Say x            -> ppFun "say" [text (show x)]
      Output v         -> ppFun "output" [ pp v ]
      Notify v         -> ppFun "notify" [ pp v ]
      NoteFail         -> ppFun "noteFail" []




instance PP CInstr where
  pp cintsr =
    case cintsr of
      Jump v        -> "jump" <+> pp v
      JumpIf b t e  -> "if" <+> pp b <+>
                          "then" <+> "jump" <+> pp t <+>
                          "else" <+> "jump" <+> pp e
      Yield         -> "yield"
      ReturnNo      -> ppFun "return_fail" []
      ReturnYes e   -> ppFun "return" [pp e]
      ReturnPure e  -> ppFun "return" [pp e]
      Call f c x y zs -> ppFun kw (pp f : pp x : pp y : map pp zs)
        where kw = case c of
                     Capture -> "call[save]"
                     NoCapture -> "call"
      TailCall f c xs -> ppFun kw (pp f : map pp xs)
        where kw = case c of
                     Capture -> "tail_call[save]"
                     NoCapture -> "tail_call"

instance PP Program where
  pp p =
    ".entry" <+> pp (pEntry p) $$
    "" $$
    vcat' (map pp (Map.elems (pBoot p))) $$
    "" $$
    vcat' (map pp (pModules p))

instance PP Module where
  pp m =
    vcat' [ "module" <+> pp (mName m) <+> "where"
          , vcat [ "import" <+> pp i | i <- mImports m ]
          , vcat' [ pp t | t <- mTypes m ]
          , vcat' [ pp f | f <- mFuns m ]
          ]

instance PP VMFun where
  pp f =
    (".function" <+> pp (vmfName f)) $$
    nest 2 (pp (vmfCaptures f)
        $$ (".entry" <+> pp (vmfEntry f))
        $$ blocks)
    where
    blocks = vcat' $ map pp $ Map.elems $ vmfBlocks f

instance PP Captures where
  pp c = case c of
          Capture   -> ".spawns"
          NoCapture -> empty

instance PP VMT where
  pp ty =
    case ty of
      TSem t    -> pp t
      TThreadId -> "thread_t"

instance PP E where
  pp val =
    case val of
      EVar v        -> pp v
      EBlockArg i   -> pp i

      EUnit         -> "unit"
      ENum i t      -> integer i <+> "@" <.> ppPrec 1 t
      EBool b       -> text (show b)
      EByteArray bs -> text (show bs)
      EMapEmpty k t -> "emptyMap" <+> "@" <.> ppPrec 1 k <+> "@" <.> ppPrec 1 t
      ENothing t    -> "nothing" <+> "@" <.> ppPrec 1 t


instance PP BV where
  pp (BV x _) = "r" <.> int x

instance PP BA where
  pp (BA x _) = "ra" <.> int x

instance PP Block where
  pp b = l <.> colon $$ nest 2
                          (vcat (map pp (blockInstrs b)) $$ pp (blockTerm b))
    where
    l = case blockArgs b of
          [] -> pp (blockName b)
          xs -> ppFun (pp (blockName b)) (map ppBinder xs)

instance PP JumpPoint where
  pp (JumpPoint l es) =
    case es of
      [] -> lab
      _  -> ppFun lab (map pp es)
    where
    lab = pp l

ppBinder :: (PP a, HasType a) => a -> Doc
ppBinder a = pp a <+> ":" <+> pp (getType a)

instance PP PrimName where
  pp pn =
    case pn of
      StructCon t -> "newStruct" <+> "@" <.> ppPrec 1 t
      NewBuilder t -> "newBuilder" <+> "@" <.> ppPrec 1 t
      Op1 op -> pp op
      Op2 op -> pp op
      Op3 op -> pp op
      OpN op -> pp op







