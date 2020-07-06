module VM where

import Data.Map(Map)

import AST(F,T(..),Effect)


-- | A program
data Program = Program
  { pFuns   :: Map FunLab VMFun
  , pBoot   :: Map Label Block
  , pEntry  :: Label
  }

-- | A function
data VMFun = VMFun
  { vmfName   :: FunLab
  , vmfEntry  :: Label
  , vmfBlocks :: Map Label Block
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
  | Notify E
  | CallPrim PrimLab [E] BV
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
  | Call FunLab JumpPoint JumpPoint [E]
  | TailCall FunLab [E]



-- | Target of a jump
data JumpPoint = JumpPoint Label [E]


-- | Constants, and acces to the VM state that does not change in a block.
data E =
    EUnit
  | EInt   Int
  | EChar  Char

  | EBlockArg BA
  | EVar      BV


-- | Types of values in the VM
data VMT =
    TSem AST.T
  | TThreadId
    deriving (Eq,Ord,Show)




--------------------------------------------------------------------------------
-- Names


data FunLab     = FL F Effect          deriving (Eq,Ord,Show)
data Label      = Label String Int     deriving (Eq,Ord,Show)
newtype PrimLab = PL F                 deriving (Eq,Ord,Show)

data BV        = BV Int VMT            deriving (Eq,Ord,Show)
data BA        = BA Int VMT            deriving (Eq,Ord,Show)

class HasType t where
  getType :: t -> VMT

instance HasType BV where getType (BV _ t) = t
instance HasType BA where getType (BA _ t) = t
instance HasType PrimLab where getType (PL f) = TSem (snd f)









