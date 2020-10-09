{-# Language OverloadedStrings #-}
module Daedalus.VM where

import Data.Set(Set)
import qualified Data.Set as Set
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
    -- XXX: we also want entry points with parameters.
  , pBoot     :: Map Label Block
  , pType     :: Src.Type     -- ^ type of value produced by parser
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
  | CallPrim BV PrimName [E]
  | GetInput BV
  | Spawn BV JumpPoint
  | NoteFail

  | Let BV E
  | Free (Set VMVar)  -- ^ variable cannot be used for the rest of the block

-- | Instructions that jump
data CInstr =
    Jump JumpPoint
  | JumpIf E JumpPoint JumpPoint
  | Yield
  | ReturnNo
  | ReturnYes E
  | ReturnPure E    -- ^ Return from a pure function (no fail cont.)
  | Call Src.FName Captures (Maybe JumpPoint) JumpPoint [E]
    -- ^ In grammars we have 2 continuation (no,yes)
    -- In expressions we only have the yes

  | TailCall Src.FName Captures [E]  -- ^ Used for both grammars and exprs

-- | A flag to indicate if a function may capture the continuation.
-- If yes, then the function could return multiple times, and we need to
-- explicitly store the continuation closures.
-- It is always safe, but less efficient, to use 'Capture'
data Captures = Capture | NoCapture


-- | Target of a jump
data JumpPoint = JumpPoint { jLabel :: Label, jArgs :: [E] }


-- | Constants, and acces to the VM state that does not change in a block.
data E =
    EUnit
  | ENum Integer Src.Type     -- ^ Onlu unboxed
  | EBool Bool

  | EMapEmpty Src.Type Src.Type
  | ENothing Src.Type

  | EBlockArg BA
  | EVar      BV

data VMVar = ArgVar BA | LocalVar BV
  deriving (Eq,Ord)


-- | Types of values in the VM
data VMT =
    TSem Src.Type
  | TThreadId
    deriving (Eq,Ord)



--------------------------------------------------------------------------------
iArgs :: Instr -> [E]
iArgs i =
  case i of
    SetInput e        -> [e]
    Say {}            -> []
    Output e          -> [e]
    Notify e          -> [e]
    CallPrim _ _ es   -> es
    GetInput {}       -> []
    Spawn _ j         -> jArgs j
    NoteFail          -> []

    Let _ e           -> [e]
    Free _            -> []       -- XXX: these could be just owned args


--------------------------------------------------------------------------------
-- Names

data Effect     = MayFail | DoesNotFail
  deriving (Eq,Ord,Show)

data Label      = Label Text Int deriving (Eq,Ord)

data BV         = BV Int VMT            deriving (Eq,Ord)
data BA         = BA Int VMT Ownership  deriving (Eq,Ord)

data Ownership  = Owned | Borrowed      deriving (Eq,Ord)

class GetOwnership t where
  getOwnership :: t -> Ownership

instance GetOwnership BA where
  getOwnership (BA _ _ o) = o

instance GetOwnership BV where
  getOwnership (BV {}) = Owned    -- XXX: in the future maybe we can consider
                                  -- borrowed locals too?

instance GetOwnership VMVar where
  getOwnership v =
    case v of
      LocalVar x -> getOwnership x
      ArgVar x   -> getOwnership x

class HasType t where
  getType :: t -> VMT

instance HasType BV where getType (BV _ t) = t
instance HasType BA where getType (BA _ t _) = t
instance HasType VMVar where
  getType x =
    case x of
      LocalVar y -> getType y
      ArgVar y   -> getType y

instance HasType E where
  getType e =
    case e of
      EUnit           -> TSem Src.TUnit
      ENum _ t        -> TSem t
      EBool {}        -> TSem Src.TBool
      EMapEmpty t1 t2 -> TSem (Src.TMap t1 t2)
      ENothing t      -> TSem (Src.TMaybe t)

      EBlockArg x     -> getType x
      EVar a          -> getType a


data PrimName =
    StructCon Src.UserType
  | NewBuilder Src.Type
  | Integer Integer
  | ByteArray ByteString
  | Op1 Src.Op1
  | Op2 Src.Op2   -- Without `And` and `Or`
  | Op3 Src.Op3   -- Without `PureIf`
  | OpN Src.OpN   -- Without `CallF`


--------------------------------------------------------------------------------


ppFun :: Doc -> [Doc] -> Doc
ppFun f ds = f <.> parens (hsep (punctuate comma ds))


instance PP Ownership where
  pp m = case m of
           Owned    -> "Owned"
           Borrowed -> "Borrowed"

instance PP Label where
  pp (Label f i) = "L_" <.> int i <.> "_" <.> pp f

instance PP Instr where
  pp instr =
    case instr of
      CallPrim x f vs  -> pp x <+> "=" <+> ppFun (pp f) (map pp vs)
      GetInput x       -> pp x <+> "=" <+> "input"
      Spawn x c        -> pp x <+> "=" <+> ppFun "spawn" [pp c]
      SetInput e       -> "input" <+> "=" <+> pp e
      Say x            -> ppFun "say" [text (show x)]
      Output v         -> ppFun "output" [ pp v ]
      Notify v         -> ppFun "notify" [ pp v ]
      NoteFail         -> ppFun "noteFail" []
      Free x           -> "free" <+> commaSep (map pp (Set.toList x))
      Let x v          -> ppBinder x <+> "=" <+> "copy" <+> pp v

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
      Call f c x y zs -> ppFun kw (pp f : ppMb x : pp y : map pp zs)
        where kw = case c of
                     Capture -> "call[save]"
                     NoCapture -> "call"
              ppMb = maybe empty pp
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
      EMapEmpty k t -> "emptyMap" <+> "@" <.> ppPrec 1 k <+> "@" <.> ppPrec 1 t
      ENothing t    -> "nothing" <+> "@" <.> ppPrec 1 t

instance PP VMVar where
  pp v =
    case v of
      LocalVar x -> pp x
      ArgVar x   -> pp x

instance PP BV where
  pp (BV x _) = "r" <.> int x

instance PP BA where
  pp (BA x _ o) = "ra" <.> int x <.> own
    where own = case o of
                  Owned    -> "o"
                  Borrowed -> "b"

instance PP Block where
  pp b = l <.> colon $$ nest 2
                          (vcat (map pp (blockInstrs b)) $$ pp (blockTerm b))
    where
    l = case blockArgs b of
          [] -> pp (blockName b)
          xs -> ppFun (pp (blockName b)) (map ppArg xs)

    ppArg a = pp a <+> ":" <+> pp (getType a)

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
      ByteArray bs -> text (show bs)
      Integer n    -> ppFun "Integer" [ pp n ]
      Op1 op -> pp op
      Op2 op -> pp op
      Op3 op -> pp op
      OpN op -> pp op







