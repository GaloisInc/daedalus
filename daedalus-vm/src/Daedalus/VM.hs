{-# Language OverloadedStrings #-}
module Daedalus.VM
  ( module Daedalus.VM
  , Src.Pattern(..)
  , Src.FName
  ) where

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
  { pModules    :: [Module]
  , pEntries    :: [Entry]
  }

-- XXX: maybe this needs a name also to be used for the external API
-- | An entry point to the program
data Entry = Entry
  { entryType   :: Src.Type          -- ^ type of value produced by parser
  , entryBoot   :: Map Label Block   -- ^ code specific to the entry point
  , entryLabel  :: Label             -- ^ start executing here
  , entryName   :: Text
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
  , vmfLoop     :: Bool     -- XXX we need to know the other loop members
                            -- for inlining
  , vmfDef      :: VMFDef   -- ^ Definition for the function, if any
  }

data VMFDef = VMExtern [BA]      -- ^ Primitive with these arguments
            | VMDef VMFBody      -- ^ Definition

data VMFBody = VMFBody
  { vmfEntry  :: Label
  , vmfBlocks :: Map Label Block
  }

-- | A basic block
data Block = Block
  { blockName     :: Label
  , blockType     :: BlockType
  , blockArgs     :: [BA]
  , blockLocalNum :: Int      -- ^ How many locals we define
  , blockInstrs   :: [Instr]
  , blockTerm     :: CInstr
  }

data BlockType =
    NormalBlock
  | ReturnBlock ReturnHow
  | ThreadBlock
    {- ^ This block is an entry point to a thread. -}
    deriving (Eq,Show)

data ReturnHow =
    RetPure             -- ^ pure function
  | RetYes Captures     -- ^ parser, success, is it a capturing parser
  | RetNo Captures      -- ^ parser, failure, is it a capturing parser
    deriving (Eq,Show)

-- | Instructions
data Instr =
    Say String
  | Output E
  | Notify E          -- Let this thread know other alternative failed
  | CallPrim BV PrimName [E]
  | Spawn BV Closure
  | NoteFail E

  | Let BV E
  | Free (Set VMVar)  -- ^ variable cannot be used for the rest of the block

-- | Instructions that jump
data CInstr =
    Jump JumpPoint
  | JumpIf E JumpChoice
  | Yield
  | ReturnNo
  | ReturnYes E E   -- ^ Result, input
  | ReturnPure E    -- ^ Return from a pure function (no fail cont.)
  | CallPure Src.FName Closure [E]
    -- ^ The jump point contains information on where to continue after
    -- return and what we need to preserve acrross the call

  | Call Src.FName Captures Closure Closure [E]
    -- The JumpPoints contain information about the return addresses.

  | TailCall Src.FName Captures [E]
    -- ^ Used for both grammars and exprs.
    -- This is basically the same as `Jump` just with the extra
    -- info that we are calling a function (e.g., for stack trace)

-- | A flag to indicate if a function may capture the continuation.
-- If yes, then the function could return multiple times, and we need to
-- explicitly store the continuation closures.
-- It is always safe, but less efficient, to use 'Capture'
data Captures = Capture | NoCapture
  deriving (Eq,Show)


-- | Target of a jump
data JumpPoint = JumpPoint { jLabel :: Label, jArgs :: [E] }

type Closure = JumpPoint

-- | Before jumping to these targets we should deallocate the given
-- variables.  We could achieve the same with just normal jumps and
-- additional basic blocks, but this seems more straight forward
data JumpWithFree = JumpWithFree
  { freeFirst  :: Set VMVar   -- ^ Free these before jumping
  , jumpTarget :: JumpPoint
  }

jumpNoFree :: JumpPoint -> JumpWithFree
jumpNoFree tgt = JumpWithFree { freeFirst = Set.empty, jumpTarget = tgt }

-- | Two joint points, but we'll use exactly one of the two.
-- This matters for memory management.
data JumpChoice = JumpCase (Map Src.Pattern JumpWithFree)

-- | Constants, and acces to the VM state that does not change in a block.
data E =
    EUnit
  | ENum Integer Src.Type     -- ^ Only unboxed
  | EBool Bool
  | EFloat Double Src.Type

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
eIsVar :: E -> Maybe VMVar
eIsVar e =
  case e of
    EVar x -> Just (LocalVar x)
    EBlockArg x -> Just (ArgVar x)
    _ -> Nothing

eVar :: VMVar -> E
eVar var =
  case var of
    LocalVar x -> EVar x
    ArgVar x   -> EBlockArg x

iArgs :: Instr -> [E]
iArgs i =
  case i of
    Say {}            -> []
    Output e          -> [e]
    Notify e          -> [e]
    CallPrim _ _ es   -> es
    Spawn _ j         -> jArgs j
    NoteFail e        -> [e]

    Let _ e           -> [e]
    Free _            -> []       -- XXX: these could be just owned args

pAllBlocks :: Program -> [Block]
pAllBlocks p =
  [ b | ent <- pEntries p, b <- Map.elems (entryBoot ent) ] ++
  [ b | m <- pModules p, f <- mFuns m, VMDef d <- [vmfDef f]
      , b <- Map.elems (vmfBlocks d) ]

extraArgs :: BlockType -> Int
extraArgs b =
  case b of
    NormalBlock -> 0
    ThreadBlock -> 1    -- notified?
    ReturnBlock h ->
      case h of
        RetPure     -> 1    -- value
        RetNo _     -> 0
        RetYes _    -> 2    -- value, input


--------------------------------------------------------------------------------
-- Names

data Effect     = MayFail | DoesNotFail
  deriving (Eq,Ord,Show)

data Label      = Label Text Int deriving (Eq,Ord,Show)

data BV         = BV Int VMT            deriving (Eq,Ord)
data BA         = BA Int VMT Ownership  deriving (Eq,Ord)

data Ownership  = Owned | Borrowed | Unmanaged      deriving (Eq,Ord,Show)

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
      EFloat _ t      -> TSem t
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
           Unmanaged -> "Unmanaged"

instance PP Label where
  pp (Label f i) = "L_" <.> int i <.> "_" <.> pp f

instance PP Instr where
  pp instr =
    case instr of
      CallPrim x f vs  -> pp x <+> "=" <+> ppFun (pp f) (map pp vs)
      Spawn x c        -> pp x <+> "=" <+> ppFun "spawn" [pp c]
      Say x            -> ppFun "say" [text (show x)]
      Output v         -> ppFun "output" [ pp v ]
      Notify v         -> ppFun "notify" [ pp v ]
      NoteFail v       -> ppFun "noteFail" [pp v]
      Free x           -> "free" <+> commaSep (map pp (Set.toList x))
      Let x v          -> ppBinder x <+> "=" <+> "copy" <+> pp v

instance PP CInstr where
  pp cintsr =
    case cintsr of
      Jump v        -> "jump" <+> pp v
      JumpIf b (JumpCase ps) -> "case" <+> pp b <+> "of"
                                $$ nest 2 (vcat (map ppAlt (Map.toList ps)))
            where ppAlt (p,g) = pp p <+> "->" <+> pp g
      Yield         -> "yield"
      ReturnNo      -> ppFun "return_fail" []
      ReturnYes e i -> ppFun "return" [pp e, pp i]
      ReturnPure e  -> ppFun "return" [pp e]
      CallPure f l es -> ppFun (pp f) (map pp es) $$ nest 2 ("jump" <+> pp l)
      Call f c no yes es ->
        vcat [ ppFun (pp f) (map pp es)
             , nest 2 $ vcat [ pp c
                             , "ok:"   <+> pp yes
                             , "fail:" <+> pp no
                             ]
             ]
      TailCall f c xs -> ppFun (pp f) (map pp xs) <+> ".tail" <+> pp c

instance PP JumpWithFree where
  pp jf = ppF <+> pp (Jump (jumpTarget jf))
    where ppF = if Set.null (freeFirst jf)
                  then empty
                  else pp (Free (freeFirst jf)) <.> semi

instance PP Program where
  pp p = vcat' (map pp (pEntries p) ++ map pp (pModules p))

instance PP Entry where
  pp entry = vcat' [ ".parser" <+> pp (entryName entry)
                   , nest 2 (vcat' $ ".entry" <+> pp (entryLabel entry)
                                   :  map pp (Map.elems (entryBoot entry))
                            )
                   ]

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
    nest 2 (pp (vmfCaptures f) <+> (if vmfLoop f then ".loop" else empty)
        $$ case vmfDef f of
             VMExtern as -> ".extern" <+>
                  hsep [ parens (pp a <+> ":" <+> pp (getType a)) | a <- as ]
             VMDef d  -> ".entry" <+> pp (vmfEntry d) $$ blocks (vmfBlocks d))
    where
    blocks = vcat' . map pp . Map.elems


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
      EFloat f _    -> double f
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
                  Unmanaged -> "u"

instance PP BlockType where
  pp b =
    case b of
      NormalBlock   -> "/* normal block */"
      ThreadBlock   -> "/* thread */"
      ReturnBlock r -> pp r

instance PP ReturnHow where
  pp r =
    case r of
      RetPure       -> "/* return pure */"
      RetYes c      -> "/* return yes" <+> pp c <+> "*/"
      RetNo c       -> "/* return no" <+> pp c <+> "*/"

instance PP Block where
  pp b = l <.> colon <+> ty $$ nest 2
                          (vcat (map pp (blockInstrs b)) $$ pp (blockTerm b))
    where
    ty = pp (blockType b)
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







