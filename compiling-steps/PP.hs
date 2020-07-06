{-# Language OverloadedStrings #-}
module PP where

import Text.PrettyPrint
import qualified Data.Map as Map
import Data.List(intersperse)

import AST(T(..))
import VM

class PP a where
  pp :: a -> Doc

(<.>) :: Doc -> Doc -> Doc
(<.>) = (Text.PrettyPrint.<>)

vcat' :: [Doc] -> Doc
vcat' = vcat . intersperse ""

ppFun :: Doc -> [Doc] -> Doc
ppFun f ds = f <.> parens (hsep (punctuate comma ds))


instance PP Label where
  pp (Label f i) = "L_" <.> int i <.> "_" <.> text f

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
      Call f x y zs -> ppFun "call" (pp f : pp x : pp y : map pp zs)
      TailCall f xs -> ppFun "tail_call" (pp f : map pp xs)

instance PP Program where
  pp p =
    ".entry" <+> pp (pEntry p) $$
    "" $$
    vcat' (map pp (Map.elems (pBoot p))) $$
    "" $$
    vcat' (map pp (Map.elems (pFuns p)))

instance PP VMFun where
  pp f =
    ".function" <+> pp (vmfName f) $$
    nest 2 (".entry" <+> pp (vmfEntry f) $$ blocks)
    where
    blocks = vcat' $ map pp $ Map.elems $ vmfBlocks f

instance PP T where
  pp ty =
    case ty of
      TChar  -> "char_t"
      TInt   -> "int_t"
      TBool  -> "bool_t"
      TUnit  -> "unit_t"
      TInput -> "input_t"

instance PP VMT where
  pp ty =
    case ty of
      TSem t    -> pp t
      TThreadId -> "thread_t"

instance PP E where
  pp val =
    case val of
      EUnit         -> "unit"
      EInt n        -> int n
      EChar c       -> text (show c)
      EVar v        -> pp v
      EBlockArg i   -> pp i


instance PP FunLab where
  pp (FL x _) = text (fst x)

instance PP PrimLab where
  pp (PL x) = text (fst x)

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
ppBinder a = pp (getType a) <+> pp a
