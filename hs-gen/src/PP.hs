module PP where

import Data.Text qualified as Text
import Data.List(intersperse)
import Text.PrettyPrint

import AST

class PP a where
  ppPrec :: Int -> a -> Doc

pp :: PP a => a -> Doc
pp = ppPrec 0

changeStyle :: IdentT -> IdentT -> Doc -> Doc
changeStyle fromT toT
  | fromT == toT = id
  | otherwise =
    case toT of
      Prefix -> \x -> hcat [ "`", x, "`" ]
      Infix  -> parens

ppIdent :: IdentT -> Ident -> Doc
ppIdent tgt i@(Ident src _) = changeStyle src tgt (pp i)

ppName :: IdentT -> Name -> Doc
ppName tgt n@(Name _ (Ident src _)) = changeStyle src tgt (pp n)

wrapAfter :: Int -> Int -> Doc -> Doc
wrapAfter tgt cur
  | tgt > cur = parens
  | otherwise = id

ppApp :: Doc -> [Doc] -> Doc
ppApp f xs = hang f 2 (fsep xs)

ppTup :: [Doc] -> Doc
ppTup ds = parens (sep (punctuate comma ds))

vsepN :: Int -> [Doc] -> Doc
vsepN n ds = vcat [ vcat (replicate n " ") $$ d | d <- ds ]

instance PP Ident where
  ppPrec _ (Ident _ t) = text (Text.unpack t)

instance PP ModName where
  ppPrec _ (ModName qual i) =
    case map pp qual of
      [] -> pp i
      ds -> hcat (intersperse "." (ds ++ [pp i]))

instance PP Name where
  ppPrec _ (Name qual t) =
    case qual of
      Nothing -> pp t
      Just m  -> hcat [ pp m, ".", pp t ]

instance PP Param where
  ppPrec _ (Param x mbT) =
    case mbT of
      Nothing -> p
      Just t  -> parens (hang (p <+> "::") 2 (pp t))
    where
    p = ppIdent Prefix x

-- 0: no parens
-- 1: wrap function
-- 2: also wrap application
instance PP Type where
  ppPrec prec ty =
    case ty of
      TVar x -> ppIdent Prefix x

      TCon c ts ->
        case map (ppPrec 2) ts of
          [] -> ppName Prefix c
          ds -> wrapAfter 1 prec $
                ppApp (ppName Prefix c) ds

      TQual ps t ->
        case ps of
          [] -> ppPrec prec t
          _  -> hang (ppTup (map pp ps) <+> "=>") 2 (pp t)

      TFun t1 t2 ->
        wrapAfter 0 prec $
        sep [ ppPrec 1 t1 <+> "->", pp t2 ]

      TTuple ts -> ppTup (map pp ts)
        -- XXX: solo?

-- 0: wrap nothing
-- 1: wrap con pats
instance PP Pat where
  ppPrec prec pat =
    case pat of
      PVar x -> pp x
      PCon c ps ->
        wrapAfter 0 prec
        case map (ppPrec 1) ps of
          [] -> ppName Prefix c
          ds -> ppApp (ppName Prefix c) ds
      PWild -> "_"

instance PP Alt where
  ppPrec _ (Alt p e) = fsep [pp p, "->", pp e ]

-- 0: wrap nothing
-- 1: wrap op, let, case, if, do
-- 2: wrap app
instance PP Expr where
  ppPrec prec expr =
    case expr of
      Var x ts es ->
        case (ts,es) of
          ([],[]) -> fu
          _ ->
            wrapAfter 1 prec $
            ppApp fu (tys ++ map (ppPrec 2) es)
        where
        fu  = ppName Prefix x
        tys = [ hcat [ "@", ppPrec 2 t ] | t <- ts ]

      Op l op r ->
        wrapAfter 0 prec $
        hang (ppPrec 1 l <+> ppName Infix op) 2 (ppPrec 1 r)

      Let ds e ->
        wrapAfter 0 prec $
        vcat [ hang "let" 2 (vsepN 1 (map pp ds))
             , hsep [ "in", pp e ]
             ]

      Case e as ->
        wrapAfter 0 prec $
        vcat [ hsep [ "case", pp e, "of" ]
             , nest 2 (vcat (map pp as))
             ]

      If e1 e2 e3 ->
        wrapAfter 0 prec $
        hang ("if" <+> pp e1) 2 (sep [ "then" <+> pp e2, "else" <+> pp e3])

      Do ss e ->
        wrapAfter 0 prec $
        "do" <+> vcat [ vcat (map pp ss), pp e ]

      Parens e -> parens (pp e)


instance PP Stmt where
  ppPrec _ (Stmt mb e) =
    case mb of
      Nothing -> pp e
      Just p  -> hang (pp p <+> "<-") 2 (pp e)


instance PP Decl where
  ppPrec _ (Decl x tps t ps e) = sig $$ def
    where
    nm    = ppIdent Prefix x
    sig   = hang (nm <+> "::") 2
            case tps of
              [] -> pp t
              _  -> hang "forall" 2 (sep [ hcat [ fsep (map pp tps), "." ]
                                         , pp t
                                         ])
    def   = hang (nm <+> fsep (map pp ps) <+> "=") 2 (pp e)


instance PP TypeDecl where
  ppPrec _ (TypeDecl x ps cs) =
    hang (hsep [ kw, ppIdent Prefix x, fsep (map pp ps) ]) 2 ctrs
    where
    kw = case cs of
           [_] -> "newtype"
           _   -> "data"
    ctrs =
      case cs of
        [] -> empty
        _  -> sep (zipWith (<+>) ("=" : repeat "|") (map pp cs))

instance PP Con where
  ppPrec _ (Con x ts) =
    case map (ppPrec 2) ts of
      [] -> nm
      ds -> ppApp nm ds
    where
    nm = ppIdent Prefix x

instance PP TopDecl where
  ppPrec _ decl =
    case decl of
      DType d -> pp d
      DValue d -> pp d

instance PP Import where
  ppPrec _ (Import x mb) =
    hsep [ "import", pp x
         , case mb of
             Nothing -> empty
             Just a  -> "qualified" <+> "as" <+> pp a
         ]

instance PP Module where
  ppPrec _ (Module x es is ds) =
    vcat [ header, imps, decls ]
    where
    header = hang ("module" <+> pp x) 2 exps <+> "where"
    exps = case es of
             Nothing -> empty
             Just ex -> parens (sep (map pp ex))
    imps = case is of
             [] -> empty
             _  -> vcat (" " : map pp is)

    decls = case ds of
              [] -> empty
              _  -> vsepN 2 (map pp ds)


