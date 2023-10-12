{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Lang where

import Text.PrettyPrint as P
import Daedalus.PP

type CExpr  = Doc
type CStmt  = Doc
type CType  = Doc
type CDecl  = Doc
type CIdent = Doc

(.::) :: Doc -> Doc -> Doc
ns .:: a = ns <.> "::" <.> a

cInst :: CExpr -> [CExpr] -> CExpr
cInst f es = f P.<> "<" <.> (fsep (punctuate comma es)) <.> ">"

cCall :: CExpr -> [CExpr] -> CExpr
cCall f es = f P.<> parens (fsep (punctuate comma es))

cCallCon :: CExpr -> [CExpr] -> CExpr
cCallCon f es = f P.<> braces (fsep (punctuate comma es))

cCallMethod :: CExpr -> CIdent -> [CExpr] -> CExpr
cCallMethod x f es = cCall (x <.> "." <.> f) es

cString :: String -> CExpr
cString = text . show

cSelect :: CExpr -> CExpr -> CExpr
cSelect x l = x <.> "." <.> l

cArraySelect :: CExpr -> CExpr -> CExpr
cArraySelect x i = x <.> brackets i

cStmt :: Doc -> CStmt
cStmt c = c <.> semi

cRefT :: CType -> CType
cRefT t = t <.> "&"

cPtrT :: CType -> CType
cPtrT t = t <.> "*"



cAssign :: CExpr -> CExpr -> CStmt
cAssign x e = cStmt (x <+> "=" <+> e)

cDeclareVar :: CType -> CIdent -> CStmt
cDeclareVar ty x = cStmt (ty <+> x)

cDeclareInitVar :: CType -> CIdent -> CExpr -> CStmt
cDeclareInitVar ty x e = cStmt (ty <+> x <+> "=" <+> e)

cDeclareConVar :: CType -> CIdent -> [CExpr] -> CStmt
cDeclareConVar ty x es = cStmt (ty <+> cCallCon x es)

cIf :: CExpr -> [CStmt] -> [CStmt] -> CStmt
cIf e ifThen ifElse =
  vcat [ "if" <+> parens e <+> "{"
       , nest 2 (vcat ifThen)
       , "} else {"
       , nest 2 (vcat ifElse)
       , "}"
       ]

cIf' :: CExpr -> [CStmt] -> CStmt
cIf' e ifThen =
  vcat [ "if" <+> parens e <+> "{"
       , nest 2 (vcat ifThen)
       , "}"
       ]

cIfConst' :: CExpr -> [CStmt] -> CStmt
cIfConst' e ifThen =
  vcat [ "if" <+> "constexpr" <+> parens e <+> "{"
       , nest 2 (vcat ifThen)
       , "}"
       ]




cSwitch :: CExpr -> [CStmt] -> CStmt
cSwitch e cs =
  vcat [ "switch" <+> parens e <+> "{"
       , nest 2 (vcat cs)
       , "}"
       ]

cSwitchDefault :: CExpr -> [CStmt] -> CStmt -> CStmt
cSwitchDefault e cs def =
  vcat [ "switch" <+> parens e <+> "{"
       , nest 2 (vcat cs $$ cDefault def)
       , "}"
       ]



cCase :: CExpr -> CStmt -> Doc
cCase e s = "case" <+> e <.> colon $$ nest 2 s

cCaseBlock :: CExpr -> [CStmt] -> Doc
cCaseBlock e s = "case" <+> e <.> colon <+> "{" $$ nest 2 (vcat s) $$ "}"

cDefault :: CStmt -> CStmt
cDefault x = "default:" $$ nest 2 x


cBreak :: CStmt
cBreak = cStmt "break"

cGoto :: CExpr -> CStmt
cGoto e = cStmt ("goto" <+> e)

cReturn :: CExpr -> CStmt
cReturn e = cStmt ("return" <+> e)

cBlock :: [CStmt] -> CStmt
cBlock xs = vcat [ "{" <+> vcat xs, "}" ]

cArgBlock :: [Doc] -> Doc
cArgBlock as =
  case as of
    [] -> "()"
    _  -> vcat (zipWith (<+>) start as) $$ ")"
      where start = "(" : repeat ","

cDefineCon :: CIdent -> [CExpr] -> [(CIdent,CExpr)] -> [CStmt] -> CStmt
cDefineCon name params is stmts =
  hang (cCall name params) 2 initializers <+> body
  where
  initializers = case is of
                   [] -> empty
                   _  -> ":" <+> fsep (punctuate comma (map doInit is))
  doInit (x,y) = x <.> parens y
  body = case stmts of
          [] -> "{}"
          _  -> "{" $$ nest 2 (vcat stmts) $$ "}"

cDefineFun :: CType -> CIdent -> [CExpr] -> [CStmt] -> CStmt
cDefineFun ty name params stmts =
  vcat [ ty <+> cCall name params <+> "{"
       , nest 2 (vcat stmts)
       , "}"
       ]

cDeclareFun :: CType -> CIdent -> [CType] -> CDecl
cDeclareFun ty name params = cStmt decl
  where decl = ty <+> name P.<> parens (fsep (punctuate comma params))

cNamespace :: CIdent -> [CDecl] -> CDecl
cNamespace nm d =
  "namespace" <+> nm <+> "{" $$ nest 2 (vcat d) $$ "}"

cUsingT :: CIdent -> CType -> CDecl
cUsingT x t = cStmt ("using" <+> x <+> "=" <+> t)

cUnreachable :: CStmt
cUnreachable = "__builtin_unreachable();"

cTemplate :: [Doc] -> Doc -> Doc
cTemplate typeArgs body = cInst "template" typeArgs $$ body


cDebugMsg :: String -> Doc
cDebugMsg msg = "std::cout <<" <+> text (show msg) <+> "<< std::endl;"


