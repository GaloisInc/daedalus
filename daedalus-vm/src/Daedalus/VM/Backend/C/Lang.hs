{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Lang where

import Text.PrettyPrint as P
import Daedalus.PP

type CExpr  = Doc
type CStmt  = Doc
type CType  = Doc
type CDecl  = Doc
type CIdent = Doc

cInst :: CExpr -> [CExpr] -> CExpr
cInst f es = f P.<> "<" <.> (fsep (punctuate comma es)) <.> ">"

cCall :: CExpr -> [CExpr] -> CExpr
cCall f es = f P.<> parens (fsep (punctuate comma es))

cCallMethod :: CExpr -> CIdent -> [CExpr] -> CExpr
cCallMethod x f es = cCall (x <.> "." <.> f) es

cString :: String -> CExpr
cString = text . show

cSelect :: CExpr -> CExpr -> CExpr
cSelect x l = x <.> "." <.> l

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

cIf :: CExpr -> [CStmt] -> [CStmt] -> CStmt
cIf e ifThen ifElse =
  vcat [ "if" <+> parens e <+> "{"
       , nest 2 (vcat ifThen)
       , "} else {"
       , nest 2 (vcat ifElse)
       , "}"
       ]

cGoto :: CExpr -> CStmt
cGoto e = cStmt ("goto" <+> e)

cBlock :: [CStmt] -> CStmt
cBlock xs = ("{" <+> vcat xs) $$ "}"

cDefineCon :: CIdent -> [CExpr] -> [(CIdent,CExpr)] -> CStmt
cDefineCon name params is =
  hang (cCall name params) 2 (fsep [ initializers, "{}" ])
  where
  initializers = case is of
                   [] -> empty
                   _  -> ":" <+> fsep (punctuate comma (map doInit is))
  doInit (x,y) = x <.> parens y

cDefineFun :: CType -> CIdent -> [CExpr] -> [CStmt] -> CStmt
cDefineFun ty name params stmts =
  vcat [ ty <+> cCall name params <+> "{"
       , nest 2 (vcat stmts)
       , "}"
       ]

