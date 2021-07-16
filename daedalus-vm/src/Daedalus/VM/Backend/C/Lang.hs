{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Lang where

import Text.PrettyPrint as P
import Daedalus.PP

type CExpr  = Doc
type CStmt  = Doc
type CType  = Doc
type CDecl  = Doc
type CIdent = Doc

cDebug :: String -> CStmt
cDebug x = cStmt (cCall "DDL::debug" [ cString x ] )

cDebugLine :: String -> CStmt
cDebugLine x = cStmt (cCall "DDL::debugLine" [ cString x ] )

cDebugNL :: CStmt
cDebugNL = cStmt (cCall "DDL::debugNL" [])

cDebugVal :: CExpr -> CStmt
cDebugVal x = cStmt (cCall "DDL::debugVal" [ x ])

cDebugValNL :: CExpr -> CStmt
cDebugValNL x = cStmt (cCall "DDL::debugValNL" [ x ])

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



cSwitch :: CExpr -> [CStmt] -> CStmt
cSwitch e cs =
  vcat [ "switch" <+> parens e <+> "{"
       , nest 2 (vcat cs)
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

cRetrun :: CExpr -> CStmt
cRetrun e = cStmt ("return" <+> e)

cBlock :: [CStmt] -> CStmt
cBlock xs = ("{" <+> vcat xs) $$ "}"

cDefineCon :: CIdent -> [CExpr] -> [(CIdent,CExpr)] -> [CStmt] -> CStmt
cDefineCon name params is stmts =
  hang (cCall name params) 2 initializers <+> "{" $$ nest 2 (vcat stmts) $$ "}"
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

cDeclareFun :: CType -> CIdent -> [CType] -> CDecl
cDeclareFun ty name params = cStmt decl
  where decl = ty <+> name P.<> parens (fsep (punctuate comma params))

cUnion :: CIdent -> [CDecl] -> CDecl
cUnion n as =
  cStmt ("union" <+> n <+> "{" $$ nest 2 (vcat as) $$ "}")

cNamespace :: CIdent -> [CDecl] -> CDecl
cNamespace nm d =
  "namespace" <+> nm <+> "{" $$ nest 2 (vcat d) $$ "}"

cUnreachable :: CStmt
cUnreachable = "__builtin_unreachable();"
