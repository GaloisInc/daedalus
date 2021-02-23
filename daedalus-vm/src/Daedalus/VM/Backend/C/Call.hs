{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Call where

import Daedalus.PP

import Daedalus.VM
import Daedalus.VM.TypeRep
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types



cClosureClass :: CIdent -> CIdent -> [VMT] -> CStmt
cClosureClass super thisTy tys = cStmt $ vcat
  [ "class" <+> thisTy <+> ": public" <+> super <+> " {"
  , nest 2 $ vcat attribs
  , "public:"
  , nest 2 $ vcat methods
  , "}"
  ]
  where
  fields  = tys `zip` [ 0 .. ]

  attribs = [ cStmt (cType t <+> cField n) | (t,n) <- fields ]

  methods =
    [ cDefineCon
        thisTy
        ("void* code" : [ cType t <+> param n | (t,n) <- fields ])
        ( (super,"code") : [ (cField n, param n) | (_,n) <- fields ] )
        [ cDebugValNL "(void*)this"
        ]

    , cDefineFun "void" "freeMembers" []
         $ [cDebugValNL "(void*)this"]++
         [ cStmt (cCallMethod (cField n) "free" [])
         | (t,n) <- fields, HasRefs <- [typeRep t]
         ]
    ] ++
    [ cDefineFun "void" ("get" <.> cField n) [cRefT (cType ty) <+> "x"] $
      [ cStmt (cCallMethod (cField n) "copy" []) | typeRep ty == HasRefs ] ++
      [ cAssign "x" (cField n) ]
    | (n,ty) <- zip [0..] tys
    ]

  param n = "x" <.> int n


