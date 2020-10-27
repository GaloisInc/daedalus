{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Call where

import Daedalus.PP

import Daedalus.VM
import Daedalus.VM.TypeRep
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types



cReturnClass :: CIdent -> Label -> [VMT] -> CStmt
cReturnClass super l tys = cStmt $ vcat
  [ "struct" <+> thisTy <+> ": public" <+> super <.> ", public DDL::HasRefs {"
  , nest 2 $ vcat attribs
  , "public:"
  , nest 2 $ vcat methods
  , "}"
  ]
  where
  thisTy  = cReturnClassName l
  fields  = tys `zip` [ 0 .. ]

  attribs = [ cStmt (cType t <+> cField n) | (t,n) <- fields ]

  methods =
    [ cDefineCon
        thisTy
        ("void* code" : [ cType t <+> param n | (t,n) <- fields ])
        ( (super,"code") : [ (cField n, param n) | (_,n) <- fields ] )

    , cDefineFun "void" "freeMembers" []
         [ cStmt (cField n <.> cCall (cField n <.> ".free") [])
         | (t,n) <- fields, HasRefs <- [typeRep t]
         ]
    ]

  param n = "x" <.> int n


