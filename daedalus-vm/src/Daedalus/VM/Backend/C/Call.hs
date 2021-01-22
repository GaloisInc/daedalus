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
  [ "class" <+> thisTy <+> ": public" <+> super <+> " {"
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
        [ "std::cout << \"  Allocated " <+> pp l <+> " at \" << (void*)this << std::endl;"
        ]

    , cDefineFun "void" "freeMembers" []
         $ ["std::cout << \"  Freeing members of" <+> (pp l) <+> "\" << (void*)this << std::endl;"]
         ++
         [ cStmt (cCallMethod (cField n) "free" [])
         | (t,n) <- fields, HasRefs <- [typeRep t]
         ] ++
          ["std::cout << \" Done freeing" <+> pp l <+> "\" << std::endl;"]
    ] ++
    [ cDefineFun "void" ("get" <.> cField n) [cRefT (cType ty) <+> "x"] $
      [ cStmt (cCallMethod (cField n) "copy" []) | typeRep ty == HasRefs ] ++
      [ cAssign "x" (cField n) ]
    | (n,ty) <- zip [0..] tys
    ]

  param n = "x" <.> int n


