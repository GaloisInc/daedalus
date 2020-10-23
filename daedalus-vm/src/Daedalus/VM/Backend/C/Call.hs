{-# Language OverloadedStrings #-}
module Daedalus.VM.Backend.C.Call where

import Daedalus.PP

import Daedalus.VM
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names



cReturnClass :: JumpPoint -> CStmt
cReturnClass j = cStmt $ vcat
  [ "struct" <+> thisTy <+> ": public DDL::StackFrame, public DDL::HasRefs {"
  , nest 2 $ vcat attribs
  , "public:"
  , nest 2 $ vcat methods
  , "}"
  ]
  where
  thisTy  = cReturnClassName j

  fields  = map getType (jArgs j) `zip` [ 0 .. ]

  attribs = [ cStmt (cType t <+> cField n) | (t,n) <- fields ]

  methods =
    [ allocFrame
    , copyFree "copy"
    , copyFree "free"
    ]

  param n = "x" <.> int n

  allocFrame =
    let boxTy  = cInst "DDL::Boxed" [ thisTy ]
        params = [ "void *code", "void *stack"] ++
                                          [ t <+> param n | (t,n) <- fields ]
    in
    "static" $$
    cDefineFun "void*" "allocate" params (
      [ cStmt (boxTy <+> "*frame" <+> "= new" <+> cCall boxTy [])
      , "frame->value.code = code;"
      , "frame->value.next = stack;"
      ] ++
      [ cStmt ("frame->value." <.> cField n <+> "=" <+> param n)
      | (_,n) <- fields ] ++
      [ "return frame;" ]
      )

  copyFree fun =
    cDefineFun "void" fun []
      [ cStmt (cField n <.> cCall ("." <.> fun) [])
      | (t,n) <- fields, HasRefs <- [typeRep t]
      ]



