{-# Language OverloadedStrings, BlockArguments #-}
module Daedalus.VM.Backend.C.Bitdata (cBitdata,bdCase,bdCaseDflt) where

import Data.Maybe(mapMaybe)
import Data.Map(Map)
import Data.List(groupBy)

import Daedalus.PP
import qualified Daedalus.BDD as BDD

import Daedalus.Core
import Daedalus.Core.Type(bdUniverse)
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types


-- XXX: generate documentation
cBitdata :: TDecl -> BDD.Pat -> BitdataDef -> Doc
cBitdata ty univ def =
  cNamespace nsUser
    [ "class" <+> cTyName <+>
                    ": public" <+> cInst (nsDDL .:: "Bitdata") [w] <+> "{"
    , nest 2 (vcat' privateMethods)
    , "public:"
    , nest 2 (vcat' methods)
    , "};"
    ]

  where
  w       = int (BDD.width univ)
  rep     = uint w
  cTyName = cTNameRoot (tName ty)
  cty     = cTNameUse GenPublic (tName ty)

  privateMethods =
    [ bdCon ]

  methods =
      defaultCon
    : fromBits
    : case def of
        BDStruct fs -> defBDStructCon w fs : mapMaybe defBDSel fs
        BDUnion  fs -> map defBDUnionCon fs ++
                       map defBDUnionSel fs


  fromBits = "static"
          $$ cDefineFun cty "fromBits" [rep <+> "x"]
               [ cReturn (cCallCon (bdTy w) ["x"]) ]

  defaultCon = cDefineCon cTyName []                 [ (bdTy w, empty) ] []
  bdCon      = cDefineCon cTyName [ bdTy w <+> "x" ] [ (bdTy w, "x")   ] []


defBDUnionCon :: (Label,Type) -> CDecl
defBDUnionCon (l,t) =
  cDefineFun "void" (unionCon l) [ cSemType t <+> "x" ]
    [ cAssign "*this" (cCall "fromBits" [ cCallMethod "x" "toBits" []]) ]

defBDUnionSel :: (Label,Type) -> CDecl
defBDUnionSel (l,t) =
  cDefineFun (cSemType t) (selName GenOwn l) []
    [ cReturn (cCall (cSemType t .:: "fromBits") [ cCall "toBits" [] ]) ]


-- assumes fields are sorted
defBDStructCon :: CExpr -> [BDField] -> CDecl
defBDStructCon w fs = cDefineFun "void" structCon args
                      [ cAssign "*this" (cCall "fromBits" [expr]) ]
  where
  args = [ cSemType t <+> escText l | f <- fs, BDData l t <- [bdFieldType f] ]
  expr = case fs of
           [] -> cCallCon (uint w) []
           _  -> fst (foldl1 jn (map fToExpr fs `zip` map bdWidth fs))

  jn (a,m) (b,n) = (cCallCon (uint (int (m+n))) [a,b], m + n)

fToExpr :: BDField -> CExpr
fToExpr f =
  case bdFieldType f of
    BDWild     -> cCallCon (uint (int (bdWidth f))) []
    BDTag n    -> cCallCon (uint (int (bdWidth f))) [integer n] -- suff?
    BDData l _ -> cCallMethod (escText l) "toBits" []


defBDSel :: BDField -> Maybe CDecl
defBDSel f =
  case bdFieldType f of
    BDData l t ->
      let base    = cCall "toBits" []
          shifted = if bdOffset f == 0
                      then base
                      else base <+> ">>" <+> cCallCon (nsDDL .:: "Size")
                                                      [int (bdOffset f)]
          bits = cCallCon (uint (int (bdWidth f)))
                  [ cCallMethod shifted "rawRep" [] ]

      in Just (cDefineFun (cSemType t) (selName GenOwn l) []
                  [ cReturn (cCall (cSemType t .:: "fromBits") [bits]) ])

    _ -> Nothing


--------------------------------------------------------------------------------

-- XXX: This can duplicate RHSs if there are multiple checks for a pattern
bdCaseDflt ::
  Map TName TDecl -> BDD.Pat -> CExpr -> [ (Type,CStmt) ] -> CStmt -> CStmt
bdCaseDflt allTys univ e cases dflt =
    foldr doCase dflt
  $ map rearrange
  $ groupBy sameMask
  $ BDD.patTestsAssumingInOrder univ
    [ (bdUniverse allTys t, s) | (t,s) <- cases ]

  where
  val = cCallMethod (cCallMethod e "toBits" []) "rep" []

  maskedWith m
    | m == ((2 ^ BDD.width univ) - 1) = val
    | otherwise = val <+> "&" <+> integer m

  doCase (mask, alts) orElse
    | mask == 0 = snd (head alts)
    | otherwise =
      cSwitchDefault
        (maskedWith mask)
        [ cCase (integer v) rhs | (v,rhs) <- alts ]
        orElse

  rearrange xs = ( BDD.patMask (fst (head xs))
                 , [ (BDD.patValue t, rhs) | (t,rhs) <- xs ]
                 )

  sameMask (x,_) (y,_) = BDD.patMask x == BDD.patMask y

bdCase ::
  Map TName TDecl -> BDD.Pat -> CExpr -> [ (Type,CStmt) ] -> CStmt
bdCase allTys univ e cases = bdCaseDflt allTys univ e cases
                                        (cCall "assert" [ "false" ])

--------------------------------------------------------------------------------
uint :: CExpr -> CType
uint n = cInst (nsDDL .:: "UInt") [n]

bdTy :: CExpr -> CType
bdTy n = cInst (nsDDL .:: "Bitdata") [n]
