{-# Language OverloadedStrings, BlockArguments #-}
module Daedalus.VM.Backend.C.UserDefined where

import qualified Data.Set as Set
import Data.List(partition)
import Data.Maybe(catMaybes)

import Daedalus.PP
import Daedalus.Rec

import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.VM.TypeRep
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types


cTypeGroup :: Rec TDecl -> Doc
cTypeGroup rec =
    "\n// --------------------------------------------------------- //\n"
    $$
    case rec of
      NonRec d ->
        case tDef d of
          TStruct {} -> vcat' [ cUnboxedProd GenPublic d
                              , generateMethods GenPublic GenUnboxed d
                              ]
          TUnion {}  -> vcat' [ cSumTags [d]
                              , cUnboxedSum GenPublic d
                              , generateMethods GenPublic GenUnboxed d
                              ]

      MutRec ds ->
        vcat' $
          -- 1. Declare names of types
          map (cTypeDecl GenPublic) (sums ++ prods) ++
          map (cTypeDecl GenPrivate) sums ++

          -- 2. Declare tags
          [ cSumTags sums ] ++

          -- 3. Declare boxed sums
          map cBoxedSum sums ++

          -- 4. Declare products
          map (cUnboxedProd GenPublic) prods ++

          -- 5. Declare unboxed sums
          map (cUnboxedSum GenPrivate) sums ++

          -- 6. Generate methods
          map (generateMethods GenPrivate GenUnboxed) sums ++
          map (generateMethods GenPublic GenUnboxed) prods ++
          map (generateMethods GenPublic GenBoxed) sums

        where
        (sums,prods) = orderRecGroup ds



{- Note: Product types shouldh't be directly recursive as that would
require infinite values, which we do not support.   There may be recursive
groups only containing products though: for example, if the recursion happens
through an array: a = [a]

A recursive sum type is represented with 3 types:
  A) an unboxed sum type describing the data
  B) a boxed pointer to A
  C) an enumeration type for the tags
All types in the group may depend on the B) part of the type.
Only the B) part of a type can depend on the A) part.


The types in a recursive group are declared in this order:
  1. Add declarations (without definitions) for all types in the group
    (withou enums)
  2. Declare enums
  3. Add class declarations (without methods) for the B) parts of sum types
  4. Add class declarations (without methods) for the struct types,
     in dependency order (without the sums these should not be recursive)
  5. Add class declarations (without methods) for the A) parts of sum types.
-}
orderRecGroup :: [TDecl] -> ([TDecl],[TDecl]) -- ^ (sums,products)
orderRecGroup tds = (sums, forgetRecs (topoOrder deps prods))
  where
  (sums,prods) = partition isSum tds
  isSum t = case tDef t of
              TUnion  {} -> True
              TStruct {} -> False

  sumSet  = Set.fromList (map tName sums)
  deps td = (tName td, freeTCons td `Set.difference` sumSet)




cTypeDecl :: GenVis -> TDecl -> CDecl
cTypeDecl vis ty = cTypeDecl' vis ty <.> semi

-- Note: this does not add the semi at the end, so we can reuse it in `Def`.
cTypeDecl' :: GenVis -> TDecl -> CDecl
cTypeDecl' vis ty =
  vcat [ cTemplateDecl ty, "class" <+> cTName' vis (tName ty) ]

cTemplateDecl :: TDecl -> Doc
cTemplateDecl ty =
  case cTypeParams ty of
    [] -> empty
    ps -> cInst "template" ps

cTypeParams :: TDecl -> [Doc]
cTypeParams ty =
  map intP (tTParamKNumber ty) ++ map tyP (tTParamKValue ty)
  where
  intP x = "int"      <+> cTParam x
  tyP x  = "typename" <+> cTParam x

-- Example: Node<T>
cTypeNameUse :: GenVis -> TDecl -> CType
cTypeNameUse vis tdecl =
  cTypeUse (cTName' vis (tName tdecl))
           (map cTParam (tTParamKNumber tdecl))
           (map cTParam (tTParamKValue tdecl))

--------------------------------------------------------------------------------
-- Signatures

-- | Signature for the @copy@ method
copyMethodSig :: Doc
copyMethodSig = cStmt ("void" <+> cCall "copy" [])

-- | Signature for the @free@ method
freeMethodSig :: Doc
freeMethodSig = cStmt ("void" <+> cCall "free" [])

eqMethodSig :: GenVis -> Doc -> TDecl -> CDecl
eqMethodSig vis op t = cStmt $ "bool" <+> cCall ("operator" <+> op) [name]
  where
  name = cTName' vis (tName t)

-- | Constructor for a product
cProdCtr :: TDecl -> CStmt
cProdCtr tdecl = cStmt ("void" <+> cCall "init" params)
  where params = [ cSemType t | (_,t) <- getFields tdecl, t /= TUnit ]

cProdSels :: TDecl -> [ CStmt ]
cProdSels tdecl =
  [ cStmt (cSemType t <+> cCall nm []) | (f,t) <- getFields tdecl
                                       , pref  <- [ "get", "borrow" ]
                                       , let nm = pref <.> "_" <.> cLabel f
                                       ]


-- | Signatures for getters of a sum
cSumGetters :: TDecl -> [CStmt]
cSumGetters tdecl =
  [ cStmt (cSemType t <+> cCall (pref <.> "_" <.> cLabel l) [])
  | (l,t) <- getFields tdecl
  , pref <- ["get","borrow"]
  ]

-- | Signatures for constructors of a sum
cSumCtrs :: TDecl -> [CStmt]
cSumCtrs tdecl =
  [ cStmt ("void" <+> cCall ("init_" <.> cLabel l)
                     [ cSemType ty | ty <- [t], t /= TUnit ])
  | (l,t) <- getFields tdecl
  ]





--------------------------------------------------------------------------------
-- Unboxed Products

-- | Interface definition for struct types
cUnboxedProd :: GenVis -> TDecl -> CDecl
cUnboxedProd vis ty =
  vcat
    [ cTypeDecl' vis ty <+> ": public DDL::HasRefs {"
    , nest 2 $ vcat attribs
    , "public:"
    , nest 2 $ vcat methods
    , "};"
    ]
  where
  TStruct fields = tDef ty
  attribs = [ cStmt (cSemType t <+> cField n)
            | ((_,t),n) <- zip fields [ 0 .. ], t /= TUnit
            ]
  methods =
       [ "// Construcotr" ]
    ++ [ cProdCtr ty ]
    ++ [ "// Selectors" ]
    ++ cProdSels ty
    ++ [ "// Memory Management" ]
    ++ [ copyMethodSig, freeMethodSig ]
    ++ [ eqMethodSig vis "==" ty, eqMethodSig vis "!=" ty ]



--------------------------------------------------------------------------------
-- Tags

-- | Declare a bunch of tag types
cSumTags :: [TDecl] -> CDecl
cSumTags sums
  | null sums = empty
  | otherwise = vcat
                  [ "namespace Tag {"
                  , nest 2 $ vcat $ map cSumTag sums
                  , "}"
                  ]

-- | The enum for a tag type
cSumTag :: TDecl -> CDecl
cSumTag ty =
  fsep $ [ "enum", cTName (tName ty), "{" ] ++
         punctuate comma (map (cLabel . fst) fields) ++
         [ "};" ]
  where
  TUnion fields = tDef ty

-- | The type of tags for the given type
cSumTagT :: TDecl -> CType
cSumTagT tdecl = "Tag::" <.> cTName (tName tdecl)

cSumTagV :: Label -> Doc
cSumTagV l = "Tag::" <.> cLabel l

-- | @getTag@ method signature
cSumGetTag :: TDecl -> Doc
cSumGetTag td = cStmt (cSumTagT td <+> cCall "getTag" [])
--------------------------------------------------------------------------------


-- | Class signature for an unboxed sum
cUnboxedSum :: GenVis -> TDecl -> CDecl
cUnboxedSum vis tdecl =
  vcat
    [ cTypeDecl' vis tdecl <+> ": DDL::HasRefs {"
    , nest 2 $ vcat attribs
    , "public:"
    , nest 2 $ vcat methods
    , "};"
    ]
  where
  fields = getFields tdecl

  attribs =
    [ cStmt $ cSumTagT tdecl <+> "tag" ] ++
    [ cStmt $ vcat
        [ "union Data {"
        , nest 2 $ vcat $ map cStmt [ cSemType t <+> cField n | (t,n) <- fs ]
        , nest 2 "Data() {}"
        , "} data"
        ]
    | let fs = [ (t,n) | ((_,t),n) <- zip fields [ 0 .. ], t /= TUnit ]
    , not (null fs)
    ]

  methods =
      [ "// Constructors" ]
   ++ cSumCtrs tdecl
   ++ [ "// Selectors" ]
   ++ [ cSumGetTag tdecl ]
   ++ cSumGetters tdecl
   ++ [ "// Memory Management" ]
   ++ [ copyMethodSig, freeMethodSig ]
   ++ [ eqMethodSig vis "==" tdecl, eqMethodSig vis "!=" tdecl ]


-- | Class signature for a boxed sum
cBoxedSum :: TDecl -> CDecl
cBoxedSum tdecl =
  vcat
    [ cTypeDecl' GenPublic tdecl <+> ": public DDL::HasRefs {"
    , nest 2 $ vcat attrs
    , "public:"
    , nest 2 $ vcat methods
    , "};"
    ]
  where
  attrs =
    [ cStmt  $ cInst "DDL::Boxed" [ cTypeNameUse GenPrivate tdecl ] <+> "ptr"
    ]

  methods =
       [ "// Constructors" ]
    ++ cSumCtrs tdecl
    ++ [ "// Selectors" ]
    ++ [ cSumGetTag tdecl ]
    ++ cSumGetters tdecl
    ++ [ "// Memory Management" ]
    ++ [ copyMethodSig, freeMethodSig ]
    ++ [ "// Comparisons" ]
    ++ [ eqMethodSig GenPublic "==" tdecl, eqMethodSig GenPublic "!=" tdecl ]


--------------------------------------------------------------------------------
-- Method definitions
--------------------------------------------------------------------------------

data GenBoxed = GenBoxed  | GenUnboxed
data GenOwn   = GenBorrow | GenOwn

generateMethods :: GenVis -> GenBoxed -> TDecl -> Doc
generateMethods vis boxed ty =
  vcat' $
    [ "// --- Methods for" <+> pp (tName ty) <+> "------------------"
    , defCopyFree vis boxed "copy" ty
    , defCopyFree vis boxed "free" ty
    ] ++
    defCons      vis boxed ty ++
    defSelectors vis boxed ty ++
    [defEq vis boxed ty, defNeq vis boxed ty]

defMethod :: GenVis -> TDecl -> CType -> Doc -> [Doc] -> [CStmt] -> CDecl
defMethod vis tdecl retT fun params def =
  vcat [ cTemplateDecl tdecl
       , "inline"
       , retT <+> cCall name params <+> "{"
       , nest 2 (vcat def)
       , "}"
       ]
  where
  name = cTypeNameUse vis tdecl <.> "::" <.> fun

--------------------------------------------------------------------------------
-- Equality

defNeq :: GenVis -> GenBoxed -> TDecl -> CDecl
defNeq vis _ tdecl =
  defMethod vis tdecl "bool" "operator !=" [ cTName' vis (tName tdecl) <+> "x" ]
    [ cStmt $ "return !operator ==(x)"
    ]

defEq :: GenVis -> GenBoxed -> TDecl -> CDecl
defEq vis boxed tdecl =
  defMethod vis tdecl "bool" "operator ==" [ cTName' vis (tName tdecl) <+> "x" ]
    case boxed of
      GenBoxed ->
        [ cStmt $ "return ptr.getValue() == x.ptr.getValue()"
        ]
      GenUnboxed ->
        case tDef tdecl of
          TStruct fs ->
            [ "if" <+> parens (f <+> "!=" <+> "x." <.> f) <+>
                                                        cStmt "return false"
            | ((_,t),n) <- fs `zip` [ 0 .. ]
            , let f = cField n
            , t /= TUnit
            ] ++
            [ cStmt "return true" ]
          TUnion fs ->
            [ "if (tag != x.tag) return false;"
            , "switch (tag) {"
            , nest 2 $ vcat
                         [ "case" <+> cSumTagV l <.> colon <+>
                            cStmt ("return" <+> f <+> "==" <+> "x." <.> f)
                         | ((l,t),n) <- fs `zip` [0..], t /= TUnit
                         , let f = "data." <.> cField n
                         ]
                   $$ "default: return true;"

            , "}"
            ]

--------------------------------------------------------------------------------
-- Construcotrs

defCons :: GenVis -> GenBoxed -> TDecl -> [CDecl]
defCons vis boxed tdecl =
  case tDef tdecl of
    TStruct {} -> [ defStructCon vis boxed tdecl ]
    TUnion  {} -> defUnionCons vis boxed tdecl


defStructCon :: GenVis -> GenBoxed -> TDecl -> CDecl
defStructCon vis boxed tdecl = defMethod vis tdecl "void" "init" params def
  where
  params = [ t <+> x | (t,x,_) <- fs ]
  def =
    case boxed of
      GenBoxed   -> [ cStmt $ cCall "ptr.allocate" []
                    , cStmt $ cCall "ptr.getValue().init" [ x | (_,x,_) <- fs ]
                    ]
      GenUnboxed -> [ cStmt (f <+> "=" <+> x) | (_,x,f) <- fs ]

  fs = [ (cSemType t, cLabel l, cField n)
       | ((l,t),n) <- getFields tdecl `zip` [ 0 .. ]
       , t /= TUnit
       ]

defUnionCons :: GenVis -> GenBoxed -> TDecl -> [CDecl]
defUnionCons vis boxed tdecl = zipWith defCon (getFields tdecl) [ 0 .. ]
  where
  defCon (l,t) n =
    let name = "init_" <.> cLabel l
        fs   = [ (cSemType t, cLabel l) | t /= TUnit ]
    in defMethod vis tdecl "void" name [ ty <+> x | (ty,x) <- fs ]
       case boxed of
         GenBoxed -> [ cStmt (cCall "ptr.allocate" [])
                     , cStmt (cCall ("ptr.getValue()." <.> name) (map snd fs))
                     ]
         GenUnboxed ->
            cStmt ("tag =" <+> cSumTagV l)
          : [ cStmt ("data." <.> cField n <+> "=" <+> cLabel l) | t /= TUnit ]


--------------------------------------------------------------------------------
-- Selectors


defSelectors :: GenVis -> GenBoxed -> TDecl -> [CDecl]
defSelectors vis boxed tdecl =
  concatMap (defSelectorsOwn vis boxed tdecl) [GenBorrow,GenOwn]

-- XXX: Maybe some selectors should return a reference?
defSelectorsOwn :: GenVis -> GenBoxed -> TDecl -> GenOwn -> [CDecl]
defSelectorsOwn vis boxed tdecl borrow = zipWith sel (getFields tdecl) [ 0 .. ]
  where
  pref = case borrow of
           GenBorrow -> "borrow"
           GenOwn    -> "get"
  uni  = case tDef tdecl of
           TStruct _ -> False
           TUnion  _ -> True
  sel (l,t) n =
    let name = pref <.> "_" <.> cLabel l
    in defMethod vis tdecl (cSemType t) name []
       case boxed of
         GenBoxed ->
           [ cStmt ("return" <+> cCall ("ptr.getValue()." <.> name) []) ]
         GenUnboxed ->
           [ s | GenOwn <- [borrow], Just s <- [ maybeCopyFree "copy" t f ] ] ++
           [ cStmt $ "return" <+>
                     case t of
                       TUnit -> "DDL::Unit()"
                       _     -> f
           ]
           where f = if uni then "data." <.> cField n else cField n

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Copy & Free

-- | Define a copy/free method for the given type
defCopyFree :: GenVis -> GenBoxed -> Doc -> TDecl -> CDecl
defCopyFree vis boxed fun tdecl = defMethod vis tdecl "void" fun [] def
  where
  def =
    case boxed of
      GenBoxed ->
        [ cStmt $ cCall ("ptr.getValue()." <.> fun) [] ]
      GenUnboxed ->
        case tDef tdecl of
          TStruct _ -> map snd (stmts True)
          TUnion _ ->
            case stmts False of
              [] -> []
              xs -> [ vcat [ "switch" <+> parens (cCall "getTag" []) <+> "{"
                           , nest 2 $ vcat' [ "case" <+> cSumTagV l <.> colon $$
                                               nest 2 (s $$ "break;")
                                            | (l,s) <- xs ] 
                                    $$ "default: break;"
                           , "}"
                           ]
                    ]

  stmts struct =
    let dat = if struct then empty else "data."
    in
    catMaybes
       [ do s <- maybeCopyFree fun t (dat <.> cField n)
            pure (l,s)

       | ((l,t),n) <- getFields tdecl `zip` [ 0.. ]
       ]


-- | Emit some code to copy/free a field
maybeCopyFree :: Doc -> Type -> CExpr -> Maybe CStmt
maybeCopyFree fun ty e =
  case ty of
    TParam {} ->
      Just $ vcat [ "if constexpr" <+> parens check <+> "{", nest 2 doIt , "}" ]
      where
      check = cInst "std::is_base_of" [ "DDL::HasRefs", cSemType ty ]
                                                                <.> "::value"

    _ -> case typeRep ty of
           HasRefs -> Just doIt
           NoRefs  -> Nothing

  where
  doIt = cStmt (e <.> "." <.> cCall fun [])



