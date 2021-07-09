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


-- | Returns (declaration, methods)
cTypeGroup :: Rec TDecl -> (Doc,Doc)
cTypeGroup rec =
    case rec of
      NonRec d ->
        case tDef d of
          TStruct {} -> ( cUnboxedProd d
                        , generateMethods GenPublic GenUnboxed d
                        )
          TUnion {}  -> ( cSumTags [d] $$ cUnboxedSum GenPublic d
                        , generateMethods GenPublic GenUnboxed d
                        )

      MutRec ds ->
        ( vcat' $
          -- 1. Declare names of types
          map (cTypeDecl GenPublic) (sums ++ prods) ++
          map (cTypeDecl GenPrivate) sums ++

          -- 2. Declare tags
          [ cSumTags sums ] ++

          -- 3. Declare boxed sums
          map cBoxedSum sums ++

          -- 4. Declare products
          map cUnboxedProd prods ++

          -- 5. Declare unboxed sums
          map (cUnboxedSum GenPrivate) sums

        , vcat' $
          -- 6. Generate methods
          map (generateMethods GenPrivate GenUnboxed) sums ++
          map (generateMethods GenPublic GenUnboxed) prods ++
          map (generateMethods GenPublic GenBoxed) sums
        )

        where
        (sums,prods) = orderRecGroup ds



{- Note: Product types shouldn't be directly recursive as that would
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
    (without enums)
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


cUserNS :: GenVis -> [Doc] -> Doc
cUserNS vis = cNamespace cUserNameSpace
            . case vis of
                GenPublic -> id
                GenPrivate -> (:[]) . cNamespace cUserPrivateNameSpace

cTypeDecl :: GenVis -> TDecl -> CDecl
cTypeDecl vis ty = cUserNS vis [ cTypeDecl' ty <.> semi ]

-- Note: this does not add the semi at the end, so we can reuse it in `Def`.
-- Note: This does not add the namespace
cTypeDecl' :: TDecl -> CDecl
cTypeDecl' ty =
  vcat [ cTemplateDecl ty, "class" <+> cTNameRoot (tName ty) ]

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
  cTypeUse (cTNameUse vis (tName tdecl))
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

-- | Constructor for a product
cProdCtr :: TDecl -> CStmt
cProdCtr tdecl = cStmt ("void" <+> cCall structCon params)
  where params = [ cSemType t | (_,t) <- getFields tdecl, t /= TUnit ]

cProdSels :: TDecl -> [ CStmt ]
cProdSels tdecl =
  [ cStmt (cSemType t <+> cCall nm []) | (f,t) <- getFields tdecl
                                       , pref  <- [ GenBorrow, GenOwn ]
                                       , let nm = selName pref f
                                       ]


-- | Signatures for getters of a sum
cSumGetters :: TDecl -> [CStmt]
cSumGetters tdecl =
  [ cStmt (cSemType t <+> cCall (selName pref l) [])
  | (l,t) <- getFields tdecl
  , pref <- [GenBorrow,GenOwn]
  ]

-- | Signatures for constructors of a sum
cSumCtrs :: TDecl -> [CStmt]
cSumCtrs tdecl =
  [ cStmt ("void" <+> cCall (unionCon l)
                                    [ cSemType ty | ty <- [t], t /= TUnit ])
  | (l,t) <- getFields tdecl
  ]





--------------------------------------------------------------------------------
-- Unboxed Products

-- | Interface definition for struct types
cUnboxedProd :: TDecl -> CDecl
cUnboxedProd ty = vcat (theClass : decFunctions GenPublic ty)
  where
  theClass =
    cNamespace cUserNameSpace
      [ cTypeDecl' ty <+> ": public DDL::HasRefs {"
      , nest 2 $ vcat attribs
      , "public:"
      , nest 2 $ vcat methods
      , "};"
      ]

  TStruct fields = tDef ty
  attribs = [ cStmt (cSemType t <+> cField n)
            | ((_,t),n) <- zip fields [ 0 .. ], t /= TUnit
            ]
  methods =
       [ "/** @name Constructor */"
       , "///@{" ]
    ++ [ cProdCtr ty ]
    ++ [ "///@}"
       , ""
       , "/** @name Selectors */"
       , "///@{" ]
    ++ cProdSels ty
    ++ [ "///@}"
       , ""
       , "/** @name Memory Management */"
       , "///@{" ]
    ++ [ copyMethodSig, freeMethodSig ]
    ++ [ "///@}"]



--------------------------------------------------------------------------------
-- Tags

-- | Declare a bunch of tag types
cSumTags :: [TDecl] -> CDecl
cSumTags sums
  | null sums = empty
  | otherwise = cNamespace cUserNameSpace
                  [ cNamespace "Tag" (map cSumTag sums) ]

  where
  cSumTag ty =
    fsep $ [ "enum class", cTNameRoot (tName ty), "{" ] ++
           punctuate comma (map (cLabel . fst) fields) ++
           [ "};" ]
    where
    TUnion fields = tDef ty

-- | The type of tags for the given type
cSumTagT :: TDecl -> CType
cSumTagT tdecl = cUserNameSpace <.> "::Tag::" <.> cTNameRoot (tName tdecl)

cSumTagV :: TName -> Label -> Doc
cSumTagV t l =
  cUserNameSpace <.> "::Tag::" <.> cTNameRoot t <.> "::" <.> cLabel l

-- | @getTag@ method signature
cSumGetTag :: TDecl -> Doc
cSumGetTag td = cStmt (cSumTagT td <+> cCall "getTag" [])
--------------------------------------------------------------------------------


-- | Class signature for an unboxed sum
cUnboxedSum :: GenVis -> TDecl -> CDecl
cUnboxedSum vis tdecl = vcat (theClass : decFunctions vis tdecl)
  where
  theClass =
    cUserNS vis
      [ cTypeDecl' tdecl <+> ": DDL::HasRefs {"
      , nest 2 $ vcat attribs
      , "public:"
      , nest 2 $ vcat methods
      , "};"
      ]


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
       [ "/** @name Constructors */"
       , "///@{" ]
   ++ cSumCtrs tdecl
   ++ [ "///@}"
       , ""
      , "/** @name Selectors */"
      , "///@{" ]
   ++ [ cSumGetTag tdecl ]
   ++ cSumGetters tdecl
   ++ [ "///@}"
       , ""
      , "/* @name Memory Management */"
      , "///@{" ]
   ++ [ copyMethodSig, freeMethodSig ]


-- | Class signature for a boxed sum
cBoxedSum :: TDecl -> CDecl
cBoxedSum tdecl = vcat (theClass : decFunctions GenPublic tdecl)
  where
  theClass = cNamespace cUserNameSpace
               [ cTypeDecl' tdecl <+> ": public DDL::IsBoxed {"
               , nest 2 $ vcat attrs
               , "public:"
               , nest 2 $ vcat methods
               , "};"
               ]


  attrs =
    [ cStmt  $ cInst "DDL::Boxed" [ cTypeNameUse GenPrivate tdecl ] <+> "ptr"
    ]

  methods =
       [ "/** @name Constructors */"
       , "///@{" ]
    ++ cSumCtrs tdecl
    ++ [ "///@}"
       , ""
       , "/** @name Selectors */"
       , "///@{" ]
    ++ [ cSumGetTag tdecl ]
    ++ cSumGetters tdecl
    ++ [ "///@}"
       , ""
       , "/** @name Memory Management */"
       , "///@{" ]
    ++ [ copyMethodSig, freeMethodSig ]


--------------------------------------------------------------------------------
-- Method definitions
--------------------------------------------------------------------------------

data GenBoxed = GenBoxed  | GenUnboxed


decFunctions :: GenVis -> TDecl -> [CDecl]
decFunctions vis ty =
  [ decCompare vis ty
  , decCmpOp "==" vis ty
  , decCmpOp "!=" vis ty
  , decCmpOp "<" vis ty
  , decCmpOp ">" vis ty
  , decCmpOp "<=" vis ty
  , decCmpOp ">=" vis ty
  ] ++
  [ decShow vis ty
  , decShowJS vis ty
  ]



generateMethods :: GenVis -> GenBoxed -> TDecl -> Doc
generateMethods vis boxed ty =
  vcat' $
    [ "// --- Methods for" <+> pp (tName ty) <+> "------------------"
    , defCopyFree vis boxed "copy" ty
    , defCopyFree vis boxed "free" ty
    ] ++
    defCons      vis boxed ty ++
    defGetTag    vis boxed ty ++
    defSelectors vis boxed ty ++
    [ defCompare vis ty
    , defCmpOp "==" vis ty
    , defCmpOp "!=" vis ty
    , defCmpOp "<" vis ty
    , defCmpOp ">" vis ty
    , defCmpOp "<=" vis ty
    , defCmpOp ">=" vis ty
    ] ++
    [defShow vis ty, defShowJS vis ty]

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
-- Output

decShow :: GenVis -> TDecl -> CDecl
decShow vis tdecl =
  cUserNS vis
  [ cTemplateDecl tdecl
  , "inline"
  , cDeclareFun "std::ostream&" "operator <<" [ "std::ostream&", ty ]
  ]
  where
  ty = cTypeNameUse vis tdecl

-- XXX: for boxed things we could delegate, but that would require
-- friendship to access `ptr`
defShow :: GenVis -> TDecl -> CDecl
defShow vis tdecl =
  cUserNS vis
  [ cTemplateDecl tdecl
  , "inline"
  , cDefineFun "std::ostream&" "operator <<" [ "std::ostream& os", ty <+> "x" ]
    case tDef tdecl of

      TStruct fs ->
        [ cStmt ("os <<" <+> fld <+> "<<" <+> thing)
        | ((l,_),sepa) <- fs `zip` ("{" : repeat ",")
        , let thing = cCallMethod "x" (selName GenBorrow l) []
              fld   = cString (show (sepa <+> pp l <+> "= "))
        ] ++
        [ cStmt ("return os <<" <+> cString " }") ]

      TUnion fs ->
        [ cStmt ("os <<" <+> cString "{| ")
        , vcat [ "switch" <+> parens (cCallMethod "x" "getTag" []) <+> "{"
               , nest 2 $ vcat
                    [ "case" <+> cSumTagV (tName tdecl) l <.> colon <+>
                      vcat [ cStmt ("os <<" <+> lab <+> "<<" <+> val)
                           , cStmt "break"
                           ]
                    | (l,_) <- fs
                    , let lab = cString (show (pp l <+> "= "))
                          val = cCallMethod "x" (selName GenBorrow l) []
                    ]
               , "}"
               ]
        , cStmt ("return os <<" <+> cString " |}")
        ]
    ]
  where
  ty = cTypeNameUse vis tdecl


decShowJS :: GenVis -> TDecl -> CDecl
decShowJS vis tdecl = 
  cNamespace "DDL"
   [ cTemplateDecl tdecl, "inline",
      cDeclareFun "std::ostream&" "toJS" [ "std::ostream&", ty ]
   ]
  where
  ty = cTypeNameUse vis tdecl

-- | Show in ppshow friendly format
defShowJS :: GenVis -> TDecl -> CDecl
defShowJS vis tdecl =
  cNamespace "DDL"
    [ cTemplateDecl tdecl
    , "inline"
    ,  cDefineFun "std::ostream&" "toJS" [ "std::ostream& os", ty <+> "x" ]
       case tDef tdecl of

         -- assumes at least one field
         TStruct fs ->
           [ cStmt (cCall "DDL::toJS" ["os <<" <+> fld, thing])
           | ((l,_),sepa) <- fs `zip` ("{" : repeat ",")
           , let thing = cCallMethod "x" (selName GenBorrow l) []
                 fld   = cString (show (sepa <+> cString (show (pp l)) <.> ": "))
           ] ++
           [ cStmt ("return os <<" <+> cString " }") ]

         TUnion fs ->
           [ vcat [ "switch" <+> parens (cCallMethod "x" "getTag" []) <+> "{"
                  , nest 2 $ vcat
                       [ "case" <+> cSumTagV (tName tdecl) l <.> colon $$ nest 2 (
                         vcat [ cStmt ("os <<" <+>
                                         cString ("{ " ++ show lab ++ ": "))
                              , cStmt (cCall "DDL::toJS" [ "os", val ])
                              , cStmt "os << \"}\""
                              , cStmt "break"
                              ])
                       | (l,_) <- fs
                       , let lab = '$' : show (pp l)
                             val = cCallMethod "x" (selName GenBorrow l) []
                       ]
                  , "}"
                  ]
           , cStmt "return os"
           ]
      ]
  where
  ty = cTypeNameUse vis tdecl



--------------------------------------------------------------------------------
-- Comparisons

decCompare :: GenVis -> TDecl -> CDecl
decCompare vis tdecl =
  cNamespace "DDL"
    [ cTemplateDecl tdecl
    , cDeclareFun "static inline int" "compare" [ ty, ty ]
    ]
  where
  ty = cTypeNameUse vis tdecl

defCompare :: GenVis -> TDecl -> CDecl
defCompare vis tdecl =
  cNamespace "DDL"
    [ cTemplateDecl tdecl
    , cDefineFun "static inline int" "compare" [ ty <+> "x", ty <+> "y" ] body
    ]
  where
  ty = cTypeNameUse vis tdecl
  body =
    case tDef tdecl of
      TStruct fs ->
        concat
        [ [cDeclareVar "int" "result"]
        , [ s
          | (f,_) <- fs
          , let m = selName GenBorrow f
          , s <-
              [ cAssign "result" (cCall "DDL::compare"
                                    [ cCallMethod "x" m []
                                    , cCallMethod "y" m []
                                    ])
              , cIf' "result != 0" [cRetrun "result"]
              ]
          ]
        , [cRetrun "0"]
        ]
      TUnion fs ->
        [ cSwitch "x.getTag()"
            [ cCaseBlock (cSumTagV (tName tdecl) f)
              [ cDeclareInitVar "auto" "tag" "y.getTag()"
              , cIf' (cSumTagV (tName tdecl) f <+> "< tag") [ cRetrun "-1" ]
              , cIf' (cSumTagV (tName tdecl) f <+> "> tag") [ cRetrun "1" ]
              , let get a = cCallMethod a (selName GenBorrow f) []
                in cRetrun (cCall "DDL::compare" [ get "x", get "y" ])
              ]

            | (f,_) <- fs
            ]
        , cUnreachable
        ]

decCmpOp :: Doc -> GenVis -> TDecl -> CDecl
decCmpOp op vis tdecl =
  cUserNS vis
  [ cTemplateDecl tdecl
  , cDeclareFun "static inline bool" ("operator" <+> op)
                      [ ty <+> "x", ty <+> "y" ]
  ]
  where ty = cTypeNameUse vis tdecl

defCmpOp :: Doc -> GenVis -> TDecl -> CStmt
defCmpOp op vis tdecl =
  cUserNS vis
  [ cTemplateDecl tdecl
  , cDefineFun "static inline bool" ("operator" <+> op)
                      [ ty <+> "x", ty <+> "y" ]
    [ cRetrun (cCall "DDL::compare" ["x","y"] <+> op <+> "0") ]
  ]
  where ty = cTypeNameUse vis tdecl



--------------------------------------------------------------------------------
-- Construcotrs

defCons :: GenVis -> GenBoxed -> TDecl -> [CDecl]
defCons vis boxed tdecl =
  case tDef tdecl of
    TStruct {} -> [ defStructCon vis boxed tdecl ]
    TUnion  {} -> defUnionCons vis boxed tdecl


defStructCon :: GenVis -> GenBoxed -> TDecl -> CDecl
defStructCon vis boxed tdecl = defMethod vis tdecl "void" structCon params def
  where
  params = [ t <+> x | (t,x,_) <- fs ]
  def =
    case boxed of
      GenBoxed   -> [ cStmt $ cCall "ptr.allocate" []
                    , cStmt $ cCall ("ptr.getValue()." <.> structCon)
                                                      [ x | (_,x,_) <- fs ]
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
    let name = unionCon l
        fs   = [ (cSemType t, cLabel l) | t /= TUnit ]
    in defMethod vis tdecl "void" name [ ty <+> x | (ty,x) <- fs ]
       case boxed of
         GenBoxed -> [ cStmt (cCall "ptr.allocate" [])
                     , cStmt (cCall ("ptr.getValue()." <.> name) (map snd fs))
                     ]
         GenUnboxed ->
            cStmt ("tag =" <+> cSumTagV (tName tdecl) l)
          : [ cStmt ("data." <.> cField n <+> "=" <+> cLabel l) | t /= TUnit ]


--------------------------------------------------------------------------------
-- Selectors


defSelectors :: GenVis -> GenBoxed -> TDecl -> [CDecl]
defSelectors vis boxed tdecl =
  concatMap (defSelectorsOwn vis boxed tdecl) [GenBorrow,GenOwn]


defGetTag :: GenVis -> GenBoxed -> TDecl -> [CDecl]
defGetTag vis boxed tdecl =
  case tDef tdecl of
    TStruct {} -> []
    TUnion {} ->
      [ defMethod vis tdecl (cSumTagT tdecl) "getTag" []
          [ case boxed of
              GenBoxed   -> cStmt "return ptr.getValue().getTag()"
              GenUnboxed -> cStmt "return tag"
          ]
      ]

-- XXX: Maybe some selectors should return a reference?
defSelectorsOwn :: GenVis -> GenBoxed -> TDecl -> GenOwn -> [CDecl]
defSelectorsOwn vis boxed tdecl borrow = zipWith sel (getFields tdecl) [ 0 .. ]
  where

  uni  = case tDef tdecl of
           TStruct _ -> False
           TUnion  _ -> True


  sel (l,t) n =
    let name       = selName borrow l
        uniNote    = if uni
                       then "\nOnly valid when getTag() is " <+> cSumTagV (tName tdecl) l
                       else ""
        doc        = vcat [ "/** Get the value of field `" <.> pp l <.> "`."
                          , uniNote
                          , "*/"
                          ]
    in doc $$ (
       defMethod vis tdecl (cSemType t) name []
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
        )

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
        [ cStmt $ cCall ("ptr." <.> fun) [] ]
      GenUnboxed ->
        case tDef tdecl of
          TStruct _ -> map snd (stmts True)
          TUnion _ ->
            case stmts False of
              [] -> []
              xs -> [ vcat [ "switch" <+> parens (cCall "getTag" []) <+> "{"
                           , nest 2 $ vcat' [ "case" <+> cSumTagV (tName tdecl) l <.> colon $$
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



