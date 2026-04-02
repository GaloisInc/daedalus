{-# Language ImportQualifiedPost, OverloadedStrings, ImplicitParams, ConstraintKinds #-}
{-# Language BlockArguments #-}
module Daedalus.VM.Backend.Rust.Type where

import Control.Exception
import Data.Text qualified as Text

import Daedalus.Panic(panic)
import Daedalus.PP
import Daedalus.Rec
import Daedalus.Core qualified as Core
import Daedalus.VM qualified as VM
import Daedalus.VM.Backend.Rust.Lang qualified as Rust
import Daedalus.VM.Backend.Rust.Names

type FnMsg = (?fnMsg :: Doc)

unsupported :: Doc -> a
unsupported x = throw (Unsupported x)

newtype Unsupported = Unsupported Doc deriving Show
instance Exception Unsupported

compileUserType :: Rec Core.TDecl -> [Rust.Item ()]
compileUserType = concatMap compileTDecl . recToList

compileTDecl :: Core.TDecl -> [Rust.Item ()]
compileTDecl td
  | not (null (Core.tTParamKNumber td)) = notYet "has numeric parameters"
  | otherwise =
    let ?fnMsg = pp tn in
    case Core.tDef td of
      Core.TStruct fs ->
        [ tyDecl "by_ref"
        , Rust.mkStruct der Rust.PublicV (nm False) gen
            [ (compileFieldLabel l, compileType VM.Owned t) | (l,t) <- fs ]
        , makeSerializeStruct as (nm False) fs
        ]
      Core.TUnion fs ->
        [ Rust.mkEnum der Rust.PublicV (nm isRec) gen
            [ (compileConLabel l, [ compileType VM.Owned t | not (Core.isUnit t) ])
            | (l,t) <- fs ]
        , makeSerializeEnum as (nm isRec) fs
        , if isRec
            then
              Rust.mkTySyn Rust.PublicV (nm False) gen
                  (Rust.pathType (Rust.pathWithTypes [ddlModName,"O"] [ tyForm True ]))
            else
              tyDecl (if and [ Core.isUnit t | (_,t) <- fs ] then "by_value" else "by_ref")
        ]

      Core.TBitdata _p _bdef -> notYet "is bitdata" -- BDD.Pat BitdataDef
  where
  der = ["Clone","PartialEq","Eq","PartialOrd","Ord"]
  isRec = Core.tnameRec tn
  tn = Core.tName td
  notYet msg = unsupported ("Type" <+> pp tn <+> msg)
  nm isPriv = compileTName isPriv tn
  as = map valTPName (Core.tTParamKValue td)
  tyForm inner =
    Rust.pathType (Rust.pathWithTypes [nm inner]
      [ Rust.pathType (Rust.simplePath (valTPName a))
      | a <- Core.tTParamKValue td
      ])

  gen = Rust.mkGenerics (map Rust.tyParam as) Rust.noWhereClause
  tyDecl how =
    Rust.macDecl (Rust.mac (Rust.simplePath' [ddlModName,how]) (Rust.tyToken (tyForm False)))


compileVMT :: FnMsg => VM.VMT -> VM.Ownership -> Rust.Ty ()
compileVMT ty own =
  case ty of
    VM.TSem t     -> compileType own t
    VM.TThreadId  -> unsupported (?fnMsg <+> "ThreadId type")


-- | Compile a type in its owned form.
compileType :: FnMsg => VM.Ownership -> Core.Type -> Rust.Ty ()
compileType own ty =
  case ty of
    Core.TStream -> maybeRef (Rust.pathType (ddlPath "Input"))

    Core.TUInt sz ->
      Rust.pathType (Rust.pathWithGen [ddlModName,"U"] [compileSizeType sz])

    Core.TSInt sz ->
      Rust.pathType (Rust.pathWithGen [ddlModName,"I"] [compileSizeType sz])

    Core.TInteger           -> xxx
    Core.TBool              -> Rust.tBool
    Core.TFloat             -> Rust.tF 32
    Core.TDouble            -> Rust.tF 64
    Core.TUnit              -> Rust.tTuple []
    Core.TArray t           -> maybeB "Array" "ArrayB" t
    Core.TBuilder t         -> maybeB "Builder" "BuilderB" t

    Core.TMaybe t           -> maybeRef (Rust.tOption (compileType VM.Owned t))
    Core.TMap tk kv         -> xxx
    Core.TIterator t        -> 
      case t of
        Core.TArray el -> maybeB "ArrayIterator" "ArrayIteratorB" el
        Core.TMap k v -> xxx
        _ -> panic "compileType" ["Unexpected iterator type"]
    Core.TUser t
      | not (null (Core.utNumArgs t)) -> unsupported (?fnMsg <> "numeric type arguments")
      | otherwise           -> Rust.pathType (Rust.pathWithTypes [nm] args)
      where
      nm = compileTName False (Core.utName t)
      args = [ compileType VM.Owned arg | arg <- Core.utTyArgs t ]

    Core.TParam bp          -> Rust.pathType (Rust.simplePath (valTPName bp))
  where
  xxx = unsupported ("XXX:" <+> pp ty)
  maybeRef rt =
    case own of
      VM.Borrowed -> Rust.tRef Nothing rt
      _           -> rt
  maybeB town tbor t = Rust.pathType p
   where
    p = Rust.pathWithTypes [ddlModName,base] [compileType VM.Owned t]
    base =
      case own of
        VM.Borrowed -> tbor
        _           -> town

compileSizeType :: Core.SizeType -> Rust.GenericArg ()
compileSizeType ty =
  Rust.constGeneric
    case ty of
      Core.TSize n -> Rust.litExpr (Rust.intLit n)
      Core.TSizeParam a -> Rust.identExpr (numTPName a)

-- | Generate a serialize_struct! macro call
makeSerializeStruct ::
  [Rust.Ident]                    {- ^ Type parameters -} ->
  Rust.Ident                      {- ^ Type name -} ->
  [(Core.Label, Core.Type)]       {- ^ Fields -} ->
  Rust.Item ()
makeSerializeStruct typeParams typeName fields =
  Rust.macDecl (Rust.mac macroPath macroArgs)
  where
    macroPath = Rust.simplePath' [ddlModName, "serialize_struct"]
    macroArgs = buildMacroArgs typeParams typeName (structFieldTokens fields)

    structFieldTokens fs = Rust.commaList
      [ fieldPairTokens label
      | (label, _) <- fs
      ]

    fieldPairTokens label =
      Rust.Stream
        [ Rust.Tree (Rust.Delimited Rust.dummySpan Rust.Paren
            (Rust.commaList
              [ Rust.identToken (compileFieldLabel label)
              , Rust.strToken (Text.unpack label)
              ]))
        ]

-- | Generate a serialize_enum! macro call
makeSerializeEnum ::
  [Rust.Ident]                    {- ^ Type parameters -} ->
  Rust.Ident                      {- ^ Type name -} ->
  [(Core.Label, Core.Type)]       {- ^ Variants -} ->
  Rust.Item ()
makeSerializeEnum typeParams typeName variants =
  Rust.macDecl (Rust.mac macroPath macroArgs)
  where
    macroPath = Rust.simplePath' [ddlModName, "serialize_enum"]
    macroArgs = buildMacroArgs typeParams typeName (enumVariantTokens variants)

    enumVariantTokens vs = Rust.commaList
      [ variantPairTokens label ty
      | (label, ty) <- vs
      ]

    variantPairTokens label ty =
      Rust.Stream
        [ Rust.Tree (Rust.Delimited Rust.dummySpan Rust.Paren
            (Rust.commaList
              [ constructorPath typeName label ty
              , Rust.strToken (Text.unpack label)
              , if Core.isUnit ty then unitValue else Rust.identToken "x"
              ]))
        ]

    -- For unit variants, emit &()
    unitValue = Rust.exprToken $
      Rust.addrOf $
      Rust.TupExpr [] [] ()

    -- Generate TypeName::Variant for unit types, or TypeName::Variant(x) for data types
    constructorPath tn label ty =
      Rust.Stream $
        [ Rust.identToken tn
        , Rust.Tree (Rust.Token Rust.dummySpan Rust.ModSep)
        , Rust.identToken (compileConLabel label)
        ] ++ if Core.isUnit ty then [] else parensWithX

    parensWithX =
      [ Rust.Tree (Rust.Delimited Rust.dummySpan Rust.Paren
          (Rust.Stream [Rust.identToken "x"]))
      ]

-- Helper to build the overall macro arguments: (<T1, T2>, TypeName<T1, T2>, (...))
buildMacroArgs :: [Rust.Ident] -> Rust.Ident -> Rust.TokenStream -> Rust.TokenStream
buildMacroArgs typeParams typeName pairsTokens =
  Rust.commaList
    [ typeParamsTokens typeParams
    , Rust.tyToken (mkTypeWithParams typeName typeParams)
    , pairsTokens
    ]
  where
    mkTypeWithParams tn tps
      | null tps = Rust.simpleType tn
      | otherwise = Rust.pathType (Rust.pathWithTypes [tn]
                      [Rust.simpleType tp | tp <- tps])

    typeParamsTokens params =
      Rust.angleTokens (Rust.commaList [Rust.identToken p | p <- params])
