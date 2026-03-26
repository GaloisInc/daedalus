{-# Language ImportQualifiedPost, OverloadedStrings, ImplicitParams, ConstraintKinds #-}
module Daedalus.VM.Backend.Rust.Type where

import Control.Exception

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
        [ Rust.mkStruct der Rust.PublicV (nm False) gen 
            [ (compileFieldLabel l, compileType VM.Owned t) | (l,t) <- fs ] ]
      Core.TUnion fs ->
        [ Rust.mkTySyn Rust.PublicV (nm False) gen
          (Rust.pathType (
            Rust.pathWithTypes [ddlModName,"O"] [
              Rust.pathType (Rust.pathWithTypes [nm True]
                                [ Rust.pathType (Rust.simplePath a) | a <- as ])
            ]))
        | isRec
        ] ++
        [ Rust.mkEnum der Rust.PublicV (nm isRec) gen
            [ (compileConLabel l, [compileType VM.Owned t]) | (l,t) <- fs ]
        ]
      Core.TBitdata p bdef -> notYet "is bitdata" -- BDD.Pat BitdataDef
  where
  der = ["Clone","PartialEq","Eq","PartialOrd","Ord"]
  isRec = Core.tnameRec tn
  tn = Core.tName td
  notYet msg = unsupported ("Type" <+> pp tn <+> msg)
  nm isPriv = compileTName isPriv tn
  as = map valTPName (Core.tTParamKValue td)
  gen = Rust.mkGenerics [] (map Rust.tyParam as) Rust.noWhereClause


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
      case sz of
        Core.TSize n
          | n `elem` [8,16,32,64] -> Rust.tU n
          | otherwise             -> xxx
        Core.TSizeParam tp -> xxx

    Core.TSInt sz ->
      case sz of
        Core.TSize n
          | n `elem` [8,16,32,64] -> Rust.tI n
          | otherwise             -> xxx
        Core.TSizeParam tp -> xxx

    Core.TInteger           -> xxx
    Core.TBool              -> Rust.tBool
    Core.TFloat             -> Rust.tF 32
    Core.TDouble            -> Rust.tF 64
    Core.TUnit              -> Rust.tTuple []
    Core.TArray t           -> maybeB "Array" "ArrayB" t
    Core.TBuilder t         -> maybeB "Builder" "BuilderB" t

    Core.TMaybe t           -> maybeRef (Rust.tOption (compileType VM.Owned t))
    Core.TMap tk kv         -> xxx
    Core.TIterator t        -> xxx
    Core.TUser t
      | not (null (Core.utNumArgs t)) -> unsupported (?fnMsg <> "numeric type arguments")
      | otherwise           -> Rust.pathType (Rust.pathWithTypes [nm] args)
      where
      nm = compileTName False (Core.utName t)
      args = [ compileType VM.Owned arg | arg <- Core.utTyArgs t ]

    Core.TParam bp          -> Rust.pathType (Rust.simplePath (valTPName bp))
  where
  xxx = error ("XXX: " ++ show (pp ty))
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
