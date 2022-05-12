{-# Language BlockArguments, RecordWildCards, OverloadedStrings, GADTs #-}
module Daedalus.CompileHS (hsModule, hsIdentMod) where

import Data.Text(Text)
import qualified Data.Text.Encoding as Text
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Char(isUpper,isLower, toUpper)
import Data.Word(Word8)
import Data.ByteString(ByteString)
import Data.List(groupBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.Panic
import qualified Daedalus.BDD as BDD

import Daedalus.Type.AST
import Daedalus.Compile.LangHS
import Daedalus.Compile.Config


data Env = Env
  { envTParser    :: Term
  , envCurMod     :: ModuleName
  , envExtern     :: Map Name ([Term] -> Term)
  , envQualNames  :: UseQual
  , envTypes      :: !(Map TCTyName TCTyDecl)
  }

lkpInEnv :: (PP k, Ord k) => String -> Map k a -> k -> a
lkpInEnv msg mp x =
  case Map.lookup x mp of
    Just r  -> r
    Nothing -> panic "lkpInEnv"
                $ [ "Missing variable"
                  , "Name: " ++ show (pp x)
                  , "Env: " ++ msg
                  , "Keys: " ++ unwords (map (show.pp) (Map.keys mp))
                  ]


--------------------------------------------------------------------------------
-- Names

-- XXX: The name translation is a bit iffy at the moment, and can result
-- in name clashes.

data NameStyle = NameDecl | NameUse

-- | Convert a value identifer to a Haskell identifier
hsIdentVal :: Ident -> String
hsIdentVal i = case Text.unpack i of
                 "$$" -> "__"
                 x | x `Set.member` kws -> "_" <> x
                 x@(c : cs)
                   | isUpper c -> 'p' : x
                   | isLower c -> x
                   | c == '?'  -> "ip_" ++ cs
                   | c == '$'  -> "cs_" ++ cs
                   | c == '_'  -> x
                 _ -> panic "hsIdentVal" ["Unexpected name", show i]
  where
  kws = Set.fromList [ "module", "import"
                     , "type", "class", "data"
                     , "where"
                     , "case", "of", "if", "then", "else"
                     ]


-- | Convert a type identifier to a Haskell type identifier
hsIdentTy :: Ident -> String
hsIdentTy i = case Text.unpack i of
                x@(c : _)
                  | isUpper c -> x
                  | isLower c || c == '_' -> 'T' : x
                _ -> panic "hsIdentTy" ["Unexpected name", show i]

hsIdentMod :: ModuleName -> String
hsIdentMod i = case Text.unpack i of
                 x@(c : rest)
                  | isLower c -> toUpper c : rest
                  | isUpper c -> x
                  | c == '_'  -> 'M' : x
                 _ -> panic "hsIdentMod" ["Unexpected name", show i]


--------------------------------------------------------------------------------

nameUse :: Env -> NameStyle -> Name -> (Ident -> String) -> Term
nameUse env use nm baseName =
  Var $
  case nameScopedIdent nm of
    ModScope m i
      | NameUse <- use,
        UseQualNames <- envQualNames env,
        m /= envCurMod env -> hsIdentMod m ++ "." ++ baseName i
      | otherwise          -> baseName i
    Local i   -> baseName i
    Unknown s -> panic "newTyName" ["Unexpected name", show s ]

hsTyName :: Env -> NameStyle -> TCTyName -> Term
hsTyName env use tn =
  case tn of
    TCTy nm       -> nameUse env use nm hsIdentTy
    TCTyAnon nm n -> nameUse env use nm (\i -> hsIdentTy i ++ "_" ++ show n)

hsTyVar :: TVar -> Term
hsTyVar tv = Var (names !! tvarId tv)
  where
  mkName c i = if i == (0 :: Int) then [c] else c : show i
  names      = [ mkName c i | i <- [ 0 .. ], c <- [ 'a' .. 'z' ] ]


hsValName :: Env -> NameStyle -> Name -> Term
hsValName env sty nm = nameUse env sty nm hsIdentVal

-- | We use the same name for the constructor as we do for the type.
hsStructConName :: Env -> NameStyle -> TCTyName -> Term
hsStructConName env sty nm = hsTyName env sty nm

-- | We prefix the constructor with the name of the type.
hsUniConName :: Env -> NameStyle -> TCTyName -> Label -> Term
hsUniConName env sty nm l = Var (tyN ++ "_" ++ Text.unpack l)
  where
  Var tyN = hsTyName env sty nm


--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Types

hsType :: Env -> Type -> Term
hsType env ty =
  case ty of
    TVar x    -> hsTyVar x
    TCon c ts -> hsTyName env NameUse c `aps` map (hsType env) ts
    Type tf ->
      case tf of
        TGrammar t  -> envTParser env `Ap` hsType env t
        TFun s t    -> ApI "->" (hsType env s) (hsType env t)
        TStream     -> "RTS.Input"
        TByteClass  -> "RTS.ClassVal"
        TNum n      -> Var (show n)
        TUInt t     -> "RTS.UInt" `Ap` hsType env t
        TSInt t     -> "RTS.SInt" `Ap` hsType env t
        TInteger    -> "HS.Integer"
        TBool       -> "HS.Bool"
        TFloat      -> "HS.Float"
        TDouble     -> "HS.Double"
        TUnit       -> Tuple []
        TArray t    -> "Vector.Vector" `Ap` hsType env t
        TMaybe t    -> "HS.Maybe" `Ap` hsType env t
        TMap k t    -> "Map.Map" `Ap` hsType env k `Ap` hsType env t
        TBuilder t  -> "Vector.Builder" `Ap` hsType env t


hsConstraint :: Env -> Constraint -> Term
hsConstraint env ctr =
  case ctr of
    Integral t -> "RTS.Numeric" `Ap` hsType env t
    Arith t    -> "RTS.Arith" `Ap` hsType env t

    HasStruct t l a ->
      aps "RTS.HasStruct" [ hsType env t, hsLabelT l, hsType env a ]

    HasUnion t l a ->
      aps "RTS.HasUnion" [ hsType env t, hsLabelT l, hsType env a ]

    Literal i t ->
      "RTS.Literal" `Ap` hsType env (Type (TNum i)) `Ap` hsType env t

    FloatingType t ->
      "HS.RealFrac" `Ap` hsType env t

    CAdd x y z   -> ApI "~" (ApI "HS.+" (hsType env x) (hsType env y))
                            (hsType env z)

    Coerce _ a b -> "RTS.Convert" `Ap` hsType env a `Ap` hsType env b

    Traversable t -> "RTS.IsLoop" `Ap` hsType env t
    Mappable s t  -> "RTS.IsMapLoop" `Ap` hsType env s `Ap` hsType env t

    ColElType s t  -> "RTS.ColElType" `Ap` hsType env s `Ap` hsType env t
    ColKeyType s t -> "RTS.ColKeyType" `Ap` hsType env s `Ap` hsType env t

    StructCon {}    -> panic "hsConstraint" ["Unexpected StructCon"]
    UnionCon {}     -> panic "hsConstraint" ["Unexpected UnionCon"]
    IsNamed {}      -> panic "hsConstraint" ["Unexpected IsNamed"]

hsRuleType :: Env -> RuleType -> Term
hsRuleType env ((as,bs) :-> y) = foldr fun (hsType env y) (map snd as ++ bs)
  where fun x t = ApI "->" (hsType env x) t

hsPolyRule :: Env -> Poly RuleType -> Qual
hsPolyRule env (Poly xs cs t) = Forall (map hsTyParam xs)
                                       (extra ++ map (hsConstraint env) cs)
                                       (hsRuleType env t)
  where
  extra    = concatMap extraCtrs xs

extraCtrs :: TVar -> [Term]
extraCtrs x = case tvarKind x of
                KValue    -> [ "RTS.DDL" `Ap` hsTyVar x ]
                KNumber   -> [ "RTS.SizeType" `Ap` hsTyVar x ]
                KClass    -> []
                KGrammar  -> panic "extraCtrs"
                              [ "A type variable of kind Grammar?" ]
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------

hsTyParam :: TVar -> Term
hsTyParam tv =
  case tvarKind tv of
    KNumber -> hasType "HS.Nat" nm
    _       -> nm
  where
  nm = hsTyVar tv



--------------------------------------------------------------------------------
-- Type Declarations

-- | Given a type declaration, compute an instantiation of the type.
-- This for use in instances.
hsThisTy :: Env -> TCTyDecl -> Term
hsThisTy env TCTyDecl { .. } =
  hsTyName env NameUse tctyName `aps` map hsTyVar tctyParams

-- | Declarations for the constructors of a type, and related instances.
hsTyDeclDef :: Env -> TCTyDecl -> ([Term],[Decl])
hsTyDeclDef env me =
  case tctyBD me of
    Nothing  -> hsTyDeclDefADT env me
    Just pat -> hsTyDeclDefBD env pat me

hsTyDeclDefBD :: Env -> BDD.Pat -> TCTyDecl -> ([Term],[Decl])
hsTyDeclDefBD env univ me@TCTyDecl { .. } = ([con], insts)
  where
  conName = hsStructConName env NameDecl tctyName
  con     = aps conName [ repTy ]
  wi      = toInteger (BDD.width univ)

  ty      = hsThisTy env me
  repTy   = hsType env (tUInt (tNum wi))

  insts   = bdinst : coerce1 : coerce2 : jsInst : selInsts

  bdinst =
    InstanceDecl
      Instance
        { instOverlaps = Normal
        , instAsmps   = []
        , instHead    = aps "RTS.Bitdata" [ ty ]
        , instMethods =
          [ Fun TypeFun (aps "BDWidth"  [ ty ]) (hsInteger wi)
          , Fun ValueFun "bdToRep" "HS.coerce"
          , Fun ValueFun "bdFromRep" "HS.coerce"
          ]
        }

  coerce1 =
    InstanceDecl
      Instance
        { instOverlaps = Overlaps
        , instAsmps   = []
        , instHead    = aps "RTS.Convert" [ repTy, ty ]
        , instMethods =
          [ Fun ValueFun "convert" "HS.coerce"
          , Fun ValueFun (aps "convertMaybe" ["x"])
            let yep = aps "HS.Just" [ aps "HS.coerce" [ "x" ] ]
            in
            case [ t | t <- BDD.patTests univ, BDD.patMask t /= 0 ] of
              [] -> yep
              ts ->
                let mkOrs = foldr1 (ApI "HS.||")
                    ok = mkOrs
                       $ map check
                       $ Map.toList
                       $ BDD.groupTestsByMask ts
                    check (m,vs) =
                      let masked
                            | m == (2^wi - 1) = aps "RTS.fromUInt" ["x"]
                            | otherwise =
                              ApI "HS..&." (aps "RTS.fromUInt" ["x"])
                                           (hsInteger m)
                      in case vs of
                           [] -> panic "ConvertInstace" ["empty test"]
                           [v] -> ApI "HS.==" masked (hsInteger v)
                           _   -> aps (Lam ["y"] body) [masked]
                              where tryIt v = ApI "HS.==" "y" (hsInteger v)
                                    body    = mkOrs (map tryIt vs)
                in If ok yep "HS.Nothing"
          ]
        }

  coerce2 =
    InstanceDecl
      Instance
        { instOverlaps = Overlaps
        , instAsmps   = []
        , instHead    = aps "RTS.Convert" [ ty, repTy ]
        , instMethods =
          [ Fun ValueFun "convert" "HS.coerce"
          , Fun ValueFun "convertMaybe" (ApI "HS.." "HS.Just" "HS.coerce")
          ]
        }


  jsInst =
    InstanceDecl
      Instance
        { instOverlaps = Normal
        , instAsmps = []
        , instHead = aps "RTS.ToJSON" [ ty ]
        , instMethods =
          [ Fun ValueFun (aps "toJSON" ["x"])
            case tctyDef of
              TCTyStruct _ fs ->
                        (aps "RTS.jsObject"
                          [ List
                              [ Tuple [ hsl
                                      , aps "RTS.toJSON"
                                      [ aps "HS.getField" [ TyParam hsl, "x" ] ]
                                      ]
                              | (l,_) <- fs
                              , let hsl = hsByteString (Text.encodeUtf8 l)
                              ]
                           ]
                         )
              TCTyUnion fs ->
                hsBitdataCase'
                  (TCon tctyName [])
                  (aps "HS.error" [ hsByteString "Pattern match failure" ])
                  env
                  "x"
                  [ ( [("y",t)]
                    , [ p ]
                    , aps "RTS.jsTagged" [ hsl, aps "RTS.toJSON" [ "y" ] ]
                    ) | (l, (t,Just p)) <- fs
                      , let hsl = hsByteString ("$" <> Text.encodeUtf8 l)
                  ]
                  Nothing
          ]
        }

  selInsts =
    case tctyDef of
      TCTyStruct ~(Just bd) _ ->
        [ InstanceDecl
            Instance
              { instOverlaps = Normal
              , instAsmps    = []
              , instHead     = aps "HS.HasField" [ hsText l, ty, hsType env t ]
              , instMethods =
                [ Fun ValueFun (aps "getField" [aps conName ["x"]])
                               (aps "RTS.bdFromRep"
                                  [ aps "RTS.cvtU" [doShift "x" (bdOffset fi)]])
                ]
              }
        | fi <- bdFields bd, BDData l t <- [ bdFieldType fi ]
        ]
      TCTyUnion {} -> []


  doShift x n
    | n == 0 = x
    | otherwise =
      aps "RTS.shiftr" [x, aps "RTS.lit" [ hsInteger (fromIntegral n)] ]


hsTyDeclDefADT :: Env -> TCTyDecl -> ([Term],[Decl])
hsTyDeclDefADT env me@TCTyDecl { .. } =
  case tctyDef of
    TCTyStruct _ fs ->
      ( [ aps (hsStructConName env NameDecl tctyName) (map snd fts) ]
      , jsinst body : map hasInst fts
      )
      where
      fts = map fldT' fs
      hasInst (f,t) =
        let pat = hsStructConName env NameUse tctyName `aps`
                     [ if f == fst fi then "x" else "_" | fi <- fts ]
            st = hsThisTy env me
            ft = hsLabelT f

        in declare Instance
             { instOverlaps = Normal
             , instAsmps = []
             , instHead  = "HS.HasField" `Ap` ft `Ap` st `Ap` t
             , instMethods = [ Fun ValueFun ("getField" `Ap` pat) "x" ]
             }
      body = aps "RTS.jsObject"
               [ List [ Tuple [ hsl
                              , aps "RTS.toJSON"
                              [ aps "HS.getField" [ TyParam hsl, "x" ] ]
                              ]
                      | (l,_) <- fs
                      , let hsl = hsByteString (Text.encodeUtf8 l)
                      ]
               ]

    TCTyUnion fs -> let (cs,insts) = unzip (map (mkCon . fldT) fs)
                    in (cs, jsinst body : concat insts)
      where
      body = Case "x"
               [ ( hsUniConName env NameUse tctyName l `Ap` "y"
                 , aps "RTS.jsTagged" [ hsByteString ("$" <> Text.encodeUtf8 l)
                                      , "RTS.toJSON" `Ap` "y" ]
                 )
               | (l,_) <- fs ]

      mkCon (f,t) =
        let st = hsThisTy env me
            ft = hsLabelT f
        in
        ( hsUniConName env NameDecl tctyName f `Ap` t
        , [ declare Instance
              { instOverlaps = Normal
              , instAsmps = []
              , instHead  = "HS.HasField" `Ap` ft `Ap` st `Ap`
                                                          ("HS.Maybe" `Ap` t)
              , instMethods =
                  let pat = hsUniConName env NameUse tctyName f `Ap` "x"
                  in [ Fun ValueFun ("getField" `Ap` pat) ("HS.Just" `Ap` "x")
                     , Fun ValueFun ("getField" `Ap` "_") "HS.Nothing"
                     ]
              }
          ]
        )

  where
  fldT :: (Label, (Type, a)) -> (Label, Term)
  fldT (f,(t, _)) = (f, hsType env t)


  fldT' :: (Label, Type) -> (Label, Term)
  fldT' (f,t) = (f, hsType env t)

  jsinst body = declare Instance
                { instOverlaps = Normal
                , instAsmps = [ c
                              | a <- tctyParams, tvarKind a == KValue
                              , let v = hsType env (TVar a)
                              , c <- [ "RTS.ToJSON" `Ap` v
                                     , "RTS.DDL" `Ap` v
                                     ]
                              ]
                , instHead = "RTS.ToJSON" `Ap` hsThisTy env me
                , instMethods = [ Fun ValueFun ("toJSON" `Ap` "x") body ]
                }



-- | Declara a type and related instances.
hsTyDecl :: Env -> TCTyDecl -> [Decl]
hsTyDecl env me@TCTyDecl { .. } =
  declare Data { dataLHS    = hsTyName env NameDecl tctyName
                                          `aps` map hsTyParam tctyParams
               , dataCons   = cons
               }
  : derive "HS.Eq" : derive "HS.Ord" : derive "HS.Show" : ddlT : selInsts

  where
  (cons, selInsts) = hsTyDeclDef env TCTyDecl { .. }

  derive c = declare Deriving
                      { deriveAsmps = concatMap extraCtrs tctyParams
                      , deriveHead  = c `Ap` hsThisTy env me
                      }

  ddlT = declare Instance
          { instOverlaps = Normal
          , instAsmps = concatMap extraCtrs tctyParams
          , instHead  = "RTS.DDL" `Ap` hsThisTy env me
          , instMethods = []
          }



--------------------------------------------------------------------------------

hsTCName :: Env -> TCName k -> Term
hsTCName env TCName { .. } = hasType (hsType env tcType)
                                     (hsValName env NameDecl tcName)

hsParam :: Env -> Param -> Term
hsParam env p =
  case p of
    ValParam x     -> hsTCName env x
    ClassParam x   -> hsTCName env x
    GrammarParam x -> hsTCName env x

hsTCDecl :: Env -> TCDecl SourceRange -> [Decl]
hsTCDecl env d@TCDecl { .. } = [sig,def]
  where
  nm  = hsValName env NameDecl tcDeclName
  sig = declare Sig { sigName = [nm]
                    , sigType = hsPolyRule env (declTypeOf d)
                    }
  def = declare Fun
          { funNS = ValueFun
          , funLHS = nm `aps` map (hsParam env) tcDeclParams
          , funDef = defRHS
          }

  defRHS = case tcDeclDef of
             ExternDecl t ->
                case nameScopedIdent tcDeclName of
                  ModScope "Debug" "Trace" ->
                    hasType (hsType env t) ("RTS.pTrace" `Ap` "message")
                  ModScope _ f -> hasType (hsType env t)
                    let ps = map (hsParam env) tcDeclParams
                        p  = case Map.lookup tcDeclName (envExtern env) of
                              Just ter -> ter
                              Nothing ->
                                aps $
                                  hsValName
                                    env { envQualNames = UseQualNames }
                                    NameUse
                                    tcDeclName
                                      { nameScopedIdent = ModScope "Extern" f }
                    in p ps

                  _ -> panic "hsTCDecl" ["Unexpected name", show tcDeclName]

             Defined e ->
               case tcDeclCtxt of
                 AValue   -> hsValue env e
                 AClass   -> hsByteClass env e
                 AGrammar -> hsGrammar env e

--------------------------------------------------------------------------------

hsValue :: Env -> TC SourceRange Value -> Term
hsValue env tc =
  case texprValue tc of
    TCLet x e1 e2 ->
      (Lam [hsTCName env x] (hsValue env e2)) `Ap` hsValue env e1

    TCLiteral l t ->
      case l of
        LNumber n _ -> hasType (hsType env t) ("RTS.lit" `Ap` hsInteger n)
        LFloating n -> hasType (hsType env t) (hsDouble n)
        LBool b   -> hsBool b
        LByte b _ -> "RTS.uint8" `Ap` hsWord8 b
        LBytes b  -> hasType (hsType env (tArray tByte))
                             ("Vector.vecFromRep" `Ap` hsByteString b)
        LPi       -> hasType (hsType env t) "HS.pi"

    TCNothing t  -> hasType ("HS.Maybe" `Ap` hsType env t) "HS.Nothing"
    TCJust e    -> "HS.Just" `Ap` hsValue env e

    TCBuilder t -> hasType ("Vector.Builder" `Ap` hsType env t)
                           "Vector.emptyBuilder"

    TCStruct fs t ->
      case t of
        TCon c _
          | Just decl <- Map.lookup c (envTypes env)
          , TCTyStruct (Just con) _ <- tctyDef decl ->
            hsBitdataStruct env t con fields
          | otherwise -> hsStructConName env NameUse c `aps` map snd fields
            where fields = [ (f, hsValue env e) | (f,e) <- fs ]
        Type TUnit | null fs -> Tuple []
        _ -> panic "hsValue" ["Not a struct type", showPP t]


    TCArray vs t  ->
      case vs of
        [] -> hasType (hsType env (tArray t)) "Vector.empty"
        _  -> "Vector.fromList" `Ap` List (map (hsValue env) vs)

    TCIn l v t ->
      case t of
        TCon c _
          | Just decl <- Map.lookup c (envTypes env)
          , Just _    <- tctyBD decl ->
            hasType (hsType env t)
              (aps "RTS.bdFromRep" [ aps "RTS.bdToRep" [ hsValue env v ] ])
          | otherwise -> hsUniConName env NameUse c l `Ap` hsValue env v
        _ -> panic "hsValue" ["Unexpected type in `TCIn`"]

    TCTriOp op v1 v2 v3 _t ->
      case op of
        RangeUp     -> tri "Vector.rangeUp"
        RangeDown   -> tri "Vector.rangeDown"
        MapDoInsert -> tri "Map.insert"
      where
      tri x = x `aps` [ hsValue env v1, hsValue env v2, hsValue env v3 ]

    TCBinOp op v1 v2 _t ->
      case op of
        Add         -> bin "RTS.add"
        Sub         -> bin "RTS.sub"
        Mul         -> bin "RTS.mul"
        Div         -> bin "RTS.div"
        Mod         -> bin "RTS.mod"

        Lt          -> binI "HS.<"
        Leq         -> binI "HS.<="
        Eq          -> binI "HS.=="
        NotEq       -> binI "HS./="

        Cat         -> bin "RTS.cat"
        LCat        -> bin "RTS.lcat"
        LShift      -> bin "RTS.shiftl"
        RShift      -> bin "RTS.shiftr"
        BitwiseAnd  -> bin "RTS.bitAnd"
        BitwiseOr   -> bin "RTS.bitOr"
        BitwiseXor  -> bin "RTS.bitXor"

        ArrayStream -> bin "RTS.arrayStream"
        LookupMap   -> bin "Map.lookup"
        BuilderEmit -> "Vector.pushBack" `Ap` hsValue env v1 `Ap` hsValue env v2
        BuilderEmitArray -> "Vector.pushBackVector" `Ap` hsValue env v1 `Ap` hsValue env v2
        BuilderEmitBuilder -> "Vector.pushBackArray" `Ap` hsValue env v1 `Ap` hsValue env v2

        LogicAnd    -> binI "HS.&&"
        LogicOr     -> binI "HS.||"
      where
      bin x = x `Ap` hsValue env v1 `Ap` hsValue env v2
      binI x = ApI x (hsValue env v1) (hsValue env v2)

    TCUniOp op v ->
      case op of
        Not               -> "HS.not" `Ap` hsValue env v
        Neg               -> "RTS.neg" `Ap` hsValue env v
        BitwiseComplement -> "RTS.bitCompl" `Ap` hsValue env v
        Concat            -> "Vector.concat" `Ap` hsValue env v
        ArrayLength       -> "Vector.length" `Ap` hsValue env v
        WordToFloat       -> "RTS.wordToFloat" `Ap` hsValue env v
        WordToDouble      -> "RTS.wordToDouble" `Ap` hsValue env v
        IsNaN             -> "HS.isNaN" `Ap` hsValue env v
        IsInfinite        -> "HS.isInfinite" `Ap` hsValue env v
        IsDenormalized    -> "HS.isDenormalized" `Ap` hsValue env v
        IsNegativeZero    -> "HS.isNegativeZero" `Ap` hsValue env v
        BytesOfStream     ->
            "Vector.vecFromRep" `Ap` ("RTS.inputBytes" `Ap` hsValue env v)
        BuilderBuild      -> "Vector.finishBuilder" `Ap` hsValue env v

    TCVar x -> hsValName env NameUse (tcName x)
    TCCall f ts as -> hsApp env f ts as

    TCIf v x y -> If (hsValue env v) (hsValue env x) (hsValue env y)

    TCSelStruct x l _t -> "HS.getField" `Ap` TyParam (hsLabelT l)
                                        `Ap` hsValue env x

    TCCoerce _t1 t2 v -> hasType (hsType env t2)
                                 ("RTS.convert" `Ap` hsValue env v)

    TCFor lp -> evalFor env lp

    TCMapEmpty t -> hasType (hsType env t) "Map.empty"

    TCCase e as d -> hsCase hsValue err env e as d
      where err = "HS.error" `Ap` Raw (describeAlts as)

hsByteClass :: Env -> TC SourceRange Class -> Term
hsByteClass env tc =
  case texprValue tc of
     TCSetAny           -> "RTS.bcAny"
     TCSetSingle e      -> "RTS.bcSingle" `Ap` hsValue env e
     TCSetComplement e  -> "RTS.bcComplement" `Ap` hsByteClass env e
     TCSetUnion xs ->
        case xs of
          [] -> "RTS.bcNone"
          _  -> foldr1 jn (map (hsByteClass env) xs)
            where jn x y = "RTS.bcUnion" `Ap` x `Ap` y
     TCSetOneOf x       -> "RTS.bcByteString" `Ap` hsByteString x
     TCSetDiff x y      -> "RTS.bcDiff" `Ap` hsByteClass env x
                                        `Ap` hsByteClass env y
     TCSetRange x y     -> "RTS.bcRange" `Ap` hsValue env x `Ap` hsValue env y

     TCCall f ts as -> hsApp env f ts as
     TCVar x        -> hsValName env NameUse (tcName x)

     TCFor {} -> panic "hsByteClass" ["Unexpected TCFor"]

     TCIf e e1 e2 -> If (hsValue env e) (hsByteClass env e1)
                                        (hsByteClass env e2)
     TCCase e as d -> hsCase hsByteClass "RTS.bcNone" env e as d


hsBitdataStruct :: Env -> Type -> BDCon -> [(Label,Term)] -> Term
hsBitdataStruct env ty con fs =
  case map doCon (bdFields con) of
    [] -> bv 0 0
    vs -> hasType (hsType env ty)
        $ aps "RTS.bdFromRep" [ foldl1 (\a b -> aps "RTS.cat" [a,b]) vs ]

  where
  bv w i = hasType (hsType env (tUInt (tNum (toInteger w))))
         $ aps "RTS.UInt" [ hsInteger i ]

  doCon f =
    case bdFieldType f of
      BDWild     -> bv (bdWidth f) 0
      BDTag n    -> bv (bdWidth f) n
      BDData l _ -> case lookup l fs of
                      Just v  -> aps "RTS.bdToRep" [v]
                      Nothing -> panic "hsBitdataStruct"
                                  [ "Missing field", showPP l ]


--------------------------------------------------------------------------------
hsLabelT :: Label -> Term
hsLabelT = Raw

hsRange :: SourceRange -> Term
hsRange x = Raw (prettySourceRange x)

hsText :: Text -> Term
hsText = Raw

hsWord8 :: Word8 -> Term
hsWord8 = Raw

hsBool :: Bool -> Term
hsBool b = if b then "HS.True" else "HS.False"

hsInteger :: Integer -> Term
hsInteger n = if n >= 0 then Raw n else Tuple [Raw n] -- hack

hsDouble :: Double -> Term
hsDouble n = if n >= 0 then Raw n else Tuple [Raw n] -- hack

hsByteString :: ByteString -> Term
hsByteString = Raw
--------------------------------------------------------------------------------

optSkip :: WithSem -> Term -> Term -> Term
optSkip s m e = ApI "HS.<$>" toRes e
  where
  toRes = case s of
            NoSem  -> "HS.const" `Ap` Tuple []
            YesSem -> m

hsCommit :: Commit -> Term
hsCommit cmt = case cmt of
                 Commit    -> "(RTS.<||)"
                 Backtrack -> "(RTS.|||)"



hsGrammar :: Env -> TC SourceRange Grammar -> Term
hsGrammar env tc =
  let erng = hsRange (range tc)
  in
  case texprValue tc of
     TCFail mbE _ ->
        case mbE of
          Nothing ->
            "RTS.pError" `Ap` "RTS.FromSystem" `Ap` erng
                         `Ap` hsText "Parse error"
          Just e ->
            "RTS.pError" `Ap` "RTS.FromUser" `Ap` erng
                         `Ap` ("Vector.vecToString" `Ap` hsValue env e)

     TCPure e -> "HS.pure" `Ap` hsValue env e
     TCDo mb m1 m2
       | Nothing <- mb, tGrammar tUnit /= typeOf m1
                   -> Do Nothing ("HS.void" `Ap` hsGrammar env m1)
                                 (hsGrammar env m2)
       | otherwise -> Do (hsTCName env <$> mb) (hsGrammar env m1)
                         (hsGrammar env m2)

     TCLabel t e -> "RTS.pEnter" `Ap` hsText t `Ap` hsGrammar env e

     TCMatch s c -> optSkip s "RTS.uint8"
                    ("RTS.pMatch1" `Ap` erng `Ap` hsByteClass env c)

     TCMatchBytes s v ->
        case s of
          YesSem -> it
          NoSem  -> ApI "HS.<$>" ("HS.const" `Ap` Tuple []) it
        where it = "RTS.pMatch" `Ap` erng `Ap` hsValue env v

     TCChoice c opts t ->
        case opts of
          [] -> ApI "::" ("RTS.pError" `Ap` "RTS.FromSystem" `Ap`
                                            erng `Ap` hsText "empty choice")
                         (hsType env (tGrammar t))
          _  -> foldr1 (\x y -> hsCommit c `Ap` x `Ap` y)
                       (map (hsGrammar env) opts)

     TCOptional c e -> "RTS.pOptional" `Ap` hsCommit c `Ap` "HS.Just" `Ap`
                                                          hsGrammar env e

     TCMany NoSem _ (Exactly e) e' ->
       "RTS.pSkipExact" `Ap` hsValue env e `Ap` hsGrammar env e'

     TCMany YesSem _ (Exactly e) e' ->
       "Vector.replicateM" `Ap` hsValue env e `Ap` hsGrammar env e'

     TCMany s cmt (Between m_le m_ue) e ->
       let orElse = hsCommit cmt
           code   = hsGrammar env e
       in
       case (hsValue env <$> m_le, hsValue env <$> m_ue) of

         (Nothing, Nothing) ->
           case s of
             YesSem -> "RTS.pMany"     `Ap` orElse `Ap` code
             NoSem  -> "RTS.pSkipMany" `Ap` orElse `Ap` code

         (Nothing, Just ub) ->
           case s of
             YesSem -> "RTS.pManyUpTo"     `Ap` orElse `Ap` ub `Ap` code
             NoSem  -> "RTS.pSkipManyUpTo" `Ap` orElse `Ap` ub `Ap` code

         (Just lb,Nothing) ->
           case s of
             YesSem -> "RTS.pMinLength" `Ap` erng `Ap` lb `Ap`
                       ("RTS.pMany" `Ap` orElse `Ap` code)

             NoSem  -> "RTS.pSkipAtLeast" `Ap` orElse `Ap` lb `Ap` code

         (Just lb, Just ub) ->
           case s of
             YesSem -> "RTS.pMinLength" `Ap` erng `Ap` lb `Ap`
                       ("RTS.pManyUpTo" `Ap` orElse `Ap` ub `Ap` code)
             NoSem  -> "RTS.pSkipWithBounds" `Ap` erng `Ap` orElse
                            `Ap` lb `Ap` ub `Ap` code

     TCVar x -> hsValName env NameUse (tcName x)


     TCEnd    -> "RTS.pEnd" `Ap` erng


     TCOffset -> "RTS.pOffset"

     TCCall f ts as ->
        case typeOf f of
          Type (TGrammar {}) ->
            "RTS.pEnter" `Ap` hsText (Text.pack (show (pp f)))
                         `Ap` hsApp env f ts as
          _ -> hsApp env f ts as

     TCMapLookup sem k mp ->
       hsMaybe sem erng (ApI "HS.++"
                            (hsText "Missing key: ")
                            ("HS.show" `Ap` hasType (hsType env (typeOf k))
                                                       (hsValue env k)))
                        ("Map.lookup" `aps` [ hsValue env k, hsValue env mp ])

     TCMapInsert sem k v mp ->
       hsMaybe sem erng (hsText "Key already present")
                        ("Map.insertMaybe" `aps` [ hsValue env k, hsValue env v
                                                 , hsValue env mp ])

     TCArrayIndex sem e ix ->
       hsMaybe sem erng (hsText "Index out of bounds")
                        ("(Vector.!?)" `aps` [ hsValue env e, hsValue env ix])

     TCCoerceCheck sem _t1 t2 v ->
        hsMaybe sem erng (hsText ("Value does not fit in target type"))
          $ hasType ("HS.Maybe" `Ap` hsType env t2)
                    ("RTS.convertMaybe" `Ap` hsValue env v)

     TCFor lp -> evalForM env lp

     TCCurrentStream -> "RTS.pPeek"
     TCSetStream v   -> "RTS.pSetInput" `Ap` hsValue env v

     TCStreamLen sem n s ->
       hsMaybe sem erng (hsText "Not enough bytes")
         $ "RTS.limitLen" `Ap` hsValue env n `Ap` hsValue env s

     TCStreamOff sem n s ->
       hsMaybe sem erng (hsText "Not enough bytes")
        $ "RTS.advanceBy" `Ap` hsValue env n `Ap` hsValue env s

     TCErrorMode m p -> "RTS.pErrorMode" `Ap` m' `Ap` hsGrammar env p
       where m' = case m of
                    Commit    -> "RTS.Abort"
                    Backtrack -> "RTS.Fail"

     TCIf e e1 e2 -> If (hsValue env e) (hsGrammar env e1) (hsGrammar env e2)

     TCCase e alts dfl -> hsCase hsGrammar err env e alts dfl
       where err = "RTS.pError" `Ap` "RTS.FromSystem" `Ap` erng
                                `Ap` hsText (Text.pack (describeAlts alts))

hsCase ::
  (Env -> TC SourceRange k -> Term) ->
  Term ->
  Env ->
  TC SourceRange Value ->
  NonEmpty (TCAlt SourceRange k) ->
  Maybe (TC SourceRange k) ->
  Term
hsCase eval ifFail env e alts dfl
  | isBitdata = hsBitdataCase eval ifFail env e alts dfl
  | isStrPat  = Case ("Vector.vecToRep" `Ap` hsValue env e) branches
  | otherwise = Case (hsValue env e) branches
  where
  isBitdata = case typeOf e of
                TCon c _
                   | Just _ <- tctyBD (lkpInEnv "typeEnv" (envTypes env) c) ->
                     True
                _ -> False

  isStrPat = typeOf e == tArray tByte

  branches =
    concatMap alt (NE.toList alts) ++ [
      case dfl of
        Nothing -> ("_", ifFail)
        Just d  -> ("_", eval env d)
    ]
  -- XXX: currently we duplicate code, we may want to name it in a where...
  alt (TCAlt ps rhs) =
    let r = eval env rhs
    in [ (hsPat env p, r) | p <- ps ]


hsBitdataUniv :: Env -> Type -> BDD.Pat
hsBitdataUniv env ty =
  case ty of
    TCon c _ -> case tctyBD (lkpInEnv "envTypes" (envTypes env) c) of
                  Just p -> p
                  _      -> bad
    TVar {} -> bad
    Type tc ->
      case tc of
        TUInt (Type (TNum n)) -> BDD.pWild (fromInteger n)
        TSInt (Type (TNum n)) -> BDD.pWild (fromInteger n)
        TFloat                -> BDD.pWild 32
        TDouble               -> BDD.pWild 64
        TUnit                 -> BDD.pWild 0
        _                     -> bad
  where
  bad = panic "hsBitdataUniv" [ "Not a bitdata type", showPP ty ]

hsPatBDD :: Env -> TCPat -> BDD.Pat
hsPatBDD env pat =
  case pat of
    TCConPat _ _ p1 -> hsPatBDD env p1
    TCVarPat x      -> hsBitdataUniv env (typeOf x)
    TCWildPat t     -> hsBitdataUniv env t

    TCNumPat {}     -> bad
    TCStrPat {}     -> bad
    TCBoolPat {}    -> bad
    TCJustPat {}    -> bad
    TCNothingPat {} -> bad

  where
  bad = panic "hsPatBDD" [ "Invlalid bitdata pattern", showPP pat ]

hsPatVars :: TCPat -> [TCName Value]
hsPatVars pat =
  case pat of
    TCVarPat x      -> [x]
    TCConPat _ _ p1 -> hsPatVars p1
    TCNumPat {}     -> []
    TCWildPat {}    -> []
    TCBoolPat {}    -> []
    TCStrPat {}     -> []
    TCJustPat p     -> hsPatVars p
    TCNothingPat {} -> []

hsBitdataCase' ::
  Type ->
  Term ->
  Env ->
  Term ->
  [ ([(Term,Type)],[BDD.Pat],Term) ] ->
  Maybe Term ->
  Term

hsBitdataCase' ty ifFail env e alts mbD =
  aps (Lam [aps "RTS.UInt" [caseValName]] actualCase)
      [ aps "RTS.bdToRep" [e] ]
  where
  caseValName = "caseVal" :: Term
  actualCase = foldr doCase finalCase tests

  finalCase = case mbD of
                Just d -> d
                Nothing -> ifFail

  doCase :: [(BDD.PatTest,Int)] -> Term -> Term
  doCase opts doElse =
    let (p,rhs) : _ = opts
    in if BDD.patMask p == 0
        then hsAlt rhs
        else Case (ApI "HS..&." caseValName (hsInteger (BDD.patMask p)))
               $ [ (hsInteger (BDD.patValue t), hsAlt n) | (t,n) <- opts ] ++
                 [ ("_", doElse) ]

  univ  = hsBitdataUniv env ty

  sameMask (x,_) (y,_) = BDD.patMask x == BDD.patMask y

  tests :: [[(BDD.PatTest,Int)]]
  tests = groupBy sameMask
        $ BDD.patTestsAssumingInOrder univ
          [ (p,n)
          | (alt,n) <- zip [ ps | (_,ps,_) <- alts ] [ 0 .. ]
          , p <- alt
          ]


  -- XXX: potentially duplicates RHS.  They could (should) be named instead
  -- We name the RHS with ints in prep for that.

  hsAlt n = let (xs,_,rhs) = alts !! n
            in aps (Lam (map fst xs) rhs)
                   [ ApI "::"
                      (aps "RTS.bdFromRep" [ aps "RTS.UInt" [caseValName] ])
                      (hsType env t)
                   | (_,t) <- xs ]
            -- XXX: if we added struct patterns this would have to
            -- change to actually project the fields.  At the moment
            -- we can have only variable which always has the same rep
            -- as the original value, but possibly at a different type





hsBitdataCase ::
  (Env -> TC SourceRange k -> Term) ->
  Term ->
  Env ->
  TC SourceRange Value ->
  NonEmpty (TCAlt SourceRange k) ->
  Maybe (TC SourceRange k) ->
  Term

hsBitdataCase doAlt ifFail env e alts mbD =
  hsBitdataCase' (typeOf e) ifFail env (hsValue env e)
      [ ( [ (hsTCName env v, typeOf v) | v <- vs ]
        , map (hsPatBDD env) (tcAltPatterns a)
        , doAlt env (tcAltBody a)
        )
      | a <- NE.toList alts
      , let vs = hsPatVars (head (tcAltPatterns a))
      ]
      (doAlt env <$> mbD)

hsPat :: Env -> TCPat -> Term
hsPat env pat =
  case pat of
    TCConPat t l p -> hsUniConName env NameUse nm l `Ap` hsPat env p
      where nm = case t of
                   TCon c _ -> c
                   _ -> panic "hsPat" [ "Unexepected type in union constructor"
                                      , show (pp t)
                                      , show (pp pat)
                                      ]
    TCNumPat t i _ ->
      case t of
        Type TInteger  -> Raw i
        Type (TUInt _) -> Tuple [ApI "->" "RTS.fromUInt" (Raw i)]
        Type (TSInt _) -> Tuple [ApI "->" "RTS.fromSInt" (Raw i)]
        _ -> panic "hsPat" [ "We don't support polymorphic case." ]

    TCStrPat bs -> Raw bs

    TCBoolPat b     -> hsBool b
    TCJustPat p     -> "HS.Just" `Ap` hsPat env p
    TCNothingPat _t -> "HS.Nothing"
    TCVarPat x      -> hsTCName env x
    TCWildPat _t    -> "_"

hsMaybe :: WithSem -> Term -> Term -> Term -> Term
hsMaybe sem erng msg val = f `aps` [ erng, msg, val ]
  where f = case sem of
              NoSem  -> "RTS.pIsJust_"
              YesSem -> "RTS.pIsJust"



hsApp :: Env -> TCName k -> [Type] -> [Arg SourceRange] -> Term
hsApp env f ts as = hsValName env NameUse (tcName f) `aps`
                      ([ TyParam (hsType env t) | t <- ts ] ++
                       [ hsArg env a | a <- as ])

hsArg :: Env -> Arg SourceRange -> Term
hsArg env arg =
  case arg of
    ValArg x     -> hsValue env x
    GrammarArg x -> hsGrammar env x
    ClassArg x   -> hsByteClass env x




evalFor :: Env -> Loop SourceRange Value -> Term
evalFor env lp =
  case loopFlav lp of

    LoopMap col ->
      let colV = hsValue env (lcCol col)
      in
      case lcKName col of
        Nothing -> hasType (hsType env (loopType lp))
                           ("RTS.loopMap" `Ap` step `Ap` colV)
              where step = Lam [ hsTCName env (lcElName col) ] bodyV

        Just k -> hasType (hsType env (loopType lp))
                          ("RTS.loopIMap" `Ap` step `Ap` colV)
              where step = Lam [ hsTCName env k
                               , hsTCName env (lcElName col)
                               ] bodyV

    Fold x s col ->
      let colV = hsValue env (lcCol col)
      in
      case lcKName col of
        Nothing -> "RTS.loopFold" `Ap` step `Ap` initVal `Ap` colV

              where initVal = hsValue env s
                    step = Lam [ hsTCName env x
                               , hsTCName env (lcElName col)
                               ] bodyV
        Just k -> "RTS.loopIFold" `Ap` step `Ap` initVal `Ap` colV
              where initVal = hsValue env s
                    step = Lam [ hsTCName env x
                               , hsTCName env k
                               , hsTCName env (lcElName col)
                               ] bodyV
  where
  bodyV   = hsValue env (loopBody lp)


evalForM :: Env -> Loop SourceRange Grammar -> Term
evalForM env lp =

  case loopFlav lp of

    LoopMap col ->
      let colV = hsValue env (lcCol col)
      in case lcKName col of
           Nothing -> hasType (hsType env (loopType lp))
                              ("RTS.loopMapM" `Ap` step `Ap` colV)
              where step = Lam [ hsTCName env (lcElName col) ] bodyV

           Just k -> hasType (hsType env (loopType lp))
                             ("RTS.loopIMapM" `Ap` step `Ap` colV)
              where step = Lam [ hsTCName env k
                               , hsTCName env (lcElName col)
                               ] bodyV

    LoopMany c x s -> "RTS.pLoopMany" `Ap` hsCommit c `Ap` step `Ap` initVal
      where initVal = hsValue env s
            step = Lam [ hsTCName env x ] bodyV

    Fold x s col ->
      let colV = hsValue env (lcCol col)
      in case lcKName col of

           Nothing -> "RTS.loopFoldM" `Ap` step `Ap` initVal `Ap` colV
             where initVal = hsValue env s
                   step = Lam [ hsTCName env x
                              , hsTCName env (lcElName col)
                              ] bodyV

           Just k -> "RTS.loopIFoldM" `Ap` step `Ap` initVal `Ap` colV
             where initVal = hsValue env s
                   step = Lam [ hsTCName env x
                              , hsTCName env k
                              , hsTCName env (lcElName col)
                              ] bodyV
  where
  bodyV   = hsGrammar env (loopBody lp)



--------------------------------------------------------------------------------

-- testing
hsModule ::
  CompilerCfg -> Map TCTyName TCTyDecl -> TCModule SourceRange -> Module
hsModule CompilerCfg { .. } allTys TCModule { .. } = Module
  { hsModName  = hsIdentMod tcModuleName
  , hsLangExts = [ "DataKinds", "KindSignatures", "TypeOperators"
                 , "MultiParamTypeClasses", "FlexibleInstances"
                 , "StandaloneDeriving"
                 , "ScopedTypeVariables"
                 , "FlexibleContexts"
                 , "AllowAmbiguousTypes"
                 , "OverloadedStrings"
                 , "TypeApplications"
                 , "TypeFamilies"
                 , "ViewPatterns"
                 ]
  , hsGHC = [ "-Wno-unused-imports" ]
  , hsImports  = cImports ++
                 case cParserType of
                   Nothing -> [ Import "RTS.Parser" (QualifyAs "RTS") ]
                   _       -> []
                  ++
                 [ Import (hsIdentMod i) Qualified
                            | i <- map thingValue tcModuleImports
                 ] ++
                 [ Import "Prelude"       (QualifyAs "HS")
                 , Import "GHC.TypeLits"  (QualifyAs "HS")
                 , Import "GHC.Records"   (QualifyAs "HS")
                 , Import "Data.Coerce"   (QualifyAs "HS")
                 , Import "Data.Bits"     (QualifyAs "HS")
                 , Import "Control.Monad" (QualifyAs "HS")
                 , Import "RTS"           (QualifyAs "RTS")
                 , Import "RTS.Input"     (QualifyAs "RTS")
                 , Import "RTS.Map"       (QualifyAs "Map")
                 , Import "RTS.Vector"    (QualifyAs "Vector")
                 , Import "RTS.Numeric"   (QualifyAs "RTS")
                 , Import "RTS.JSON"      (QualifyAs "RTS")
                 ]
  , hsDecls = concatMap (hsTyDecl env) (concatMap recToList tcModuleTypes) ++
              concatMap (hsTCDecl env) (concatMap recToList tcModuleDecls)
  }
  where
  env = Env { envCurMod  = tcModuleName
            , envTParser = case cParserType of
                             Nothing -> "RTS.Parser"
                             Just t  -> t
            , envExtern  = cPrims
            , envQualNames = cQualNames
            , envTypes = allTys
            }

