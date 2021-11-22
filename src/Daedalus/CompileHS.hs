{-# Language BlockArguments, RecordWildCards, OverloadedStrings, GADTs #-}
module Daedalus.CompileHS (hsModule) where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Char(isUpper,isLower, toUpper)
import Data.Word(Word8)
import Data.ByteString(ByteString)
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
  , envExtern     :: Map Name Term
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

  insts   = bdinst : coerce1 : coerce2 : selInsts

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
      , concatMap hasInst fts
      )
      where
      fts = map fldT' fs
      hasInst (f,t) =
        let pat = hsStructConName env NameUse tctyName `aps`
                     [ if f == fst fi then "x" else "_" | fi <- fts ]
            st = hsThisTy env me
            ft = hsLabelT f

        in [ declare Instance
             { instOverlaps = Normal
             , instAsmps = []
             , instHead  = "HS.HasField" `Ap` ft `Ap` st `Ap` t
             , instMethods = [ Fun ValueFun ("getField" `Ap` pat) "x" ]
             }
           ]

    TCTyUnion fs -> let (cs,insts) = unzip (map (mkCon . fldT) fs)
                    in (cs, concat insts)
      where
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
  fldT (f,(t, _)) = (f, hsType env t) -- FIXME: this erases BitData info


  fldT' :: (Label, Type) -> (Label, Term)
  fldT' (f,t) = (f, hsType env t) -- FIXME: this erases BitData info

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
                  _ -> hasType (hsType env t)
                              (lkpInEnv "envExtern" (envExtern env) tcDeclName)

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
        LBytes b  -> "Vector.vecFromRep" `Ap` hsByteString b
        LPi       -> hasType (hsType env t) "HS.pi"

    TCNothing t  -> hasType ("HS.Maybe" `Ap` hsType env t) "HS.Nothing"
    TCJust e    -> "HS.Just" `Ap` hsValue env e
    TCUnit      -> Tuple []

    TCStruct fs t ->
      case t of
        TCon c _
          | Just decl <- Map.lookup c (envTypes env)
          , TCTyStruct (Just con) _ <- tctyDef decl ->
            hsBitdataStruct env t con fields
          | otherwise -> hsStructConName env NameUse c `aps` map snd fields
            where fields = [ (f, hsValue env e) | (f,e) <- fs ]
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
        RangeUp -> tri "Vector.rangeUp"
        RangeDown -> tri "Vector.rangeDown"
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
        WordToFloat       -> "RTS.wordToFloat" `Ap` hsValue env v
        WordToDouble      -> "RTS.wordToDouble" `Ap` hsValue env v
        IsNaN             -> "HS.isNaN" `Ap` hsValue env v
        IsInfinite        -> "HS.isInfinite" `Ap` hsValue env v
        IsDenormalized    -> "HS.isDenormalized" `Ap` hsValue env v
        IsNegativeZero    -> "HS.isNegativeZero" `Ap` hsValue env v

    TCVar x -> hsValName env NameUse (tcName x)
    TCCall f ts as -> hsApp env f ts as

    TCIf v x y -> If (hsValue env v) (hsValue env x) (hsValue env y)

    TCSelStruct x l _t -> "HS.getField" `Ap` TyParam (hsLabelT l)
                                        `Ap` hsValue env x

    TCCoerce _t1 t2 v -> hasType (hsType env t2)
                                 ("RTS.convert" `Ap` hsValue env v)

    TCFor lp -> evalFor env lp

    TCMapEmpty t -> hasType (hsType env t) "Map.empty"
    TCArrayLength e -> "Vector.length" `Ap` hsValue env e

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
     TCGetByte s -> optSkip s "RTS.uint8"
                    ("RTS.pByte" `Ap` erng)

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
hsCase eval ifFail env e alts dfl = Case (hsValue env e) branches
  where
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
  case loopKName lp of
    Nothing ->
      case loopFlav lp of
        LoopMap -> hasType (hsType env (loopType lp))
                 $ ("RTS.loopMap" `Ap` step `Ap` colV)
          where step = Lam [ hsTCName env (loopElName lp) ] bodyV

        Fold x s -> "RTS.loopFold" `Ap` step `Ap` initVal `Ap` colV
          where initVal = hsValue env s
                step = Lam [ hsTCName env x
                           , hsTCName env (loopElName lp)
                           ] bodyV
    Just k ->
      case loopFlav lp of
        LoopMap -> hasType (hsType env (loopType lp))
                 $ "RTS.loopIMap" `Ap` step `Ap` colV
          where step = Lam [ hsTCName env k
                           , hsTCName env (loopElName lp)
                           ] bodyV

        Fold x s -> "RTS.loopIFold" `Ap` step `Ap` initVal `Ap` colV
          where initVal = hsValue env s
                step = Lam [ hsTCName env x
                           , hsTCName env k
                           , hsTCName env (loopElName lp)
                           ] bodyV
  where
  colV    = hsValue env (loopCol lp)
  bodyV   = hsValue env (loopBody lp)


evalForM :: Env -> Loop SourceRange Grammar -> Term
evalForM env lp =
  case loopKName lp of
    Nothing ->
      case loopFlav lp of
        LoopMap -> hasType (hsType env (loopType lp))
                 $ "RTS.loopMapM" `Ap` step `Ap` colV
          where step = Lam [ hsTCName env (loopElName lp) ] bodyV

        Fold x s -> "RTS.loopFoldM" `Ap` step `Ap` initVal `Ap` colV
          where initVal = hsValue env s
                step = Lam [ hsTCName env x
                           , hsTCName env (loopElName lp)
                           ] bodyV
    Just k ->
      case loopFlav lp of
        LoopMap -> hasType (hsType env (loopType lp))
                 $ "RTS.loopIMapM" `Ap` step `Ap` colV
          where step = Lam [ hsTCName env k
                           , hsTCName env (loopElName lp)
                           ] bodyV

        Fold x s -> "RTS.loopIFoldM" `Ap` step `Ap` initVal `Ap` colV
          where initVal = hsValue env s
                step = Lam [ hsTCName env x
                           , hsTCName env k
                           , hsTCName env (loopElName lp)
                           ] bodyV
  where
  colV    = hsValue   env (loopCol lp)
  bodyV   = hsGrammar env (loopBody lp)



--------------------------------------------------------------------------------

-- testing
hsModule :: CompilerCfg -> TCModule SourceRange -> Module
hsModule CompilerCfg { .. } TCModule { .. } = Module
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
                 ]
  , hsDecls = concatMap (hsTyDecl env) (concatMap recToList tcModuleTypes) ++
              concatMap (hsTCDecl env) (concatMap recToList tcModuleDecls)
  }
  where
  env = Env { envCurMod  = tcModuleName
            , envTParser = cParserType
            , envExtern  = cPrims
            , envQualNames = cQualNames
            , envTypes = Map.fromList [ (tctyName d, d)
                                      | d <- concatMap recToList tcModuleTypes ]
            }


