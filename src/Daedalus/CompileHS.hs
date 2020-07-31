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

import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.Panic

import Daedalus.Type.AST
import Daedalus.Compile.LangHS
import Daedalus.Compile.Config


data Env = Env
  { envTParser    :: Term
  , envCurMod     :: ModuleName
  , envExtern     :: Map Name Term
  , envQualNames  :: UseQual
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
  case nameScope nm of
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
        TUnit       -> Tuple []
        TArray t    -> "Vector.Vector" `Ap` hsType env t
        TMaybe t    -> "HS.Maybe" `Ap` hsType env t
        TMap k t    -> "Map.Map" `Ap` hsType env k `Ap` hsType env t


hsConstraint :: Env -> Constraint -> Term
hsConstraint env ctr =
  case ctr of
    Numeric t -> "RTS.Numeric" `Ap` hsType env t

    HasStruct t l a ->
      aps "RTS.HasStruct" [ hsType env t, hsLabelT l, hsType env a ]

    HasUnion t l a ->
      aps "RTS.HasUnion" [ hsType env t, hsLabelT l, hsType env a ]

    Literal i t ->
      "RTS.Literal" `Ap` hsType env (Type (TNum i)) `Ap` hsType env t

    CAdd x y z   -> ApI "~" (ApI "+" (hsType env x) (hsType env y))
                            (hsType env z)

    Coerce _ a b -> "RTS.Convert" `Ap` hsType env a `Ap` hsType env b

    Traversable t -> "RTS.IsLoop" `Ap` hsType env t
    Mappable s t  -> "RTS.IsMapLoop" `Ap` hsType env s `Ap` hsType env t

    ColElType s t  -> "RTS.ColElType" `Ap` hsType env s `Ap` hsType env t
    ColKeyType s t -> "RTS.ColElType" `Ap` hsType env s `Ap` hsType env t

    TyDef {}        -> panic "hsConstraint" ["Unexpected TyDef"]
    IsNamed {}      -> panic "hsConstraint" ["Unexpected IsNamed"]

hsRuleType :: Env -> RuleType -> Term
hsRuleType env (xs :-> y) = foldr fun (hsType env y) xs
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
hsTyDeclDef env me@TCTyDecl { .. } =
  case tctyDef of
    TCTyStruct fs ->
      ( [ aps (hsStructConName env NameDecl tctyName) (map snd fts) ]
      , concatMap hasInst fts
      )
      where
      fts = map fldT fs
      hasInst (f,t) =
        let pat = hsStructConName env NameUse tctyName `aps`
                     [ if f == fst fi then "x" else "_" | fi <- fts ]
            st = hsThisTy env me
            ft = hsLabelT f

        in [ declare Instance
             { instAsmps = []
             , instHead  = "HS.HasField" `Ap` ft `Ap` st `Ap` t
             , instMethods = [ Fun ("getField" `Ap` pat) "x" ]
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
              { instAsmps = []
              , instHead  = "HS.HasField" `Ap` ft `Ap` st `Ap`
                                                          ("HS.Maybe" `Ap` t)
              , instMethods =
                  let pat = hsUniConName env NameUse tctyName f `Ap` "x"
                  in [ Fun ("getField" `Ap` pat) ("HS.Just" `Ap` "x")
                     , Fun ("getField" `Ap` "_") "HS.Nothing"
                     ]
              }
          ]
        )

  where
  fldT (f,t) = (f, hsType env t)

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

  derivCtx c = [ case tvarKind p of
                   KNumber -> "RTS.SizeType" `Ap` a
                   KValue  -> c `Ap` a
                   k       -> panic "hsTyDecl" ["Unexpected kind", show (pp k) ]
               | p <- tctyParams, let a = hsTyVar p ]

  derive c = declare Deriving
                      { deriveAsmps = derivCtx c
                      , deriveHead  = c `Ap` hsThisTy env me
                      }

  ddlT = declare Instance
          { instAsmps = concatMap extraCtrs tctyParams
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
          { funLHS = nm `aps` map (hsParam env) tcDeclParams
          , funDef = defRHS
          }

  defRHS = case tcDeclDef of
             ExternDecl t -> hasType (hsType env t)
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
    TCNumber n t -> hasType (hsType env t) ("RTS.lit" `Ap` hsInteger n)
    TCBool b     -> hsBool b
    TCNothing t  -> hasType ("HS.Maybe" `Ap` hsType env t) "HS.Nothing"
    TCJust e    -> "HS.Just" `Ap` hsValue env e
    TCByte b    -> "RTS.uint8" `Ap` hsWord8 b
    TCUnit      -> Tuple []
    TCStruct fs t ->
      case t of
        TCon c _ -> hsStructConName env NameUse c `aps`
                          [ hsValue env e | (_,e) <- fs ]
        _ -> panic "hsValue" ["Unexpected type in `TCStruct`"]


    TCByteArray b -> "Vector.vecFromRep" `Ap` hsByteString b

    TCArray vs t  ->
      case vs of
        [] -> hasType (hsType env (tArray t)) "Vector.empty"
        _  -> "Vector.fromList" `Ap` List (map (hsValue env) vs)

    TCIn l v t ->
      case t of
        TCon c _ -> hsUniConName env NameUse c l `Ap` hsValue env v
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
      where
      bin x = x `Ap` hsValue env v1 `Ap` hsValue env v2
      binI x = ApI x (hsValue env v1) (hsValue env v2)

    TCUniOp op v ->
      case op of
        Not               -> "HS.not" `Ap` hsValue env v
        Neg               -> "RTS.neg" `Ap` hsValue env v
        BitwiseComplement -> "RTS.bitCompl" `Ap` hsValue env v
        Concat            -> "Vector.concat" `Ap` hsValue env v

    TCVar x -> hsValName env NameUse (tcName x)
    TCCall f ts as -> hsApp env f ts as

    TCIf v x y -> If (hsValue env v) (hsValue env x) (hsValue env y)

    TCSelStruct x l _t -> "HS.getField" `Ap` TyParam (hsLabelT l)
                                        `Ap` hsValue env x

    TCCoerce _t1 t2 v -> hasType (hsType env t2)
                                 ("RTS.convert" `Ap` hsValue env v)

    TCFor lp -> evalFor env lp

    TCMapEmpty t -> hasType (hsType env t) "Map.empty"
    TCArrayLength e -> "HS.toInteger" `Ap` ("Vector.length" `Ap` hsValue env e)

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

     TCGuard v -> "RTS.pGuard" `Ap` erng `Ap` hsText "guard failed"
                                         `Ap` hsValue env v

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


     TCOffset -> ApI "HS.<$>" "HS.toInteger" "RTS.pOffset"

     TCCall f ts as -> "RTS.pEnter" `Ap` hsText (Text.pack (show (pp f)))
                                    `Ap` hsApp env f ts as

     TCSelJust sem val _t -> hsMaybe sem erng (hsText "Expected `Just`")
                                              (hsValue env val)
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

     TCSelUnion sem v l _ ->
        hsMaybe sem erng (hsText ("Expected `" <> l <> "`"))
                         ("HS.getField" `Ap` TyParam (hsLabelT l)
                                        `Ap` hsValue env v)

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
                 ]
  , hsImports  = cImports ++
                 [ Import (hsIdentMod i) Qualified
                            | i <- map thingValue tcModuleImports
                 ] ++
                 [ Import "Prelude"       (QualifyAs "HS")
                 , Import "GHC.TypeLits"  (QualifyAs "HS")
                 , Import "GHC.Records"   (QualifyAs "HS")
                 , Import "Control.Monad" (QualifyAs "HS")
                 , Import "RTS"           (QualifyAs "RTS")
                 , Import "RTS.Input"     (QualifyAs "RTS")
                 , Import "RTS.Map"       (QualifyAs "Map")
                 , Import "RTS.Vector"    (QualifyAs "Vector")
                 ]
  , hsDecls = concatMap (hsTyDecl env) (concatMap recToList tcModuleTypes) ++
              concatMap (hsTCDecl env) (concatMap recToList tcModuleDecls)
  }
  where
  env = Env { envCurMod  = tcModuleName
            , envTParser = cParserType
            , envExtern  = cPrims
            , envQualNames = cQualNames
            }


