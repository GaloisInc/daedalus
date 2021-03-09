{-# Language ImplicitParams, ConstraintKinds, OverloadedStrings #-}
{-# Language BlockArguments #-}
-- | Validate types, for sanity.
module Daedalus.Core.TypeCheck where

import Data.Map(Map)
import qualified Data.Map as Map

import Daedalus.PP
import Daedalus.Core
import Daedalus.Core.Type

type TCResult  = Either Doc
type TypeError = Doc
type GEnv      = (?tenv :: Map TName TDecl)
type Env       = Map Name Type


typeError :: Doc -> [Doc] -> TCResult a
typeError x xs = Left (hang x 2 (bullets xs))

extEnv :: Name -> Type -> Env -> Env
extEnv = Map.insert

getUserType :: GEnv => UserType -> TCResult TDef
getUserType ut =
  do def <- case Map.lookup (utName ut) ?tenv of
              Nothing -> bad "Undefined user type:" ""
              Just d  -> pure (tDef d)
     case def of
       TStruct fs -> TStruct <$> mapM instField fs
       TUnion  fs -> TUnion  <$> mapM instField fs
  where
  instField (l,t) = do t1 <- instType t
                       pure (l,t1)
  instType t =
    case t of
      TParam (TP n) -> case drop n (utTyArgs ut) of
                         a : _ -> pure a
                         _ -> bad "Undefined type parmaeter:"
                                  ("Parameter:" <+> int n)
      TStream     -> pure t
      TUInt sz    -> TUInt <$> instSizeType sz
      TSInt sz    -> TSInt <$> instSizeType sz
      TInteger    -> pure t
      TBool       -> pure t
      TUnit       -> pure t
      TArray t    -> TArray <$> instType t
      TMaybe t    -> TMaybe <$> instType t
      TMap t1 t2  -> TMap <$> instType t1 <*> instType t2
      TBuilder t  -> TBuilder <$> instType t
      TIterator t -> TIterator <$> instType t
      TUser u     ->
        do ns <- mapM instSizeType (utNumArgs u)
           ts <- mapM instType     (utTyArgs u)
           pure (TUser u { utNumArgs = ns, utTyArgs = ts })

  instSizeType ty =
    case ty of
      TSize _ -> pure ty
      TSizeParam (TP n) ->
        case drop n (utNumArgs ut) of
          a : _ ->
            case a of
              TSize _ -> pure a
              _       -> bad "Uninstantiated numeric parameter"
                             ("Numeric parametr #:" <+> int n)
          _ -> bad "Undefined numeric parametr"
                             ("Numeric parametr #:" <+> int n)

  bad a b = typeError a [ "Type:" <+> pp (utName ut), b ]


checkExpr :: GEnv => Expr -> Env -> TCResult Type
checkExpr expr env =
  case expr of
    Var x ->
      let t1 = typeOf x
      in case Map.lookup x env of
           Just t | t == t1 -> pure t1
                  | otherwise ->
                    typeError "Type mistmatch:"
                              [ "Variable:" <+> backticks (pp x)
                              , "Expected:" <+> pp t
                              , "Actual:"   <+> pp t1
                              ]
           Nothing -> typeError "Undefined variable:"
                                [ "Variable:" <+> backticks (pp x) ]
    PureLet x e1 e2 ->
      do t <- checkExpr e1 env
         checkExpr e1 (extEnv x t env)

    Struct ut fields ->
      do def     <- getUserType ut
         dFields <- case def of
                      TStruct fs -> pure fs
                      TUnion _ -> typeError "Type mistmach:"
                                    [ "Expected: struct"
                                    , "Actual: union"
                                    ]
         let checkField (l,e) =
               do t <- checkExpr e env
                  case lookup l dFields of
                    Nothing -> typeError "Malformed struct:"
                                  [ "Extra field:" <+> backticks (pp l) ]
                    Just t1
                      | t == t1 -> typeError "Type mismtach:"
                                      [ "Field:" <+> backticks (pp l)
                                      , "Expected:" <+> pp t1
                                      , "Actual:" <+> pp t
                                      ]
         case [ l | (l,_) <- dFields, not (l `elem` map fst fields) ] of
           []    -> pure ()
           l : _ -> typeError "Missing field:" [ "Field:" <+> backticks (pp l) ]
         pure (TUser ut)

    ECase c -> checkCase checkExpr c env

    -- XXX: what's a good way to check these?
    Ap0 op0 -> undefined
    Ap1 op1 e -> undefined
    Ap2 op2 e1 e2 -> undefined
    Ap3 op3 e1 e2 e3 -> undefined
    ApN opN es -> undefined


checkCase ::
  GEnv => (a -> Env -> TCResult Type) -> Case a -> Env -> TCResult Type
checkCase = undefined


