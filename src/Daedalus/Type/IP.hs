{-# Language BlockArguments #-}
{-# Language ImplicitParams #-}
{-# Language NamedFieldPuns #-}
{-# Language GADTs #-}
module Daedalus.Type.IP where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map

import Daedalus.Panic(panic)
import Daedalus.PP(pp)
import Daedalus.GUID(HasGUID)

import Daedalus.Pass
import Daedalus.AST
import Daedalus.Type.Monad


resolveRuleIPs :: Map Name [IPName] -> [Rule] -> STypeM [(Rule,[IPName])]
resolveRuleIPs extIps rs =
  do mapM_ (addWarning . WarnUnusiedIP) (concatMap Set.toList (Map.elems errs))
     mapM rewRule rs
  where

  ruleIPs r = (ruleName r, case ruleDef r of
                             Nothing -> UseIPs Set.empty
                             Just e  -> getIPs e)
  thisIPs   = Map.fromList (map ruleIPs rs)

  extSet    = Set.fromList <$> extIps
  thisSet   = fixIPs extSet thisIPs
  thisList  = Set.toList <$> thisSet
  allList   = Map.union thisList extIps

  -- XXX: report
  errs      = let ?funs = Map.union thisSet extSet
                  ?cur  = Set.empty
              in findUnused . externalIPs <$> thisIPs
  rewRule r =
    case Map.lookup (ruleName r) allList of
      Just is ->
        do xs <- mapM newIP is
           let toParam x = RuleParam { paramName = x, paramType = Nothing }
               toIP x y  = (x, pExprAt (ruleName r) (EVar y))
               ipMap     = Map.fromList (zipWith toIP is xs)
               doExpr    = let ?funs = allList
                           in makeExplicit ipMap

           def <- traverse doExpr (ruleDef r)
           pure ( r { ruleParams = map toParam xs ++ ruleParams r
                    , ruleDef = def
                    }
                , is
                )

      Nothing -> panic "resolveRuleIPs" ["Missing IPs", show (pp (ruleName r)) ]

  newIP IPName { ipName, ipContext, ipRange } =
    do y <- freshLocalName ipName ipContext
       pure y { nameRange = ipRange }

--------------------------------------------------------------------------------

data IPs = DefIP IPName IPs | UseIPs (Set IPName) | PassIPsTo Name
         | AndIPs IPs IPs

instance Monoid IPs where
  mempty = UseIPs Set.empty

instance Semigroup IPs where
  xs <> ys = case (xs,ys) of
               (UseIPs is, UseIPs js) -> UseIPs (Set.union is js)
               _                      -> AndIPs xs ys

getIPs :: Expr -> IPs
getIPs expr =
  case exprValue expr of
    EImplicit x -> UseIPs (Set.singleton x)
    EStruct fs  -> getStructIPs fs
    EVar x      -> PassIPsTo x
    EApp f es   -> PassIPsTo f <> foldMap getIPs es
    other       -> foldMap getIPs other

getStructIPs :: [StructField Expr] -> IPs
getStructIPs fs =
  case fs of
    f : more ->
      foldMap getIPs f <>
      case f of
        x :?= _ -> DefIP x (getStructIPs more)
        _       -> getStructIPs more
    [] -> mempty

--------------------------------------------------------------------------------

-- | Find implicit parameters that were defined but not used
-- Assumes that functions have been resolved.
findUnused :: IPs -> Set IPName
findUnused = fst . go
  where
  go ips =
    case ips of
      DefIP x is ->
        let (bad,used) = go is
        in if not (x `Set.member` used)
              then (Set.insert x bad, used)
              else (bad, Set.delete x used)
      AndIPs xs ys ->
        let (as,is) = go xs
            (bs,js) = go ys
        in (Set.union as bs, Set.union is js)
      UseIPs xs -> (Set.empty, xs)
      PassIPsTo f -> panic "findUnused" [ "Unresolved funciton", show (pp f) ]


-- | Lookup the implicit parameters of a function.
-- If we don't have an entry we return the empty set, which should deal
-- with local variables, etc.
lookupIPs :: (?funs :: Map Name (Set IPName)) => Name -> Set IPName
lookupIPs f = Map.findWithDefault Set.empty f ?funs

-- | Given the implicit parameters of functions, compute the
-- concrete set of implicit parameters.
ipsToSet :: (?funs :: Map Name (Set IPName)) => IPs -> Set IPName
ipsToSet ips =
  case ips of
    AndIPs x y  -> Set.union (ipsToSet x) (ipsToSet y)
    DefIP x is  -> Set.delete x (ipsToSet is)
    UseIPs xs   -> xs
    PassIPsTo f -> lookupIPs f

-- | Resolve the implicit parameters for functions outside the current SCC.
externalIPs :: (?funs :: Map Name (Set IPName), ?cur :: Set Name) => IPs -> IPs
externalIPs ips =
  case ips of
    AndIPs xs ys -> externalIPs xs <> externalIPs ys
    DefIP x is   -> DefIP x (externalIPs is)
    UseIPs {}    -> ips
    PassIPsTo f
      | f `Set.member` ?cur -> ips
      | otherwise           -> UseIPs (lookupIPs f)

-- | Compute a fixed point for the implicit parameters of an SCC
fixIPs :: Map Name (Set IPName) -> Map Name IPs -> Map Name (Set IPName)
fixIPs external defs0 = loop initS
  where
  defs :: Map Name IPs
  defs = let ?funs = external
             ?cur  = Map.keysSet defs0
          in externalIPs <$> defs0

  step :: Map Name (Set IPName) -> Map Name (Set IPName)
  step s = let ?funs = s
           in ipsToSet <$> defs

  initS :: Map Name (Set IPName)
  initS = const Set.empty <$> defs

  loop :: Map Name (Set IPName) -> Map Name (Set IPName)
  loop s = let s1 = step s
           in if s == s1 then s else loop s1

makeExplicit :: (?funs :: Map Name [IPName], HasGUID m) =>
  Map IPName Expr -> Expr -> m Expr
makeExplicit ips expr =
   case exprValue expr of

    EVar f ->
      pure
      case Map.lookup f ?funs of
        Nothing -> expr
        Just is -> case is of
                     [] -> expr
                     _  -> pExprAt expr (EApp f (map resolveIP is))

    EImplicit x -> pure (resolveIP x)

    EApp f es ->
      pExprAt expr <$>
      case Map.lookup f ?funs of
        Nothing -> EApp f <$> traverse (makeExplicit ips) es
        Just is ->
          EApp f . (map resolveIP is ++) <$> traverse (makeExplicit ips) es

    EStruct fs -> pExprAt expr . EStruct <$> makeStructExplicit ips fs

    other -> pExprAt expr <$> traverse (makeExplicit ips) other

  where
  resolveIP x =
    case Map.lookup x ips of
      Just e  -> case exprValue e of
                   EVar y -> pExprAt x (EVar y { nameRange = ipRange x })
                   _      -> e
      Nothing -> panic "makeExplicit"
                       [ "Missing implicit parameter", show (pp x) ]




makeStructExplicit ::
  (?funs :: Map Name [IPName], HasGUID m) =>
  Map IPName Expr -> [StructField Expr] -> m [StructField Expr]
makeStructExplicit ips fs =
  case fs of
    f : more ->
      case f of

        x@IPName { ipName, ipContext } :?= e ->
          case ipContext of
            AValue ->
              do y  <- freshLocalName ipName AValue
                 e' <- doExpr e
                 let def = pExprAt x (EVar y)
                 rest' <- makeStructExplicit (Map.insert x def ips) more
                 pure ( (y :@= e') : rest')
            _ -> do e' <- doExpr e
                    makeStructExplicit (Map.insert x e' ips) more

        _ -> do f'    <- traverse doExpr f
                more' <- makeStructExplicit ips more
                pure (f' : more')

    [] -> pure []
  where
  doExpr = makeExplicit ips


