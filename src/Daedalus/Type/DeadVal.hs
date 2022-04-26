{-# Language GADTs, BlockArguments, OverloadedStrings, NamedFieldPuns #-}
{-# Language RecordWildCards #-}
module Daedalus.Type.DeadVal(ArgInfo, deadValModule) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe(catMaybes)
import Control.Monad(guard,zipWithM,liftM,ap,forM)
import Data.Parameterized.Some
import qualified Data.List.NonEmpty as NE

import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.Panic(panic)
import Daedalus.Pass
import Daedalus.GUID

import Daedalus.Type.RefreshGUID
import Daedalus.Type.AST
import Daedalus.Type.Free


{-
XXX: It might make more sense to that after specialization,
  1. we would handle function parameters better,
  2. the whole thing would be simpler
  3. we'd generate less code, as we'd only have the functions
     that are actually used.
-}

-- deadValModules :: [TCModule SourceRange] -> [TCModule SourceRange]
-- deadValModules = go [] Map.empty
--   where
--   go done known ms =
--     case ms of
--       [] -> reverse done
--       m : more ->
--         case deadValModule known m of
--           (m1, newKnown) -> go (m1 : done) newKnown more

deadValModule ::
  Map Name ArgInfo ->
  TCModule SourceRange ->
  PassM (TCModule SourceRange, Map Name ArgInfo)
deadValModule k0 mo = do
  (matchDecls,allKnown) <- go [] k0 (concatMap recToList (tcModuleDecls mo))
  pure (mo { tcModuleDecls = topoOrder matchDecls }, allKnown)
  where
  

  go :: [TCDecl SourceRange] ->
        Map Name ArgInfo ->
        [TCDecl SourceRange] ->
        PassM ([TCDecl SourceRange], Map Name ArgInfo)
  go done known ds =
    case ds of
      [] -> pure (done, known)
      d : more -> do
        m_d' <- declareMatchFun known d
        case m_d' of
          Just (d1,info) -> do
            let newKnown = Map.insert (tcDeclName d) info known
            cud <- cleanUpDecl newKnown d
            go (cud : d1 : done) newKnown more
          Nothing -> do cud <- cleanUpDecl known d
                        go (cud : done) known more

data ArgInfo = ArgInfo { matchFun       :: TCName Grammar
                       , passArg        :: [Bool]
                       , extraMatchArgs :: [Int]
                       }

newtype NoFunM a = NFM (RO -> RW -> PassM (a,RW))

instance Functor NoFunM where
  fmap = liftM

instance Applicative NoFunM where
  pure a = NFM \_ s -> pure (a,s)
  (<*>)  = ap

instance Monad NoFunM where
  NFM m >>= f = NFM \r s -> do
    v <- m r s
    case v of
      (a,s1) -> let NFM m1 = f a
                in m1 r s1

data RO = RO
  { roMatchFuns    :: Map Name ArgInfo
  , roChangeParams :: Bool
  }

newtype RW = RW
  { matchArgs :: Map (TCName Grammar) (TCName Grammar)
  }

runNoFunM :: Bool -> Map Name ArgInfo -> NoFunM a -> PassM (a, RW)
runNoFunM changePs r (NFM m) = m RO { roChangeParams = changePs
                                    , roMatchFuns = r
                                    }
                                 RW { matchArgs = Map.empty }
                               
instance HasGUID NoFunM where
  guidState f = NFM \_ rw -> (,) <$> guidState f <*> pure rw

attempt :: NoFunM (Maybe a) -> NoFunM a -> NoFunM a
attempt (NFM m1) (NFM m2) = NFM \r s -> do
  v <- m1 r s
  case v of
    (Just a, s1) -> pure (a, s1)
    (Nothing,_)  -> m2 r s

newMParam ::
  HasRange r => r -> TCName Grammar -> NoFunM (TC SourceRange Grammar)
newMParam r x = NFM \ro s ->
  if roChangeParams ro
    then case Map.lookup x (matchArgs s) of
           Just y  -> pure (exprAt r (TCVar y), s)
           Nothing -> do
             y <- erasedGrmName (tcName x)
             pure  ( exprAt r (TCVar y)
                   , s { matchArgs = Map.insert x y (matchArgs s) }
                   )
    else pure ( mkDo r Nothing (exprAt r (TCVar x)) (noSemPure r)
              , s)

-- FIXME: refresh
modifyName :: Ident -> Ident
modifyName i = "_" <> i

erasedGrmName :: HasGUID m => Name -> m (TCName Grammar)
erasedGrmName n = do
  newName <- deriveNameWith modifyName n
  pure TCName { tcNameCtx = AGrammar
              , tcType    = tGrammar tUnit
              , tcName    = newName
              }

lookupMatchFun :: TCName Grammar -> NoFunM (Maybe ArgInfo)
lookupMatchFun x = NFM \r s -> pure (Map.lookup (tcName x) (roMatchFuns r), s)

--------------------------------------------------------------------------------

type Info = Set (Some TCName)

cleanUpDecl :: Map Name ArgInfo -> TCDecl SourceRange -> PassM (TCDecl SourceRange)
cleanUpDecl mfs dcl =
  case dcl of
    TCDecl { tcDeclCtxt = AGrammar, tcDeclDef, .. } ->
      case tcDeclDef of
        ExternDecl _ -> pure dcl
        Defined d -> do
          r <- runNoFunM False mfs (mbSem d)
          case r of
            ((def,_),_) -> pure TCDecl { tcDeclDef = Defined def
                                       , tcDeclCtxt = AGrammar, .. }
    _ -> pure dcl


declareMatchFun ::
  Map Name ArgInfo -> TCDecl SourceRange -> PassM (Maybe (TCDecl SourceRange, ArgInfo))
declareMatchFun mfs dcl =
  case dcl of
    TCDecl { tcDeclCtxt = AGrammar, tcDeclDef, tcDeclAnnot } -> do
      newName <- erasedGrmName (tcDeclName dcl)
      case tcDeclDef of

        -- XXX: It seems that we could just leave this as is
        -- and just ignore the argments at the call site...
        ExternDecl _ -> do
          newDecl' <- refreshDecl newDecl
          pure (Just (newDecl', newInfo))
           where
           newDecl = TCDecl
                      { tcDeclName     = tcName newName
                      , tcDeclTyParams = tcDeclTyParams dcl
                      , tcDeclCtrs     = tcDeclCtrs dcl
                      , tcDeclImplicit = tcDeclImplicit dcl
                      , tcDeclParams   = tcDeclParams dcl
                      , tcDeclDef      = Defined newDef
                      , tcDeclCtxt     = AGrammar
                      , tcDeclAnnot    = tcDeclAnnot
                      }
           newInfo = ArgInfo { matchFun = newName
                             , passArg  = map (const True) (tcDeclParams dcl)
                             , extraMatchArgs = []
                             }
           loc     = range (tcDeclName dcl)
           newDef  = mkDo loc Nothing theCall (noSemPure loc)
           theCall = exprAt loc
                   $ TCCall oldName (map TVar (tcDeclTyParams dcl))
                                    (map mkArg (tcDeclParams dcl))
           mkArg p = case p of
                       GrammarParam x -> GrammarArg $ exprAt loc $ TCVar x
                       ClassParam   x -> ClassArg   $ exprAt loc $ TCVar x
                       ValParam     x -> ValArg     $ exprAt loc $ TCVar x

           oldName = TCName { tcNameCtx = AGrammar
                            , tcName    = tcDeclName dcl
                            , tcType    = typeOf tcDeclDef
                            }


        Defined d -> do
          r <- runNoFunM True mfs (noSem' d)
          case r of
            ((def,vs),rw) -> do
              newDecl' <- refreshDecl newDecl
              pure (Just (newDecl', newInfo))
              where
              pused p = pname p `Set.member` vs

              pMap = Map.fromList $ zip (map pname (tcDeclParams dcl)) [ 0 .. ]

              pname p = case p of
                          ClassParam x   -> Some x
                          ValParam x     -> Some x
                          GrammarParam x -> Some x

              newDecl = TCDecl
                          { tcDeclName     = tcName newName
                          , tcDeclTyParams = tcDeclTyParams dcl
                          , tcDeclCtrs     = tcDeclCtrs dcl
                          , tcDeclImplicit = [] -- assumes this is post TC
                          , tcDeclParams   = newParams
                          , tcDeclDef      = Defined def
                          , tcDeclCtxt     = AGrammar
                          , tcDeclAnnot    = tcDeclAnnot
                          }

              newParams = [ p | p <- tcDeclParams dcl, pused p ] ++
                          [ GrammarParam nm | nm <- Map.elems (matchArgs rw) ]

              badPMap x = panic "pMap error" $
                            [ "Function: " ++ show (pp (tcDeclName dcl))
                            , "Param: " ++ show (pp x)
                            , "PMAP"
                            ] ++
                            [ "  " ++ show (pp v) ++ " := " ++ show y
                                  | (Some v,y) <- Map.toList pMap ]


              newInfo = ArgInfo
                          { matchFun = newName
                          , passArg = map pused (tcDeclParams dcl)
                          , extraMatchArgs =
                              [ case Map.lookup (Some x) pMap of
                                  Just a -> a
                                  Nothing -> badPMap x
                              | x <- Map.keys (matchArgs rw)
                              ]
                          }

    _ -> pure Nothing




-- | Look for opportunities within a grammar to use matching instead of parsing
mbSem :: TC SourceRange Grammar -> NoFunM (TC SourceRange Grammar, Info)
mbSem tc =
  case texprValue tc of
    TCFail e _ -> pure (tc, tcFree e)

    TCPure e -> pure (tc, tcFree e)

    TCDo mb m1 m2 ->
      do (m2',i2) <- mbSem m2
         (mb',m1',i1) <- case mb of
                           Just x | Some x `Set.member` i2 ->
                                do (m1',i1) <- mbSem m1
                                   pure (Just x,m1',i1)

                           _ -> do (m1',i1) <- noSem' m1
                                   pure (Nothing, m1',i1)
         pure (mkDo tc mb' m1' m2', i1 <> i2)

    TCLabel l m ->
      do (m',i) <- mbSem m
         pure (exprAt tc (TCLabel l m'),i)

    TCGetByte {}      -> pure (tc, tcFree tc)
    TCMatch {}        -> pure (tc, tcFree tc)
    TCMatchBytes {}   -> pure (tc, tcFree tc)
    TCEnd             -> pure (tc, tcFree tc)
    TCOffset          -> pure (tc, tcFree tc)
    TCCurrentStream   -> pure (tc, tcFree tc)
    TCSetStream {}    -> pure (tc, tcFree tc)
    TCStreamLen _ e s -> pure (tc, tcFree e <> tcFree s)
    TCStreamOff _ e s -> pure (tc, tcFree e <> tcFree s)

    TCMapLookup {}    -> pure (tc, tcFree tc)
    TCMapInsert {}    -> pure (tc, tcFree tc)
    TCArrayIndex  {}  -> pure (tc, tcFree tc)

    TCCoerceCheck {}  -> pure (tc, tcFree tc)
    TCVar {}          -> pure (tc, tcFree tc)

    TCChoice c ms t -> do (ms',is) <- unzip <$> mapM mbSem ms
                          pure (exprAt tc (TCChoice c ms' t), mconcat is)

    TCOptional c m ->
      do (m',i) <- mbSem m
         pure (exprAt tc (TCOptional c m'), i)

    TCMany s c b m ->
      do (m',i) <- mbSem m
         pure (exprAt tc (TCMany s c b m'), tcFree b <> i)

    TCFor lp  ->
      do (m',i) <- mbSem (loopBody lp)
         let bnd = tcBinds (loopFlav lp)

         pure ( exprAt tc (TCFor lp { loopBody = m' })
              , tcFree (loopFlav lp) <>
                Set.difference i bnd
              )

    TCCall f ts as ->
      do (as',is) <- unzip <$> forM as \a ->
                       case a of
                         GrammarArg m ->
                            do (m',i) <- mbSem m
                               pure (GrammarArg m', i)
                         ClassArg {} -> pure (a, tcFree a)
                         ValArg {}   -> pure (a, tcFree a)

         let extra = if isLocalName (tcName f) then Set.singleton (Some f)
                                               else mempty
         pure (exprAt tc (TCCall f ts as'), extra <> mconcat is)

    TCErrorMode m p -> do (p', vs) <- mbSem p
                          pure (exprAt tc (TCErrorMode m p'), vs)

    TCIf e e1 e2 ->
      do (e1',vs1) <- mbSem e1
         (e2',vs2) <- mbSem e2
         pure (exprAt tc (TCIf e e1' e2'), mconcat [ tcFree e, vs1, vs2 ])

    TCCase e pats mb ->
      do (pats1,vs1) <- NE.unzip <$> mapM (noSemAltWith mbSem) pats
         mb1 <- traverse mbSem mb
         case mb1 of
           Nothing -> pure ( exprAt tc $ TCCase e pats1 Nothing
                           , Set.unions (tcFree e : NE.toList vs1)
                           )
           Just (d,vs2) -> pure ( exprAt tc $ TCCase e pats1 (Just d)
                                , Set.unions (tcFree e : vs2 : NE.toList vs1)
                                )


-- | Rewrite productions to avoid constructing a semantic value.
-- It also returns the variables actually used by the rewritten expression.
noSem' :: TC SourceRange Grammar -> NoFunM (TC SourceRange Grammar, Info)
noSem' tc =
  case texprValue tc of

     TCFail e _ -> pure (exprAt tc (TCFail e tUnit), tcFree e)

     TCPure _ -> pure (noSemPure tc, mempty)

     TCDo mb m1 m2 ->
       do (m2',i2) <- noSem' m2
          case mb of
            Just x | Some x `Set.member` i2 ->
              do (m1',i1) <- mbSem m1
                 pure (mkDo tc (Just x) m1' m2', i1 <> i2)

            _ -> do (m1',i1) <- noSem' m1
                    pure (mkDo tc Nothing m1' m2', i1 <> i2)

     TCLabel l m ->
       do (m',i) <- noSem' m
          pure (exprAt tc (TCLabel l m'), i)

     TCGetByte _      -> pure (exprAt tc (TCGetByte NoSem),   mempty)
     TCMatch _ c      -> pure (exprAt tc (TCMatch   NoSem c), tcFree c)
     TCMatchBytes _ v -> pure (exprAt tc (TCMatchBytes NoSem v), tcFree v)

     TCChoice c ms _ ->
       do (ms',is) <- unzip <$> mapM noSem' ms
          pure (noSemUni tc c ms', mconcat is)

     TCOptional c m ->
       do (m', i) <- noSem' m
          pure (noSemUni tc c [ m', noSemPure tc ], i)

     TCMany _ c bs m ->
       do (m', i) <- noSem' m
          pure (exprAt tc (TCMany NoSem c bs m'), tcFree bs <> i)

     TCEnd    -> pure (tc, mempty)
     TCOffset -> pure (noSemPure tc, mempty)
     TCCurrentStream -> pure (noSemPure tc, mempty)
     TCSetStream s   -> pure ( exprAt tc (TCSetStream s), tcFree tc)

     TCStreamLen _ e s -> pure ( exprAt tc (TCStreamLen NoSem e s)
                               , tcFree e <> tcFree s
                               )
     TCStreamOff _ e s -> pure ( exprAt tc (TCStreamLen NoSem e s)
                               , tcFree e <> tcFree s
                               )

     TCMapLookup _ k mp ->
      pure (exprAt tc (TCMapLookup NoSem k mp), tcFree k <> tcFree mp)

     TCMapInsert _ k v mp ->
      pure ( exprAt tc (TCMapInsert NoSem k v mp)
           , tcFree k <> tcFree v <> tcFree mp )

     TCArrayIndex _ e ix ->
      pure (exprAt tc (TCArrayIndex NoSem e ix), tcFree e <> tcFree ix)

     TCCoerceCheck _ t1 t2 v ->
      pure ( exprAt tc (TCCoerceCheck NoSem t1 t2 v), tcFree v )

     TCErrorMode m p -> do (p',is) <- noSem' p
                           pure (exprAt tc (TCErrorMode m p'), is)


     TCFor lp ->
       attempt
         do (m', i) <- noSem' (loopBody lp)
            case loopFlav lp of
              -- map (i,x in xs) e
              -- ~>
              -- for (i,x in xs) e'
              LoopMap _ -> pure Nothing -- XXX

              Fold x _s col ->

                -- fold body has not effect, and we don't care about result
                case texprValue m' of
                  TCPure {} ->
                    pure $ pure ( exprAt tc (TCPure (exprAt tc tcUnit))
                                , Set.empty
                                )
                  _ ->
                    pure do guard (not (Some x `Set.member` i)) -- state unused

                            let v' = x { tcType = tUnit }
                                lp' = Loop { loopFlav = Fold v'
                                                          (exprAt tc tcUnit) col
                                           , loopBody = m'
                                           , loopType = tGrammar tUnit
                                           }
                            pure ( exprAt tc (TCFor lp')
                                 , tcFree (loopFlav lp) <> Set.difference i bnd
                                 )

         do (m'', i') <- mbSem (loopBody lp)
            pure ( mkDo tc Nothing
                        (exprAt tc (TCFor lp { loopBody = m'' })) (noSemPure tc)
                 , tcFree (loopFlav lp) <> Set.difference i' bnd
                 )

       where bnd = tcBinds (loopFlav lp)

     TCVar x ->
       case typeOf x of
         Type (TGrammar (Type TUnit)) -> pure (tc, mempty)
         _ -> do e <- newMParam tc x
                 pure (e, mempty)

     TCCall f ts as ->
       do mbArgInfo <- lookupMatchFun f
          case mbArgInfo of
            Nothing ->
              pure ( mkDo tc Nothing tc (noSemPure tc)
                   , if isLocalName (tcName f)
                         then Set.singleton (Some f) <> tcFree as
                         else tcFree as
                   )

            Just argInfo ->
              do let argMap = Map.fromList (zip [ 0 .. ] as)
                     extraArg i =
                        case Map.lookup i argMap of
                          Just (GrammarArg e) -> do (e',i') <- noSem' e
                                                    pure (GrammarArg e', i')
                          _ -> error "missing/wrong arg"

                     normalArg e yes
                       | yes = case e of
                                 GrammarArg e' -> do (e'',i) <- mbSem e'
                                                     pure (Just (GrammarArg e'',i))
                                 ValArg e' -> pure (Just (ValArg e', tcFree e'))
                                 ClassArg e' -> pure (Just (ClassArg e', tcFree e'))

                       | otherwise = pure Nothing

                 (as1,is1) <- unzip . catMaybes <$>
                                        zipWithM normalArg as (passArg argInfo)
                 (as2,is2) <- unzip <$> mapM extraArg (extraMatchArgs argInfo)
                 pure ( exprAt tc (TCCall (matchFun argInfo) ts (as1 ++ as2))
                      , mconcat (is1 ++ is2)
                      )

     TCIf e e1 e2 ->
        do (e1',vs1) <- noSem' e1
           (e2',vs2) <- noSem' e2
           pure (exprAt tc (TCIf e e1' e2'), Set.unions [ tcFree e, vs1, vs2 ])

     TCCase e pats mb ->
       do (pats1,vs1) <- NE.unzip <$> mapM (noSemAltWith noSem') pats
          mb1 <- traverse noSem' mb
          case mb1 of
            Nothing -> pure ( exprAt tc $ TCCase e pats1 Nothing
                            , Set.unions (tcFree e : NE.toList vs1)
                            )
            Just (d,vs2) -> pure ( exprAt tc $ TCCase e pats1 (Just d)
                                 , Set.unions (tcFree e : vs2 : NE.toList vs1)
                                 )




noSemAltWith ::
  (TC SourceRange Grammar -> NoFunM (TC SourceRange Grammar, Info)) ->
  TCAlt SourceRange Grammar -> NoFunM (TCAlt SourceRange Grammar, Info)
noSemAltWith f (TCAlt ps e) =
  do (e1,vs) <- f e
     let bound = Set.map Some (patBindsSet (head ps))
     pure (TCAlt ps e1, Set.difference vs bound)

noSemUni ::
  HasRange r =>
  r ->
  Commit ->
  [TC SourceRange Grammar] ->
  TC SourceRange Grammar
noSemUni tc c ms = exprAt tc (TCChoice c ms tUnit)

noSemPure :: HasRange r => r -> TC SourceRange Grammar
noSemPure tc = exprAt tc (TCPure (exprAt tc tcUnit))

mkDo :: HasRange r =>
        r ->
        Maybe (TCName Value) ->
        TC SourceRange Grammar ->
        TC SourceRange Grammar ->
        TC SourceRange Grammar
mkDo r x m1 m2
  | Type (TGrammar (Type TUnit)) <- typeOf m1
  , TCPure e <- texprValue m2
  , isTCUnit (texprValue e)
    = m1

  | Nothing <- x
  , TCPure _ <- texprValue m1
    = m2

  | otherwise = exprAt r (TCDo x m1 m2)
