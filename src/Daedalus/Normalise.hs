{-# LANGUAGE GADTs, OverloadedStrings, RecordWildCards, RankNTypes #-}
module Daedalus.Normalise where

import Daedalus.Panic
import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.GUID

import Daedalus.Type.AST
import Daedalus.Normalise.AST

-- Essentially just forgets type variables
ntype :: Type -> NType
ntype (Type t)     = NType (ntype <$> t)
ntype (TCon tc ts) = NTCon tc (map ntype ts)
ntype (TVar {})    = panic "ntype" ["expecting a non-type variable type"]



-- Should be a Value (?)

nname :: TCName k -> NName
nname tn = NName (tcName tn) (ntype $ tcType tn)

normaliseArg :: Arg a -> NVExpr
normaliseArg (ValArg e) = normaliseV e
normaliseArg _ = panic "expecting a ValArg" []

normalise :: TCDecl a -> NDecl
normalise d@(TCDecl {..}) =
  NDecl tcDeclName (map mkParam tcDeclParams)
                   (ntype (typeOf tcDeclDef))
                   (mkBody tcDeclCtxt tcDeclDef)
  where
    mkParam (ValParam p) = nname p
    mkParam _            = panic "expecting a Value parameter" ["at " ++ show (pp d)]

    mkBody :: forall a k'. Context k' -> TCDeclDef a k' -> NDeclBody
    mkBody AClass (Defined tc)   = NCDecl (normaliseC tc)
    mkBody AValue (Defined tc)   = NVDecl (normaliseV tc)
    mkBody AGrammar (Defined tc) = NGDecl (normaliseG tc)
    mkBody _ (ExternDecl _)      = NExtern

normaliseV :: TC a Value -> NVExpr
normaliseV tc =
  case texprValue tc of
    TCMapEmpty t -> NMapEmpty (ntype t)
    TCCoerce t t' v -> NCoerce (ntype t) (ntype t') (normaliseV v)

    TCLiteral (LNumber n) t -> NNumber n (ntype t)
    TCLiteral (LBool b)   _ -> NBool b
    TCLiteral (LByte b)   _ -> NByte b
    TCLiteral (LBytes bs) _ -> NByteArray bs

    TCNothing t     -> NNothing (ntype t)
    TCJust e        -> NJust (normaliseV e)
    TCUnit          -> NUnit
    TCStruct fs t   -> NStruct [(l, normaliseV v) | (l, v) <- fs] (ntype t)
    TCArray vs t    -> NArray (map normaliseV vs) (ntype t)
    TCArrayLength v -> NArrayLength (normaliseV v)
    TCIn l v t      -> NIn l (normaliseV v) (ntype t)
    TCTriOp op v1 v2 v3 t ->
      NTriOp op (normaliseV v1) (normaliseV v2) (normaliseV v3) (ntype t)
    TCBinOp op v v' t -> NBinOp op (normaliseV v) (normaliseV v') (ntype t)
    TCUniOp op v    -> NUniOp op (normaliseV v)

    TCFor lp ->
      case loopFlav lp of
        LoopMap ->
          NVMap (nname <$> loopKName lp)
                (nname (loopElName lp))
                (normaliseV (loopCol lp))
                (normaliseV (loopBody lp))

        Fold x s ->
          NVFor (nname x)
                (normaliseV s)
                (nname <$> loopKName lp)
                (nname (loopElName lp))
                (normaliseV (loopCol lp))
                (normaliseV (loopBody lp))

    TCSelStruct v n t -> NSelStruct (normaliseV v) n (ntype t)
    TCIf b te fe      -> NIf (normaliseV b) (normaliseV te) (normaliseV fe)
    TCVar v           -> NVar (nname v)
    TCCall f [] args   -> NVCall (nname f) (map normaliseArg args)
    TCCall _ _ _       -> panic "Saw a function call with non-empty type args" []
    _ -> panic "not handled" []


normaliseC :: TC a Class -> NCExpr
normaliseC tc =
  case texprValue tc of
    TCSetAny           -> NSetAny
    TCSetSingle v      -> NSetSingle     $ normaliseV v
    TCSetComplement cl -> NSetComplement $ normaliseC cl
    TCSetUnion cls     -> NSetUnion      $ map normaliseC cls
    TCSetOneOf bs      -> NSetOneOf bs
    TCSetDiff cl cl'   -> NSetDiff (normaliseC cl) (normaliseC cl')
    TCSetRange v v'    -> NSetRange (normaliseV v) (normaliseV v')
    TCFor {}           -> panic "normaliseC" ["Saw a for in a class"]

    TCCall f [] args   -> NCCall (nname f) (map normaliseArg args)
    TCCall _ _ _       -> panic "Saw a function call with non-empty type args" []
    TCVar {}           -> panic "Saw a non-Value var reference" []
    _ -> panic "not handled" []

-- Everything but Do
normaliseG' :: TC a Grammar -> NGExpr
normaliseG' tc =
  case texprValue tc of
    TCCurrentStream         -> NCurrnetStream
    TCSetStream str         -> NSetStream (normaliseV str)
    TCStreamLen sem len str -> NStreamLen sem (normaliseV len) (normaliseV str)
    TCStreamOff sem len str -> NStreamOff sem (normaliseV len) (normaliseV str)

    TCGetByte s       -> NGetByte s
    TCLabel l g       -> NLabel l (normaliseG g)
    TCMatch s cl      -> NMatch s (normaliseC cl)
    TCMatchBytes s e  -> NMatchBytes s (normaliseV e)
    TCChoice c gs t   -> NChoice c (map normaliseG gs) (ntype t)
    TCOptional c g    -> NOptional c (normaliseG g)
    TCMany s c bnds g -> NMany s c (normaliseV <$> bnds) (normaliseG g)
    TCEnd             -> NEnd
    TCOffset          -> NOffset
    TCMapLookup s k m -> NMapLookup s (normaliseV k) (normaliseV m)
    TCMapInsert s k v m ->
                      NMapInsert s (normaliseV k) (normaliseV v) (normaliseV m)
    TCArrayIndex s v ix -> NArrayIndex s (normaliseV v) (normaliseV ix)
    TCCoerceCheck s t t' v -> NCoerceCheck s (ntype t) (ntype t') (normaliseV v)
    TCFor lp ->
      case loopFlav lp of
        LoopMap ->
          NGMap (nname <$> loopKName lp)
                (nname (loopElName lp))
                (normaliseV (loopCol lp))
                (normaliseG (loopBody lp))

        Fold x s ->
          NGFor (nname x)
                (normaliseV s)
                (nname <$> loopKName lp)
                (nname (loopElName lp))
                (normaliseV (loopCol lp))
                (normaliseG (loopBody lp))

    TCVar v            -> panic "Saw a grammar variable" ["Variable: " ++ show (pp v)]
    TCCall f [] args   -> NGCall (nname f) (map normaliseArg args)
    TCCall _ _ _       -> panic "Saw a function call with non-empty type args" []

    TCErrorMode s p -> NGErrorMode s (normaliseG p)

    TCPure v           -> NGPure (normaliseV v)
    TCDo   {}   -> panic "impossible" []
    TCFail mbM t -> NGFail (normaliseV <$> mbM) (ntype t)
    _ -> panic "not handled" []

-- FIXME: we can strip units here as well

normaliseG :: TC a Grammar -> NGrammar
normaliseG tc = flattenDo (Just ret) tc (NPure (NVar ret))
  where
    ret = mkRetN (ntype $ typeOf tc)

    mkRetN :: NType -> NName
    mkRetN = NName (Name (Local "ret") AValue synthetic invalidGUID) -- FIXME

    flattenDo :: Maybe NName -> TC a Grammar -> NGrammar -> NGrammar
    flattenDo m_x tc1 rest =
      case texprValue tc1 of
        TCDo m_y l r -> flattenDo (nname <$> m_y) l (flattenDo m_x r rest)
        _            -> NBind m_x (normaliseG' tc1) rest
