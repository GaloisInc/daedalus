{-# Language OverloadedStrings #-}
{-# Language GADTs, RecordWildCards #-}

module Talos.Analysis.Annot (synthAnnotModule
                            , TCSynthAnnot (..)
                            , SynthAnnot (..)
                            , ChoiceVar -- This can be left opaque
                            -- , BytesVar
                            , isSimpleTC -- FIXME: might be better elsewhere?
                            ) where

import Daedalus.Type.AST
import Daedalus.Type.Traverse

import Daedalus.PP
import Daedalus.GUID
import Daedalus.SourceRange

-- FIXME: we can maybe remove this whole module?

-- FIXME: These could possible be replaced by a 'AnnotVar' or
-- something, they cannot occur on the same AST node and are guarded
-- by a SynthAnnot ctor as well
newtype ChoiceVar = ChoiceVar GUID
  deriving (Eq, Ord, Show)

-- FIXME: UNUSED
newtype BytesVar = BytesVar GUID
  deriving (Eq, Ord, Show)

freshChoiceVar :: HasGUID m => m ChoiceVar
freshChoiceVar = ChoiceVar <$> getNextGUID

freshBytesVar :: HasGUID m => m BytesVar
freshBytesVar = BytesVar <$> getNextGUID

-- | This annotates TC nodes with information that is required during
-- analysis/synthesis
data SynthAnnot = NoAnnot -- ^ This node does not require an annotation
                | ChoiceAnnot ChoiceVar
                | BytesAnnot  BytesVar
  deriving (Eq, Ord, Show)

data TCSynthAnnot = TCSynthAnnot
  { srange :: SourceRange -- ^ Retained from original AST
  , sannot :: SynthAnnot
  } deriving (Eq, Show)

noAnnot :: SourceRange -> TCSynthAnnot
noAnnot sr = TCSynthAnnot { srange = sr, sannot = NoAnnot }

choiceAnnot :: HasGUID m => SourceRange -> m TCSynthAnnot
choiceAnnot sr = TCSynthAnnot sr . ChoiceAnnot <$> freshChoiceVar

bytesAnnot :: HasGUID m => SourceRange -> m TCSynthAnnot
bytesAnnot sr = TCSynthAnnot sr . BytesAnnot <$> freshBytesVar

synthAnnotModule :: HasGUID m => TCModule SourceRange -> m (TCModule TCSynthAnnot)
synthAnnotModule mo = do
  decls' <- traverse (traverse synthAnnotDecl) (tcModuleDecls mo)
  pure (mo { tcModuleDecls = decls' })

synthAnnotDecl :: HasGUID m => TCDecl SourceRange -> m (TCDecl TCSynthAnnot)
synthAnnotDecl TCDecl { tcDeclDef = Defined def
                      , tcDeclCtxt = AGrammar
                      , tcDeclAnnot = a
                      , ..} = mk <$> synthAnnotTC def
  where
    mk def' = TCDecl { tcDeclDef   = Defined def'
                     , tcDeclCtxt  = AGrammar
                     , tcDeclAnnot = noAnnot a, .. }
  
synthAnnotDecl TCDecl { tcDeclDef = Defined def
                      , tcDeclAnnot = a
                      , ..} = mk <$> noAnnotTC def
  where
    mk def' = TCDecl { tcDeclDef   = Defined def'
                     , tcDeclAnnot = noAnnot a, .. }

synthAnnotDecl TCDecl { tcDeclDef = ExternDecl ty
                      , tcDeclAnnot = a
                      , ..} =
  pure (TCDecl { tcDeclDef = ExternDecl ty, tcDeclAnnot =  noAnnot a, .. })

noAnnotTC :: HasGUID m => TC SourceRange k -> m (TC TCSynthAnnot k)
noAnnotTC = traverseTC (pure . noAnnot) noAnnotTC

synthAnnotTC :: HasGUID m => TC SourceRange k -> m (TC TCSynthAnnot k)
synthAnnotTC tc = do
  annot <- case texprValue tc of
    TCChoice {}       -> choiceAnnot (texprAnnot tc)
    _ | isSimpleTC tc -> bytesAnnot  (texprAnnot tc)
    _                 -> pure (noAnnot (texprAnnot tc))
  TC . TCAnnot annot <$> traverseTCF synthAnnotTC (texprValue tc)

--------------------------------------------------------------------------------
-- Simple nodes
--
-- A simple node is one where the synthesis can completely determine
-- the value and bytes, e.g. Match, UInt8, etc.  Simple nodes are
-- annotated with a corresponding BytesVar which is used to track
-- bytes assigned earlier.

isSimpleTC :: TC a k -> Bool
isSimpleTC tc =
  case texprValue tc of
    TCMatchBytes {}  -> True
    TCPure {}        -> True
    TCGetByte {}     -> True
    TCMatch {}       -> True
    TCCoerceCheck {} -> True
    _               -> False -- includes unsupported operations as well

--------------------------------------------------------------------------------
-- Instances

instance HasRange TCSynthAnnot where
  range = srange
    
instance PP ChoiceVar where
  pp (ChoiceVar v) = "%c" <> pp v

instance PP BytesVar where
  pp (BytesVar v) = "%b" <> pp v
