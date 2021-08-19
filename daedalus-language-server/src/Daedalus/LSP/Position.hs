{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- ---------------------------------------------------------------------------------------
-- Mapping positions to things

module Daedalus.LSP.Position where

import           Data.Monoid
import           Data.Parameterized.Some
import qualified Data.Text               as Text

import qualified Language.LSP.Types      as J

import           Daedalus.PP
import           Daedalus.SourceRange

import           Daedalus.Type.AST
import Data.Maybe (maybeToList, fromMaybe)
import Daedalus.Type.Traverse
import Daedalus.Rec (forgetRecs)
import Data.Foldable
import Daedalus.LSP.Diagnostics (sourceRangeToRange)


data NameRefClass = NameDef | NameUse
  deriving Eq

data NameInfo =
  NameInfo { niNameRefClass :: NameRefClass
           , niName     :: Name
           , niType     :: Type -- result type for function calls
           }

instance HasRange NameInfo where
  range = range . niName

-- We can't use the free vars stuff here as we want each occurrence of
-- a name, while that will just tell which vars are used (the free
-- functions ignore source ranges).
declToNames :: TCDecl SourceRange -> [NameInfo]
declToNames d@TCDecl { tcDeclName = n, tcDeclParams = ps, tcDeclDef = def } =
  fdef n (typeOf d) : map paramName ps ++
  case def of
    ExternDecl _ -> []
    Defined tc   -> go tc
  where
    paramName p = case p of
      ValParam v     -> vdef v
      ClassParam v   -> vdef v
      GrammarParam v -> vdef v

    vdef :: forall k. TCName k -> NameInfo
    vdef v = NameInfo NameDef (tcName v) (typeOf v)

    vuse :: forall k. TCName k -> NameInfo
    vuse v = NameInfo NameUse (tcName v) (typeOf v)

    fdef :: Name -> Type -> NameInfo
    fdef = NameInfo NameDef

    fuse :: forall k. TCName k -> NameInfo
    fuse v = NameInfo NameUse (tcName v) (typeOf v)

    go :: forall k. TC SourceRange k -> [NameInfo]
    go tc = case texprValue tc of
      TCVar v           -> [vuse v]
      TCDo (Just v) _ _ -> vdef v : goBody tc
      TCFor l ->
        case loopFlav l of
          Fold v _ -> [vdef v]
          _ -> []
        ++ (vdef <$> maybeToList (loopKName l))
        ++ [ vdef (loopElName l) ]
        ++ goBody tc
      TCCall v _ _      -> fuse v : goBody tc
      TCCase _ alts _ -> (vdef <$> foldMap altBinds alts) ++ goBody tc
      _ -> goBody tc

    goBody = foldMapTC go

typeAtModule :: J.Position -> TCModule SourceRange -> Alt Maybe (Type, SourceRange)
typeAtModule pos m = foldMap (typeAtDecl pos) (forgetRecs (tcModuleDecls m))

declAtPos :: J.Position -> TCModule SourceRange -> Maybe (TCDecl SourceRange)
declAtPos pos m = find (positionInRange pos . tcDeclAnnot) (forgetRecs (tcModuleDecls m))
  
typeAtDecl :: J.Position -> TCDecl SourceRange -> Alt Maybe (Type, SourceRange)
typeAtDecl pos d | not (positionInRange pos (tcDeclAnnot d)) = mempty
typeAtDecl pos TCDecl { tcDeclName = n, tcDeclParams = ps, tcDeclDef = def
                      , tcDeclAnnot = r } =
  tryOne' n def <> foldMap tryOne ps <> defOne <> fallback
  where
    defOne = case def of
      ExternDecl _ -> mempty
      Defined tc   -> typeAtTC pos tc

    tryOne :: (HasRange a, TypeOf a) => a -> Alt Maybe (Type, SourceRange)
    tryOne v = tryOne' v v

    tryOne' :: (HasRange a, TypeOf b) => a -> b -> Alt Maybe (Type, SourceRange)
    tryOne' v v' = Alt $ if positionInRange pos v then Just (typeOf v', range v) else Nothing
    
    -- If nothing else, return the whole thing
    fallback = Alt (Just (typeOf def, r))

typeAtTC :: J.Position -> TC SourceRange k -> Alt Maybe (Type, SourceRange)
typeAtTC pos tc = do
  exprs <- positionToExprs pos tc
  Alt $ case reverse exprs of
    [] -> Nothing -- means the position isn't inside tc
    -- We need to 'fixup' the results of exprs at, as we may be looking at e.g. a binder
    Some tc' : _ -> case texprValue tc' of
      TCDo (Just n) _ _ | positionInRange pos n  -> Just (typeOf n, range n)
      TCCall fn _ _     | positionInRange pos fn -> Just (typeOf fn, range fn) -- FIXME: this just returns the result type, not the fn type
      TCFor Loop { loopFlav = Fold n _ } | positionInRange pos n  -> Just (typeOf n, range n)
      TCFor Loop { loopKName = Just n  } | positionInRange pos n  -> Just (typeOf n, range n)
      TCFor Loop { loopElName = n  }     | positionInRange pos n  -> Just (typeOf n, range n)
      _ -> Just (typeOf tc', range tc') -- Fallthough, at least return something.

exprTree :: TCDecl SourceRange -> Doc
exprTree TCDecl { tcDeclDef = ExternDecl _  } = "external"
exprTree TCDecl { tcDeclDef = Defined def } = bullets (go def)
  where
    go :: forall k. TC SourceRange k -> [Doc]
    go tc =
      let kids = foldMapTC go tc
      in [ hang (text (prettySourceRange (range tc)) $$ pp tc) 4 (bullets kids) ]
        
-- This assumes that a position cannot be in sibling expressions
positionToExprs :: J.Position -> TC SourceRange k -> Alt Maybe [Some (TC SourceRange)]
positionToExprs pos = go
  where
    go :: forall k. TC SourceRange k -> Alt Maybe [Some (TC SourceRange)]
    go tc | not (positionInRange pos tc) = mempty
    -- Nothing means either no matches, or no children.  We know _we_
    -- match, so either some child also matches, or no TC child
    -- matches (but some non-tc type may match, e.g. the variable in a
    -- do).
    go tc = Alt (Just (Some tc : fromMaybe [] (getAlt $ foldMapTC go tc)))

positionInRange :: HasRange a => J.Position -> a -> Bool
positionInRange (J.Position line0 col0) (range -> SourceRange start end) =
  (sourceLine start < line || (sourceLine start == line && sourceColumn start <= col))
  &&
  (sourceLine end > line || (sourceLine end == line && sourceColumn end >= col))
  where
    line = line0 + 1
    col  = col0  + 1

sourceRangeToLocation :: SourceRange -> J.Location
sourceRangeToLocation pos =
  J.Location (J.filePathToUri (Text.unpack $ sourceFile (sourceFrom pos)))
             (sourceRangeToRange pos)

