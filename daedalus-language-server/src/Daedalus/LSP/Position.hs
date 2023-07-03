{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

-- ---------------------------------------------------------------------------------------
-- Mapping positions to things

module Daedalus.LSP.Position where

import           Data.Monoid
import           Control.Monad(guard)
import           Data.Parameterized.Some
import qualified Data.Text               as Text

import qualified Language.LSP.Protocol.Types as J

import           Daedalus.PP
import           Daedalus.SourceRange

import           Daedalus.Type.AST
import Data.Maybe (maybeToList, fromMaybe)
import Daedalus.Type.Traverse
import Daedalus.Rec (forgetRecs)
import Data.Foldable
import Daedalus.LSP.Diagnostics (sourceRangeToRange, jSourceColumn, jSourceLine)


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
          Fold v  _ c     -> vdef v : vDefCol c
          LoopMany _ v _  -> [vdef v]
          LoopMap c       -> vDefCol c
        ++ goBody tc
      TCCall v _ _      -> fuse v : goBody tc
      TCCase _ alts _ -> (vdef <$> foldMap altBinds alts) ++ goBody tc
      _ -> goBody tc

    vDefCol c = maybeToList (vdef <$> lcKName c) ++ [ vdef (lcElName c) ]

    goBody = foldMapTC go

data TypeInfo = TypeInfo
                  { nameOfDecl :: Name
                  , typeOfDecl :: Poly RuleType
                  , typeLoc    :: SourceRange
                  , typeOfExpr :: Maybe Type
                  }

typeAtModule :: J.Position -> TCModule SourceRange -> Maybe TypeInfo
typeAtModule pos m =
  msum (map (typeAtDecl pos) (forgetRecs (tcModuleDecls m)))

typeAtDecl :: J.Position -> TCDecl SourceRange -> Maybe TypeInfo
typeAtDecl pos d@TCDecl { tcDeclName = n, tcDeclParams = ps, tcDeclDef = def
                         , tcDeclAnnot = r }
  | not (positionInRange pos (tcDeclAnnot d)) = Nothing
  | otherwise = msum (inName : map inParam ps ++ [ inBody, Just here ])
  where
  here = TypeInfo { nameOfDecl = n
                  , typeOfDecl = declTypeOf d
                  , typeLoc    = r
                  , typeOfExpr = Nothing
                  }

  inName    = do guard (positionInRange pos n)
                 pure here { typeLoc = range n }
  inParam p = do guard (positionInRange pos p)
                 pure here { typeLoc = range p, typeOfExpr = Just (typeOf p) }
  inBody    = case def of
                Defined tc    ->
                  do (ty,rng) <- getAlt (typeAtTC pos tc)
                     pure here { typeLoc = rng, typeOfExpr = Just ty }
                ExternDecl {} -> Nothing



declAtPos :: J.Position -> TCModule SourceRange -> Maybe (TCDecl SourceRange)
declAtPos pos m =
  find (positionInRange pos . tcDeclAnnot) (forgetRecs (tcModuleDecls m))

typeAtTC :: J.Position -> TC SourceRange k -> Alt Maybe (Type, SourceRange)
typeAtTC pos tc = do
  exprs <- positionToExprs pos tc
  Alt $ case reverse exprs of
    [] -> Nothing -- means the position isn't inside tc
    -- We need to 'fixup' the results of exprs at, as we may be looking at e.g. a binder
    Some tc' : _ -> case texprValue tc' of
      TCDo (Just n) _ _ | positionInRange pos n  -> Just (typeOf n, range n)
      TCCall fn _ _     | positionInRange pos fn -> Just (typeOf fn, range fn) -- FIXME: this just returns the result type, not the fn type

      TCFor (loopFlav -> Fold n _ c)
        | positionInRange pos n  -> Just (typeOf n, range n)
        | Just yes <- colNames c -> Just yes

      TCFor (loopFlav -> LoopMany _ n _)
        | positionInRange pos n  -> Just (typeOf n, range n)

      TCFor (loopFlav -> LoopMap c)
        | Just yes <- colNames c -> Just yes

      _ -> Just (typeOf tc', range tc') -- Fallthough, at least return something.

  where
  colNames col = msum [ do guard (positionInRange pos x)
                           pure (typeOf x, range x)
                      | x <- maybeToList (lcKName col) ++ [lcElName col]
                      ]


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
  (jSourceLine start < line || (jSourceLine start == line && jSourceColumn start <= col))
  &&
  (jSourceLine end > line || (jSourceLine end == line && jSourceColumn end >= col))
  where
    line = line0 + 1
    col  = col0  + 1

sourceRangeToLocationLink :: SourceRange -> J.LocationLink
sourceRangeToLocationLink pos = J.LocationLink
  { J._originSelectionRange = Nothing
  , J._targetUri = J.filePathToUri (Text.unpack $ sourceFile (sourceFrom pos))
  , J._targetRange = sourceRangeToRange pos
  , J._targetSelectionRange = sourceRangeToRange pos
  }
