
-- This implements the 'regions' command, which returns a list of
-- regions surrounding a given position, useful for e.g. cutting an
-- expression (and debugging source locations)
module Daedalus.LSP.Command.Regions (positionToRegions) where

import           Data.Maybe               (fromMaybe)
import           Data.Monoid
import           Data.Parameterized.Some

import qualified Language.LSP.Types       as J

import           Daedalus.SourceRange
import           Daedalus.Type.AST

import           Daedalus.LSP.Diagnostics (sourceRangeToRange)
import           Daedalus.LSP.Position

positionToRegions :: J.Position -> TCModule SourceRange -> J.List J.Range
positionToRegions pos m = fromMaybe mempty $ do
  TCDecl { tcDeclName = n, tcDeclParams = ps, tcDeclDef = def
         , tcDeclAnnot = r } <- declAtPos pos m

  let defRanges = case def of
        ExternDecl _ -> Alt Nothing
        Defined tc   -> map (viewSome range) <$> positionToExprs pos tc
        
  branges <- getAlt $ tryOne n <> foldMap tryOne ps <> defRanges
      
  pure $ J.List (reverse . map sourceRangeToRange $ r : branges)
  where
    tryOne :: (HasRange a) => a -> Alt Maybe [SourceRange]
    tryOne v = Alt $ if positionInRange pos v then Just [range v] else Nothing
    

