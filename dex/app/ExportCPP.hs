module ExportCPP where

import Data.Text(Text)
import Data.Void(absurd)
import Text.PrettyPrint

import Quote
import AST

type CPP = Doc

class Export t where
  export :: t -> CPP

{-
instance Export Module where
  export m =
    vcat [
        
    ]  
-}
instance Export (Q Doc) where
  export = renderQuote
