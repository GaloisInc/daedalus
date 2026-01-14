module Check where

import AST

data ValidationError = XXX

checkModule :: Module -> [ValidationError]
checkModule m = undefined
  where
  ds = moduleDecls m