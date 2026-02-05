module ExportCPP where

import Data.Map(Map)
import Data.Map qualified as Map
import Data.Text(Text)
import Data.Void(absurd)
import Text.PrettyPrint

import Daedalus.PP
import Daedalus.VM.Backend.C.Lang
import Quote
import AST
import Type
import Name

type Ctx = (?ftAliases :: Map QName ForeignTypeDecl)

genForeignType :: Ctx => Type QName -> CType
genForeignType ty =
  case ty of
    TVar x -> pp (locThing x)
    Type tc args sizes
      | null sizes ->
        case Map.lookup (locThing tc) ?ftAliases of
          Just tdef -> renderQuote (ppP <$> ftDef tdef)
            where
            xs = map locThing (ftParams tdef)
            mp = Map.fromList (zip xs (map genForeignType args))
            ppP x =
              case Map.lookup (locThing x) mp of
                Just ty -> ty
                Nothing -> error "[BUG] genForeignType: missing param" 
          Nothing -> error ("[BUG] genForeignType: missing type alias: " ++ show (pp tc))
      | otherwise -> error "[BUG] genForeingType: sizes"

