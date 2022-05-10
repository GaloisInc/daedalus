{-# Language OverloadedLabels, QuasiQuotes #-}
module Daedalus.Compile.Config where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Vector(Vector)
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Control.Applicative((<|>))
import Control.Exception(throwIO)

import Daedalus.Panic(panic)
import Daedalus.Value.Extract
import Daedalus.Type.AST(primName,Name,ModuleName)
import Daedalus.Compile.ConfigParser(pConfig)
import Daedalus.Compile.LangHS


data CompilerCfg = CompilerCfg
  { cImports :: [ Import ]
    -- ^ Additional imports to add to each module

  , cPrims :: Map Name ([Term] -> Term)
    -- ^ Implementation for primitives

  , cParserType :: Maybe Term
    -- ^ Use this type for parsers, default is to use RTS.Parser

  , cQualNames :: UseQual
  }

-- | Left one takes precedence when there is conflict.
instance Semigroup CompilerCfg where
  new <> old = CompilerCfg { cImports     = cImports new <> cImports old
                           , cPrims       = cPrims new <> cPrims old
                           , cParserType  = cParserType new <|> cParserType old
                           , cQualNames   = cQualNames new
                           }

instance Monoid CompilerCfg where
  mempty =
    CompilerCfg
      { cImports    = []
      , cPrims      = Map.empty
      , cParserType = Nothing
      , cQualNames  = UseQualNames
      }

data UseQual = UseQualNames | DoNotUseQualNames

moduleConfigsFromFile :: CompilerCfg -> FilePath -> IO (ModuleName -> CompilerCfg)
moduleConfigsFromFile dflt file =
  do bytes <- BS.readFile file
     case pConfig bytes of
       Left err -> throwIO err
       Right v  -> pure (moduleConfigs dflt v)

moduleConfigs :: CompilerCfg -> Value -> ModuleName -> CompilerCfg
moduleConfigs dflt v =
  \m -> addDefaults (Map.findWithDefault dflt m mods)
  where
  (imps,pType)  = header (#header v)
  addDefaults c = c { cImports = imps ++ cImports c
                    , cParserType = case cParserType c of
                                      Just a  -> Just a
                                      Nothing -> pType
                    }
  mods          = Map.fromList (map moduleDecl (#modules v))

moduleDecl :: Value -> (ModuleName, CompilerCfg)
moduleDecl v = (m, CompilerCfg { cImports    = imps
                               , cParserType = pT
                               , cPrims      = prims
                               , cQualNames  = UseQualNames
                               } )
  where
  m         = Text.pack (#name v)
  (imps,pT) = header (#header v)
  prims     = Map.fromList (map (primDecl m) (#prims v))

header :: Value -> ([Import], Maybe Term)
header v = ( #imports v, #monad v )

primDecl :: ModuleName -> Value -> (Name, [Term] -> Term)
primDecl m v = (nm, fun)
  where
  nm  = primName m (Text.pack (#name v))
  as  = (#args v) :: Vector Value
  def = #definition v
  fun = \ts -> aps def (drop (Vector.length as) ts)

instance FromValue Term where
  fromValue v = aps (Var (#fun v)) (map funArg (#args v))

funArg :: Value -> Term
funArg val =
  case val of
    VUnionElem "TArg" v -> TyParam (Raw (fromValue v :: String))
    VUnionElem "Arg"  v -> fromValue v
    _                   -> panic "funArg" ["Invalid funArg"]

instance FromValue Import where
  fromValue v = Import (#module v) (#qualifier v)

instance FromValue ImportQualifier where
  fromValue v = case fromValue v of
                  Nothing -> Unqualified
                  Just a  -> QualifyAs a

