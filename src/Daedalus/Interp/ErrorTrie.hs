module Daedalus.Interp.ErrorTrie where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.ByteString.Short(fromShort)

import qualified RTS.ParseError as RTS
import RTS.Input
import RTS.JSON

import Daedalus.Interp.DebugAnnot
import Daedalus.Interp.Env

data ErrorTrie = ErrorTrie (Maybe ParseError) (Map CallSite ErrorTrie)


emptyErrorTrie :: ErrorTrie
emptyErrorTrie = ErrorTrie Nothing mempty

insertError :: [DebugAnnot] -> ParseError -> ErrorTrie -> ErrorTrie
insertError path err (ErrorTrie here there) =
  case path of
    [] -> ErrorTrie newHere there
      where
      newHere =
        case here of
          Nothing -> Just err
          Just other -> Just (err <> other)

    e : more ->
      case e of
        TextAnnot {}  -> insertError more err (ErrorTrie here there)
        ScopeAnnot {} -> insertError more err (ErrorTrie here there)
        CallAnnot site ->
          let remote = Map.findWithDefault emptyErrorTrie site there
              newRemote = insertError more err remote
          in ErrorTrie here (Map.insert site newRemote there)

parseErrorToTrie :: ParseError -> ErrorTrie
parseErrorToTrie = foldr insert emptyErrorTrie
                 . zipWith addNum [ 0 .. ]
                 . RTS.parseErrorToList
  where
  insert e t = insertError (reverse (RTS.peStack e)) e t
  addNum n e = e { RTS.peNumber = n }

parseErrorTrieToJSON :: ParseError -> JSON
parseErrorTrieToJSON top =
  jsObject
    [ ("tree",    jsTrie (parseErrorToTrie top))
    , ("inputs",  jsObject [ (fromShort k, jsText v)
                           | (k,v) <- Map.toList (RTS.getInputs top)
                           ])
    ]
  where
  jsTrie (ErrorTrie here there) =
    jsObject
      [ ("errors", jsErrMb here)
      , ("frames", jsMap there)
      ]

  jsMap mp =
    jsArray
      [ jsObject
          [ ("frame",toJSON k)
          , ("nest", jsTrie v)
          ]
      | (k,v) <- Map.toList mp ]

  jsErrMb mb =
    jsArray
      case mb of
        Nothing -> []
        Just es -> [ jsErr e | e <- RTS.parseErrorToList es ]

  jsErr pe =
    jsObject
      [ ("error",   jsString (RTS.peMsg pe))
      , ("input",   toJSON (inputName (RTS.peInput pe)))
      , ("offset",  toJSON (inputOffset (RTS.peInput pe)))
      , ("grammar", toJSON (RTS.peGrammar pe))
      , ("trace",   toJSON (RTS.peITrace pe))
      , ("stack",   toJSON (RTS.peStack pe))
      , ("number",  toJSON (RTS.peNumber pe))
      ]






