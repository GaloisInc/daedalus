{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad.Reader (ReaderT(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Data.IORef

import CommandLine

import Daedalus.Interp (vUnit)
import Daedalus.Interp.Value (Value(..), mkUInt)
import Daedalus.PP hiding (empty, mode)
import Daedalus.Rec
import Daedalus.Type.AST

import Talos
import Machine

main :: IO ()
main = do
  opts <- getOptions
  go opts

handler ::
  Valuable a k =>
  Eff a k -> Env a -> ByteString -> Cont ByteString a k ->
  IO (Either (TC a k) (ValueOf a k), Env a, ByteString, Cont ByteString a k)
handler (Match cv) e input k =
  putStrLn ("match " ++ show cv) >>
  case BS.uncons input of
    Nothing -> handler Fail e input k
    Just (c, cs) ->
      if matches cv c
        then pure (Right $ ValGrammar (Pure (mkUInt 8 (fromIntegral c))), e, cs, k)
        else handler Fail e input k
handler Fail e input k =
  case k of
    KDone -> error "Parsing failed"
    KDo k' mx next -> handler Fail e input k'
    KCall (k' :: Cont ByteString a k') f argVals argStxs ->
      case ctx @k' of
        AGrammar -> handler Fail e input k'
        _ -> error "Non-grammar parent frame in grammar continuation"
    KSavedEnv k' e' -> handler Fail e' input k'
    KChoose k' e' s' _mode [] -> handler Fail e' s' k'
    KChoose k' e' s' mode (ch:chs) -> pure (Left ch, e', s', KChoose k' e' s' mode chs)
handler (Guard b) e input k =
  putStrLn ("guard " ++ show b) >>
  if b
    then pure (Right $ ValGrammar (Pure vUnit), e, input, k)
    else handler Fail e input k
handler (Many bounds parser) e input k =
  case bounds of
    Exactly i -> error "TODO"
    Between lo hi -> error "TODO"

parse :: MachineState ByteString a -> IO (Step ByteString a)
parse st = run handler st

go :: Options -> IO ()
go opts = do
  let ddlFile = optDDLInput opts
  (mainRule, unorderedTys, mods, _) <- runDaedalus ddlFile Nothing
  let allDecls  = concatMap (reverse . forgetRecs . tcModuleDecls) mods
  let Just rootDecl = find (\d -> nameScopedIdent (tcDeclName d) == mainRule) allDecls
  case rootDecl of
    TCDecl {tcDeclDef = Defined rootDeclDef, tcDeclCtxt = AGrammar} -> do

      input <- BS.readFile $ fromMaybe "/dev/null" (optInfile opts)
      let st = initState input rootDeclDef
      st' <- parse st
      case st' of
        Halt v -> print (pp v)
        other -> print other
