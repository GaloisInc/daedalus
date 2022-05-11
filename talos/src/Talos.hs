{-# Language GeneralizedNewtypeDeriving #-}
{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}

-- Empty for now.
module Talos (
  -- * High-level synthesis operators
  synthesise,
  summarise,
  -- * Useful helpers
  runDaedalus,
  ProvenanceMap, -- XXX: should do this properly 
  ) where

import           Control.Monad.State
import           Data.ByteString              (ByteString)
import           Data.IORef                   (modifyIORef', newIORef,
                                               readIORef, writeIORef)
import           Data.List                    (find)
import           Data.List.Split              (splitWhen)
import qualified Data.Map                     as Map
import           Data.String                  (fromString)
import           Data.Version
import qualified SimpleSMT                    as SMT
import           System.Exit                  (exitFailure)
import           System.IO                    (IOMode (..), hFlush, hPutStr,
                                               hPutStrLn, openFile, stderr,
                                               stdout)
import           System.IO.Streams            (InputStream)
import           Text.ParserCombinators.ReadP (readP_to_S)
import qualified Text.ParserCombinators.ReadP as RP

import           Daedalus.AST                 (nameScopeAsModScope)
import           Daedalus.Core
import           Daedalus.Driver              hiding (State)
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Rec                 (forgetRecs)
import           Daedalus.Type.AST            (tcDeclName, tcModuleDecls)
import           Daedalus.Value               (Value)

import qualified Talos.Analysis               as A
import           Talos.Analysis.AbsEnv        (AbsEnvTy (AbsEnvTy))
import           Talos.Analysis.Monad         (makeDeclInvs)
import           Talos.Passes
import           Talos.Strategy
import           Talos.Strategy.Monad
import           Talos.SymExec.Path           (ProvenanceMap)
import qualified Talos.Synthesis              as T

-- -- FIXME: move, maybe to GUID.hs?
-- newtype FreshGUIDM a = FreshGUIDM { getFreshGUIDM :: State GUID a }
--   deriving (Functor, Applicative, Monad)

-- instance HasGUID FreshGUIDM where
--   getNextGUID = FreshGUIDM $ state (mkGetNextGUID' id const)

summarise :: FilePath -> Maybe FilePath -> Maybe String -> String
          -> IO Doc
summarise inFile m_invFile m_entry absEnv = do
  (_mainRule, md, nguid) <- runDaedalus inFile m_invFile m_entry

  AbsEnvTy p <- case lookup absEnv A.absEnvTys of
    Just x -> pure x
    _      -> errorExit ("Unknown abstract env " ++ absEnv)

  let invs = makeDeclInvs (mGFuns md) (mFFuns md)
  putStrLn "Inverses"
  print (pp <$> Map.keys invs)
  putStrLn "Slices"
  let (summs, _) = A.summarise p md nguid
  pure (bullets (map goF (Map.toList summs)))
  where
    goF (fn, m) =
      hang (pp fn) 2 $
        bullets [ hang (pp fid) 2 (pp d)
                | (fid, d) <- Map.toList m]

_z3VersionCheck :: SMT.Solver -> IO ()
_z3VersionCheck s = do
  r <- SMT.command s (SMT.fun "get-info" [SMT.const ":name"])
  case r of
    SMT.List [ _, SMT.Atom name ]
      | name == "\"Z3\"" -> pure ()
      | otherwise -> errorExit ("Unsupported solver: " ++ name)
    _ -> errorExit ("Unexpected solver response: " ++ SMT.showsSExpr r "")

  let versionP = RP.between (RP.char '"') (RP.char '"') parseVersion <* RP.eof
  r' <- SMT.command s (SMT.fun "get-info" [SMT.const ":version"])
  case r' of
    SMT.List [ _, SMT.Atom version ] ->
      case readP_to_S versionP version of
        (v, _) : _ ->
          if v < makeVersion [4,8,10]
          then errorExit "Unsupported version of Z3: use 4.8.10 or later"
          else pure ()
        _ -> errorExit "Could not parse version"
    _ -> errorExit ("Unexpected solver response: " ++ SMT.showsSExpr r' "")


errorExit :: String -> IO b
errorExit msg = do
  hPutStrLn stderr msg
  hFlush stderr
  exitFailure


synthesise :: FilePath           -- ^ DDL file
           -> Maybe FilePath     -- ^ Inverse file
           -> Maybe String       -- ^ Entry
           -> FilePath           -- ^ Backend solver executable
           -> [String]           -- ^ Solver args
           -> [(String, String)] -- ^ Backend solver options
           -> IO ()              -- ^ Backend solver init
           -> Maybe String       -- ^ Synthesis strategy 
           -> Maybe (Int, Maybe FilePath) -- ^ Logging options
           -> Maybe Int          -- ^ Random seed
           -> String             -- ^ Analysis abstract env.
           -> IO (InputStream (Value, ByteString, ProvenanceMap))
synthesise inFile m_invFile m_entry backend bArgs bOpts bInit stratOpt m_logOpts m_seed absEnv = do
  (mainRule, md, nguid) <- runDaedalus inFile m_invFile m_entry

  -- SMT init
  logger <- case m_logOpts of
              Nothing        -> pure Nothing
              Just (i, m_f)  -> Just <$> newFileLogger m_f i

  solver <- SMT.newSolver backend bArgs logger

  -- Check version: z3 before 4.8.10 (or .9) seems to have an issue
  -- with match.  
  -- z3VersionCheck solver

  -- Set options
  forM_ bOpts $ \(opt, val) -> do
    r <- SMT.setOptionMaybe solver (':' : opt) val
    unless r $ hPutStrLn stderr ("WARNING: solver does not support option " ++ opt)

  absty <- case lookup absEnv A.absEnvTys of
    Just x -> pure x
    _      -> errorExit ("Unknown abstract env " ++ absEnv)


  let strat = case stratOpt of
             Nothing    -> allStrategies
             Just "all" -> allStrategies
             Just names  ->
               let stratNames = (splitWhen (==',') names) in
               let maybes = map (\n -> find (\s -> (stratName s) == n) allStrategies) stratNames in
                 [x | Just x <- maybes]

  -- Setup stdlib by initializing the solver and then defining the
  -- Talos standard library
  bInit
  T.synthesise m_seed nguid solver absty strat mainRule md

-- | Run DaeDaLus on a source file, returning a triple that consists
-- of the name of the main rule (the entry point), a list of type
-- declarations (that are named struct or union types), and a list of
-- type-checked modules. The main rule's name includes the module
-- information needed to find it.r
runDaedalus :: FilePath -> Maybe FilePath -> Maybe String ->
               IO (FName, Module, GUID)
runDaedalus inFile m_invFile m_entry = daedalus $ do
  mm <- ddlPassFromFile ddlLoadModule inFile
  extras <- case m_invFile of
    Nothing -> pure []
    Just f  -> do
      im <- ddlPassFromFile ddlLoadModule f
      m  <- ddlGetAST im astTC
      pure $ map (nameScopeAsModScope . tcDeclName) (forgetRecs (tcModuleDecls m))

  let entryName = maybe "Main" fromString m_entry
      specMod  = "DaedalusMain"

  passSpecialize specMod ((mm, entryName) : extras)
  passCore specMod
  passStripFail specMod
  passSpecTys specMod
  passConstFold specMod
  
  entry <- ddlGetFName mm entryName

  md    <- ddlGetAST specMod astCore >>= ddlRunPass . allPassesM entry

  nguid <- ddlRunPass getNextGUID

  pure (entry, md, nguid)

newFileLogger :: Maybe FilePath -> Int -> IO SMT.Logger
newFileLogger  m_f l  =
  do tab <- newIORef 0
     lev <- newIORef 0

     hdl <- case m_f of
              Nothing -> pure stdout
              Just f  -> openFile f WriteMode

     let logLevel    = readIORef lev
         logSetLevel = writeIORef lev

         shouldLog m =
           do cl <- logLevel
              when (cl >= l) m

         logMessage x = shouldLog $
           do let ls = lines x
              t <- readIORef tab
              hPutStr hdl $ unlines [ replicate t ' ' ++ l' | l' <- ls ]
              hFlush  hdl

         logTab   = shouldLog (modifyIORef' tab (+ 2))
         logUntab = shouldLog (modifyIORef' tab (subtract 2))
     return SMT.Logger { .. }





