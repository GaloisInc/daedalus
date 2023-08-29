{-# Language GeneralizedNewtypeDeriving #-}
{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}

-- Empty for now.
module Talos (
  -- * High-level synthesis operators
  synthesise,
  SynthesisOptions(..),
  summarise,
  residuals, 
  -- * Useful helpers
  runDaedalus,
  ProvenanceMap, -- XXX: should do this properly 
  ) where

import qualified Colog.Core                   as Log
import           Control.Monad                (forM_, unless, when)
import           Data.ByteString              (ByteString)
import           Data.IORef                   (modifyIORef', newIORef,
                                               readIORef, writeIORef)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe, isJust)
import           Data.String                  (fromString)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Version
import qualified SimpleSMT                    as SMT
import qualified Streaming                    as S
import           System.Exit                  (exitFailure)
import           System.IO                    (IOMode (..), hFlush, hPutStr,
                                               hPutStrLn, openFile, stderr)
import qualified Text.ParserCombinators.ReadP as RP
import           Text.ParserCombinators.ReadP (readP_to_S)

import           Daedalus.AST                 (nameScopeAsModScope)
import           Daedalus.Core
import           Daedalus.Driver              hiding (State)
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Rec                 (forgetRecs)
import           Daedalus.Type.AST            (tcDeclName, tcModuleDecls)
import           Daedalus.Value               (Value)

import           Data.Functor.Of              (Of)
import qualified Talos.Analysis               as A
import           Talos.Analysis.AbsEnv        (AbsEnvTy (AbsEnvTy))
import           Talos.Analysis.Monad         (makeDeclInvs)
import           Talos.Monad                  (LogKey, getLogKey, logKeyEnabled,
                                               runTalosM, runTalosStream)
import           Talos.Passes
import           Talos.Path                   (ProvenanceMap)
import qualified Talos.Residuals              as R
import           Talos.Strategy
import qualified Talos.Synthesis              as T

-- -- FIXME: move, maybe to GUID.hs?
-- newtype FreshGUIDM a = FreshGUIDM { getFreshGUIDM :: State GUID a }
--   deriving (Functor, Applicative, Monad)

-- instance HasGUID FreshGUIDM where
--   getNextGUID = FreshGUIDM $ state (mkGetNextGUID' id const)

summarise :: FilePath -> Maybe FilePath -> Maybe String -> Int -> Bool ->
             String -> IO Doc
summarise inFile m_invFile m_entry verbosity noLoops absEnv = do
  (_mainRule, md, nguid) <- runDaedalus inFile m_invFile m_entry noLoops

  AbsEnvTy p <- case lookup absEnv A.absEnvTys of
    Just x -> pure x
    _      -> errorExit ("Unknown abstract env " ++ absEnv)

  let invs = makeDeclInvs (mGFuns md) (mFFuns md)
  putStrLn "Inverses"
  print (pp <$> Map.keys invs)
  putStrLn "Slices"
  summs <- runTalosM md nguid mempty mempty (A.summarise p)
  
  pure (bullets (map goF (Map.toList summs)))
  where
    goF (fn, m) =
      hang (pp fn) 2 $
        bullets [ hang (pp fid) 2 (pp d)
                | (fid, d) <- Map.toList m]

residuals :: FilePath -> Maybe String -> IO Doc
residuals inFile m_entry = do
  (mainRule, md, nguid) <- runDaedalus inFile Nothing m_entry False
  
  resfuns <- runTalosM md nguid mempty mempty (R.residuals mainRule)
  
  pure (bullets (map pp resfuns))
  where
    -- goF fun = pp fun
    --   -- hang (pp fn) 2 $
    --   --   bullets [ hang (pp fid) 2 (pp d)
    --   --           | (fid, d) <- Map.toList m]


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

data SynthesisOptions = SynthesisOptions
  { inputFile       :: FilePath           -- ^ DDL file
  , inverseFile     :: Maybe FilePath     -- ^ Inverse file
  , entry           :: Maybe String       -- ^ Entry
  , solverPath      :: FilePath           -- ^ Backend solver executable
  , solverArgs      :: [String]           -- ^ Solver args
  , solverOpts      :: [(String, String)] -- ^ Backend solver options
  , solverInit      :: IO ()              -- ^ Backend solver init
  , synthesisStrats :: Maybe [String]     -- ^ Synthesis strategies
  , seed            :: Maybe Int          -- ^ Random seed
  , analysisEnv     :: String             -- ^ Analysis abstract env.
  , verbosity       :: Int                -- ^ Verbosity
  , eraseLoops      :: Bool               -- ^ No loops
  , logFile         :: Maybe FilePath     -- ^ General logging
  , debugKeys       :: [String]           -- ^ Keys for logging debug messages
  , statsFile       :: Maybe FilePath     -- ^ Output file for stats
  , statsKeys       :: [String]           -- ^ Keys for stats
  , smtLogFile      :: Maybe FilePath     -- ^ SMT logging file  
  }

synthesise :: SynthesisOptions
           -> IO (S.Stream (Of (Value, ByteString, ProvenanceMap)) IO ())
synthesise SynthesisOptions { .. } = do
  (mainRule, md, nguid) <- runDaedalus inputFile inverseFile entry eraseLoops

  -- SMT init
  logger <- case smtLogFile of
              Nothing -> pure Nothing
              Just f  -> Just <$> newFileLogger f 0

  solver <- SMT.newSolver solverPath solverArgs logger

  -- Check version: z3 before 4.8.10 (or .9) seems to have an issue
  -- with match.  
  -- z3VersionCheck solver

  -- Set options
  forM_ solverOpts $ \(opt, val) -> do
    r <- SMT.setOptionMaybe solver (':' : opt) val
    unless r $ hPutStrLn stderr ("WARNING: solver does not support option " ++ opt)

  absty <- case lookup analysisEnv A.absEnvTys of
    Just x -> pure x
    _      -> errorExit ("Unknown abstract env " ++ analysisEnv)

  let strats = fromMaybe ["pathsymb"] synthesisStrats

  stratInsts <- case parseStrategies strats of
                  Left err -> errorExit err
                  Right sis -> pure sis

  -- Setup stdlib by initializing the solver and then defining the
  -- Talos standard library
  solverInit

  when (isJust statsFile && null statsKeys) $
    errorExit "ERROR: no stats keys but stats output requested"
  
  withLogStringFileMaybe logFile Log.logStringStdout $ \logact ->
    withLogStringFileMaybe statsFile mempty $ \statsact -> do
    let strm = T.synthesise seed solver absty stratInsts mainRule
        logact'   = Log.cmapMaybe logAction logact
        statsact' = Log.cfilter (keyCFilter (map Text.pack statsKeys)) (Log.cmap ppStat statsact)
        
    pure (runTalosStream md nguid statsact' logact' strm)

  where
    -- We have a lazy stream which persists after the call returns, so
    -- we can't use Log.withLogStringFile (as it closes the handle)
    withLogStringFileMaybe Nothing    dflt f = f dflt
    withLogStringFileMaybe (Just fn) _dlft f = do
      hdl <- openFile fn WriteMode
      f (Log.logStringHandle hdl <> Log.logFlush hdl)

    logAction (key, (lvl, msg))
      -- Always produce debug output if the key was requested
      | any (logKeyEnabled key) (map Text.pack debugKeys) = Just msg'
      -- Otherwise follow the verbosity level
      | verbosity >= fromEnum lvl             = Just msg'
      | otherwise = Nothing
      where
        msg' = "[" <> show lvl <> "] " <> Text.unpack (getLogKey key) <> " " <> msg

    ppStat (key, stat) = Text.unpack (getLogKey key) <> " " <> Text.unpack stat
        
-- Passes on only those keys that are prefixed by one of the argument
-- keys
keyCFilter :: [Text] -> (LogKey, a) -> Bool
keyCFilter ks = \(k, _v) -> any (logKeyEnabled k) ks 

-- | Run DaeDaLus on a source file, returning a triple that consists
-- of the name of the main rule (the entry point), a list of type
-- declarations (that are named struct or union types), and a list of
-- type-checked modules. The main rule's name includes the module
-- information needed to find it.r
runDaedalus :: FilePath -> Maybe FilePath -> Maybe String -> Bool ->
               IO (FName, Module, GUID)
runDaedalus inFile m_invFile m_entry noLoops = daedalus $ do
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
  passNoBitdata specMod
  when noLoops $ passNoLoops specMod
  passStripFail specMod
  passSpecTys specMod
  passConstFold specMod
  
  entry <- ddlGetFName mm entryName

  md    <- ddlGetAST specMod astCore >>= ddlRunPass . allPassesM entry

  nguid <- ddlRunPass getNextGUID

  pure (entry, md, nguid)




newFileLogger :: FilePath -> Int -> IO SMT.Logger
newFileLogger f l  =
  do tab <- newIORef 0
     lev <- newIORef 0

     hdl <- openFile f WriteMode

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





