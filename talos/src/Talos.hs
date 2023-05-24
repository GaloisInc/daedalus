{-# Language GeneralizedNewtypeDeriving #-}
{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}

-- Empty for now.
module Talos (
  -- * High-level synthesis operators
  synthesise,
  SynthesisOptions(..),
  summarise,
  -- * Useful helpers
  runDaedalus,
  ProvenanceMap, -- XXX: should do this properly 
  ) where

import           Control.Monad                (forM_, unless, when)
import           Control.Monad.State
import           Data.ByteString              (ByteString)
import           Data.IORef                   (modifyIORef', newIORef,
                                               readIORef, writeIORef)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           Data.String                  (fromString)
import           Data.Version
import qualified SimpleSMT                    as SMT
import qualified Streaming                    as S
import           System.Exit                  (exitFailure)
import           System.IO                    (Handle, IOMode (..), hFlush,
                                               hPutStr, hPutStrLn, openFile,
                                               stderr, stdout)
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
import           Talos.Monad                  (runTalosM, runTalosStream)
import           Talos.Passes
import           Talos.Strategy
import           Talos.SymExec.Path           (ProvenanceMap)
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
  summs <- runTalosM md nguid Nothing (A.summarise p)
  
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

data SynthesisOptions = SynthesisOptions
  { inputFile       :: FilePath           -- ^ DDL file
  , inverseFile     :: Maybe FilePath     -- ^ Inverse file
  , entry           :: Maybe String       -- ^ Entry
  , solverPath      :: FilePath           -- ^ Backend solver executable
  , solverArgs      :: [String]           -- ^ Solver args
  , solverOpts      :: [(String, String)] -- ^ Backend solver options
  , solverInit      :: IO ()              -- ^ Backend solver init
  , synthesisStrats :: Maybe [String]     -- ^ Synthesis strategies
  , loggingOpts     :: Maybe (Int, Maybe FilePath) -- ^ Logging options
  , seed            :: Maybe Int          -- ^ Random seed
  , analysisEnv     :: String             -- ^ Analysis abstract env.
  , verbosity       :: Int                -- ^ Verbosity
  , eraseLoops      :: Bool               -- ^ No loops
  , statsHandle     :: Maybe Handle       -- ^ Output file for stats
  }

synthesise :: SynthesisOptions
           -> IO (S.Stream (Of (Value, ByteString, ProvenanceMap)) IO ())
synthesise SynthesisOptions { .. } = do
  (mainRule, md, nguid) <- runDaedalus inputFile inverseFile entry eraseLoops

  -- SMT init
  logger <- case loggingOpts of
              Nothing        -> pure Nothing
              Just (i, m_f)  -> Just <$> newFileLogger m_f i

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
  let strm = T.synthesise seed solver absty stratInsts mainRule
  pure (runTalosStream md nguid statsHandle strm)

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





