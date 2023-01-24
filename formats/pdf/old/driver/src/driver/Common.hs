{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Common where


import System.Exit (exitFailure)
import System.IO (Handle, IOMode(..), openFile, stdout, stderr
                 , hPutStr, hPutStrLn, hFlush, hSetEncoding, utf8)

import RTS.ParserAPI
import RTS.ParserUntraced
import RTS.ParseError(ParseErrorG(..),peOffset,ppAnnot)
import CommandLine
import Control.Monad.State
import Text.PrettyPrint hiding ((<>))
import Data.Text(Text)
import qualified Data.Text.IO as TextIO

data ReportCode =
    RError
  | RWarning
  | RCritical
  | RUnsafe
  | RInfo

-- XXX: now we have more info and can print a much nicer error
ppParserError :: ParseError -> Doc
ppParserError pe = vcat $
  [ "byte offset:" <+> int (peOffset pe)
  , "error:" <+> text (peMsg pe)
  , "call stack:" $$ nest 2 (vcat [ ppAnnot x | x <- peStack pe ])
  ]


data ReportState  = ReportState { infoHandle   :: Handle
                                , errHandle    :: Handle
                                , accept       :: Bool -- Whether to accept or reject file
                                , safe         :: Bool -- Whether the file contains unsafe elements
                                }

newtype ReportM a = ReportM { runReportM :: StateT ReportState IO a }
  deriving (Applicative, Functor, Monad, MonadState ReportState, MonadIO)


finalReport :: ReportState -> IO ()
finalReport st = do
  -- FIXME: we always print, even if there were no errors.
  hPutStrLn (errHandle st) (if accept st then "ACCEPT" else "REJECT")
  hPutStrLn (errHandle st) (if safe st   then "SAFE"   else "UNSAFE")
  hFlush (infoHandle st)
  hFlush (errHandle st)

runReport :: Options -> ReportM a -> IO a
runReport opts m = do
  infoH <- case optOutput opts of
             "-" -> return stdout
             f   -> openFile f WriteMode
  let st = ReportState infoH stderr True True
  (r, st') <- runStateT (runReportM m) st
  finalReport st'
  return r

-- XXX: The error location needs to be more sophisticated to reporesent
-- stream locaiton.
report :: ReportCode -> FilePath -> Int -> Doc -> ReportM ()
report code fileN off d = do
  st <- get
  -- Print everything
  liftIO . hPutStr (infoHandle st) . unlines . map (pfx <>) . lines . show $ d
  let st' = case code of
             RError    -> st { accept = False }
             RWarning  -> st
             RCritical -> st { accept = False }
             RUnsafe   -> st { safe = False }
             RInfo     -> st
  put st'
  where
  pfx   = show (code' <+> "-" <+> text fileN <+> "at" <+> int off <+> "- ")
  code' = case code of
           RError    -> "ERROR"
           RWarning  -> "WARNING"
           RCritical -> "CRITICAL"
           RUnsafe   -> "UNSAFE"
           RInfo     -> "INFO"

-- Print text extracted to output file and does not change the report
reportTextExtraction :: Options -> Text -> ReportM ()
reportTextExtraction opts d = do
  outFile <- liftIO (
        case optTextOutput opts of
          "-" -> return stdout
          f   -> openFile f WriteMode
        )
  liftIO (do -- enc <- hGetEncoding outFile
             -- putStrLn (show enc)
             hSetEncoding outFile utf8
             TextIO.hPutStr outFile d
             hPutStr outFile "\n"
         )



-- same as above, but we can't continue.  Maybe we should throw an exception or something.
reportCritical :: FilePath -> Int -> Doc -> ReportM a
reportCritical fileN off d = do
  report RCritical fileN off d
  st <- get
  liftIO $ do finalReport st
              exitFailure
