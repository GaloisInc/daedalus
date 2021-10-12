{-# Language TypeApplications #-}
{-# Language DataKinds #-}
module Main(main) where

import GHC.Records(getField)

import Control.Monad(unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.List.NonEmpty(NonEmpty(..))
import Text.Show.Pretty(pPrint)
import System.Environment(getArgs)
import System.Exit(exitSuccess,exitFailure)
import System.Console.ANSI
import Hexdump

import RTS.Parser(runParser)
import RTS.ParserAPI
import RTS.Input
import qualified RTS.Vector as Vector
import RTS.Numeric(asInt)
import ICC(pMain, pParseTag, ProfileHeader, Tag, TagEntry)

main :: IO ()
main =
  do args <- getArgs
     case args of
      [file] -> parseFile (const fmtFAW) file
      ["-h",file] -> parseFile fmtHuman file
      _ -> putStrLn "USAGE: I need a single file, -h for verbose output."


parseFile :: (ByteString -> Format) -> FilePath -> IO ()
parseFile fmtF file =
  do bytes <- BS.readFile file
     let fmt = fmtF bytes
     let input = newInput (Text.encodeUtf8 (Text.pack file)) bytes
     case runParser pMain input of
       NoResults e ->
         do profileError fmt e
            bye False
       Results xs ->
         case xs of
           a :| [] ->
            do let hdr = getField @"profileHeader" a
               profileOK fmt hdr
               let v@(vmaj,vmin,_) = getVersion hdr
               unless (vmaj == 4 && vmin == 3) (versionMismatch fmt v)

               oks <- mapM (checkTag fmt input)
                           (Vector.toList (getField @"tagTable" a))
               bye (and oks)
           _ -> error "[BUG] Ambiguous parse in profile header"

bye :: Bool -> IO ()
bye allGood =
  if allGood then putStrLn "STATUS: all checks passed." >> exitSuccess
             else putStrLn "STATUS: detected errors." >> exitFailure


checkTag :: Format -> Input -> TagEntry -> IO Bool
checkTag fmt input entry =
  case runParser (pParseTag entry) input of
    NoResults e -> tagError fmt entry e >> pure False
    Results xs ->
      case xs of
        a :| [] -> tagOK fmt entry a >> pure True
        _ -> error ("[BUG] Ambiguous parse in tag: " ++ show entry)


getVersion :: ProfileHeader -> (Integer,Integer,Integer)
getVersion hdr =
  ( asInt (getField @"major" v)
  , asInt (getField @"minor" v)
  , asInt (getField @"bugfix" v)
  )
  where v = getField @"version" hdr


showVersion :: (Integer, Integer, Integer) -> String
showVersion (x,y,z) = show x ++ "." ++ show y ++ "." ++ show z

--------------------------------------------------------------------------------
data Format = Format
  { profileError :: ParseError -> IO ()
  , versionMismatch :: (Integer, Integer, Integer) -> IO ()
  , profileOK    :: ProfileHeader -> IO ()
  , tagError     :: TagEntry -> ParseError -> IO ()
  , tagOK        :: TagEntry -> Tag -> IO ()
  }


fmtFAW :: Format
fmtFAW = Format
  { profileError = \e   -> shortErr Nothing e
  , versionMismatch = \v ->
        putStrLn ("WARNING: document version " ++ showVersion v ++
                                          " does not match the spec")
  , profileOK    = \p ->
        putStrLn ("INFO: Version " ++ showVersion (getVersion p))
  , tagError     = \t e -> shortErr (Just t) e
  , tagOK        = \_ _ -> pure ()
  }
  where
  shortErr :: Maybe TagEntry -> ParseError -> IO ()
  shortErr mb e =
    let tagInfo =
          case mb of
            Nothing -> ""
            Just t ->
              let start = asInt (getField @"offset_to_data_element" t)
                  sz    = asInt (getField @"size_of_data_element" t)
                  tag   = getField @"tag_signature" t
              in "[" ++ show start ++ ":" ++ show sz ++ ":" ++ show tag ++  "] "
    in putStrLn ("ERROR:" ++ show (peOffset e) ++ " " ++ tagInfo ++ peMsg e)

fmtHuman :: ByteString -> Format
fmtHuman bytes = Format
  { profileError = \e -> putStrLn "[ERROR] Malformed header" >> humanError bytes e
  , versionMismatch = \v ->
        putStrLn ("[WARNING] Document version " ++ showVersion v ++
                                          " does not match the spec")
  , profileOK = \p -> do putStrLn "[OK] Header"

                         pPrint p
  , tagError = \ent e -> putStrLn ("[ERROR] " ++ show ent) >> humanError bytes e
  , tagOK = \ent _ -> putStrLn ("[OK] " ++ show ent)
  }


humanError :: ByteString -> ParseError -> IO ()
humanError bs err =
  do putStrLn "--- Parse error: "
     print (ppParseError err)
     let ctxtAmt = 32
         errLoc  = peOffset err
         start = max 0 (errLoc - ctxtAmt)
         end   = errLoc + 10
         len   = end - start
         ctx = BS.take len (BS.drop start bs)
         startErr =
            setSGRCode [ SetConsoleIntensity
                         BoldIntensity
                       , SetColor Foreground Vivid Red ]
         endErr = setSGRCode [ Reset ]
         cfg = defaultCfg { startByte = start
                          , transformByte =
                             wrapRange startErr endErr
                             errLoc errLoc }
     putStrLn "File context:"
     putStrLn $ prettyHexCfg cfg ctx


