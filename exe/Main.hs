{-# Language OverloadedStrings, ScopedTypeVariables, BlockArguments #-}
module Main where

import Data.Text () -- IsString
import qualified Data.Map as Map
import Control.Exception( catches, Handler(..), SomeException(..)
                        , displayException
                        )
import Control.Monad(when)
import Data.Maybe(fromMaybe)
import System.FilePath hiding (normalise)
import qualified Data.ByteString as BS
import System.Directory(createDirectoryIfMissing)
import System.Exit(exitSuccess,exitFailure,exitWith)
import System.IO(stdin,stdout,stderr,hSetEncoding,utf8)
import System.Console.ANSI
import Data.Traversable(for)
import Data.Foldable(for_,toList)
import Text.Show.Pretty (ppDoc)

import Hexdump

import Daedalus.PP
import Daedalus.SourceRange

import Daedalus.Driver

import qualified RTS.ParserAPI as RTS
import qualified RTS.Input as RTS

import Daedalus.AST hiding (Value)
import Daedalus.Interp
import Daedalus.Interp.Value(valueToJS)
import Daedalus.Compile.LangHS
import qualified Daedalus.ExportRuleRanges as Export
import Daedalus.Type.AST(TCModule(..))
import Daedalus.ParserGen as PGen
import qualified Daedalus.VM.Compile.Decl as VM
import qualified Daedalus.VM.BorrowAnalysis as VM
import qualified Daedalus.VM.InsertCopy as VM
import qualified Daedalus.VM.Backend.C as C

import CommandLine

main :: IO ()
main =
  do opts <- inputHack <$> getOptions
     when (optForceUTF8 opts)
       do hSetEncoding stdout utf8
          hSetEncoding stderr utf8
          hSetEncoding stdin  utf8
     daedalus (handleOptions opts)
       `catches`
       [ Handler \e ->
           do putStrLn =<< prettyDaedalusError e
              exitFailure

       , Handler \e -> exitWith e

       , Handler \(SomeException e) ->
           do putStrLn (displayException e)
              exitFailure
       ]


handleOptions :: Options -> Daedalus ()
handleOptions opts
  | DumpRuleRanges <- optCommand opts =
    do mm   <- ddlPassFromFile passResolve (optParserDDL opts)
       mods <- mapM (`ddlGetAST` astParse) =<< ddlBasis mm
       ddlPutStrLn (Export.jsModules mods)
       ddlIO exitSuccess

  | DumpRaw <- optCommand opts =
    do mm   <- ddlPassFromFile passParse (optParserDDL opts)
       mo <- ddlGetAST mm astParse
       ddlPrint (ppDoc mo)

  | otherwise =
    do mm <- ddlPassFromFile ddlLoadModule (optParserDDL opts)
       allMods <- ddlBasis mm
       let mainRule = (mm,"Main")
           specMod  = "DaedalusMain"
           mainNm = Name { nameScope = ModScope (fst mainRule) (snd mainRule)
                         , nameContext = AGrammar
                         , nameRange = synthetic
                         }

       case optCommand opts of

         DumpTC ->
           for_ allMods \m -> ddlPrint . pp =<< ddlGetAST m astTC

         DumpSpec ->
           do passSpecialize specMod [mainRule]
              mo <- ddlGetAST specMod astTC
              ddlPrint (pp mo)

         DumpNorm ->
           do passSpecialize specMod [mainRule]
              mapM_ (ddlPrint . pp) =<< normalizedDecls

         DumpCore ->
           do passSpecialize specMod [mainRule]
              passCore specMod
              ddlPrint . pp =<< ddlGetAST specMod astCore

         DumpVM ->
           do passSpecialize specMod [mainRule]
              passCore specMod
              passVM specMod
              passCaptureAnalysis
              m <- ddlGetAST specMod astVM
              entry <- ddlGetFName mainNm
              let prog = VM.addCopyIs
                       $ VM.doBorrowAnalysis
                       $ VM.moduleToProgram entry [m]
              ddlPrint (pp prog)

         DumpGen ->
           do passSpecialize specMod [mainRule]
              prog <- ddlGetAST specMod astTC
              -- prog <- normalizedDecls
              ddlIO (
                do let (_gbl, aut) = PGen.buildArrayAut [prog]
                   PGen.autToGraphviz aut
                   let dfa = PGen.createDFA aut
                   PGen.statsDFA aut dfa
                )

         Interp inp ->
           case optBackend opts of
             UseInterp ->
               do prog <- for allMods \m -> ddlGetAST m astTC
                  ddlIO (interpInterp (optShowJS opts) inp prog mainRule)
             UsePGen ->
               do passSpecialize specMod [mainRule]
                  prog <- ddlGetAST specMod astTC
                  ddlIO (interpPGen (optShowJS opts) inp [prog])
               --do passSpecialize specMod [mainRule]
               --   prog <- normalizedDecls
               --   ddlIO (interpPGen inp prog)

         DumpRuleRanges -> error "Bug: DumpRuleRanges"
         DumpRaw -> error "Bug: DumpRaw"

         CompileHS ->
            mapM_ (saveHS (optOutDir opts) cfg) allMods
            where
            cfg = CompilerCfg
                    { cPrims      = Map.empty -- Don't support prims
                    , cParserType = "Parser"
                    , cImports    = [Import "RTS.Parser" Unqualified]
                    , cQualNames  = UseQualNames
                    }

         CompileCPP ->
           -- XXX: package into Driver, but probably need to add proper
           -- support for multiple entry points, and entry points with
           -- parameters.
           case optBackend opts of
             UseInterp ->
               do passSpecialize specMod [mainRule]
                  passCore specMod
                  passVM specMod
                  passCaptureAnalysis
                  m <- ddlGetAST specMod astVM
                  entry <- ddlGetFName mainNm
                  let prog = VM.addCopyIs
                           $ VM.doBorrowAnalysis
                           $ VM.moduleToProgram entry [m]
                      outFileRoot = "main_parser"
                      (hpp,cpp) = C.cProgram outFileRoot prog
                  root <- case optOutDir opts of
                            Nothing -> pure outFileRoot
                            Just d  -> do ddlIO $ createDirectoryIfMissing True d
                                          pure (d </> outFileRoot)
                  ddlIO do writeFile (addExtension root "h") (show hpp)
                           writeFile (addExtension root "cpp") (show cpp)
             UsePGen ->
               do passSpecialize specMod [mainRule]
                  prog <- ddlGetAST specMod astTC
                  let outDir = fromMaybe "." $ optOutDir opts
                  compilePGen [prog] outDir

         ShowHelp -> ddlPutStrLn "Help!" -- this shouldn't happen


interpInterp ::
  Bool -> FilePath -> [TCModule SourceRange] -> (ModuleName,Ident) -> IO ()
interpInterp useJS inp prog (m,i) =
  do (_,res) <- interpFile inp prog (ModScope m i)
     dumpResult useJS res
     case res of
       Results {}   -> exitSuccess
       NoResults {} -> exitFailure


interpPGen :: Bool -> FilePath -> [TCModule SourceRange] -> IO ()
interpPGen useJS inp moduls =
  do let (gbl, aut) = PGen.buildArrayAut moduls
     let dfa = PGen.createDFA aut                   -- LL
     let repeatNb = 1 -- 200
     do mapM_ (\ _ ->
                 do bytes <- BS.readFile inp
                    let results = PGen.runnerLL gbl bytes aut dfa  -- LL
                    -- let results = PGen.runnerBias gbl bytes aut
                    let resultValues = PGen.extractValues results
                    if null resultValues
                      then do putStrLn $ PGen.extractParseError bytes results
                              exitFailure
                      else do dumpValues useJS resultValues
                              exitSuccess
              ) [(1::Int)..repeatNb]

compilePGen :: [TCModule SourceRange] -> FilePath -> Daedalus ()
compilePGen moduls outDir =
  do let (_, aut) = PGen.buildArrayAut moduls
     t <- ddlIO $ PGen.generateTextIO aut
     -- TODO: This needs more thought
     finalText <- completeContent t
     let outFile = outDir </> "grammar.c"
     ddlIO $ writeFile outFile finalText
     where
       completeContent t = do
         let templateFile = "." </> "rts-pgen-c" </> "template.c"
         template <- ddlIO $ readFile templateFile
         return $ template ++ t




inputHack :: Options -> Options
inputHack opts =
  case optCommand opts of
    DumpTC | let f = optParserDDL opts
             , takeExtension f == ".input"
             , let xs = takeWhile (/= '.') (takeFileName f) ->
               opts { optParserDDL = addExtension (takeDirectory f </> xs) "ddl"
                    , optCommand = Interp f
                    }
    _ -> opts



dumpResult :: Bool -> RTS.Result Value -> IO ()
dumpResult useJS r =
  case r of

   RTS.NoResults err ->
     do putStrLn "--- Parse error: "
        print (RTS.ppParseError err)
        let ctxtAmt = 32
            bs      = RTS.inputTopBytes (RTS.peInput err)
            errLoc  = RTS.peOffset err
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

   RTS.Results as -> dumpValues useJS (toList as)

dumpValues :: Bool -> [Value] -> IO ()
dumpValues useJS as =
  do putStrLn $ "--- Found " ++ show (length as) ++ " results:"
     print $ vcat' $ map (if useJS then valueToJS else pp) as


