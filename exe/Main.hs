{-# Language OverloadedStrings, ScopedTypeVariables, BlockArguments #-}
module Main where

import Data.Text () -- IsString
import qualified Data.Map as Map
import Control.Exception( catches, Handler(..), SomeException(..)
                        , displayException
                        )
import Control.Monad(when)
import System.FilePath hiding (normalise)
import qualified Data.ByteString as BS
import System.Exit(exitSuccess,exitFailure,exitWith)
import System.IO(stdin,stdout,stderr,hSetEncoding,utf8)
import System.Console.ANSI
import Data.Traversable(for)
import Data.Foldable(for_,toList)

import Hexdump

import Daedalus.PP
import Daedalus.SourceRange

import Daedalus.Driver

import qualified RTS.ParserAPI as RTS
import qualified RTS.Input as RTS

import Daedalus.AST hiding (Value)
import Daedalus.Interp
import Daedalus.Compile.LangHS
import qualified Daedalus.ExportRuleRanges as Export
import Daedalus.Normalise.AST(NDecl)
import Daedalus.Type.AST(TCModule(..))
import Daedalus.ParserGen as PGen
import qualified Daedalus.VM.Compile.Decl as VM
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

  | otherwise =
    do mm <- ddlPassFromFile ddlLoadModule (optParserDDL opts)
       allMods <- ddlBasis mm
       let mainRule = (mm,"Main")
           specMod  = "DaedalusMain"

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
              ddlPrint . pp =<< ddlGetAST specMod astVM

         DumpGen ->
           do passSpecialize specMod [mainRule]
              prog <- normalizedDecls
              ddlIO (
                do let (_gbl, aut) = PGen.buildArrayAut prog
                   let dfa = PGen.createDFA aut
                   PGen.statsDFA dfa
                   PGen.autToGraphviz aut
                )

         Interp inp ->
           case optBackend opts of
             UseInterp ->
               do prog <- for allMods \m -> ddlGetAST m astTC
                  ddlIO (interpInterp inp prog mainRule)
             UsePGen ->
               do passSpecialize specMod [mainRule]
                  prog <- normalizedDecls
                  ddlIO (interpPGen inp prog)

         DumpRuleRanges -> error "Bug: DumpRuleRanges"

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
           do passSpecialize specMod [mainRule]
              passCore specMod
              passVM specMod
              passCaptureAnalysis
              m <- ddlGetAST specMod astVM
              let nm = Name { nameScope = ModScope (fst mainRule) (snd mainRule)
                            , nameContext = AGrammar
                            , nameRange = synthetic
                            }
              entry <- ddlGetFName nm
              let prog = VM.moduleToProgram entry [m]
              ddlPrint (C.cProgram prog)

         ShowHelp -> ddlPutStrLn "Help!" -- this shouldn't happen


interpInterp ::
  FilePath -> [TCModule SourceRange] -> (ModuleName,Ident) -> IO ()
interpInterp inp prog (m,i) =
  do (_,res) <- interpFile inp prog (ModScope m i)
     dumpResult res
     case res of
       Results {}   -> exitSuccess
       NoResults {} -> exitFailure


interpPGen :: FilePath -> [NDecl] -> IO ()
interpPGen inp norms =
  do let (gbl, aut) = PGen.buildArrayAut norms
     bytes <- BS.readFile inp
     -- let dfa = PGen.createDFA aut                   -- LL
     let repeatNb = 1 -- 200
     do mapM_ (\ _ ->
                 do -- let results = PGen.runnerLL gbl bytes aut dfa  -- LL
                    let results = PGen.runnerBias gbl bytes aut
                    let resultValues = PGen.extractValues results
                    if null resultValues
                      then do putStrLn $ PGen.extractParseError bytes results
                              exitFailure
                      else do putStrLn $ "--- Found " ++ show (length resultValues) ++ " results:"
                              print $ vcat' $ map pp $ resultValues
                              exitSuccess
              ) [(1::Int)..repeatNb]


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



dumpResult :: PP a => RTS.Result a -> IO ()
dumpResult r =
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

   RTS.Results as ->
     do putStrLn $ "--- Found " ++ show (length as) ++ " results:"
        print $ vcat' $ map pp $ toList as
