{-# Language ScopedTypeVariables #-}
{-# Language ImplicitParams #-}
{-# Language GADTs #-}
module Main where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Map as Map
import Control.Exception( catches, Handler(..), SomeException(..)
                        , displayException
                        )
import Control.Monad(when,unless,forM)
import Data.Maybe(fromMaybe,fromJust,isNothing)
import System.FilePath hiding (normalise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Directory(createDirectoryIfMissing)
import System.Exit(exitSuccess,exitFailure,exitWith)
import System.IO(stdin,stdout,stderr,hPutStrLn,hSetEncoding,utf8)
import System.Console.ANSI
import Data.Traversable(for)
import Data.Foldable(for_,toList)
import Text.Show.Pretty (ppDoc)

import Hexdump

import Daedalus.Panic(panic)
import Daedalus.PP hiding ((<.>))
import Daedalus.SourceRange
import Daedalus.Core(checkModule)

import Daedalus.Driver
import Daedalus.DriverHS

-- import qualified RTS.ParserTraced as RTS
import qualified RTS.Input as RTS
import qualified RTS.ParseError as RTS
import qualified RTS.ParserAPI as RTS

import Daedalus.Value
import Daedalus.Interp

import Daedalus.AST hiding (Value)
import Daedalus.Compile.LangHS hiding (Import(..))
import qualified Daedalus.Compile.LangHS as HS
import Daedalus.CompileHS(hsIdentMod)
import qualified Daedalus.TH.Compile as THC
import Daedalus.Type.AST(TCModule(..))
import Daedalus.Type.Monad(TypeWarning(..))
import Daedalus.Type.Pretty(ppTypes)
import Daedalus.ParserGen as PGen
import qualified Daedalus.Core as Core
import qualified Daedalus.Core.Semantics.Decl as Core
import qualified Daedalus.Core.Inline as Core
import qualified Daedalus.Core.NoMatch as Core
import qualified Daedalus.Core.X as Core
import qualified Daedalus.VM as VM
import qualified Daedalus.VM.Compile.Decl as VM
import qualified Daedalus.VM.BorrowAnalysis as VM
import qualified Daedalus.VM.InsertCopy as VM
import qualified Daedalus.VM.GraphViz as VM
import qualified Daedalus.VM.Backend.C as C
import qualified Daedalus.VM.Semantics as VM

import CommandLine
import Templates

main :: IO ()
main =
  do opts <- inputHack <$> getOptions
     when (optForceUTF8 opts)
       do hSetEncoding stdout utf8
          hSetEncoding stderr utf8
          hSetEncoding stdin  utf8

     let printError = hPutStrLn stderr

     daedalus (configure opts >> handleOptions opts)
       `catches`
       [ Handler \e ->
           do printError =<< prettyDaedalusError e
              exitFailure

       , Handler \e -> exitWith e

       , Handler \e ->
          do printError ("[Error] " ++ displayException (e :: InterpError))
             exitFailure

       , Handler \(SomeException e) ->
           do printError (displayException e)
              exitFailure
       ]


-- Currently during specialization we combine all input modules into a single
-- module.  This is the name of the resulting module
specMod :: ModuleName
specMod = "DaedalusMain"


configure :: Options -> Daedalus ()
configure opts =
  do when (optNoWarnUnbiasedFront opts) $
       ddlSetOpt optWarnings \w -> case w of
                                     WarnUnbiasedChoice {} -> False
                                     _                     -> True
     ddlSetOpt optSearchPath (optModulePath opts)

handleOptions :: Options -> Daedalus ()
handleOptions opts

  | DumpRaw <- optCommand opts =
    do mm   <- ddlPassFromFile passParse (optParserDDL opts)
       mo   <- ddlGetAST mm astParse
       ddlPrint (ppDoc mo)

  | DumpResolve <- optCommand opts =
    do mm       <- ddlPassFromFile passResolve (optParserDDL opts)
       mo       <- ddlGetAST mm astParse
       ddlPrint (ppDoc mo)

  | DumpTypes <- optCommand opts =
    do mm  <- ddlPassFromFile passTC (optParserDDL opts)
       mo  <- ddlGetAST mm astTC
       ddlPrint (ppTypes mo)

  | JStoHTML <- optCommand opts = jsToHTML opts

  | otherwise =
    do mm <- ddlPassFromFile ddlLoadModule (optParserDDL opts)
       allMods <- ddlBasis mm

       let mainRules = parseEntries opts mm
       case optCommand opts of

         DumpTC ->
           for_ allMods \m -> ddlPrint . pp =<< ddlGetAST m astTC

         DumpSpec ->
           do passSpecialize specMod mainRules
              mo <- ddlGetAST specMod astTC
              ddlPrint (pp mo)

         DumpCore ->
            do _ <- doToCore opts mm
               ddlPrint . pp =<< ddlGetAST specMod astCore

         DumpRel -> dumpRel opts mm

         DumpVM -> ddlPrint . pp =<< doToVM opts mm

         DumpGraph onlyFun ->
           do prog <- doToVM opts mm
              let sty = if onlyFun then VM.OnlyCalls else VM.Everything
              ddlPutStrLn (VM.toGraphViz sty prog)


         DumpGen ->
           do passSpecialize specMod mainRules
              prog <- ddlGetAST specMod astTC
              ddlIO (
                do let aut = PGen.buildArrayAut [prog]
                   PGen.autToGraphviz aut
                   let llas = PGen.buildPipelineLLA aut
                   PGen.llaToGraphviz aut llas
                   PGen.statsLLA aut llas
                )

         Interp inp ->
           case optBackend opts of
             UseInterp ->
               do prog <- for allMods \m -> ddlGetAST m astTC
                  ddlIO (interpInterp opts inp prog mainRules)

             UseCore -> interpCore opts mm inp

             UseVM -> interpVM opts mm inp

             UsePGen flagMetrics ->
               do passSpecialize specMod mainRules
                  prog <- ddlGetAST specMod astTC
                  ddlIO (interpPGen (optShowJS opts) inp [prog] flagMetrics)

         DumpRaw -> error "Bug: DumpRaw"
         DumpResolve -> error "Bug: DumpResolve"
         DumpTypes -> error "Bug: DumpTypes"
         JStoHTML -> error "Bug: JStoHTML"

         CompileHS -> generateHS opts mm allMods

         CompileCPP ->
           -- XXX: this is a backend in a different sense
           case optBackend opts of
             UseInterp -> generateCPP opts mm
             UseCore   -> generateCPP opts mm
             UseVM     -> generateCPP opts mm
             UsePGen _ ->
               do passSpecialize specMod mainRules
                  prog <- ddlGetAST specMod astTC
                  let outDir = fromMaybe "." $ optOutDir opts
                  compilePGen [prog] outDir


         ShowHelp -> ddlPutStrLn "Help!" -- this shouldn't happen


interpInterp ::
  Options -> Maybe FilePath -> [TCModule SourceRange] -> [(ModuleName,Ident)] ->
    IO ()
interpInterp opts inp prog ents =
  do start <- case [ ModScope m i | (m,i) <- ents ] of
                [ent] -> pure ent
                es -> interpError (MultipleStartRules es)
     (_,res) <- interpFile inp prog start
     let ?useJS = optShowJS opts
     let txt1   = dumpResult dumpInterpVal res
         txt2   = if optShowHTML opts then dumpHTML txt1 else txt1
     print txt2
     case res of
       Results {}   -> exitSuccess
       NoResults {} -> exitFailure

interpCore :: Options -> ModuleName -> Maybe FilePath -> Daedalus ()
interpCore opts mm inpMb =
  do ents <- doToCore opts mm
     env  <- Core.evalModuleEmptyEnv <$> ddlGetAST specMod astCore
     inp  <- ddlIO (RTS.newInputFromFile inpMb)
     let ?useJS = optShowJS opts
     -- XXX: html, etc
     undefined
{-
     ddlIO $ forM_ ents \ent ->
                  print (dumpResult dumpInterpVal (Core.runEntry env ent inp))
-}

interpVM :: Options -> ModuleName -> Maybe FilePath -> Daedalus ()
interpVM opts mm inpMb =
 do r <- doToVM opts mm
    inp  <- ddlIO (RTS.newInputFromFile inpMb)
    let entries = VM.semModule (head (VM.pModules r))
    let ?useJS = optShowJS opts
    for_ (Map.elems entries) \impl ->
        ddlPrint (dumpValues dumpInterpVal (VM.resultToValues (impl [VStream inp])))

doToCore :: Options -> ModuleName -> Daedalus [Core.FName]
doToCore opts mm =
  do let entries = parseEntries opts mm
     passSpecialize specMod entries
     passCore specMod
     checkCore "Core"
     ents <- mapM (uncurry ddlGetFName) entries
     when (optInline opts)
        do fs <- forM (optInlineThis opts) \s ->
                    uncurry ddlGetFName (parseEntry specMod s)
           case fs of
             [] -> passInline Core.AllBut ents specMod
             _  -> passInline Core.Only fs specMod
           checkCore "Inline"

     when (optStripFail opts) (passStripFail specMod >> checkCore "StripFail")
     when (optSpecTys opts) (passSpecTys specMod >> checkCore "SpecTys")
     when (optDeterminize opts) (passDeterminize specMod >> checkCore "Det")
     when (optDeterminize opts) (passNorm specMod >> checkCore "Norm")
     unless (optNoWarnUnbiased opts) (passWarnFork specMod)
     pure ents

  where
  checkCore x =
    when (optCheckCore opts)
       do core <- ddlGetAST specMod astCore
          case checkModule core of
            Just err -> panic ("Malformed Core [" ++ x ++ "]") [ show err ]
            Nothing  -> pure ()



doToVM :: Options -> ModuleName -> Daedalus VM.Program
doToVM opts mm =
  do ddlSetOpt optDebugMode (optErrorStacks opts)
     _ <- doToCore opts mm
     passVM specMod
     m <- ddlGetAST specMod astVM
     let addMM = VM.addCopyIs . VM.doBorrowAnalysis
     pure $ addMM $ VM.moduleToProgram [m]


parseEntries :: Options -> ModuleName -> [(ModuleName,Ident)]
parseEntries opts mm =
  case optEntries opts of
    [] -> [(mm,"Main")]
    es -> map (parseEntry mm) es

parseEntry :: ModuleName -> String -> (ModuleName,Ident)
parseEntry mm x =
  case break (== '.') x of
    (as,_:bs) -> (Text.pack as, Text.pack bs)
    _         -> (mm, Text.pack x)


generateCPP :: Options -> ModuleName -> Daedalus ()
generateCPP opts mm =
  do let makeExe = null (optEntries opts) && isNothing (optUserState opts)
     when (makeExe && optOutDir opts == Nothing)
       $ ddlIO $ throwOptError
           [ "Generating a parser executable requires an output directory" ]

     prog <- doToVM opts mm
     let ccfg = C.CCodeGenConfig
                  { cfgFileNameRoot = optFileRoot opts
                  , cfgUserState    = text <$> optUserState opts
                  , cfgUserNS       = text (optUserNS opts)
                  , cfgExtraInclude = optExtraInclude opts
                  , cfgExternal     = optExternMods opts
                  }
         (hpp,cpp,warns) = C.cProgram ccfg prog

     mapM_ (\w -> ddlPrint ("[WARNING]" <+> w)) warns
     ddlIO (saveFiles makeExe (C.cfgFileNameRoot ccfg) hpp cpp)

  where
  saveFiles makeExe outFileRoot hpp cpp =
    do dir <- case optOutDir opts of
                Nothing -> pure "."
                Just d  -> do createDirectoryIfMissing True d
                              pure d
       hdir <- case optOutDirHeaders opts of
                 Nothing -> pure dir
                 Just d  -> do createDirectoryIfMissing True d
                               pure d

       let root = dir </> outFileRoot
           hroot = hdir </> outFileRoot
       writeFile (addExtension hroot "h") (show hpp)
       writeFile (addExtension root "cpp") (show cpp)

       when makeExe
         do let save (x,b) =
                  do let d = dir </> takeDirectory x
                     createDirectoryIfMissing True d
                     BS.writeFile (dir </> x) b
            mapM_ save c_template_files

generateHS :: Options -> ModuleName -> [ModuleName] -> Daedalus ()
generateHS opts mainMod allMods
  | hsoptCore hsopts =
    let cfg = THC.defaultConfig -- XXX
    in ddlIO $ THC.saveDDLWith cfg (THC.FromModule mainMod) (Just "out.hs") --XX

  | otherwise =
  do let makeExe = null (optEntries opts)
     when (makeExe && optOutDir opts == Nothing)
       $ ddlIO $ throwOptError
           [ "Generating a parser executable requires an output directory" ]
     cfg <- case hsoptFile hsopts of
              Nothing -> pure  (const dfltCfg)
              Just f  -> ddlIO (moduleConfigsFromFile dfltCfg f)

     let saveModule = saveHS (optOutDir opts) cfg
     mapM_ saveModule allMods
     when makeExe $ ddlIO
       do let outD = fromJust (optOutDir opts)
              name = takeFileName outD
              mkMod = Text.encodeUtf8 . Text.pack . hsIdentMod
              vars = Map.fromList
                       [ ("EXE", BS8.pack name)
                       , ("AUTHOR", "Daedalus")
                       , ("EMAIL", "unknown@email.com")
                       , ("MODULES", BS8.intercalate "," (map mkMod allMods))
                       ]

              mainVars = Map.fromList
                [ ("IMPORT", mkMod mainMod <> "(pMain)")
                ]
              Just main_template  = lookup "Main.hs" hs_template_files
              Just cabal_template = lookup "template.cabal" hs_template_files


          BS.writeFile (outD </> "Main.hs")
                       (substTemplate mainVars main_template)

          BS.writeFile (outD </> name <.> "cabal")
                       (substTemplate vars cabal_template)

  where
  hsopts = optHS opts

  imps = [ HS.Import a case b of
                         Nothing -> Unqualified
                         Just m  -> QualifyAs m
         | (a,b) <- hsoptImports hsopts ]

  primMap = Map.fromList
              [ (primName m x, aps (Var p)) | (m,x,p) <- hsoptPrims hsopts ]

  dfltCfg =
    CompilerCfg
      { cPrims      = primMap
      , cParserType = Var <$> hsoptMonad hsopts
      , cImports    = imps
      , cQualNames  = UseQualNames
      }




interpPGen :: Bool -> Maybe FilePath -> [TCModule SourceRange] -> Bool -> IO ()
interpPGen useJS inp moduls flagMetrics =
  do let ?useJS = useJS
     let aut = PGen.buildArrayAut moduls
     let lla = PGen.createLLA aut                   -- LL
     let repeatNb = 1 -- 200
     do
       mapM_
         (\ i ->
           do bytes <-
                case inp of
                  Nothing -> pure BS.empty
                  Just f  -> BS.readFile f
              let results = PGen.runnerLL bytes aut lla flagMetrics  -- LL
              -- let results = PGen.runner bytes aut
              let resultValues = PGen.extractValues results
              if null resultValues
                then
                  do putStrLn $ PGen.extractParseError bytes results
                     if flagMetrics
                       then
                         let countBacktrack = fst (extractMetrics results)
                             countLL =        snd (extractMetrics results)
                         in
                           putStrLn
                           ( "\nScore (LL / (Backtrack + LL)): " ++
                             if (countBacktrack + countLL) == 0
                             then "NA"
                             else (show ((countLL * 100) `div` (countBacktrack + countLL))) ++ "%"
                           )
                       else
                       exitFailure
                else
                  do
                    if (i == 1)
                      then print $ dumpValues dumpInterpVal resultValues
                      else return ()
                    if flagMetrics
                      then
                      let countBacktrack = fst (extractMetrics results)
                          countLL =        snd (extractMetrics results)
                      in
                        do putStrLn
                             ( "\nScore (LL / (Backtrack + LL)): " ++
                               if (countBacktrack + countLL) == 0
                               then "NA"
                               else (show ((countLL * 100) `div` (countBacktrack + countLL))) ++ "%"
                             )
                      else return ()
                    exitSuccess -- comment this with i > 1
         ) [(1::Int)..repeatNb]

compilePGen :: [TCModule SourceRange] -> FilePath -> Daedalus ()
compilePGen moduls outDir =
  do let aut = PGen.buildArrayAut moduls
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
                    , optCommand = Interp (Just f)
                    }
    _ -> opts



dumpResult :: (?useJS :: Bool) => (a -> Doc) -> Result a -> Doc
dumpResult ppVal r =
  case r of
   RTS.NoResults err -> dumpErr err
   RTS.Results as -> dumpValues ppVal' (toList as)
  where
  ppVal' (a,x) = ppVal a $$ "----" $$ RTS.ppITrace x

dumpValues :: (?useJS :: Bool) => (a -> Doc) -> [a] -> Doc
dumpValues ppVal as
  | ?useJS = brackets (vcat $ punctuate comma $ map ppVal as)
  | otherwise =
    vcat [ "--- Found" <+> int (length as) <+> "results:"
         , vcat' (map ppVal as)
         ]

dumpInterpVal :: (?useJS :: Bool) => Value -> Doc
dumpInterpVal = if ?useJS then valueToJS else pp

dumpErr :: (?useJS :: Bool) => ParseError -> Doc
dumpErr err
  | ?useJS = RTS.jsToDoc (parseErrorTrieToJSON err)
  | otherwise =
    vcat
      [ "--- Parse error: "
      , text (show (RTS.ppParseError err))
      , "File context:"
      , text (prettyHexCfg cfg ctx)
      ]
  where
  ctxtAmt = 32
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


jsToHTML :: Options -> Daedalus ()
jsToHTML opts =
  ddlPrint . dumpHTML . text =<<
    ddlIO
    if null (optParserDDL opts)
      then getContents
      else readFile (optParserDDL opts)


dumpHTML :: Doc -> Doc
dumpHTML jsData = vcat
  [ "<html>"
  , "<head>"
  , "<style>"
  , bytes tstyle
  , "</style>"
  , "<script>"
  , "const inf = 'inf'"
  , "const data =" <+> jsData
  , bytes trender
  , "</script>"
  , "</head>"
  , "<body id='container' onload=\"main()\">"
  , "</body>"
  , "</html>"
  ]
  where
  Just tstyle  = lookup "style.css" html_files
  Just trender = lookup "render.js" html_files
  bytes = text . BS8.unpack


dumpRel ::  Options -> ModuleName -> Daedalus ()
dumpRel opts mm =
  do _    <- doToCore opts mm
     m    <- ddlGetAST specMod astCore
     m1   <- ddlRunPass (Core.noMatch m)
     fs   <- ddlRunPass (Core.suceedsMod m1)
     ddlPrint (vcat (map pp fs))
