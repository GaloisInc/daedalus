{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

-- base

import qualified Data.ByteString.Char8 as B8 
import qualified Data.ByteString       as BS
import           Data.List
import           System.Environment
import           System.Exit

-- pkg process:
import System.Process

-- local:
import DomGeneration
import DomRender
import DomExamples


---- types -------------------------------------------------------------------

data Test = Test { t_name     :: String 
                 , t_dom      :: PDF_DOM
                 , t_rtype    :: RenderType
                 , t_edits    :: Edits
                 , t_ctgy     :: Ctgy
                 , t_comments :: [BS]
                 }

data Edits = E_None | E_Shell String [String]
             deriving (Eq, Ord, Read, Show)

data Ctgy = Good                       -- ^ completely valid PDF
          | Probs [(MsgCtgy, String)]  -- ^ some forms of warnings/errors
          deriving (Eq, Ord, Read, Show)

data MsgCtgy = Error
             | Warn         -- distinguish exuberance?
             | NotStrict
             deriving (Eq, Ord, Read, Show)

---- functions ---------------------------------------------------------------

main =
  do
  as <- getArgs
  case as of
    [p] -> mapM_ (writeTest p) allTests
    _   -> usageError

usageError :: IO a
usageError =
  do
  pn <- getProgName
  putStrLn $ "Usage: " ++ pn ++ " directory-to-write-files"
  exitFailure

renderTest :: Test -> IO BS
renderTest (Test _nm (h1,h2,ds,di) ty edits ctgy notes) =
  applyEdits edits
             (render_PDF_DOM ty (h1, commentsAndSuch <> h2, ds, di))

  where
  commentsAndSuch =
      BS.intercalate "\n"
    $ (   map (\s->"% NOTE: " <> fillTo100 s) notes
       ++ map (\s-> "% " <> fillTo100 (B8.pack s)) (normalizeCtgy $ ppCtgy ctgy)
       ++ [""])

  normalizeCtgy ls = take 5 (ls ++ cycle [[]])
  
  fillTo100 s = B8.take 100 (s <> " " <> B8.replicate 100 '%')
    
writeTest :: FilePath -> Test -> IO ()
writeTest dir t = renderTest t >>= B8.writeFile (dir ++"/" ++ t_name t ++ ".pdf")

outputTest :: Test -> IO ()
outputTest t = renderTest t >>= B8.putStrLn 

applyEdits :: Edits -> BS -> IO BS
applyEdits e bs =
  case e of
    E_None         -> return bs
    E_Shell f args -> B8.pack <$> readProcess f args (B8.unpack bs)
                               -- above throws IOError if not ExitSuccess
                      
   -- FIXME:
   --  - this is fragile enough as it is: you should fail if the shell script fails!
   --    - sed doesn't 'fail' on on no-match ..., so ?
   --    - check that file changed?

ppCtgy :: Ctgy -> [String]
ppCtgy = \case
           Good     -> ["CATEGORY: good PDF"]
           Probs ps -> "CATEGORY: discrepancies:"
                       : map ppLine ps
                       
ppLine (mc,s) = "  " ++ show mc ++ ": " ++ s


---- tests -------------------------------------------------------------------

allTests =
  -- these tests all based on the same "DOM" pdf1:
  [ Test "1_valid_xref_trad" pdf1 RT_XRef_Trad E_None Good
      ["no compressed objects, traditional xref table"]
      
  , Test "1_invalid_xref_trad-a" pdf1 RT_XRef_Trad
      (E_Shell "sed" ["s/^0000001162 00000 n/0000001163 00000 n/"])  -- add 1 to xref offset!
      (Probs [(Error    , "object id (11) mismatch")
             ,(NotStrict, "cavity with \"1\" in it, before object 11")
             ])
      ["no compressed objects, traditional xref table; object 11 pointing +1"]
      
  , Test "1_invalid_xref_trad-b" pdf1 RT_XRef_Trad
      (E_Shell "sed" ["s/^11 0 obj/23 0 obj/"])
      (Probs [(Error , "object id (11 /= 23) mismatch")
             ])
      ["no compressed objects, traditional xref table; object 11 xref pointing to object 23"]
      
  , Test "1_valid_xref_strm" pdf1 RT_XRef_Strm E_None  Good
      ["no compressed objects, xref is stream"]

  -- pdf2 based tests: pdf2 uses "compressed objects":
  
  , Test "2_valid_xref_strm" pdf2 RT_XRef_Strm E_None Good
      ["compressed objects, thus xref is stream"]
      
  , Test "2_valid_xref_strm_indirects" pdf2 RT_XRef_Strm
      (E_Shell "sed" [intercalate ";" ["s#/N 5#/N 1 0 R#"           -- using 4 extra chars
                                      ,"s#/First 28#/First 2 0 R#"  -- using 3 extra chars
                                      ,"s/UNUSED 1234567/UNUSED /" -- remove 7 chars
                                      ]])
      Good
      ["compressed objects, thus xref is stream, using indirects for N and First"]
      
  , Test "2_invalid_xref_strm_rec_indirects" pdf2 RT_XRef_Strm
      (E_Shell "sed" [intercalate ";" ["s#/N 5#/N 6 0 R#"           -- using 4 extra chars
                                      ,"s#/First 28#/First 11 0 R#" -- using 4 extra chars
                                      ,"s/UNUSED 12345678/UNUSED /"  -- remove 8 chars
                                      ]])
      (Probs [(Error , "recursive object stream lookup")
             ])
      ["compressed objects, thus xref is stream, using RECURSIVE indirects for N and First"]
      
  -- based on pdf3{a,b}:
  , Test "3a_invalid_emptyvalue" pdf3a RT_XRef_Trad
      E_None
      (Probs [(Error, "bad syntax for value, object id 1")])
      ["simple file, one syntax error, traditional xref table"]
      
  , Test "3b_invalid_badvaluesyntax" pdf3b RT_XRef_Trad
      E_None
      (Probs [(Error, "bad syntax for value, object id 1")])
      ["simple file, one syntax error, traditional xref table"]

  -- based on pdf4       
  , Test "4a_valid_2xref_strms" pdf4 RT_XRef_Strm
      E_None
      Good
      ["two compressed objects"]
  , Test "4b_invalid_2xref_strms_recursive" pdf4 RT_XRef_Strm
      (E_Shell "sed" [intercalate ";" ["s#/N 4#/N 13 0 R#"          -- using 5 extra chars
                                      ,"22s#UNUSED 12345#UNUSED #"  -- remove 5 chars
                                      ,"s#10013#00004#"
                                      ,"s#/N 3#/N 6 0 R#"           -- using 4 extra chars
                                      ,"s#10006#00003#"
                                      ,"67s#UNUSED 1234#UNUSED #"  -- remove 4 chars
                                      ]])
      (Probs [(Error, "xref strms mutually recursive")])
      ["two compressed objects, mutually recursive in N values"]
  ]

