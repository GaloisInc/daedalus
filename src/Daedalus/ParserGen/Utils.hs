module Daedalus.ParserGen.Utils where

import System.IO

import Daedalus.ParserGen.Action
import Daedalus.ParserGen.Aut

-- Generate a graphviz code that could be run with python3 and produce
-- a graphical diagraph of an automaton
autToGraphviz:: Aut -> IO ()
autToGraphviz aut =
  do
    autFile <- openFile "aut.dot" WriteMode
    hPutStrLn autFile prelude
    mapM_ (hPutStrLn autFile) trans
    hPutStrLn autFile postlude
    hClose autFile
  where
    (_i, tr, _) = toListAut aut
    f a b =
      let (n1, act, n2) = a
          thestr =
            makeEdge startNode arrivNode strAct isDotted
            where
              arrivNode = case act of
                            CAct (Push _ _ n) -> "S" ++ show n
                            CAct (Pop n) -> "Pop_to_S" ++ show n
                            _     -> "S" ++ show n2
              startNode = "S" ++ show n1
              strAct = case act of
                         CAct (Push name _ _) -> "Push_" ++ tail (removeLast (show name)) ++ "_S" ++ show n2
                         CAct (Pop n) -> "Pop" ++ show n
                         _     -> show act
              isDotted = case act of
                         CAct (Push _ _ _) -> True
                         CAct (Pop _) -> True
                         _ -> False
              removeLast [] = []
              removeLast [ _ ] = []
              removeLast (x : xs) = x : removeLast xs


      in thestr : b
    trans = foldr f [] tr
    prelude =
      "// copy this to aut.dot and dot -Tpdf aut.dot > aut.pdf \n"
      ++ "digraph G { size=\"8,5\"; rankdir=\"LR\";"
    makeEdge start arriv label b =
      start ++ " -> " ++ arriv ++ " [ label=" ++ label ++ (if b then " style=dashed color=blue" else "") ++ " ];"
    postlude = "}" ++ "\n// dot -Tpdf aut.dot > aut.pdf " -- "f.view()\n"
