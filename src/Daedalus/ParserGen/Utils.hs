module Daedalus.ParserGen.Utils where

import System.IO
import Data.List

import Daedalus.ParserGen.Action
import Daedalus.ParserGen.Aut

-- Generate a graphviz code that could be run with python3 and produce
-- a graphical diagraph of an automaton
autToGraphviz:: Aut a => a -> IO ()
autToGraphviz aut =
  do
    autFile <- openFile "aut.dot" WriteMode
    hPutStrLn autFile prelude
    mapM_ (hPutStrLn autFile) trans
    hPutStrLn autFile postlude
    hClose autFile
  where
    (_i, tr, _) = destructureAut aut
    f a b =
      let (n1, act, n2) = a
          thestr =
            makeEdge startNode arrivNode strAct isDotted
            where
              arrivNode = case act of
                            CAct (Push _ _ n) -> "q" ++ show n
                            _ -> "q" ++ show n2
              startNode = "q" ++ show n1
              strAct =
                case act of
                  CAct (Push name _ _) ->
                    "Push_" ++ tail (removeLast (showName name)) ++ "_q" ++ show n2
                  _ -> show act
              isDotted = case act of
                         CAct (Push _ _ _) -> True
                         _ -> False
              removeLast [] = []
              removeLast [ _ ] = []
              removeLast (x : xs) = x : removeLast xs


      in thestr : b
    trans = foldr f [] tr
    prelude =
      "// copy this to aut.dot and run\n"
      ++ "// dot -Tpdf aut.dot > aut.pdf \n"
      ++ "digraph G { size=\"8,5\"; rankdir=\"LR\";"
    makeEdge start arriv label b =
      let (fontsize,newlabel) =
            if isPrefixOf "Match-" label
            then ("fontsize = 26, fontname = courrier, color = blue, ", drop 6 label)
            else ("", label)

      in start ++ " -> " ++ arriv ++
         " [ " ++ fontsize ++ (if b then "style=dashed, color = green " else "") ++
         "label=\"" ++ newlabel ++ "\" ];"
    postlude = "}" ++ "\n// dot -Tpdf aut.dot > aut.pdf " -- "f.view()\n"
