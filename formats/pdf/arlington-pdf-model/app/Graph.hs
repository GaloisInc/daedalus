module Graph where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Data.List(intersperse)
import Data.Graph(graphFromEdges,flattenSCC)
import Data.Graph.SCC

type NodeId = Text

data G = G
  { nodes :: [(NodeId,Text)]
  , edges :: [(NodeId,[NodeId])]
  }

sccG :: G -> G
sccG gin =
  G { nodes = [ (nodeName i, sccLab s) | (s,i,_) <- g1 ]
    , edges = [ (nodeName i, map nodeName j) | (_,i,j) <- g1 ]
    }
  where
  nodeName x     = Text.pack ("node_" <> show x)
  labMap         = Map.fromList (nodes gin)
  scLab x        = let (i,_,_) = nFromV x
                   in Map.findWithDefault i i labMap
  sccLab         = Text.unlines . map scLab . flattenSCC
  g1             = sccGraph g
  (g, nFromV, _) = graphFromEdges [ (i,i,js) | (i,js) <- edges gin ]


dotGraph :: G -> String
dotGraph g =
  unlines $
    [ "digraph G {"
    , "size=\"6,4\";"
    , "ratio=\"fill\";"
    ] ++
    nodeLs ++
    edgeLs ++
    ["}"
    ]

  where
  nodeLs = [ show x ++ "[label=" ++ show l ++ "];" | (x,l) <- nodes g ]
  edgeLs = [ nx ++ " -> " ++ show y ++ ";"
           | (x,ys) <- edges g, let nx = show x, y <- ys ]


