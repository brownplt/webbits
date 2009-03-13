module Main where

import Control.Monad (liftM )

import WebBits.Test
import WebBits.JavaScript.Simplify (simplify)
import WebBits.JavaScript.Core
import WebBits.JavaScript.ToCore (jsToCore)
import WebBits.JavaScript.Env
import WebBits.JavaScript.ANFUtils
import WebBits.JavaScript.Intraprocedural
import qualified Data.Graph.Inductive as  G
import qualified Data.GraphViz as GV

intraprocGraphToDot :: Graph -> GV.DotGraph
intraprocGraphToDot gr = GV.graphToDot gr [] -- attributes
  (\(n,s) -> [GV.Label (show s)]) -- node attributes
  (const []) -- edge attributes

main = do
  str <- getContents
  let script = parse "" str
  -- putStrLn (show (localVars script))
  putStrLn (pretty $ simplify script)
  let core = jsToCore (simplify script)
  let funcExprs = allFuncExprs core
  let graphs = map (intraprocGraph'.funcExprBody) funcExprs
  mapM_ (putStrLn.show.(G.labEdges)) graphs
  let vizs = map (show.intraprocGraphToDot) graphs
  mapM_ (putStrLn) vizs
