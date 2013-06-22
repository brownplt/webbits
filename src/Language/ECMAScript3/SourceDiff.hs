-- | Simple textual diffing of JavaScript programs for inspecting test
-- failures
module Language.ECMAScript3.SourceDiff where

import Data.Algorithm.Diff
--import Data.Algorithm.DiffOutput
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint
import Data.List (intersperse)

jsDiff :: JavaScript a -> JavaScript a -> String
jsDiff js1 js2 = 
  -- let plines = lines . show . prettyPrint
  -- in  ppDiff $ getGroupedDiff (plines js1) (plines js2)
  let plines = lines . show . prettyPrint
      diff = getDiff (plines js1) (plines js2)
      formatDiff :: Diff String -> String
      formatDiff d = case d of
        First s  -> '-':s
        Second s -> '+':s
        Both s _ -> ' ':s
  in  concat $ intersperse "\n" $ map formatDiff diff
