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
      diff = getGroupedDiff (plines js1) (plines js2)
      formatDiff :: Diff [String] -> String
      formatDiff d = let (prefix, strs) = case d of
                           First ss  -> ('+', ss)
                           Second ss -> ('-', ss)
                           Both ss _ -> (' ', ss)
                     in concat $ intersperse "\n" $  map (prefix:) strs
  in  concat $ intersperse "\n" $ map formatDiff diff
