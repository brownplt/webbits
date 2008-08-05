module Test.Util.Environment 
  ( testLabelEq
  , testLabelDiff
  , testFile
  , labelAt
  , runTestTT
  , annotationAt
  , module Test.HUnit.Base
  ) where

import Control.Monad.State
import Test.HUnit.Base
import Test.HUnit.Text (runTestTT)
import qualified Data.Foldable as F
import Data.Foldable (Foldable)
import Data.Maybe (catMaybes,listToMaybe)
import Text.ParserCombinators.Parsec.Pos

import WebBits.JavaScript.JavaScript (parseJavaScriptFromFile)
import WebBits.JavaScript.Environment

annotationAt :: (Foldable t) 
        => [t (Env,Int,SourcePos)]
        -> (Int,Int) -- ^row and column
        -> [(Env,Int,SourcePos)]
annotationAt terms (line,column) =
  let match a@(_,_,loc) =
        if sourceLine loc == line && sourceColumn loc == column
          then [a] 
          else []
  in concatMap (F.concatMap match) terms 

envAt :: (Foldable t) => [t (Env,Int,SourcePos)] -> (Int,Int) -> Env
envAt terms p = env where
  (env,_,_) = last $ annotationAt terms p

labelAt :: (Foldable t) => [t (Env,Int,SourcePos)] -> (Int,Int) -> Int
labelAt terms p = label where
  (_,label,_) = last $ annotationAt terms p

  
assertLabelEq :: [LabelledStatement] -> (Int,Int) -> (Int,Int) ->  IO ()
assertLabelEq stmts p1 p2 = do
  let lbl1 = labelAt stmts p1
  let lbl2 = labelAt stmts p2
  assertEqual "same label" lbl1 lbl2
  
assertLabelDiff :: [LabelledStatement] -> (Int,Int) -> (Int,Int) ->  IO ()
assertLabelDiff stmts p1 p2 = do
  let lbl1 = labelAt stmts p1
  let lbl2 = labelAt stmts p2
  assert (lbl1 /= lbl2)
  
testLabelEq stmts p1 p2 = TestCase (assertLabelEq stmts p1 p2)

testLabelDiff stmts p1 p2 = TestCase (assertLabelDiff stmts p1 p2)

testFile :: String -> IO [LabelledStatement]
testFile filename = do
  stmts <- parseJavaScriptFromFile filename
  let (stmts',_,_) = staticEnvironment stmts
  return stmts'
