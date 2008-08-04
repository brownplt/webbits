module Test.Util.Environment 
  ( testLabelEq
  , testFile
  , labelAt
  , runTestTT
  ) where

import Control.Monad.State
import Test.HUnit.Base
import Test.HUnit.Text (runTestTT)
import qualified Data.Foldable as F
import Data.Foldable (Foldable)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec.Pos

import WebBits.JavaScript.JavaScript (parseJavaScriptFromFile)
import WebBits.JavaScript.Environment

annotationAt :: (Foldable t) 
        => [t (Env,Int,SourcePos)]
        -> (Int,Int) -- ^row and column
        -> (Env,Int,SourcePos)
annotationAt terms (line,column) =
  let match loc = sourceLine loc == line && sourceColumn loc == column
      results = map (F.find (\(_,_,loc) -> match loc)) terms
    in case catMaybes results of
         (result:_) -> result
         [] -> error $ "no term at line " ++ show line ++ ", column " 
                       ++ show column

envAt :: (Foldable t) => [t (Env,Int,SourcePos)] -> (Int,Int) -> Env
envAt terms p = env where
  (env,_,_) = annotationAt terms p

labelAt :: (Foldable t) => [t (Env,Int,SourcePos)] -> (Int,Int) -> Int
labelAt terms p = label where
  (_,label,_) = annotationAt terms p

  
assertLabelEq :: [LabelledStatement] -> (Int,Int) -> (Int,Int) ->  IO ()
assertLabelEq stmts p1 p2 = do
  let lbl1 = labelAt stmts p1
  let lbl2 = labelAt stmts p2
  assertEqual "same label" lbl1 lbl2
  
testLabelEq stmts p1 p2 = TestCase (assertLabelEq stmts p1 p2)

testFile :: String -> IO [LabelledStatement]
testFile filename = do
  stmts <- parseJavaScriptFromFile filename
  let (stmts',_,_) = staticEnvironment stmts
  return stmts'
