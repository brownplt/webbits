-- Tests for the identity rhino jsSrc = rhino.pretty.parse jsSrc, where
-- parse and pretty are our parser and pretty-printer and rhino is Mozilla
-- Rhino's JavaScript parser and pretty-printer.
module Rhino where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import WebBits.Test



rhino :: FilePath -- ^Path to the file
      -> B.ByteString -- ^JavaScript source
      -> IO B.ByteString -- ^JavaScript source, parsed and printed by Rhino
rhino path {- not used -} src = do
  result <- commandIO "/usr/bin/env" 
              ["java", "-classpath", ".:rhino.jar", "RhinoTest"]
              src
  case result of
    Just str -> return str
    Nothing -> fail "RhinoTest signalled an error"

testRhino:: FilePath -> String -> Test
testRhino src str = TestCase $ do
  let src' = src ++ " (pretty-printed)"
  lhs <- ((rhino src') . B.pack . pretty . (parse src)) str
  rhs <- rhino src (B.pack str)
  if lhs == rhs 
    then return ()
    else assertFailure ("testRhino failed on " ++ src)
  

main = do
  testPaths <- liftM concat $ mapM getJsPaths ["parse-pretty", "libs"]
  testData <- mapM readFile testPaths
  let tests = zipWith testRhino testPaths testData
  return (TestList tests)
