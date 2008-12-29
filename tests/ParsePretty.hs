module ParsePretty where

import Control.Monad ( liftM )

import Test.HUnit.Base
import Test.HUnit.Text
import Data.Data

import WebBits.Test


eraseSourcePos x = fmap (const ()) x

assertEqualWithoutSourcePos lhs rhs = 
  case eraseSourcePos lhs == eraseSourcePos rhs of
    True -> return ()
    False -> assertFailure $ "expected:\n" ++ show lhs ++ "\ngot:\n" ++
                             show rhs ++ "\n"

testParsePrettyIdentity :: FilePath -> String -> Test
testParsePrettyIdentity src str = TestCase $ 
  assertEqualWithoutSourcePos lhs rhs where
    lhs = ((parse (src ++ " (PRETTY-PRINTING)")) . pretty . (parse src)) str
    rhs = parse src str

main = do
  testPaths <- liftM concat $ mapM getJsPaths ["parse-pretty"]
  testData <- mapM readFile testPaths
  let tests = zipWith testParsePrettyIdentity testPaths testData
  return (TestList tests)
