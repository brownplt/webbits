-- | This test-suite checks that 'parse src == (parse . pretty
-- . parse) src' for test cases from the test262 test-suite
-- (http://test262.ecmascript.org).
-- To be run with 'cabal(-dev) test'

module Main where

import Test.Framework
import Test.Framework.Runners.Console
import Test.Framework.Providers.HUnit
import System.IO.HVFS.Utils
import System.IO.HVFS
import Test.HUnit
import System.FilePath

main = do cases <- listTestCasePaths
          mapM putStrLn cases
--  defaultMain tests

tests = []

listTestCasePaths :: IO [FilePath]
listTestCasePaths = 
  do cd <- vGetCurrentDirectory SystemFS
     files <- recurseDir SystemFS (joinPath [cd, "test", "test262"])
     return $ filter isJavaScript files
  
isJavaScript :: FilePath -> Bool
isJavaScript fp | length fp < 3 = False
isJavaScript fp | otherwise = (last 3 fp) == ".js"
  where last :: Int -> String -> String
        last i xs | length xs <= i = xs
        last i xs | otherwise = last i (tail xs)