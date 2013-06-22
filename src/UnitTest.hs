module Main where

import Test.HUnit
import System.Exit
import System.Directory
import qualified System.FilePath as FilePath
import Language.ECMAScript3.Parser
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Annotations
import Test.SourceDiff
import Control.Monad

testDir = "tests/parse-pretty"


-- | tests the parser with pre-defined test-cases
parsePrettyTest filename = TestLabel filename $ TestCase $ 
  readFile filename >>= \src ->
  case parseScriptFromString "" src of
    Left err -> assertFailure $ "Can't parse a test-case: " ++ filename
    Right js -> let str = show $ prettyPrint js
                in  case parseScriptFromString "" str of
                  Left err ->
                    let msg = "Can't parse pretty-printed code. The error was: "
                              ++ (show err)
                              ++ "\nThe pretty-printed code in question:\n" ++ str
                    in assertFailure msg
                  Right js' -> do
                    let str' = show $ prettyPrint js'
                    unless (str == str') $ do
                      let msg = "The parse of the pretty-printed AST didn't match the original\n"
                                ++ "Diff:\n" ++ jsDiff js js'
                      assertFailure msg

main = do
  allFiles <- getDirectoryContents testDir
  let files =  map (testDir `FilePath.combine`) $ 
        filter (\x -> FilePath.takeExtension x == ".js") allFiles
  let parsePretty = TestLabel "parser - printer composition" 
        (TestList (map parsePrettyTest files))
  results <- runTestTT parsePretty
  if errors results > 0 || failures results > 0 
    then exitFailure
    else putStrLn "All tests passed."
