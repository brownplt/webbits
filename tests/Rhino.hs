-- Tests for the identity rhino jsSrc = rhino.pretty.parse jsSrc, where
-- parse and pretty are our parser and pretty-printer and rhino is Mozilla
-- Rhino's JavaScript parser and pretty-printer.
module Rhino where

import Test.HUnit.Base
import Test.HUnit.Text
import Data.Data
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, FilePath, (</>))
import qualified Data.List as L
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import System.IO
import System.Environment
import System.Exit

import WebBits.Common (pp)
import Text.ParserCombinators.Parsec (ParseError, sourceName, errorPos)
import WebBits.JavaScript.PrettyPrint () -- instances only
import WebBits.JavaScript.Syntax (JavaScript (..))
import WebBits.JavaScript.Parser (parseScriptFromString, ParsedStatement)
import Text.PrettyPrint.HughesPJ (render, vcat)


pretty :: [ParsedStatement] -> String
pretty stmts = render $ vcat $ map pp stmts

parse :: FilePath -> String -> [ParsedStatement]
parse src str = case parseScriptFromString src str of
  Left err -> error (show err)
  Right (Script _ stmts) -> stmts

commandIO :: FilePath -- ^path of the executable
          -> [String] -- ^command line arguments
          -> String  -- ^stdin
          -> IO (Maybe String) -- ^stdout or 'Nothing' on failure
commandIO path args stdinStr = do
  let cp = CreateProcess (RawCommand path args) Nothing Nothing CreatePipe
                         CreatePipe CreatePipe True
  (Just hStdin, Just hStdout, Just hStderr, hProcess) <- createProcess cp
  hPutStr hStdin stdinStr
  stdoutStr <- hGetContents hStdout
  stderrStr <- hGetContents hStderr
  hPutStrLn stderr stderrStr -- echo errors to our stderr
  exitCode <- waitForProcess hProcess
  case exitCode of
    ExitSuccess -> return (Just stdoutStr)
    ExitFailure n -> do
      hPutStrLn stderr $ "Sub-process died with exit code " ++ show n
      return Nothing

rhino :: FilePath -- ^Path to the file
      -> String -- ^JavaScript source
      -> IO String -- ^JavaScript source, parsed and printed by Rhino
rhino path src = do
  hPutStrLn stderr ("Starting Rhino on " ++ path)
  rhinoPath <- getEnv "RHINO"
  let classpath = rhinoPath ++ ":../java"
  result <- commandIO "/usr/bin/env" 
              ["java", "-classpath", classpath, "RhinoTest"]
              src
  case result of
    Just str -> return str
    Nothing -> fail "RhinoTest signalled an error"

testRhino:: FilePath -> String -> Test
testRhino src str = TestCase $ do
  let src' = src ++ " (pretty-printed)"
  lhs <- ((rhino src') . pretty . (parse src)) str
  rhs <- rhino src str
  assertEqual "testRhino" lhs rhs
  

main = do
  allPaths <- getDirectoryContents "parse-pretty"
  let testPaths = map ("parse-pretty"</>)
                    $ filter ((== ".js") . takeExtension) allPaths
  testData <- mapM readFile testPaths
  let tests = zipWith testRhino testPaths testData
  return (TestList tests)
