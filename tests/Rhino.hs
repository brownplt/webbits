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
import qualified Data.ByteString.Char8 as B

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
          -> B.ByteString  -- ^stdin
          -> IO (Maybe B.ByteString) -- ^stdout or 'Nothing' on failure
commandIO path args stdinStr = do
  let cp = CreateProcess (RawCommand path args) Nothing Nothing CreatePipe
                         CreatePipe CreatePipe True
  (Just hStdin, Just hStdout, Just hStderr, hProcess) <- createProcess cp
  B.hPutStr hStdin stdinStr
  stdoutStr <- B.hGetContents hStdout
  stderrStr <- hGetContents hStderr
  hPutStrLn stderr stderrStr -- echo errors to our stderr
  exitCode <- waitForProcess hProcess
  case exitCode of
    ExitSuccess -> return (Just stdoutStr)
    ExitFailure n -> do
      B.hPutStrLn stdout stdoutStr -- echo for errors
      hPutStrLn stderr $ "Sub-process died with exit code " ++ show n
      return Nothing

rhino :: FilePath -- ^Path to the file
      -> B.ByteString -- ^JavaScript source
      -> IO B.ByteString -- ^JavaScript source, parsed and printed by Rhino
rhino path {- not used -} src = do
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
  lhs <- ((rhino src') . B.pack . pretty . (parse src)) str
  rhs <- rhino src (B.pack str)
  if lhs == rhs 
    then return ()
    else assertFailure ("testRhino failed on " ++ src)
  

main = do
  allPaths <- getDirectoryContents "parse-pretty"
  let testPaths = map ("parse-pretty"</>)
                    $ filter ((== ".js") . takeExtension) allPaths
  testData <- mapM readFile testPaths
  let tests = zipWith testRhino testPaths testData
  return (TestList tests)
