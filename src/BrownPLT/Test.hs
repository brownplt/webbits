module BrownPLT.Test
  ( pretty
  , parse
  , parseJavaScriptFromFile
  , isJsFile
  , getJsPaths
  , commandIO
  , rhinoIO
  , rhinoIOFile
  , module Test.HUnit 
  ) where

import qualified Data.List as L
import Data.List ( isSuffixOf )
import Data.Maybe (catMaybes)
import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)
import Control.Monad
import qualified Data.Map as M

import System.Directory
import System.FilePath
import System.IO.Unsafe ( unsafePerformIO )
import Data.Generics
import Test.HUnit

import qualified Data.ByteString.Char8 as B
import System.Process
import System.IO
import System.Exit
import Control.Exception as E

import Text.PrettyPrint.HughesPJ ( render, vcat )
import Text.ParserCombinators.Parsec (ParseError,sourceName,sourceLine,
  sourceColumn,errorPos,SourcePos)
import BrownPLT.Common ( pp )
import BrownPLT.JavaScript.PrettyPrint ()
import BrownPLT.JavaScript.Syntax
import BrownPLT.JavaScript.Parser (parseScriptFromString,parseJavaScriptFromFile,
  ParsedStatement)

pretty :: [ParsedStatement] -> String
pretty stmts = render $ vcat $ map pp stmts

isPrettyPrintError :: ParseError -> Bool
isPrettyPrintError pe = 
  "(PRETTY-PRINTING)" `isSuffixOf` sourceName (errorPos pe)

parse :: FilePath -> String -> [ParsedStatement]
parse src str = case parseScriptFromString src str of
  Left err | isPrettyPrintError err -> 
               (unsafePerformIO $ putStrLn str) `seq` error (show err)
           | otherwise -> error (show err)
  Right (Script _ stmts) -> stmts

isJsFile :: String -> Bool
isJsFile = (== ".js") . takeExtension 

getJsPaths :: FilePath -> IO [FilePath]
getJsPaths dpath = do
    exists <- doesDirectoryExist dpath
    paths <- if exists then getDirectoryContents dpath else return []
    return [dpath </> p | p <- paths, isJsFile p]


labelAt :: (Foldable t) 
        => [t (a,Int,SourcePos)]
        -> (Int,Int) -- ^row and column
        -> Int
labelAt terms (line,column) =
  let match loc = sourceLine loc == line && sourceColumn loc == column
      results = map (Foldable.find (\(_,_,loc) -> match loc)) terms
    in case catMaybes results of
         ((_,lbl,_):_) -> lbl
         [] -> error ("Test.Ovid.Scripts.LabelAt: no term at line " ++
                      show line ++ ", column " ++ show column)



commandIO :: FilePath -- ^path of the executable
          -> [String] -- ^command line arguments
          -> B.ByteString  -- ^stdin
          -- |'Left stderr' on 'ExitFailure'. 'Right stdout' on 'ExitSuccess'.
          -> IO (Either B.ByteString B.ByteString)
commandIO path args stdinStr = do
  let cp = CreateProcess (RawCommand path args) Nothing Nothing CreatePipe
                         CreatePipe CreatePipe True
  (Just hStdin, Just hStdout, Just hStderr, hProcess) <- createProcess cp
  B.hPutStr hStdin stdinStr
  stdoutStr <- B.hGetContents hStdout
  stderrStr <- B.hGetContents hStderr
  exitCode <- waitForProcess hProcess
  case exitCode of
    ExitSuccess -> return (Right stdoutStr)
    ExitFailure n -> return (Left stderrStr)

rhinoIO :: B.ByteString -- ^stdin
        -> IO (Either B.ByteString B.ByteString) -- ^stderr/stdout
rhinoIO stdin =
  commandIO "/usr/bin/env" 
    ["java","-classpath","rhino.jar","org.mozilla.javascript.tools.shell.Main"]
    stdin

-- |Like 'rhinoIO', but the input stream is first placed in a temporary file.
rhinoIOFile :: B.ByteString -- ^file contents
            -> IO (Either B.ByteString B.ByteString) -- ^stderr/stdout
rhinoIOFile instream = do
  (path,handle) <- openTempFile "." "webbits.js" 
  B.hPutStr handle instream
  hClose handle
  let cmd = commandIO "/usr/bin/env" 
              ["java","-classpath","rhino.jar",
               "org.mozilla.javascript.tools.shell.Main","-f",path] 
               B.empty
  cmd `E.finally` (removeFile path)


