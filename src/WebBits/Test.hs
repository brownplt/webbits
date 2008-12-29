module WebBits.Test
  ( pretty
  , parse
  , isJsFile
  , getJsPaths
  ) where

import Data.List ( isSuffixOf )

import System.Directory
import System.FilePath
import System.IO.Unsafe ( unsafePerformIO )

import Text.PrettyPrint.HughesPJ ( render, vcat )
import Text.ParserCombinators.Parsec (ParseError, sourceName, errorPos)

import WebBits.Common ( pp )
import WebBits.JavaScript.PrettyPrint ()
import WebBits.JavaScript.Syntax ( JavaScript (..) )
import WebBits.JavaScript.Parser ( parseScriptFromString, ParsedStatement )


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
