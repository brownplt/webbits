module WebBits.Test
  ( pretty
  , parse
  , parseJavaScriptFromFile
  , env
  , isJsFile
  , getJsPaths
  , sameIds
  ) where

import qualified Data.List as L
import Data.List ( isSuffixOf )

import System.Directory
import System.FilePath
import System.IO.Unsafe ( unsafePerformIO )
import Data.Generics
import Test.HUnit

import Text.PrettyPrint.HughesPJ ( render, vcat )
import Text.ParserCombinators.Parsec (ParseError,sourceName,sourceLine,
  sourceColumn,errorPos)
import WebBits.Common ( pp )
import WebBits.JavaScript.PrettyPrint ()
import WebBits.JavaScript.Syntax
import WebBits.JavaScript.Parser (parseScriptFromString,parseJavaScriptFromFile,
  ParsedStatement)
import WebBits.JavaScript.Environment (LabelledStatement,LabelledExpression,
  Ann,staticEnvironment,Env)

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

env :: [ParsedStatement] -> [LabelledStatement]
env stmts = labelledStmts where
  (labelledStmts,_,_) = staticEnvironment stmts

idWithPos :: (Int,Int)
          -> Id Ann
          -> [Int]
idWithPos (line,col) (Id (_,lbl,pos) _)
  | line == sourceLine pos && col == sourceColumn pos = [lbl]
idWithPos _ _ = []

labelAt :: [LabelledStatement]
            -> (Int,Int)
            -> IO Int
labelAt stmts pos  = 
  case everything (++) (mkQ [] (idWithPos pos)) stmts of
    [] -> do 
      assertFailure $ "no VarRef at " ++ show pos
      fail "no VarRef"
    (x:_) -> return x -- assumption: extra labels are from Ids inside VarRefs.

sameIds :: [(Int,Int)] -- ^positions of identifiers that reference the same
                       -- variable
        -> [LabelledStatement]
        -> Assertion
sameIds [] stmts = 
  assertFailure "sameIds called with no identifiers"
sameIds idLocs stmts = do
  lbls <- mapM (labelAt stmts) idLocs 
  case lbls of
    [] -> fail "WebBits/Test.hs : no labels returned"
    (lbl:_) -> assertEqual "some labels are distinct" [lbl] (L.nub lbls)
