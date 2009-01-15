module WebBits.Test
  ( pretty
  , parse
  , parseJavaScriptFromFile
  , label
  , globals
  , isJsFile
  , getJsPaths
  , sameIds
  , diffIds
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

import Text.PrettyPrint.HughesPJ ( render, vcat )
import Text.ParserCombinators.Parsec (ParseError,sourceName,sourceLine,
  sourceColumn,errorPos,SourcePos)
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

globals :: [ParsedStatement] -> [String]
globals stmts = M.keys env where
  (_,_,env,_) = staticEnvironment stmts

label :: [ParsedStatement] -> [LabelledStatement]
label stmts = labelledStmts where
  (labelledStmts,_,_,_) = staticEnvironment stmts

idWithPos :: (Int,Int)
          -> Id Ann
          -> [Int]
idWithPos (line,col) (Id (_,lbl,pos) _)
  | line == sourceLine pos && col == sourceColumn pos = [lbl]
idWithPos _ _ = []


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


sameIds :: [(Int,Int)] -- ^positions of identifiers that reference the same
                       -- variable
        -> [LabelledStatement]
        -> Assertion
sameIds [] stmts = 
  assertFailure "sameIds called with no identifiers"
sameIds idLocs stmts = do
  let lbls = map (labelAt stmts) idLocs 
  when (length (L.nub lbls) /= 1) $
    assertFailure $ "sameIds: distinct labels in " ++ show lbls
  return ()

diffIds :: [(Int,Int)] -- ^positions of identifiers that reference distinct
                       -- variables
        -> [LabelledStatement]
        -> Assertion
diffIds idLocs stmts = do
  let lbls = map (labelAt stmts) idLocs
  when (L.nub lbls /= lbls) $
    assertFailure $ "diffIds : some labels are the same in " ++ show lbls
  return ()
