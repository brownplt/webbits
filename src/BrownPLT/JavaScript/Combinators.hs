module BrownPLT.JavaScript.Combinators 
  ( scriptStatements
  , isParenExpr
  , syntaxAt
  , expressionsAt
  , statementsAt
  ) where

import Data.Generics (Data,Typeable, everything, mkQ)
import Text.ParserCombinators.Parsec (SourcePos)
import qualified Data.Foldable as F
import Data.Foldable (Foldable)
import BrownPLT.JavaScript.Syntax
import BrownPLT.JavaScript.Instances ()
import BrownPLT.Common () 

scriptStatements:: JavaScript a -> [Statement a]
scriptStatements (Script _ ss) = ss

isParenExpr (ParenExpr _ _) = True
isParenExpr _               = False

syntaxAt :: (Data a, Typeable a, Data (t SourcePos), 
             Typeable (t SourcePos), Foldable t)
         => SourcePos -> a -> [t SourcePos]
syntaxAt pos a = everything (++) (mkQ [] isMatch) a where
  isMatch stx = case F.find (const True) stx of
    Nothing -> []
    Just pos' | pos == pos' -> [stx]
              | otherwise   -> []

expressionsAt :: (Data a, Typeable a) 
              => SourcePos -> a -> [Expression SourcePos]
expressionsAt = syntaxAt

statementsAt :: (Data a, Typeable a) 
              => SourcePos -> a -> [Statement SourcePos]
statementsAt = syntaxAt

