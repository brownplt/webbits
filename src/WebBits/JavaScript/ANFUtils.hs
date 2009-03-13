module WebBits.JavaScript.ANFUtils where

import Data.Generics
import Text.ParserCombinators.Parsec.Pos
import WebBits.Common
import WebBits.JavaScript.Core

allFuncExprs :: [Stmt SourcePos] -> [Expr SourcePos]
allFuncExprs stmts = everything (++) (mkQ [] getFuncExpr) stmts where

  getFuncExpr :: Expr SourcePos -> [Expr SourcePos]
  getFuncExpr fn@(FuncExpr{}) = [fn]
  getFuncExpr _ = []


