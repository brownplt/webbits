-- |
-- Maintainer: arjun@cs.brown.edu
--
-- Determine the environment of a JavaScript function.
module WebBits.JavaScript.Env
  (
  -- * Environment
   localVars
  ) where

import Data.Generics
import Data.List (foldl')
import qualified Data.Set as S
import Data.Set (Set)
import Text.ParserCombinators.Parsec(SourcePos)

import WebBits.Common
import WebBits.JavaScript.Syntax

unId (Id _ v) = v

    
-- ----------------------------------------------------------------------------
-- Environment

-- |Locally defined variables in a list of statements.  Does not descend
-- into nested functions.
localVars :: [Statement SourcePos] -> Set String
localVars stmts = everythingBut S.union excludeFunctions query stmts where
  query :: GenericQ (Set String)
  query = (mkQ S.empty collectVarDecl) `extQ` collectForInInit

  collectVarDecl :: VarDecl SourcePos -> Set String
  collectVarDecl (VarDecl _ v _) = S.singleton (unId v)

  collectForInInit :: ForInInit SourcePos -> Set String
  collectForInInit (ForInVar v)  = S.singleton (unId v)
  collectForInInit (ForInNoVar _) = S.empty

-- |Free variables of a function.  Naturally, this reports free variables
-- in nested functions as well.
-- TODO: Does not report free variables in nested functions!
funcFreeVars :: Expression SourcePos -> Set String
funcFreeVars (FuncExpr _ argIds body) = free where
  free = S.difference (S.difference used declared) args
  -- Collect VarRef's in this function, but don't descend into nested functions.
  used = everythingBut S.union excludeFunctions (mkQ S.empty getVar) body
  -- Variables declared in this function.
  declared = localVars [body]
  -- Arguments as a set
  args = S.fromList (map unId argIds)
funcFreeVars _ = error "funcFreeVars requires a FuncExpr"


getVar :: Expression SourcePos -> Set String
getVar (VarRef _ (Id _ v)) = S.singleton v
getVar _                   = S.empty
