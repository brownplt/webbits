-- | Defines commonly used datatypes and functions.
module BrownPLT.Common
  ( initialPos
  , SourcePos
  , sourceName
  , excludeFunctions
  ) where

import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Char (toLower)
import Control.Applicative
import qualified System.IO as IO
import Control.Monad.State.Strict
import Control.Monad.Identity
import qualified Data.List as L
import Data.Generics hiding (GT)
import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)
import qualified Data.Traversable as Traversable
import Data.Traversable (Traversable, traverse)
import qualified Text.PrettyPrint.HughesPJ as Pp
import Text.Parsec.Pos (SourcePos, initialPos, sourceName)
import BrownPLT.JavaScript.Syntax

-- |For generics, this type cannot be quantified.
isNotFuncExpr :: Expression SourcePos -> Bool
isNotFuncExpr (FuncExpr{}) = False
isNotFuncExpr _            = True

isNotFuncStmt :: Statement SourcePos -> Bool
isNotFuncStmt (FunctionStmt{}) = False
isNotFuncStmt _                = True

excludeFunctions :: GenericQ Bool
excludeFunctions = (mkQ True isNotFuncExpr) `extQ` isNotFuncStmt

lowercase = map toLower
