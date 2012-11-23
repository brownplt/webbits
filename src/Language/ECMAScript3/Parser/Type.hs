module Language.ECMAScript3.Parser.Type where

import Language.ECMAScript3.Parser.State
import Text.Parsec
import Control.Monad.Identity

type Parser s a = ParsecT s ParserState Identity a