module Language.ECMAScript3.Parser.Type where

import Language.ECMAScript3.Parser.State
import Text.Parsec
import Control.Monad.Identity

-- | The parser type, parametrised by the stream type @s@ and the
-- return value @a@
type Parser s a = ParsecT s ParserState Identity a
