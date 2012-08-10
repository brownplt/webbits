{- Misc parsec combinators -}
module Language.ECMAScript5.Parser.Util where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

-- parses any character in the range [left .. right]
rangeChar :: Char -> Char -> CharParser st Char
rangeChar left right = satisfy (\x -> x >= left && x <= right)

-- parses anything, but not the given parser
notP :: Show v => GenParser c st v -> GenParser c st v2
notP p = do s <- p
            unexpected (show s)

-- parses anything parsable by the first parser, and not the second
butNot :: Show v => Show v1 => GenParser c st v -> GenParser c st v1 -> GenParser c st v
butNot positive negative = do lookAhead $ notP negative 
                              positive

-- to accomodate the requirement that <CR><LF> should be considered as a single token
stringify :: CharParser st Char -> CharParser st String
stringify p = do c <- p
                 return [c]

-- a parser that forgets the output of the parameter
forget   :: CharParser st a -> CharParser st ()
forget p = p >> return ()

-- concat 
concatM   :: Monad m => m [[a]] -> m [a] 
concatM x = x >>= (\y -> return $ concat y)
               