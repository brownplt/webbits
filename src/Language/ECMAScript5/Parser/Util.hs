{- Misc parsec combinators -}
module Language.ECMAScript5.Parser.Util where

import Text.Parsec.Prim
import Text.Parsec.Char

-- parses any character in the range [left .. right]
rangeChar :: Stream s m Char => Char -> Char -> ParsecT s u m Char
rangeChar left right = satisfy (\x -> x >= left && x <= right)

-- parses anything, but not the given parser
notP :: (Show a, Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
notP p = do s <- p
            unexpected (show s)

-- parses anything parsable by the first parser, and not the second
butNot :: (Show v, Show v1, Stream s m t) 
       => ParsecT s u m v
       -> ParsecT s u m v1
       -> ParsecT s u m v
butNot positive negative = do lookAhead $ notP negative 
                              positive

-- to accomodate the requirement that <CR><LF> should be considered as a single token
stringify :: (Stream s m Char) => ParsecT s u m Char -> ParsecT s u m String
stringify p = do c <- p
                 return [c]

-- a parser that forgets the output of the parameter
forget   :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
forget p = p >> return ()

-- concat 
concatM   :: Monad m => m [[a]] -> m [a] 
concatM x = x >>= (\y -> return $ concat y)
               