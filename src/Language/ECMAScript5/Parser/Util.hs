{- Misc parsec combinators -}
module Language.ECMAScript5.Parser.Util where

import Text.Parsec
import Text.Parsec.Char
import Control.Applicative ((<$>), (<*>), (<$))

-- | parses any character in the range [left .. right]
rangeChar :: (Monad m, Stream s m Char)
          => Char -> Char -> ParsecT s st m Char
rangeChar left right = satisfy (\x -> x >= left && x <= right)

-- | parses anything parsable by the first parser, and not the second
butNot :: (Monad m, Stream s m c, Show a, Show a1)
       => ParsecT s st m a -> ParsecT s st m a1 -> ParsecT s st m a
butNot positive negative = do notFollowedBy negative
                              positive

-- | to accomodate the requirement that <CR><LF> should be considered
-- as a single token
stringify :: (Monad m, Stream s m Char)
          => ParsecT s st m Char -> ParsecT s st m String
stringify p = do c <- p
                 return [c]

-- | a parser that forgets the output of the parameter
forget   :: ParsecT s st m a -> ParsecT s st m ()
forget p = p >> return ()

-- | concat in a monad
concatM   :: Monad m => m [[a]] -> m [a]
concatM x = x >>= (\y -> return $ concat y)

makePostfix ps =
  foldr (flip (.)) id <$> many (choice ps)
  
makePrefix ps =
  foldl (.) id <$> many (choice ps)

withPostfix qs p =
  flip ($) <$> p <*> makePostfix qs

withPrefix qs p =
  ($) <$> makePrefix qs <*> p
