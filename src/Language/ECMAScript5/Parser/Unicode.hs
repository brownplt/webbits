{- Definitions of unicode character parsers -}
module Language.ECMAScript5.Parser.Unicode where

import Data.CharSet
import Data.CharSet.Unicode.Category
import Text.Parsec hiding (space, letter)

--7.1
uZWNJ :: Stream s m Char => ParsecT s u m Char
uZWNJ = char '\x200C'
uZWJ :: Stream s m Char => ParsecT s u m Char
uZWJ  = char '\x200D' -- the spec mistakingly lists it as \x200C

--7.2
uTAB  :: Stream s m Char => ParsecT s u m Char
uTAB  = char '\x0009'
uVT  :: Stream s m Char => ParsecT s u m Char
uVT   = char '\x000B'
uFF  :: Stream s m Char => ParsecT s u m Char
uFF   = char '\x000C'
uSP  :: Stream s m Char => ParsecT s u m Char
uSP   = char '\x0020'
uNBSP  :: Stream s m Char => ParsecT s u m Char
uNBSP = char '\x00A0'
uBOM  :: Stream s m Char => ParsecT s u m Char
uBOM  = char '\xFEFF'
uUSP  :: Stream s m Char => ParsecT s u m Char
uUSP  = satisfy $ \x -> member x space

--7.3
uLF :: Stream s m Char => ParsecT s u m Char
uLF = char '\x000A'
uCR :: Stream s m Char => ParsecT s u m Char
uCR = char '\x000D'
uLS :: Stream s m Char => ParsecT s u m Char
uLS = char '\x2028'
uPS :: Stream s m Char => ParsecT s u m Char
uPS = char '\x2029'
uCRLF :: Stream s m Char => ParsecT s u m [Char]
uCRLF = string "\x000D\x000A"

--7.6
unicodeLetter :: Stream s m Char => ParsecT s u m Char
unicodeLetter = satisfy $ \x -> member x (letter `union` letterNumber)
unicodeCombiningMark :: Stream s m Char => ParsecT s u m Char
unicodeCombiningMark = satisfy $ \x -> member x (nonSpacingMark `union` spacingCombiningMark)
unicodeDigit :: Stream s m Char => ParsecT s u m Char
unicodeDigit = satisfy $ \x -> member x decimalNumber
unicodeConnectorPunctuation :: Stream s m Char => ParsecT s u m Char
unicodeConnectorPunctuation = satisfy $ \x -> member x connectorPunctuation

--7.8.4
--character code definitions
cNUL = '\x0000'
cBS  = '\x0008'
cHT  = '\x0009'
cLF  = '\x000A'
cVT  = '\x000B'
cFF  = '\x000C'
cCR  = '\x000D'
