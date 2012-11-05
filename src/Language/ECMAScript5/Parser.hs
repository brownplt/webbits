module Language.ECMAScript5.Parser (parse
                                   , parseScriptFromString
                                   , parseJavaScriptFromFile
                                   , parseScript
                                   , parseExpression
                                   , parseString
                                   , ParsedStatement
                                   , ParsedExpression
                                   , parseSimpleExpr'
                                   , parseBlockStmt
                                   , parseStatement
                                   , StatementParser
                                   , ExpressionParser
                                   , assignExpr
                                   ) where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Parser.Util
import Language.ECMAScript5.Parser.Unicode
import Data.Default
import Text.Parsec hiding (parse, spaces)
import Text.Parsec.Char (char, string, satisfy, oneOf, noneOf, hexDigit, anyChar)
import Text.Parsec.Char as ParsecChar hiding (spaces)
import Text.Parsec.Combinator
import Text.Parsec.Prim

import Control.Monad(liftM,liftM2)
import Control.Monad.Trans (MonadIO,liftIO)
import Numeric(readDec,readOct,readHex)
import Data.Char
import Control.Monad.Identity
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Applicative ((<$>))

--import Numeric as Numeric

-- the statement label stack
type ParserState = [String]

type Parser s a = ParsecT s ParserState Identity a

type Span = (SourcePos, SourcePos)
type PositionedExpression = Expression Span
type PositionedStatement  = Statement Span
type PositionedId         = Id Span

initialParserState :: ParserState
initialParserState = []

-- | checks if the label is not yet on the stack, if it is -- throws
-- an error; otherwise it pushes it onto the stack
pushLabel :: String -> Parser s ()
pushLabel lab = do labs <- getState
                   pos <- getPosition
                   if lab `elem` labs 
                     then fail $ "Duplicate label at " ++ show pos
                     else putState (lab:labs)

popLabel :: Parser s ()
popLabel = modifyState safeTail
  where safeTail [] = []
        safeTail (_:xs) = xs

clearLabels :: ParserState -> ParserState
clearLabels _ = []

withFreshLabelStack :: Parser s a -> Parser s a
withFreshLabelStack p = do oldState <- getState
                           putState $ clearLabels oldState
                           a <- p
                           putState oldState
                           return a

-- a convenience wrapper to take care of the position, "with position"
withPos   :: HasAnnotation t => Parser s (t Span) -> Parser s (t Span)
withPos p = do start <- getPosition
               result <- p
               end <- getPosition
               return $ setAnnotation (start, end) result

-- Below "x.y.z" are references to ECMAScript 5 spec chapters that discuss the corresponding grammar production
--7.2
whiteSpace :: (Stream s Identity Char) => Parser s ()
whiteSpace = forget $ choice [uTAB, uVT, uFF, uSP, uNBSP, uBOM, uUSP]

spaces :: (Stream s Identity Char) => Parser s ()
spaces = skipMany (whiteSpace <|> comment <?> "")

lexeme p = do{ x <- p; spaces; return x}

--7.3
uCRalone = do c <- uCR
              notFollowedBy uLF
              return c
              
lineTerminator :: Stream s Identity Char => Parser s ()
lineTerminator = forget (uLF <|> uCR <|> uLS <|> uPS)
lineTerminatorSequence = (forget (uLF <|> uCRalone <|> uLS <|> uPS )) <|> (forget uCRLF)

--7.4
comment :: Stream s Identity Char => Parser s ()
comment = multiLineComment <|> singleLineComment

singleLineCommentChars :: Stream s Identity Char => Parser s ()
singleLineCommentChars = singleLineCommentChar >> singleLineCommentChars

singleLineCommentChar :: Stream s Identity Char => Parser s ()
singleLineCommentChar  = notP lineTerminator

multiLineCommentChars :: Stream s Identity Char => Parser s ()
multiLineCommentChars  = (multiLineNotAsteriskChar >> multiLineCommentChars) <|>
                         (char '*' >> postAsteriskCommentChars)

multiLineComment :: Stream s Identity Char => Parser s ()
multiLineComment = string "/*" >> optional multiLineCommentChars >> (forget (string "*/"))

singleLineComment :: Stream s Identity Char => Parser s ()
singleLineComment = string "//" >> optional singleLineCommentChars

multiLineNotAsteriskChar :: Stream s Identity Char => Parser s ()
multiLineNotAsteriskChar = notP $ char '*'

multiLineNotForwardSlashOrAsteriskChar :: Stream s Identity Char => Parser s Char
multiLineNotForwardSlashOrAsteriskChar = noneOf "/*"

postAsteriskCommentChars :: Stream s Identity Char => Parser s ()
postAsteriskCommentChars = (multiLineNotForwardSlashOrAsteriskChar >>
                            optional multiLineCommentChars)
                           <|>
                           (char '*' >> optional postAsteriskCommentChars)

--7.5
--token = identifierName <|> punctuator <|> numericLiteral <|> stringLiteral

--7.6
identifier :: Stream s Identity Char => Parser s PositionedExpression
identifier = lexeme $ withPos $ do name <- identifierName `butNot` reservedWord
                                   return $ VarRef def name

identifierName :: Stream s Identity Char => Parser s PositionedId
identifierName = withPos $ do c  <- identifierStart 
                              cs <- many identifierPart
                              return $ Id def (c:cs)

identifierStart :: Stream s Identity Char => Parser s Char
identifierStart = unicodeLetter <|> char '$' <|> char '_' <|> unicodeEscape

unicodeEscape :: Stream s Identity Char => Parser s Char
unicodeEscape = char '\\' >> unicodeEscapeSequence

identifierPart :: Stream s Identity Char => Parser s Char
identifierPart = identifierStart <|> unicodeCombiningMark <|> unicodeDigit <|>
                 unicodeConnectorPunctuation <|> uZWNJ <|> uZWJ

--7.6.1
reservedWord :: Stream s Identity Char => Parser s ()
reservedWord = choice [forget keyword, forget futureReservedWord, forget nullLiteral, forget booleanLiteral]

--7.6.1.1
keyword = choice [kbreak, kcase, kcatch, kcontinue, kdebugger, kdefault, kdelete,
                  kdo, kelse, kfinally, kfor, kfunction, kif, kinstanceof, knew,
                  kreturn, kswitch, kthis, kthrow, ktry, ktypeof, kvar, kvoid, kwhile, kwith]

-- ECMAScript keywords
kbreak = string "break"
kcase  = string "case"
kcatch = string "catch"
kcontinue = string "continue"
kdebugger = string "debugger"
kdefault = string "default"
kdelete = string "delete"
kdo = string "do"
kelse = string "else"
kfinally = string "finally"
kfor = string "for"
kfunction = string "function"
kif = string "if"
kin = string "in"
kinstanceof = string "instanceof"
knew = string "new"
kreturn = string "return"
kswitch = string "switch"
kthis = string "this"
kthrow = string "throw"
ktry = string "try"
ktypeof = string "typeof"
kvar = string "var"
kvoid = string "void"
kwhile = string "while"
kwith = string "with"

--7.6.1.2
futureReservedWord = choice [kclass, kconst, kenum, kexport, kextends, kimport, ksuper]

kclass = string "class"
kconst = string "const"
kenum = string "enum"
kexport = string "export"
kextends = string "extends"
kimport = string "import"
ksuper = string "super"

--7.7
punctuator = choice [ passignadd, passignsub, passignmul, passignmod, passignshl, passignshr,
                      passignushr, passignband, passignbor, passignbxor,
                      pshl, pshr, pushr,
                      pleqt, pgeqt,
                      plbrace, prbrace, plparen, prparen, plbracket, prbracket, pdot, psemi, pcomma,
                      plangle, prangle, pseq, peq, psneq, pneq,
                      pplusplus, pminusminus,
                      padd, psub, pmul,
                      pand, por,
                      pmod, pband, pbor, pbxor, pnot, pbnot,
                      pquestion, pcolon, passign ]

plbrace = string "{"
prbrace = string "}"
plparen = string "("
prparen = string ")"
plbracket = string "["
prbracket = string "]"
pdot = string "."
psemi = string ";"
pcomma = string ","
plangle = string "<"
prangle = string ">"
pleqt = string "<="
pgeqt = string ">="
peq  = string "=="
pneq = string "!="
pseq = string "==="
psneq = string "!=="
padd = string "+"
psub = string "-"
pmul = string "*"
pmod = string "%"
pplusplus = string "++"
pminusminus = string "--"
pshl = string "<<"
pshr = string ">>"
pushr = string ">>>"
pband = string "&"
pbor = string "|"
pbxor = string "^"
pnot = string "!"
pbnot = string "~"
pand = string "&&"
por = string "||"
pquestion = string "?"
pcolon = string ":"
passign = string "="
passignadd = string "+="
passignsub = string "-="
passignmul = string "*="
passignmod = string "%="
passignshl = string "<<="
passignshr = string ">>="
passignushr = string ">>>="
passignband = string "&="
passignbor = string "|="
passignbxor = string "^="

divPunctuator = choice [ passigndiv, pdiv ]

passigndiv = string "/="
pdiv = string "/"

--7.8
literal :: Parser s (PositionedExpression)
literal = choice [nullLiteral, booleanLiteral, numericLiteral, stringLiteral, regularExpressionLiteral]

--7.8.1
nullLiteral :: Parser s (PositionedExpression)
nullLiteral = lexeme $ withPos (string "null" >> return $ NullLit def)

--7.8.2
booleanLiteral :: Parser s (PositionedExpression)
booleanLiteral = lexeme $ withPos $ ((string "true" >> return (BoolLit def True)) 
                                 <|> (string "false" >> return (BoolLit def False)))

--7.8.3
numericLiteral :: Parser s (PositionedExpression)
numericLiteral = hexIntegerLiteral <|> decimalLiteral

-- Creates a decimal value from a whole, fractional and exponent parts.
mkDecimal :: Integer -> Integer -> Integer -> Integer -> Double
mkDecimal whole frac fracLen exp = 
  ((fromInteger whole) + (frac * (10 ^^ (-fracLen)))) * (10 ^^ exp)

-- Creates an integer value from a whole and exponent parts.
mkInteger :: Integer -> Integer -> Int
mkInteger whole exp = fromInteger $ whole * (10 ^^ exp)

decimalLiteral = lexeme $ withPos $
  (do whole <- decimalIntegerLiteral
      mfrac <- optionMaybe (pdot >> decimalDigits)
      mexp  <- optionMaybe exponentPart
      if (mfrac == Nothing && mexp == Nothing)
        then return $ IntLit def $ fromIntegral whole
        else return $ NumLit def $ mkDecimal whole (fromMaybe 0 mfrac
                                             (fromIntegral (fromMaybe 0 mfrac))
                                             (fromIntegral (fromMaybe 0 mexp)))) <|>
  (do frac <- pdot >> decimalDigits
      exp <- option 0 exponentPart
      return $ NumLit def $ mkDecimal 0.0 (fromIntegral frac) (fromIntegral exp))

decimalDigits :: Parser s (Integer, Integer)   
decimalDigits = many decimalDigit >>= fromDecimal

decimalIntegerLiteral :: Parser s Integer
decimalIntegerLiteral = 
  (char '0' >> return 0) <|> 
  (do d  <- nonZeroDecimalDigit
      ds <- many decimalDigit
      return $ foldr (\(pow, n) -> (pow*10, n*pow)) (1, 0) (d:ds))

-- the spec says that decimalDigits should be intead of decimalIntegerLiteral, but that seems like an error
signedInteger = (char '+' >> decimalIntegerLiteral) <|> 
                (char '-' >> negate <$> decimalIntegerLiteral) <|>
                decimalIntegerLiteral

decimalDigit :: Parser s Integer
decimalDigit  = do c <- rangeChar '0' '9'
                   return $ ord c - ord '0'
                   
nonZeroDecimalDigit :: Parser s Integer
nonZeroDecimalDigit  = do c <- rangeChar '1' '9'
                          return $ ord c - ord '0'

                                                   
--hexDigit = ParsecChar.hexDigit

exponentPart = (char 'e' <|> char 'E') >> signedInteger
       

fromHex digits = do [(hex,"")] <- return $ Numeric.readHex digits
                    return hex
                    
fromDecimal digits = do [(hex,"")] <- return $ Numeric.readDec digits
                        return hex

hexIntegerLiteral = lexeme $ withPos $ do
  try (char '0' >> (char 'x' <|> char 'X'))
  digits <- many1 hexDigit
  n <- fromHex digits
  return $ IntLit def n
                         
--7.8.4
dblquote = char '"'
quote = char '\''
backslash = char '\\'
dblquotes = between dblquote dblquote
quotes = between quote quote


stringLiteral :: Parser s (PositionedExpression)
stringLiteral =  lexeme $ withPos $ 
                 do s <- ((dblquotes $ concatM $ many doubleStringCharacter) <|> 
                          (quotes $ concatM $ many singleStringCharacter))
                    return $ StringLit def s

doubleStringCharacter :: Parser s String
doubleStringCharacter =  (stringify ((anyChar `butNot` choice[forget dblquote, forget backslash, lineTerminator]) <|>
                         (backslash >> escapeSequence))) <|>                        
                         lineContinuation 

singleStringCharacter :: Parser s String
singleStringCharacter =  (stringify ((anyChar `butNot` choice[forget quote, forget backslash, forget lineTerminator]) <|>
                         (backslash >> escapeSequence))) <|>
                         lineContinuation

lineContinuation = backslash >> lineTerminatorSequence >> return ""

escapeSequence = characterEscapeSequence <|>
                 (char '0' >> notFollowedBy decimalDigit >> return cNUL) <|>
                 hexEscapeSequence <|>
                 unicodeEscapeSequence

characterEscapeSequence = singleEscapeCharacter <|> nonEscapeCharacter

singleEscapeCharacter = choice $ map (\(ch, cod) -> (char ch >> return cod)) [ 
                         ('b', cBS), ('t', cHT), ('n', cLF), ('v', cVT),
                         ('f', cFF), ('r', cCR), ('"', '"'), ('\'', '\''), ('\\', '\\')]

nonEscapeCharacter = anyChar `butNot` (forget escapeCharacter <|> lineTerminator)

escapeCharacter = singleEscapeCharacter <|> decimalDigit <|> char 'x' <|> char 'u'

hexEscapeSequence =  do digits <- (char 'x' >> count 2 hexDigit)
                        hex <- fromHex digits
                        return $ chr hex

unicodeEscapeSequence :: Stream s Identity Char => Parser s Char
unicodeEscapeSequence = do digits <- char 'u' >> count 4 hexDigit
                           hex <- fromHex digits
                           return $ chr hex

--7.8.5 and 15.10.4.1
regularExpressionLiteral = 
    lexeme $ withPos $ do 
      body <- between (char '/') (char '/') regularExpressionBody
      (g, i, m) <- regularExpressionFlags
      return $ RegexpLit body g i m 
                           
-- TODO: The spec requires the parser to make sure the body is a valid
-- regular expression; were are not doing it at present.
regularExpressionBody = do c <- regularExpressionFirstChar 
                           cs <- concatM regularExpressionChars  
                           return (c++cs)
                         
regularExpressionChars = many regularExpressionChar

regularExpressionFirstChar :: Parser s String
regularExpressionFirstChar = 
  choice [
    stringify $ regularExpressionNonTerminator `butNot` choice [char '*', char '\\', char '/', char '[' ],
    regularExpressionBackslashSequence,
    regularExpressionClass ]

regularExpressionChar :: Parser s String
regularExpressionChar = 
  choice [
    stringify $ regularExpressionNonTerminator `butNot` choice [char '\\', char '/', char '[' ],
    regularExpressionBackslashSequence,
    regularExpressionClass ]
                         
regularExpressionBackslashSequence = do c <-char '\\'  
                                        e <- regularExpressionNonTerminator
                                        return (c:[e])
    
regularExpressionNonTerminator = notP lineTerminator
                           
regularExpressionClass = do l <- char '[' 
                            rc <- concatM $ many regularExpressionClassChar
                            r <- char ']'
                            return (l:(rc++[r]))

regularExpressionClassChar = stringify (regularExpressionNonTerminator `butNot` (char ']' <|> char '\\')) <|>
                             regularExpressionBackslashSequence
    
regularExpressionFlags :: Parser s (Bool, Bool, Bool) -- g, i, m    
regularExpressionFlags = regularExpressionFlags' (False, False, False)
  
regularExpressionFlags' :: (Bool, Bool, Bool) -> Parser s (Bool, Bool, Bool)
regularExpressionFlags' (g, i, m) = 
    (char 'g' >> (if not g then regularExpressionFlags' (True, i, m) else unexpected "duplicate 'g' in regular expression flags")) <|>
    (char 'i' >> (if not g then regularExpressionFlags' (g, True, m) else unexpected "duplicate 'i' in regular expression flags")) <|>
    (char 'm' >> (if not g then regularExpressionFlags' (g, i, True) else unexpected "duplicate 'g' in regular expression flags")) <|>
    return (g, i, m)
    
-- | 7.9 || TODO: write tests based on examples from Spec 7.9.2, once I
-- get the parser finished! Automatic Semicolon Insertion algorithm,
-- rule 1; to be used in place of `semi`/`char 'x'` in parsers for
-- emptyStatement, variableStatement, expressionStatement,
-- doWhileStatement, continuteStatement, breakStatement,
-- returnStatement and throwStatement.
autoSemi :: Parser s ()
autoSemi = (forget $ char ';') <|> 
           lineTerminator <|>
           (forget $ char '}')
  
-- | Automatic Semicolon Insertion algorithm, rule 2;
-- to be used at the end of the program
endOfProgram :: Parser s ()
endOfProgram = forget (char ';') <|> eof
           
-- | Automatic Semicolon Insertion algorithm, rule 3; it takes 2
-- parsers: 'left' that parses whatever is to the left of [no
-- LineTerminator here] and 'right' that parses whatever is to the
-- right; if after parsing 'left' and any number of whiteSpaces a
-- lineTerminator is found, 'right' is not invoked and (l, Nothing) is
-- returned, where 'l' is the result of left; otherwise (l, Just r) is
-- returned, where 'l' and 'r' are results of left and right
-- respectively.
noLineTerminator :: Parser s a -> Parser s b -> Parser s (a, Maybe b)
noLineTerminator left right = do l <- left
                                 spaces
                                 ((try lineTerminator >>
                                  return (l, Nothing)) <|>
                                  (right >>= (\r-> return (l, Just r))))

-- 11.1
-- primary expressions
primaryExpression :: Parser s (PositionedExpression)
primaryExpression = choice [ lexeme $ withPos (kthis >> return $ ThisRef def),
                             identifier,
                             literal,
                             arrayLiteral,
                             parenExpression ]

parenExpression :: Parser s (PositionedExpression)
parenExpression = lexeme $ withPos (between (char '(') (char ')') expression)
                                    
                                        
-- 11.1.4
arrayLiteral :: Parser s (PositionedExpression)
arrayLiteral = lexeme $ withPos $ 
               do char '['
                  e <- elementsListWithElision
                  char ']'
                  return e
               
elisionOpt :: Parser s Int
elisionOpt = many (char ',') >>= (return . length)
               
-- elementsListWithElision :: Parser [ a
elementsListWithElision = error "not implemented"
             
-- elementsList :: Parser 
             
expression :: Parser s (PositionedExpression)
expression = error "not implemented"