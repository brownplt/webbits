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
import Control.Arrow

--import Numeric as Numeric

-- the statement label stack
type ParserState = [String]

type Parser s a = ParsecT s ParserState Identity a

type SourceSpan = (SourcePos, SourcePos)

type PositionedExpression = Expression SourceSpan
type PositionedStatement  = Statement SourceSpan
type PositionedId         = Id SourceSpan

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
withPos   :: (HasAnnotation x, Stream s Identity Char)
          => Parser s (x SourceSpan) -> Parser s (x SourceSpan)
withPos p = do start <- getPosition
               result <- p
               end <- getPosition
               return $ setAnnotation (start, end) result

-- Below "x.y.z" are references to ECMAScript 5 spec chapters that discuss the corresponding grammar production
--7.2
whiteSpace :: Stream s Identity Char => Parser s ()
whiteSpace = forget $ choice [uTAB, uVT, uFF, uSP, uNBSP, uBOM, uUSP]

spaces :: Stream s Identity Char => Parser s ()
spaces = skipMany (whiteSpace <|> comment <?> "")

lexeme p = do{ x <- p; spaces; return x}

--7.3
uCRalone :: Stream s Identity Char => Parser s Char
uCRalone = do c <- uCR
              notFollowedBy uLF
              return c
lineTerminator :: Stream s Identity Char => Parser s ()
lineTerminator = forget (uLF <|> uCR <|> uLS <|> uPS)
lineTerminatorSequence  :: Stream s Identity Char => Parser s ()
lineTerminatorSequence = (forget (uLF <|> uCRalone <|> uLS <|> uPS )) <|> (forget uCRLF)

--7.4
comment :: Stream s Identity Char => Parser s ()
comment = multiLineComment <|> singleLineComment

singleLineCommentChars :: Stream s Identity Char => Parser s ()
singleLineCommentChars = singleLineCommentChar >> singleLineCommentChars
singleLineCommentChar :: Stream s Identity Char => Parser s ()
singleLineCommentChar  = notP lineTerminator
multiLineCommentChars :: Stream s Identity Char => Parser s ()
multiLineCommentChars  = (multiLineNotAsteriskChar >> multiLineCommentChars) 
                      <|>(char '*' >> postAsteriskCommentChars)
multiLineComment :: Stream s Identity Char => Parser s ()
multiLineComment = forget (string "/*" >> optional multiLineCommentChars >> string "*/")
singleLineComment :: Stream s Identity Char => Parser s ()
singleLineComment = string "//" >> optional singleLineCommentChars
multiLineNotAsteriskChar :: Stream s Identity Char => Parser s ()
multiLineNotAsteriskChar = notP $ char '*'
multiLineNotForwardSlashOrAsteriskChar :: Stream s Identity Char
                                       => Parser s Char
multiLineNotForwardSlashOrAsteriskChar = noneOf "/*"
postAsteriskCommentChars :: Stream s Identity Char
                         => Parser s ()
postAsteriskCommentChars = (multiLineNotForwardSlashOrAsteriskChar >>
                            forget (optionMaybe multiLineCommentChars)) 
                        <|>(char '*' >>
                            forget (optionMaybe postAsteriskCommentChars))

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
keyword :: Stream s Identity Char => Parser s String
keyword = choice [kbreak, kcase, kcatch, kcontinue, kdebugger, kdefault, kdelete,
                  kdo, kelse, kfinally, kfor, kfunction, kif, kinstanceof, knew,
                  kreturn, kswitch, kthis, kthrow, ktry, ktypeof, kvar, kvoid, kwhile, kwith]

-- ECMAScript keywords
kbreak :: Stream s Identity Char => Parser s String
kbreak = string "break"
kcase :: Stream s Identity Char => Parser s String
kcase  = string "case"
kcatch :: Stream s Identity Char => Parser s String
kcatch = string "catch"
kcontinue :: Stream s Identity Char => Parser s String
kcontinue = string "continue"
kdebugger :: Stream s Identity Char => Parser s String
kdebugger = string "debugger"
kdefault :: Stream s Identity Char => Parser s String
kdefault = string "default"
kdelete :: Stream s Identity Char => Parser s String
kdelete = string "delete"
kdo :: Stream s Identity Char => Parser s String
kdo = string "do"
kelse :: Stream s Identity Char => Parser s String
kelse = string "else"
kfinally :: Stream s Identity Char => Parser s String
kfinally = string "finally"
kfor :: Stream s Identity Char => Parser s String
kfor = string "for"
kfunction :: Stream s Identity Char => Parser s String
kfunction = string "function"
kif :: Stream s Identity Char => Parser s String
kif = string "if"
kin :: Stream s Identity Char => Parser s String
kin = string "in"
kinstanceof :: Stream s Identity Char => Parser s String
kinstanceof = string "instanceof"
knew :: Stream s Identity Char => Parser s String
knew = string "new"
kreturn :: Stream s Identity Char => Parser s String
kreturn = string "return"
kswitch :: Stream s Identity Char => Parser s String
kswitch = string "switch"
kthis :: Stream s Identity Char => Parser s String
kthis = string "this"
kthrow :: Stream s Identity Char => Parser s String
kthrow = string "throw"
ktry :: Stream s Identity Char => Parser s String
ktry = string "try"
ktypeof :: Stream s Identity Char => Parser s String
ktypeof = string "typeof"
kvar :: Stream s Identity Char => Parser s String
kvar = string "var"
kvoid :: Stream s Identity Char => Parser s String
kvoid = string "void"
kwhile :: Stream s Identity Char => Parser s String
kwhile = string "while"
kwith :: Stream s Identity Char => Parser s String
kwith = string "with"

--7.6.1.2
futureReservedWord :: Stream s Identity Char => Parser s String
futureReservedWord = choice [kclass, kconst, kenum, kexport, kextends, kimport, ksuper]
kclass :: Stream s Identity Char => Parser s String
kclass = string "class"
kconst :: Stream s Identity Char => Parser s String
kconst = string "const"
kenum :: Stream s Identity Char => Parser s String
kenum = string "enum"
kexport :: Stream s Identity Char => Parser s String
kexport = string "export"
kextends :: Stream s Identity Char => Parser s String
kextends = string "extends"
kimport :: Stream s Identity Char => Parser s String
kimport = string "import"
ksuper :: Stream s Identity Char => Parser s String
ksuper = string "super"

--7.7
punctuator :: Stream s Identity Char => Parser s ()
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
plbrace :: Stream s Identity Char => Parser s ()
plbrace = forget $ string "{"
prbrace :: Stream s Identity Char => Parser s ()
prbrace = forget $ string "}"
plparen :: Stream s Identity Char => Parser s ()
plparen = forget $ string "("
prparen :: Stream s Identity Char => Parser s ()
prparen = forget $ string ")"
plbracket :: Stream s Identity Char => Parser s ()
plbracket = forget $ string "["
prbracket :: Stream s Identity Char => Parser s ()
prbracket = forget $ string "]"
pdot :: Stream s Identity Char => Parser s ()
pdot = forget $ string "."
psemi :: Stream s Identity Char => Parser s ()
psemi = forget $ string ";"
pcomma :: Stream s Identity Char => Parser s ()
pcomma = forget $ string ","
plangle :: Stream s Identity Char => Parser s ()
plangle = forget $ string "<"
prangle :: Stream s Identity Char => Parser s ()
prangle = forget $ string ">"
pleqt :: Stream s Identity Char => Parser s ()
pleqt = forget $ string "<="
pgeqt :: Stream s Identity Char => Parser s ()
pgeqt = forget $ string ">="
peq :: Stream s Identity Char => Parser s ()
peq  = forget $ string "=="
pneq :: Stream s Identity Char => Parser s ()
pneq = forget $ string "!="
pseq :: Stream s Identity Char => Parser s ()
pseq = forget $ string "==="
psneq :: Stream s Identity Char => Parser s ()
psneq = forget $ string "!=="
padd :: Stream s Identity Char => Parser s ()
padd = forget $ string "+"
psub :: Stream s Identity Char => Parser s ()
psub = forget $ string "-"
pmul :: Stream s Identity Char => Parser s ()
pmul = forget $ string "*"
pmod :: Stream s Identity Char => Parser s ()
pmod = forget $ string "%"
pplusplus :: Stream s Identity Char => Parser s ()
pplusplus = forget $ string "++"
pminusminus :: Stream s Identity Char => Parser s ()
pminusminus = forget $ string "--"
pshl :: Stream s Identity Char => Parser s ()
pshl = forget $ string "<<"
pshr :: Stream s Identity Char => Parser s ()
pshr = forget $ string ">>"
pushr :: Stream s Identity Char => Parser s ()
pushr = forget $ string ">>>"
pband :: Stream s Identity Char => Parser s ()
pband = forget $ string "&"
pbor :: Stream s Identity Char => Parser s ()
pbor = forget $ string "|"
pbxor :: Stream s Identity Char => Parser s ()
pbxor = forget $ string "^"
pnot :: Stream s Identity Char => Parser s ()
pnot = forget $ string "!"
pbnot :: Stream s Identity Char => Parser s ()
pbnot = forget $ string "~"
pand :: Stream s Identity Char => Parser s ()
pand = forget $ string "&&"
por :: Stream s Identity Char => Parser s ()
por = forget $ string "||"
pquestion :: Stream s Identity Char => Parser s ()
pquestion = forget $ string "?"
pcolon :: Stream s Identity Char => Parser s ()
pcolon = forget $ string ":"
passign :: Stream s Identity Char => Parser s ()
passign = forget $ string "="
passignadd :: Stream s Identity Char => Parser s ()
passignadd = forget $ string "+="
passignsub :: Stream s Identity Char => Parser s ()
passignsub = forget $ string "-="
passignmul :: Stream s Identity Char => Parser s ()
passignmul = forget $ string "*="
passignmod :: Stream s Identity Char => Parser s ()
passignmod = forget $ string "%="
passignshl :: Stream s Identity Char => Parser s ()
passignshl = forget $ string "<<="
passignshr :: Stream s Identity Char => Parser s ()
passignshr = forget $ string ">>="
passignushr :: Stream s Identity Char => Parser s ()
passignushr = forget $ string ">>>="
passignband :: Stream s Identity Char => Parser s ()
passignband = forget $ string "&="
passignbor :: Stream s Identity Char => Parser s ()
passignbor = forget $ string "|="
passignbxor :: Stream s Identity Char => Parser s ()
passignbxor = forget $ string "^="
divPunctuator :: Stream s Identity Char => Parser s ()
divPunctuator = choice [ passigndiv, pdiv ]
passigndiv :: Stream s Identity Char => Parser s ()
passigndiv = forget $ string "/="
pdiv :: Stream s Identity Char => Parser s ()
pdiv = forget $ string "/"



--7.8
literal :: Stream s Identity Char => Parser s PositionedExpression
literal = choice [nullLiteral, booleanLiteral, numericLiteral, stringLiteral, regularExpressionLiteral]

--7.8.1
nullLiteral :: Stream s Identity Char => Parser s PositionedExpression
nullLiteral = lexeme $ withPos (string "null" >> return (NullLit def))

--7.8.2
booleanLiteral :: Stream s Identity Char => Parser s PositionedExpression
booleanLiteral = lexeme $ withPos $ ((string "true" >> return (BoolLit def True)) 
                                 <|> (string "false" >> return (BoolLit def False)))

--7.8.3
numericLiteral :: Stream s Identity Char => Parser s PositionedExpression
numericLiteral = hexIntegerLiteral <|> decimalLiteral

-- Creates a decimal value from a whole, fractional and exponent parts.
mkDecimal :: Integer -> Integer -> Integer -> Integer -> Double
mkDecimal whole frac fracLen exp = 
  ((fromInteger whole) + ((fromInteger frac) * (10 ^^ (-fracLen)))) * (10 ^^ exp)

-- Creates an integer value from a whole and exponent parts.
mkInteger :: Integer -> Integer -> Int
mkInteger whole exp = fromInteger $ whole * (10 ^ exp)

decimalLiteral :: Stream s Identity Char => Parser s PositionedExpression
decimalLiteral = lexeme $ withPos $
  (do whole <- decimalInteger
      mfraclen <- optionMaybe (pdot >> decimalDigitsWithLength)
      mexp  <- optionMaybe exponentPart
      if (mfraclen == Nothing && mexp == Nothing)
        then return $ IntLit def $ fromIntegral whole
        else let (frac, flen) = fromMaybe (0, 0) mfraclen 
                 exp          = fromMaybe 0 mexp 
             in  return $ NumLit def $ mkDecimal whole frac flen exp)
  <|>
  (do (frac, flen) <- pdot >> decimalDigitsWithLength
      exp <- option 0 exponentPart
      return $ NumLit def $ mkDecimal 0 frac flen exp)

decimalDigitsWithLength :: Stream s Identity Char
                        => Parser s (Integer, Integer)   
decimalDigitsWithLength = do digits <- many decimalDigit
                             return $ digits2NumberAndLength digits
                             
digits2NumberAndLength :: [Integer] -> (Integer, Integer)
digits2NumberAndLength is = 
  let (_, n, l) = foldr (\d (pow, acc, len) -> (pow*10, acc + d*pow, len+1)) 
                        (1, 0, 0) is
  in (n, l)
          
decimalIntegerLiteral :: Stream s Identity Char
                      => Parser s PositionedExpression
decimalIntegerLiteral = lexeme $ withPos $ decimalInteger >>= 
                        \i -> return (IntLit def (fromInteger i))
                                  
decimalInteger :: Stream s Identity Char => Parser s Integer
decimalInteger = (char '0' >> return 0)
              <|>(do d  <- nonZeroDecimalDigit
                     ds <- many decimalDigit
                     return $ fst $ digits2NumberAndLength (d:ds))

-- the spec says that decimalDigits should be intead of decimalIntegerLiteral, but that seems like an error
signedInteger :: Stream s Identity Char => Parser s Integer
signedInteger = (char '+' >> decimalInteger) <|> 
                (char '-' >> negate <$> decimalInteger) <|>
                decimalInteger
  where flipSign :: Monad m => PositionedExpression -> m PositionedExpression
        flipSign (IntLit a i) = return $ IntLit a (-i)

decimalDigit :: Stream s Identity Char => Parser s Integer
decimalDigit  = do c <- decimalDigitChar
                   return $ toInteger $ ord c - ord '0'
                   
decimalDigitChar :: Stream s Identity Char => Parser s Char
decimalDigitChar = rangeChar '0' '9'
                   
nonZeroDecimalDigit :: Stream s Identity Char => Parser s Integer
nonZeroDecimalDigit  = do c <- rangeChar '1' '9'
                          return $ toInteger $ ord c - ord '0'
                                                   
--hexDigit = ParsecChar.hexDigit

exponentPart :: Stream s Identity Char => Parser s Integer
exponentPart = (char 'e' <|> char 'E') >> signedInteger
       

fromHex digits = do [(hex,"")] <- return $ Numeric.readHex digits
                    return hex
                    
fromDecimal digits = do [(hex,"")] <- return $ Numeric.readDec digits
                        return hex
hexIntegerLiteral :: Stream s Identity Char => Parser s PositionedExpression
hexIntegerLiteral = lexeme $ withPos $ do
  try (char '0' >> (char 'x' <|> char 'X'))
  digits <- many1 hexDigit
  n <- fromHex digits
  return $ IntLit def n
                         
--7.8.4
dblquote :: Stream s Identity Char => Parser s Char
dblquote = char '"'
quote :: Stream s Identity Char => Parser s Char
quote = char '\''
backslash :: Stream s Identity Char => Parser s Char
backslash = char '\\'
inDblQuotes :: Stream s Identity Char => Parser s a -> Parser s a
inDblQuotes = between dblquote dblquote
inQuotes :: Stream s Identity Char => Parser s a -> Parser s a
inQuotes = between quote quote

stringLiteral :: Stream s Identity Char => Parser s (PositionedExpression)
stringLiteral =  lexeme $ withPos $ 
                 do s <- ((inDblQuotes $ concatM $ many doubleStringCharacter)
                          <|> 
                          (inQuotes $ concatM $ many singleStringCharacter))
                    return $ StringLit def s

doubleStringCharacter :: Stream s Identity Char => Parser s String
doubleStringCharacter = (stringify ((anyChar `butNot` choice[forget dblquote, forget backslash, lineTerminator]) <|>(backslash >> escapeSequence)))
                     <|>lineContinuation 

singleStringCharacter :: Stream s Identity Char => Parser s String
singleStringCharacter =  (stringify ((anyChar `butNot` choice[forget quote, forget backslash, forget lineTerminator])<|> (backslash >> escapeSequence)))
                     <|>lineContinuation

lineContinuation :: Stream s Identity Char => Parser s String
lineContinuation = backslash >> lineTerminatorSequence >> return ""

escapeSequence :: Stream s Identity Char => Parser s Char
escapeSequence = characterEscapeSequence
              <|>(char '0' >> notFollowedBy decimalDigitChar >> return cNUL)
              <|>hexEscapeSequence
              <|>unicodeEscapeSequence
                 
characterEscapeSequence :: Stream s Identity Char => Parser s Char
characterEscapeSequence = singleEscapeCharacter <|> nonEscapeCharacter

singleEscapeCharacter :: Stream s Identity Char => Parser s Char
singleEscapeCharacter = choice $ map (\(ch, cod) -> (char ch >> return cod)) 
                        [('b', cBS), ('t', cHT), ('n', cLF), ('v', cVT),
                         ('f', cFF), ('r', cCR), ('"', '"'), ('\'', '\''), 
                         ('\\', '\\')]

nonEscapeCharacter :: Stream s Identity Char => Parser s Char
nonEscapeCharacter = anyChar `butNot` (forget escapeCharacter <|> lineTerminator)

escapeCharacter :: Stream s Identity Char => Parser s Char
escapeCharacter = singleEscapeCharacter
               <|>decimalDigitChar
               <|>char 'x'
               <|>char 'u'

hexEscapeSequence :: Stream s Identity Char => Parser s Char
hexEscapeSequence =  do digits <- (char 'x' >> count 2 hexDigit)
                        hex <- fromHex digits
                        return $ chr hex

unicodeEscapeSequence :: Stream s Identity Char => Parser s Char
unicodeEscapeSequence = do digits <- char 'u' >> count 4 hexDigit
                           hex <- fromHex digits
                           return $ chr hex

--7.8.5 and 15.10.4.1
regularExpressionLiteral :: Stream s Identity Char
                         => Parser s PositionedExpression
regularExpressionLiteral = 
    lexeme $ withPos $ do 
      body <- between pdiv pdiv regularExpressionBody
      (g, i, m) <- regularExpressionFlags
      return $ RegexpLit def body g i m 
                           
-- TODO: The spec requires the parser to make sure the body is a valid
-- regular expression; were are not doing it at present.
regularExpressionBody = do c <- regularExpressionFirstChar 
                           cs <- concatM regularExpressionChars  
                           return (c:cs)
                         
regularExpressionChars :: Stream s Identity Char => Parser s String
regularExpressionChars = many regularExpressionChar

regularExpressionFirstChar :: Stream s Identity Char => Parser s Char
regularExpressionFirstChar = 
  choice [
    stringify $ regularExpressionNonTerminator `butNot` choice [char '*', char '\\', char '/', char '[' ],
    regularExpressionBackslashSequence,
    regularExpressionClass ]

regularExpressionChar :: Stream s Identity Char => Parser s String
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
    
regularExpressionFlags :: Stream s Identity Char 
                       => Parser s (Bool, Bool, Bool) -- g, i, m    
regularExpressionFlags = regularExpressionFlags' (False, False, False)
  
regularExpressionFlags' :: Stream s Identity Char
                        => (Bool, Bool, Bool) 
                        -> Parser s (Bool, Bool, Bool)
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
autoSemi :: Stream s Identity Char => Parser s ()
autoSemi = (forget $ char ';') <|> 
           lineTerminator <|>
           (forget $ char '}')
  
-- | Automatic Semicolon Insertion algorithm, rule 2;
-- to be used at the end of the program
endOfProgram :: Stream s Identity Char => Parser s ()
endOfProgram = forget (char ';') <|> eof
           
-- | Automatic Semicolon Insertion algorithm, rule 3; it takes 2
-- parsers: 'left' that parses whatever is to the left of [no
-- LineTerminator here] and 'right' that parses whatever is to the
-- right; if after parsing 'left' and any number of whiteSpaces a
-- lineTerminator is found, 'right' is not invoked and (l, Nothing) is
-- returned, where 'l' is the result of left; otherwise (l, Just r) is
-- returned, where 'l' and 'r' are results of left and right
-- respectively.
noLineTerminator :: Stream s Identity Char
                 => Parser s a -> Parser s b -> Parser s (a, Maybe b)
noLineTerminator left right = do l <- left
                                 spaces
                                 ((try lineTerminator >>
                                  return (l, Nothing)) <|>
                                  (right >>= (\r-> return (l, Just r))))

-- 11.1
-- primary expressions
primaryExpression :: Stream s Identity Char => Parser s PositionedExpression
primaryExpression = choice [ lexeme $ withPos (kthis >> return $ ThisRef def),
                             identifier,
                             literal,
                             arrayLiteral,
                             parenExpression ]

parenExpression :: Stream s Identity Char => Parser s PositionedExpression
parenExpression = lexeme $ withPos (between (char '(') (char ')') expression)
                                    
                                        
-- 11.1.4
arrayLiteral :: Stream s Identity Char => Parser s PositionedExpression
arrayLiteral = lexeme $ withPos $ 
               do char '['
                  e <- elementsListWithElision
                  char ']'
                  return e
               
elisionOpt :: Stream s Identity Char => Parser s Int
elisionOpt = many (char ',') >>= (return . length)
               
-- elementsListWithElision :: Parser [ a
elementsListWithElision = error "not implemented"
             
-- elementsList :: Parser 
             
expression :: Stream s Identity Char => Parser s PositionedExpression
expression = error "not implemented"