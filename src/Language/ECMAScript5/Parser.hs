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
import Text.Parsec hiding (parse)
import Text.Parsec.Char (CharParser, char, string, satisfy, oneOf, noneOf, hexDigit, anyChar)
import Text.Parsec.Char as ParsecChar hiding (spaces)
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (Parser)

import Control.Monad(liftM,liftM2)
import Control.Monad.Trans (MonadIO,liftIO)
import Numeric(readDec,readOct,readHex)
import Data.Char
import Control.Monad.Identity
import Data.Maybe (isJust, isNothing, fromMaybe)

import Numeric as Numeric

type Parser n = CharParser () n {-nodetype-}

-- a convenience wrapper to take care of the position, "with position"
withPos   :: Parser n -> Parser (Positioned n)
withPos p = do start <- getPosition
               result <- p
               end <- getPosition
               return $ positioned result (start, end)

-- Below "x.y.z" are references to ECMAScript 5 spec chapters that discuss the corresponding grammar production
--7.2
whiteSpace = forget $ choice [uTAB, uVT, uFF, uSP, uNBSP, uBOM, uUSP]

spaces = skipMany (whiteSpace <|> comment <?> "")

lexeme p = do{ x <- p; spaces; return x  }

--7.3
uCRalone = do c <- uCR
              notFollowedBy uLF
              return c
lineTerminator = forget (uLF <|> uCR <|> uLS <|> uPS)
lineTerminatorSequence = (forget (uLF <|> uCRalone <|> uLS <|> uPS )) <|> (forget uCRLF)

--7.4
comment :: Parser ()
comment = multiLineComment <|> singleLineComment

singleLineCommentChars = singleLineCommentChar >> singleLineCommentChars
singleLineCommentChar  = notP lineTerminator
multiLineCommentChars  = (multiLineNotAsteriskChar >> multiLineCommentChars) <|>
                         (char '*' >> postAsteriskCommentChars)

multiLineComment = string "/*" >> optional multiLineCommentChars >> (forget (string "*/"))

singleLineComment = string "//" >> optional singleLineCommentChars

multiLineNotAsteriskChar = notP $ char '*'
multiLineNotForwardSlashOrAsteriskChar = noneOf "/*"
postAsteriskCommentChars = (do c  <- multiLineNotForwardSlashOrAsteriskChar
                               cs <- option "" multiLineCommentChars
                               return (c:cs)) <|>
                           (do c  <- char '*'
                               cs <- option "" postAsteriskCommentChars
                               return (c:cs))

--7.5
--token = identifierName <|> punctuator <|> numericLiteral <|> stringLiteral

--7.6
identifier :: Parser (Positioned PrimExp)
identifier = lexeme $ withPos $ do name <- identifierName `butNot` reservedWord
                                   return $ EIdent name

identifierName = do c  <- identifierStart 
                    cs <- many identifierPart
                    return (c:cs)

identifierStart = unicodeLetter <|> char '$' <|> char '_' <|> unicodeEscape

unicodeEscape = char '\\' >> unicodeEscapeSequence

identifierPart = identifierStart <|> unicodeCombiningMark <|> unicodeDigit <|>
                 unicodeConnectorPunctuation <|> uZWNJ <|> uZWJ


--7.6.1
reservedWord :: Parser ()
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
literal :: Parser (Positioned PrimExp)
literal = choice [nullLiteral, booleanLiteral, numericLiteral, stringLiteral, regularExpressionLiteral]

--7.8.1
nullLiteral :: Parser (Positioned PrimExp)
nullLiteral = lexeme $ withPos (string "null" >> return ENull)

--7.8.2
booleanLiteral :: Parser (Positioned PrimExp)
booleanLiteral = lexeme $ withPos $ ((string "true" >> return (EBool True)) <|> 
                                     (string "false" >> return (EBool False)))

--7.8.3
numericLiteral :: Parser (Positioned PrimExp)
numericLiteral = hexIntegerLiteral <|> decimalLiteral


-- Creates a decimal value from a whole, fractional and exponent part.
mkDecimal:: Double -> Double -> Int -> Double
mkDecimal w f e =
  if (f >= 1.0)
    then mkDecimal w (f / 10.0) e
    else (w + f) * (10.0 ^^ e)

decimalLiteral = lexeme $ withPos $
  (do whole <- decimalIntegerLiteral
      mfrac <- optionMaybe (pdot >> decimalDigits)
      mexp <-  optionMaybe exponentPart
      if (mfrac == Nothing && mexp == Nothing)
        then return $ EInt $ fromIntegral whole
        else return $ EDouble $ mkDecimal (fromIntegral whole) 
                                         (fromIntegral (fromMaybe 0 mfrac))
                                         (fromIntegral (fromMaybe 0 mexp))) <|>
  (do frac <- pdot >> decimalDigits
      exp <- option 0 exponentPart
      return $ EDouble $ mkDecimal 0.0 (fromIntegral frac) (fromIntegral exp))

decimalDigits = many decimalDigit >>= fromDecimal

decimalIntegerLiteral = (char '0' >> return 0) <|> 
                        (do c  <- rangeChar '1' '9'
                            ds <- many decimalDigit
                            fromHex (c:ds))

-- the spec says that decimalDigits should be intead of decimalIntegerLiteral, but that seems like an error
signedInteger = (char '+' >> decimalIntegerLiteral) <|> 
                (char '-' >> negate `fmap` decimalIntegerLiteral) <|>
                decimalIntegerLiteral

decimalDigit  = ParsecChar.digit
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
  return $ EHex n
                         
--7.8.4
dblquote = char '"'
quote = char '\''
backslash = char '\\'
dblquotes = between dblquote dblquote
quotes = between quote quote


stringLiteral :: Parser (Positioned PrimExp)
stringLiteral =  lexeme $ withPos $ 
                 do s <- ((dblquotes $ concatM $ many doubleStringCharacter) <|> 
                          (quotes $ concatM $ many singleStringCharacter))
                    return $ EString s

doubleStringCharacter :: Parser String
doubleStringCharacter =  (stringify ((anyChar `butNot` choice[forget dblquote, forget backslash, lineTerminator]) <|>
                         (backslash >> escapeSequence))) <|>                        
                         lineContinuation 

singleStringCharacter :: Parser String
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



unicodeEscapeSequence = do digits <- char 'u' >> count 4 hexDigit
                           hex <- fromHex digits
                           return $ chr hex

--7.8.5 and 15.10.4.1
regularExpressionLiteral = 
    lexeme $ withPos $ do 
      body <- between (char '/') (char '/') regularExpressionBody
      (g, i, m) <- regularExpressionFlags
      return $ ERegexp body g i m 
                           
-- TODO: The spec requires the parser to make sure the body is a valid regular expression;
-- were are not doing it at present.
regularExpressionBody = do c <- regularExpressionFirstChar 
                           cs <- concatM regularExpressionChars  
                           return (c++cs)
                         
regularExpressionChars = many regularExpressionChar

regularExpressionFirstChar :: Parser String
regularExpressionFirstChar = 
  choice [
    stringify $ regularExpressionNonTerminator `butNot` choice [char '*', char '\\', char '/', char '[' ],
    regularExpressionBackslashSequence,
    regularExpressionClass ]

regularExpressionChar :: Parser String
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
    
regularExpressionFlags :: Parser (Bool, Bool, Bool) -- g, i, m    
regularExpressionFlags = regularExpressionFlags' (False, False, False)
  
regularExpressionFlags' :: (Bool, Bool, Bool) -> Parser (Bool, Bool, Bool)
regularExpressionFlags' (g, i, m) = 
    (char 'g' >> (if not g then regularExpressionFlags' (True, i, m) else unexpected "duplicate 'g' in regular expression flags")) <|>
    (char 'i' >> (if not g then regularExpressionFlags' (g, True, m) else unexpected "duplicate 'i' in regular expression flags")) <|>
    (char 'm' >> (if not g then regularExpressionFlags' (g, i, True) else unexpected "duplicate 'g' in regular expression flags")) <|>
    return (g, i, m)
    
-- 7.9 || TODO: write tests based on examples from Spec 7.9.2, once I get the parser finished
-- Automatic Semicolon Insertion algorithm, rule 1;
-- to be used in place of `semi`/`char 'x'` 
-- in parsers for emptyStatement, variableStatement, 
-- expressionStatement, doWhileStatement, continuteStatement, breakStatement,
-- returnStatement and throwStatement.
autoSemi :: Parser ()
autoSemi = (forget $ char ';') <|> 
           lineTerminator <|>
           (forget $ char '}')
  
-- Automatic Semicolon Insertion algorithm, rule 2;
-- to be used at the end of the program
endOfProgram :: Parser ()
endOfProgram = forget (char ';') <|> eof
           
-- Automatic Semicolon Insertion algorithm, rule 3; it takes 2
-- parsers: 'left' that parses whatever is to the left of [no
-- LineTerminator here] and 'right' that parses whatever is to the
-- right; if after parsing 'left' and any number of whiteSpaces a
-- lineTerminator is found, 'right' is not invoked and (l, Nothing) is
-- returned, where 'l' is the result of left; otherwise (l, Just r) is
-- returned, where 'l' and 'r' are results of left and right
-- respectively.
noLineTerminator :: Parser a -> Parser b -> Parser (a, Maybe b)
noLineTerminator left right = do l <- left
                                 spaces
                                 ((try lineTerminator >>
                                  return (l, Nothing)) <|>
                                  (right >>= (\r-> return (l, Just r))))

-- 11.1
-- primary expressions
primaryExpression :: Parser (Positioned PrimExp)
primaryExpression = choice [ lexeme $ withPos (kthis >> return EThis),
                             identifier,
                             literal,
                             arrayLiteral,
                             parenExpression ]

parenExpression :: Parser (Positioned PrimExp)
parenExpression = lexeme $ withPos (between (char '(') (char ')') expression >>=
                                    (return . EParen . node))
                                        
-- 11.1.4
arrayLiteral :: Parser (Positioned PrimExp)
arrayLiteral = lexeme $ withPos $ 
               do char '['
                  e <- elementsListWithElision
                  char ']'
                  return e
               
elisionOpt :: Parser Int
elisionOpt = many (char ',') >>= (return . length)
               
-- elementsListWithElision :: Parser [ a
             
-- elementsList :: Parser 
             
expression :: Parser (Positioned Exp)
expression = error "not implemented"


------------------------------------------------------
-- Old module from ECMAScript3 starts here -----------
------------------------------------------------------

-- We parameterize the parse tree over source-locations.
type ParsedStatement = Statement SourcePos
type ParsedExpression = Expression SourcePos

type CharParser a = ParsecT String ParserState Identity a

-- These parsers can store some arbitrary state
type StatementParser  = CharParser ParsedStatement
type ExpressionParser = CharParser ParsedExpression
-- the statement label stack
type ParserState = [String]

initialParserState :: ParserState
initialParserState = []

-- | checks if the label is not yet on the stack, if it is -- throws
-- an error; otherwise it pushes it onto the stack
pushLabel :: String -> CharParser ()
pushLabel lab = do labs <- getState
                   pos <- getPosition
                   if lab `elem` labs 
                     then fail $ "Duplicate label at " ++ show pos
                     else putState (lab:labs)

popLabel :: CharParser ()
popLabel = modifyState safeTail
  where safeTail [] = []
        safeTail (_:xs) = xs

clearLabels :: ParserState -> ParserState
clearLabels _ = []

withFreshLabelStack :: CharParser a -> CharParser a
withFreshLabelStack p = do oldState <- getState
                           putState $ clearLabels oldState
                           a <- p
                           putState oldState
                           return a

identifier :: CharParser (Id SourcePos)
identifier =
  liftM2 Id getPosition Lexer.identifier

--{{{ Statements

-- Keep in mind that Token.reserved parsers (exported from the lexer) do not
-- consume any input on failure.  Note that all statements (expect for labelled
-- and expression statements) begin with a reserved-word.  If we fail to parse
-- this reserved-word, no input is consumed.  Hence, we can have the massive or
-- block that is parseExpression.  Note that if the reserved-word is parsed, it 
-- must be whatever statement the reserved-word indicates.  If we fail after the
-- reserved-word, we truly have a syntax error.  Since input has been consumed,
-- <|> will not try its alternate in parseExpression, and we will fail.

parseIfStmt:: StatementParser  
parseIfStmt = do
  pos <- getPosition
  reserved "if"
  test <- parseParenExpr <?> "parenthesized test-expression in if statement"
  consequent <- parseStatement <?> "true-branch of if statement"
  optional semi -- TODO: in spec?
  ((do reserved "else"
       alternate <- parseStatement
       return $ IfStmt pos test consequent alternate)
   <|> return (IfSingleStmt pos test consequent))

parseSwitchStmt:: StatementParser
parseSwitchStmt =
  let parseDefault = do
        pos <- getPosition
        reserved "default"
        colon
        statements <- many parseStatement
        return (CaseDefault pos statements)
      parseCase = do
         pos <- getPosition
         reserved "case"
         condition <- parseListExpr
         colon
         actions <- many parseStatement
         return (CaseClause pos condition actions)
      isCaseDefault (CaseDefault _ _) = True   
      isCaseDefault _                 = False
      checkClauses cs = case filter isCaseDefault cs of
        (_:c:_) -> fail $ "duplicate default clause in switch statement at " ++
                          show (getAnnotation c)
        _ -> return ()                  
    in do pos <- getPosition
          reserved "switch"
          test <- parseParenExpr
          clauses <- braces $ many $ parseDefault <|> parseCase
          checkClauses clauses
          return (SwitchStmt pos test clauses)

parseWhileStmt:: StatementParser
parseWhileStmt = do
  pos <- getPosition
  reserved "while"
  test <- parseParenExpr <?> "parenthesized test-expression in while loop"
  body <- parseStatement
  return (WhileStmt pos test body)

parseDoWhileStmt:: StatementParser
parseDoWhileStmt = do
  pos <- getPosition
  reserved "do"
  body <- parseBlockStmt
  reserved "while" <?> "while at the end of a do block"
  test <- parseParenExpr <?> "parenthesized test-expression in do loop"
  optional semi
  return (DoWhileStmt pos body test)

parseContinueStmt:: StatementParser
parseContinueStmt = do
  pos <- getPosition
  reserved "continue"
  pos' <- getPosition
  -- Ensure that the identifier is on the same line as 'continue.'
  id <- if sourceLine pos == sourceLine pos'
        then liftM Just identifier <|> return Nothing
        else return Nothing
  optional semi
  return $ ContinueStmt pos id

parseBreakStmt:: StatementParser
parseBreakStmt = do
  pos <- getPosition
  reserved "break"
  pos' <- getPosition
  -- Ensure that the identifier is on the same line as 'break.'
  id <- if sourceLine pos == sourceLine pos'
        then liftM Just identifier <|> return Nothing
        else return Nothing
  optional semi           
  return $ BreakStmt pos id

parseBlockStmt:: StatementParser
parseBlockStmt = do
  pos <- getPosition
  statements <- braces (many parseStatement)
  return (BlockStmt pos statements)

parseEmptyStmt:: StatementParser 
parseEmptyStmt = do
  pos <- getPosition
  semi
  return (EmptyStmt pos)

parseLabelledStmt:: StatementParser
parseLabelledStmt = do
  pos <- getPosition
  -- Lookahead for the colon.  If we don't see it, we are parsing an identifier
  -- for an expression statement.
  label <- try (do label <- identifier
                   colon
                   return label)
  pushLabel $ unId label
  statement <- parseStatement
  popLabel
  return (LabelledStmt pos label statement)

parseExpressionStmt:: StatementParser
parseExpressionStmt = do
  pos <- getPosition
  expr <- parseListExpr -- TODO: spec 12.4?
  optional semi
  return $ ExprStmt pos expr


parseForInStmt:: StatementParser
parseForInStmt =
  let parseInit = (reserved "var" >> liftM ForInVar identifier)
               <|> liftM ForInLVal lvalue
  in do pos <- getPosition
        -- Lookahead, so that we don't clash with parseForStmt
        (init,expr) <- try $ do reserved "for"
                                parens $ do init <- parseInit
                                            reserved "in"
                                            expr <- parseExpression
                                            return (init,expr)
        body <- parseStatement
        return $ ForInStmt pos init expr body

parseForStmt:: StatementParser
parseForStmt =
  let parseInit = (reserved "var" >> liftM VarInit (parseVarDecl `sepBy` comma))
               <|> liftM ExprInit parseListExpr
               <|> return NoInit
    in do pos <- getPosition
          reserved "for"
          reservedOp "("
          init <- parseInit
          semi
          test <- optionMaybe parseExpression
          semi
          iter <- optionMaybe parseListExpr
          reservedOp ")" <?> "closing paren"
          stmt <- parseStatement
          return $ ForStmt pos init test iter stmt

parseTryStmt:: StatementParser
parseTryStmt =
  let parseCatchClause = do pos <- getPosition
                            reserved "catch"
                            id <- parens identifier
                            stmt <- parseStatement
                            return $ CatchClause pos id stmt
  in do reserved "try"
        pos <- getPosition
        guarded <- parseStatement
        mCatch <- optionMaybe parseCatchClause
        mFinally <- optionMaybe $ reserved "finally" >> parseStatement
        -- the spec requires at least a catch or a finally block to
        -- be present
        if isJust mCatch || isJust mFinally 
          then return $ TryStmt pos guarded mCatch mFinally
          else fail $ "A try statement should have at least a catch\ 
                      \ or a finally block, at " ++ show pos

parseThrowStmt:: StatementParser
parseThrowStmt = do
  pos <- getPosition
  reserved "throw"
  expr <- parseExpression
  optional semi
  return (ThrowStmt pos expr)

parseReturnStmt:: StatementParser
parseReturnStmt = do
  pos <- getPosition
  reserved "return"
  expr <- optionMaybe parseListExpr
  optional semi
  return (ReturnStmt pos expr)

parseWithStmt:: StatementParser
parseWithStmt = do
  pos <- getPosition
  reserved "with"
  context <- parseParenExpr
  stmt <- parseStatement
  return (WithStmt pos context stmt)

parseVarDecl = do
  pos <- getPosition
  id <- identifier
  init <- (reservedOp "=" >> liftM Just parseExpression) <|> return Nothing
  return (VarDecl pos id init)

parseVarDeclStmt:: StatementParser
parseVarDeclStmt = do 
  pos <- getPosition
  reserved "var"
  decls <- parseVarDecl `sepBy` comma
  optional semi
  return (VarDeclStmt pos decls)

parseFunctionStmt:: StatementParser
parseFunctionStmt = do
  pos <- getPosition
  name <- try (reserved "function" >> identifier) -- ambiguity with FuncExpr
  args <- parens (identifier `sepBy` comma)
  -- label sets don't cross function boundaries
  body <- withFreshLabelStack parseBlockStmt <?> "function body in { ... }"
  return (FunctionStmt pos name args body)

parseStatement:: StatementParser
parseStatement = parseIfStmt <|> parseSwitchStmt <|> parseWhileStmt 
  <|> parseDoWhileStmt <|> parseContinueStmt <|> parseBreakStmt 
  <|> parseBlockStmt <|> parseEmptyStmt <|> parseForInStmt <|> parseForStmt
  <|> parseTryStmt <|> parseThrowStmt <|> parseReturnStmt <|> parseWithStmt 
  <|> parseVarDeclStmt  <|> parseFunctionStmt
  -- labelled, expression and the error message always go last, in this order
  <|> parseLabelledStmt <|> parseExpressionStmt <?> "statement"

--}}}

--{{{ Expressions

-- References used to construct this stuff:
-- + http://developer.mozilla.org/en/docs/
--     Core_JavaScript_1.5_Reference:Operators:Operator_Precedence
-- + http://www.mozilla.org/js/language/grammar14.html
--
-- Aren't expression tables nice?  Well, we can't quite use them, because of 
-- JavaScript's ternary (?:) operator.  We have to use two expression tables.
-- We use one expression table for the assignment operators that bind looser 
-- than ?: (assignTable).  The terms of assignTable are ternary expressions 
-- (parseTernaryExpr).  parseTernaryExpr left-factors the left-recursive
-- production for ?:, and is defined over the second expression table, 
-- exprTable, which consists of operators that bind tighter than ?:.  The terms
-- of exprTable are atomic expressions, parenthesized expressions, functions and
-- array references.

--{{{ Primary expressions

parseThisRef:: ExpressionParser
parseThisRef = do
  pos <- getPosition
  reserved "this"
  return (ThisRef pos)

parseNullLit:: ExpressionParser
parseNullLit = do
  pos <- getPosition
  reserved "null"
  return (NullLit pos)


parseBoolLit:: ExpressionParser
parseBoolLit = do
    pos <- getPosition
    let parseTrueLit  = reserved "true"  >> return (BoolLit pos True)
        parseFalseLit = reserved "false" >> return (BoolLit pos False)
    parseTrueLit <|> parseFalseLit

parseVarRef:: ExpressionParser
parseVarRef = liftM2 VarRef getPosition identifier

parseArrayLit:: ExpressionParser
parseArrayLit = liftM2 ArrayLit getPosition (squares (parseExpression `sepEndBy` comma))
  
parseFuncExpr = do
  pos <- getPosition
  reserved "function"
  name <- optionMaybe identifier
  args <- parens (identifier `sepBy` comma)
  -- labels don't cross function boundaries
  body <- withFreshLabelStack parseBlockStmt
  return $ FuncExpr pos name args body

--{{{ parsing strings

escapeChars =
 [('\'','\''),('\"','\"'),('\\','\\'),('b','\b'),('f','\f'),('n','\n'),
  ('r','\r'),('t','\t'),('v','\v'),('/','/'),(' ',' '),('0','\0')]

allEscapes:: String
allEscapes = map fst escapeChars

parseEscapeChar = do
  c <- oneOf allEscapes
  let (Just c') = lookup c escapeChars -- will succeed due to line above
  return c' 

parseAsciiHexChar = do
  char 'x'
  d1 <- hexDigit
  d2 <- hexDigit
  return ((chr.fst.head.readHex) (d1:d2:""))

parseUnicodeHexChar = do
  char 'u'
  liftM (chr.fst.head.readHex) 
        (sequence [hexDigit,hexDigit,hexDigit,hexDigit])
        
isWhitespace ch = ch `elem` " \t"


-- The endWith argument is either single-quote or double-quote, depending on how
-- we opened the string.
parseStringLit' endWith =
  (char endWith >> return "") <|>
  (do try (string "\\'")
      cs <- parseStringLit' endWith
      return $ "'" ++ cs) <|>
  (do char '\\'
      c <- parseEscapeChar <|> parseAsciiHexChar <|> parseUnicodeHexChar <|> 
           char '\r' <|> char '\n'
      cs <- parseStringLit' endWith
      if c == '\r' || c == '\n' 
        then return (c:dropWhile isWhitespace cs) 
        else return (c:cs)) <|>
   liftM2 (:) anyChar (parseStringLit' endWith)

parseStringLit:: ExpressionParser
parseStringLit = do
  pos <- getPosition
  -- parseStringLit' takes as an argument the quote-character that opened the
  -- string.
  str <- lexeme $ (char '\'' >>= parseStringLit') <|> (char '\"' >>= parseStringLit')
  -- CRUCIAL: Parsec.Token parsers expect to find their token on the first
  -- character, and read whitespaces beyond their tokens.  Without 'lexeme'
  -- above, expressions like:
  --   var s = "string"   ;
  -- do not parse.
  return $ StringLit pos str

--}}}

parseRegexpLit:: ExpressionParser
parseRegexpLit = do
  let parseFlags = do
        flags <- many (oneOf "mgi")
        return $ \f -> f ('g' `elem` flags) ('i' `elem` flags) 
  let parseEscape :: CharParser Char
      parseEscape = char '\\' >> anyChar
  let parseChar :: CharParser Char
      parseChar = noneOf "/"
  let parseRe = (char '/' >> return "") <|> 
                (do char '\\'
                    ch <- anyChar -- TOOD: too lenient
                    rest <- parseRe
                    return ('\\':ch:rest)) <|> 
                liftM2 (:) anyChar parseRe
  pos <- getPosition
  char '/'
  pat <- parseRe --many1 parseChar
  flags <- parseFlags
  spaces -- crucial for Parsec.Token parsers
  return $ flags (RegexpLit pos pat)
          
parseObjectLit:: ExpressionParser
parseObjectLit =
  let parseProp = do
        -- Parses a string, identifier or integer as the property name.  I
        -- apologize for the abstruse style, but it really does make the code
        -- much shorter.
        name <- liftM (\(StringLit p s) -> PropString p s) parseStringLit
            <|> liftM2 PropId getPosition identifier
            <|> liftM2 PropNum getPosition decimal
        colon
        val <- assignExpr
        return (name,val)
    in do pos <- getPosition
          props <- braces (parseProp `sepEndBy` comma) <?> "object literal"
          return $ ObjectLit pos props

--{{{ Parsing numbers.  From pg. 17-18 of ECMA-262.

hexLit = do
  try (string "0x")
  digits <- many1 (oneOf "0123456789abcdefABCDEF")
  [(hex,"")] <- return $ Numeric.readHex digits
  return (True, hex)

-- Creates a decimal value from a whole, fractional and exponent part.
mkDecimal:: Double -> Double -> Int -> Double
mkDecimal w f e =  if f >= 1.0
                   then mkDecimal w (f / 10.0) e
                   else (w + f) * (10.0 ^^ e)

exponentPart = do
  oneOf "eE"
  (char '+' >> decimal) <|> (char '-' >> negate `fmap` decimal) <|> decimal

--wrap a parser's result in a Just:
jparser = liftM Just

decLit = 
  (do whole <- decimal
      mfrac <- option Nothing (jparser (char '.' >> decimal))
      mexp <-  option Nothing (jparser exponentPart)
      if isNothing mfrac && isNothing mexp
        then return (True, fromIntegral whole)
        else return (False, mkDecimal (fromIntegral whole) 
                                      (fromIntegral (fromMaybe 0 mfrac))
                                      (fromIntegral (fromMaybe 0 mexp)))) <|>
  (do frac <- char '.' >> decimal
      exp <- option 0 exponentPart
      return (False, mkDecimal 0.0 (fromIntegral frac) (fromIntegral exp)))

parseNumLit:: ExpressionParser
parseNumLit = do
    pos <- getPosition
    (isint, num) <- lexeme $ hexLit <|> decLit
    notFollowedBy identifierStart <?> "whitespace"
    if isint
      then return $ IntLit pos (round num) 
      else return $ NumLit pos num


------------------------------------------------------------------------------
-- Position Helper
------------------------------------------------------------------------------

withPos cstr p = do { pos <- getPosition; e <- p; return $ cstr pos e }

-------------------------------------------------------------------------------
-- Compound Expression Parsers
-------------------------------------------------------------------------------

dotRef e = (reservedOp "." >> withPos cstr identifier) <?> "property.ref"
    where cstr pos = DotRef pos e

funcApp e = parens (withPos cstr (parseExpression `sepBy` comma)) 
         <?>"(function application)"
    where cstr pos = CallExpr pos e

bracketRef e = brackets (withPos cstr parseExpression) <?> "[property-ref]"
    where cstr pos = BracketRef pos e

-------------------------------------------------------------------------------
-- Expression Parsers
-------------------------------------------------------------------------------

parseParenExpr:: ExpressionParser
parseParenExpr = withPos ParenExpr (parens parseListExpr)

-- everything above expect functions
parseExprForNew = parseThisRef <|> parseNullLit <|> parseBoolLit <|> parseStringLit 
  <|> parseArrayLit <|> parseParenExpr <|> parseNewExpr <|> parseNumLit 
  <|> parseRegexpLit <|> parseObjectLit <|> parseVarRef

-- all the expression parsers defined above
parseSimpleExpr' = parseThisRef <|> parseNullLit <|> parseBoolLit 
  <|> parseStringLit <|> parseArrayLit <|> parseParenExpr
  <|> parseFuncExpr <|> parseNumLit <|> parseRegexpLit <|> parseObjectLit
  <|> parseVarRef

parseNewExpr =
  (do pos <- getPosition
      reserved "new"
      constructor <- parseSimpleExprForNew Nothing -- right-associativity
      arguments <- try (parens (parseExpression `sepBy` comma)) <|> return []
      return (NewExpr pos constructor arguments)) <|>
  parseSimpleExpr'

parseSimpleExpr (Just e) = ((dotRef e <|> funcApp e <|> bracketRef e) >>=
                            parseSimpleExpr . Just)  
                        <|> return e
parseSimpleExpr Nothing = do
  e <- parseNewExpr <?> "expression (3)"
  parseSimpleExpr (Just e)

parseSimpleExprForNew (Just e) = ((dotRef e <|> bracketRef e) >>=
                                  parseSimpleExprForNew . Just)
                              <|> return e
parseSimpleExprForNew Nothing = do
  e <- parseNewExpr <?> "expression (3)"
  parseSimpleExprForNew (Just e)
    
--}}}

makeInfixExpr str constr = Infix parser AssocLeft where
  parser:: CharParser (Expression SourcePos -> Expression SourcePos -> Expression SourcePos)
  parser = do
    pos <- getPosition
    reservedOp str
    return (InfixExpr pos constr)  -- points-free, returns a function


-- apparently, expression tables can't handle immediately-nested prefixes
parsePrefixedExpr = do
  pos <- getPosition
  op <- optionMaybe $ (reservedOp "!" >> return PrefixLNot) <|> 
                      (reservedOp "~" >> return PrefixBNot) <|>
                      (try (lexeme $ char '-' >> notFollowedBy (char '-')) >>
                       return PrefixMinus) <|>
                      (try (lexeme $ char '+' >> notFollowedBy (char '+')) >>
                       return PrefixPlus) <|>
                      (reserved "typeof" >> return PrefixTypeof) <|>
                      (reserved "void" >> return PrefixVoid) <|>
                      (reserved "delete" >> return PrefixDelete)
  case op of
    Nothing -> unaryAssignExpr
    Just op -> do
      innerExpr <- parsePrefixedExpr
      return (PrefixExpr pos op innerExpr)

exprTable:: [[Operator String ParserState Identity ParsedExpression]]
exprTable = 
  [ [ makeInfixExpr "==" OpEq
    , makeInfixExpr "!=" OpNEq
    , makeInfixExpr "===" OpStrictEq
    , makeInfixExpr "!==" OpStrictNEq
    ]

  , [ makeInfixExpr "||" OpLOr ]

  , [ makeInfixExpr "&&" OpLAnd ]
  
  , [ makeInfixExpr "|" OpBOr ]

  , [ makeInfixExpr "^" OpBXor ]

  , [ makeInfixExpr "&" OpBAnd ]

  , [ makeInfixExpr "<" OpLT
    , makeInfixExpr "<=" OpLEq
    , makeInfixExpr ">" OpGT
    , makeInfixExpr ">=" OpGEq
    , makeInfixExpr "instanceof" OpInstanceof
    , makeInfixExpr "in" OpIn
    ]

  , [ makeInfixExpr "<<" OpLShift
    , makeInfixExpr ">>" OpSpRShift
    , makeInfixExpr ">>>" OpZfRShift
    ]

  , [ makeInfixExpr "+" OpAdd
    , makeInfixExpr "-" OpSub
    ]

  , [ makeInfixExpr "*" OpMul
    , makeInfixExpr "/" OpDiv
    , makeInfixExpr "%" OpMod
    ]
  ]

parseExpression' = 
  buildExpressionParser exprTable parsePrefixedExpr <?> "simple expression"

asLValue :: SourcePos
         -> Expression SourcePos 
         -> CharParser (LValue SourcePos)
asLValue p' e = case e of
  VarRef p (Id _ x) -> return (LVar p x)
  DotRef p e (Id _ x) -> return (LDot p e x)
  BracketRef p e1 e2 -> return (LBracket p e1 e2)
  otherwise -> fail $ "expected a left-value at " ++ show p'

lvalue :: CharParser (LValue SourcePos)
lvalue = do
  p <- getPosition
  e <- parseSimpleExpr Nothing
  asLValue p e


unaryAssignExpr :: CharParser ParsedExpression
unaryAssignExpr = do
  p <- getPosition
  let prefixInc = do
        reservedOp "++"
        liftM (UnaryAssignExpr p PrefixInc) lvalue
  let prefixDec = do
        reservedOp "--"
        liftM (UnaryAssignExpr p PrefixDec) lvalue
  let postfixInc e = do
        reservedOp "++"
        liftM (UnaryAssignExpr p PostfixInc) (asLValue p e)
  let postfixDec e = do
        reservedOp "--"
        liftM (UnaryAssignExpr p PostfixDec) (asLValue p e)
  let other = do
        e <- parseSimpleExpr Nothing
        postfixInc e <|> postfixDec e <|> return e
  prefixInc <|> prefixDec <|> other


parseTernaryExpr':: CharParser (ParsedExpression,ParsedExpression)
parseTernaryExpr' = do
    reservedOp "?"
    l <- assignExpr
    colon
    r <- assignExpr
    return (l,r)

parseTernaryExpr:: ExpressionParser
parseTernaryExpr = do
  e <- parseExpression'
  e' <- optionMaybe parseTernaryExpr'
  case e' of
    Nothing -> return e
    Just (l,r) -> do p <- getPosition
                     return $ CondExpr p e l r


assignOp :: CharParser AssignOp
assignOp = 
  (reservedOp "=" >> return OpAssign) <|>
  (reservedOp "+=" >> return OpAssignAdd) <|>
  (reservedOp "-=" >> return OpAssignSub) <|>
  (reservedOp "*=" >> return OpAssignMul) <|>
  (reservedOp "/=" >> return OpAssignDiv) <|>
  (reservedOp "%=" >> return OpAssignMod) <|>
  (reservedOp "<<=" >> return OpAssignLShift) <|>
  (reservedOp ">>=" >> return OpAssignSpRShift) <|>
  (reservedOp ">>>=" >> return OpAssignZfRShift) <|>
  (reservedOp "&=" >> return OpAssignBAnd) <|>
  (reservedOp "^=" >> return OpAssignBXor) <|>
  (reservedOp "|=" >> return OpAssignBOr)


assignExpr :: ExpressionParser
assignExpr = do
  p <- getPosition
  lhs <- parseTernaryExpr
  let assign = do
        op <- assignOp
        lhs <- asLValue p lhs
        rhs <- assignExpr
        return (AssignExpr p op lhs rhs)
  assign <|> return lhs

parseExpression:: ExpressionParser
parseExpression = assignExpr

parseListExpr = liftM2 ListExpr getPosition (assignExpr `sepBy1` comma)

parseScript:: CharParser (JavaScript SourcePos)
parseScript = do
  whiteSpace
  liftM2 Script getPosition (parseStatement `sepBy` whiteSpace)
  
-- | Parse from a stream; same as 'Text.Parsec.parse'
parse :: Stream s Identity t =>
         Parsec s [String] a -- ^ The parser to use
      -> SourceName -- ^ Name of the source file
      -> s -- ^ the stream to parse, usually a 'String'
      -> Either ParseError a
parse p = runParser p initialParserState

-- | Read a JavaScript program from file an parse it into a list of
-- statements
parseJavaScriptFromFile :: MonadIO m => String -- ^ file name
                        -> m [Statement SourcePos]
parseJavaScriptFromFile filename = do
  chars <- liftIO $ readFile filename
  case parse parseScript filename chars of
    Left err               -> fail (show err)
    Right (Script _ stmts) -> return stmts

-- | Parse a JavaScript program from a string
parseScriptFromString :: String -- ^ source file name
                      -> String -- ^ JavaScript source to parse
                      -> Either ParseError (JavaScript SourcePos)
parseScriptFromString = parse parseScript

-- | Parse a JavaScript source string into a list of statements
parseString :: String -- ^ JavaScript source
            -> [Statement SourcePos]
parseString str = case parse parseScript "" str of
  Left err -> error (show err)
  Right (Script _ stmts) -> stmts