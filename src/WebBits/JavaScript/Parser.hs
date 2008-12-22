module WebBits.JavaScript.Parser(parseScript,parseExpression,parseStatement
   , parseScriptFromString
   , emptyParsedJavaScript
   , ParsedStatement
   , ParsedExpression
   , parseJavaScriptFromFile
   , parseSimpleExpr'
   , parseBlockStmt
   , StatementParser
   , ExpressionParser
   , parseAssignExpr
   ) where

import WebBits.JavaScript.Lexer hiding (identifier)
import qualified WebBits.JavaScript.Lexer as Lexer
import WebBits.JavaScript.Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Monad(liftM,liftM2)
import Control.Monad.Trans (MonadIO,liftIO)
import Numeric(readDec,readOct,readHex)
import Data.Char(chr)

-- We parameterize the parse tree over source-locations.
type ParsedStatement = Statement SourcePos
type ParsedExpression = Expression SourcePos


-- These parsers can store some arbitrary state
type StatementParser state = CharParser state ParsedStatement
type ExpressionParser state = CharParser state ParsedExpression

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

parseIfStmt:: StatementParser st  
parseIfStmt = do
  pos <- getPosition
  reserved "if"
  test <- parseParenExpr <?> "parenthesized test-expression in if statement"
  consequent <- parseStatement <?> "true-branch of if statement"
  optional semi -- TODO: in spec?
  ((do reserved "else"
       alternate <- parseStatement
       return (IfStmt pos test consequent alternate))
    <|> return (IfSingleStmt pos test consequent))

parseSwitchStmt:: StatementParser st
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
    in do pos <- getPosition
          reserved "switch"
          test <- parseParenExpr
          clauses <- braces $ many $ parseDefault <|> parseCase
          return (SwitchStmt pos test clauses)

parseWhileStmt:: StatementParser st
parseWhileStmt = do
  pos <- getPosition
  reserved "while"
  test <- parseParenExpr <?> "parenthesized test-expression in while loop"
  body <- parseStatement
  return (WhileStmt pos test body)

parseDoWhileStmt:: StatementParser st
parseDoWhileStmt = do
  pos <- getPosition
  reserved "do"
  body <- parseStatement
  reserved "while"
  test <- parseParenExpr <?> "parenthesized test-expression in do loop"
  optional semi
  return (DoWhileStmt pos body test)

parseContinueStmt:: StatementParser st
parseContinueStmt = do
  pos <- getPosition
  reserved "continue"
  pos' <- getPosition
  -- Ensure that the identifier is on the same line as 'continue.'
  id <- (if (sourceLine pos == sourceLine pos')
           then (liftM Just identifier) <|> (return Nothing)
           else return Nothing)
  return (ContinueStmt pos id)

parseBreakStmt:: StatementParser st
parseBreakStmt = do
  pos <- getPosition
  reserved "break"
  pos' <- getPosition
  -- Ensure that the identifier is on the same line as 'break.'
  id <- (if (sourceLine pos == sourceLine pos')
           then (liftM Just identifier) <|> (return Nothing)
           else return Nothing)
  optional semi           
  return (BreakStmt pos id)

parseBlockStmt:: StatementParser st
parseBlockStmt = do
  pos <- getPosition
  statements <- braces (many parseStatement)
  return (BlockStmt pos statements)

parseEmptyStmt:: StatementParser st 
parseEmptyStmt = do
  pos <- getPosition
  semi
  return (EmptyStmt pos)

parseLabelledStmt:: StatementParser st
parseLabelledStmt = do
  pos <- getPosition
  -- Lookahead for the colon.  If we don't see it, we are parsing an identifier
  -- for an expression statement.
  label <- try (do label <- identifier
                   colon
                   return label)
  statement <- parseStatement
  return (LabelledStmt pos label statement)

parseExpressionStmt:: StatementParser st
parseExpressionStmt = do
  pos <- getPosition
  expr <- parseListExpr -- TODO: spec 12.4?
  optional semi
  return (ExprStmt pos expr)


parseForInStmt:: StatementParser st
parseForInStmt =
  let parseInit = (reserved "var" >> liftM ForInVar identifier)
                  <|> (liftM ForInNoVar identifier)
    in do pos <- getPosition
          -- Lookahead, so that we don't clash with parseForStmt
          (init,expr) <- try (do reserved "for"
                                 parens (do init <- parseInit
                                            reserved "in"
                                            expr <- parseExpression
                                            return (init,expr)))
          body <- parseStatement
          return (ForInStmt pos init expr body) 

parseForStmt:: StatementParser st
parseForStmt =
  let parseInit =
        (reserved "var" >> liftM VarInit (parseVarDecl `sepBy` comma)) <|>
        (liftM ExprInit parseListExpr) <|>
        (return NoInit)
    in do pos <- getPosition
          reserved "for"
          reservedOp "("
          init <- parseInit
          semi
          test <- (liftM Just parseExpression) <|> (return Nothing)
          semi
          iter <- (liftM Just parseExpression) <|> (return Nothing)
          reservedOp ")" <?> "closing paren"
          stmt <- parseStatement
          return (ForStmt pos init test iter stmt)

parseTryStmt:: StatementParser st
parseTryStmt =
  let parseCatchClause = do
        pos <- getPosition
        reserved "catch"
        id <- parens identifier
        stmt <- parseStatement
        return (CatchClause pos id stmt)
    in do reserved "try"
          pos <- getPosition
          guarded <- parseStatement
          catches <- many parseCatchClause
          finally <- (reserved "finally" >> liftM Just parseStatement) 
                      <|> (return Nothing)
          return (TryStmt pos guarded catches finally)

parseThrowStmt:: StatementParser st
parseThrowStmt = do
  pos <- getPosition
  reserved "throw"
  expr <- parseExpression
  optional semi
  return (ThrowStmt pos expr)

parseReturnStmt:: StatementParser st
parseReturnStmt = do
  pos <- getPosition
  reserved "return"
  expr <- (liftM Just parseListExpr) <|> (return Nothing)
  optional semi
  return (ReturnStmt pos expr)

parseWithStmt:: StatementParser st
parseWithStmt = do
  pos <- getPosition
  reserved "with"
  context <- parseParenExpr
  stmt <- parseStatement
  return (WithStmt pos context stmt)

parseVarDecl = do
  pos <- getPosition
  id <- identifier
  init <- (reservedOp "=" >> liftM Just parseExpression) <|> (return Nothing)
  return (VarDecl pos id init)

parseVarDeclStmt:: StatementParser st
parseVarDeclStmt = do 
  pos <- getPosition
  reserved "var"
  decls <- parseVarDecl `sepBy` comma
  optional semi
  return (VarDeclStmt pos decls)

parseFunctionStmt:: StatementParser st
parseFunctionStmt = do
  pos <- getPosition
  name <- try (reserved "function" >> identifier) -- ambiguity with FuncExpr
  args <- parens (identifier `sepBy` comma)
  body <- parseBlockStmt <?> "function body in { ... }"
  return (FunctionStmt pos name args body)

parseStatement:: StatementParser st
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

parseThisRef:: ExpressionParser st
parseThisRef = do
  pos <- getPosition
  reserved "this"
  return (ThisRef pos)

parseNullLit:: ExpressionParser st
parseNullLit = do
  pos <- getPosition
  reserved "null"
  return (NullLit pos)


parseBoolLit:: ExpressionParser st
parseBoolLit =
  let parseTrueLit = do
        pos <- getPosition
        reserved "true"
        return (BoolLit pos True)
      parseFalseLit = do
        pos <- getPosition
        reserved "false"
        return (BoolLit pos False)
    in parseTrueLit <|> parseFalseLit

parseVarRef:: ExpressionParser st
parseVarRef = liftM2 VarRef getPosition identifier

parseArrayLit:: ExpressionParser st
parseArrayLit = liftM2 ArrayLit getPosition (squares (parseExpression `sepBy` comma))
  
parseFuncExpr = do
  pos <- getPosition
  reserved "function"
  args <- parens (identifier `sepBy` comma)
  body <- parseBlockStmt
  return $ FuncExpr pos args body

--{{{ parsing strings

escapeChars =
 [('\'','\''),('\"','\"'),('\\','\\'),('b','\b'),('f','\f'),('n','\n'),
  ('r','\r'),('t','\t'),('v','\v'),('/','/')]

allEscapes:: String
allEscapes = map fst escapeChars

parseEscapeChar = do
  c <- oneOf allEscapes
  (Just c') <- return $ lookup c escapeChars
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
      c <- (parseEscapeChar <|> parseAsciiHexChar <|> parseUnicodeHexChar <|> char '\r' <|> char '\n')
      cs <- parseStringLit' endWith
      -- TODO: Leading whitespace on the new line is probably meant to be ignored.
      if c == '\r' || c == '\n' then return (c:(dropWhile isWhitespace cs)) else return cs) <|>
   (liftM2 (:) anyChar (parseStringLit' endWith))

parseStringLit:: ExpressionParser st
parseStringLit = do
  pos <- getPosition
  -- parseStringLit' takes as an argument the quote-character that opened the
  -- string.
  str <- (char '\'' >>= parseStringLit') <|> (char '\"' >>= parseStringLit')
  -- CRUCIAL: Parsec.Token parsers expect to find their token on the first
  -- character, and read whitespaces beyond their tokens.  Without this,
  -- expressions like:
  --   var s = "string"   ;
  -- do not parse.
  spaces 
  return (StringLit pos str)

--}}}

parseRegexpLit:: ExpressionParser st
parseRegexpLit = do
  let parseFlags = do
        flags <- many (oneOf "mgi")
        return $ \f -> f ('g' `elem` flags) ('i' `elem` flags) 
  let parseEscape = char '\\' >> anyChar
  let parseChar = noneOf "/"
  let parseRe = (char '/' >> return "") <|> (char '\\' >> liftM2 (:) anyChar parseRe) <|> (liftM2 (:) anyChar parseRe)
  pos <- getPosition
  char '/'
  pat <- parseRe --many1 parseChar
  flags <- parseFlags
  spaces -- crucial for Parsec.Token parsers
  return $ flags (RegexpLit pos pat)
          
parseObjectLit:: ExpressionParser st
parseObjectLit =
  let parseProp = do
        -- Parses a string, identifier or integer as the property name.  I
        -- apologize for the abstruse style, but it really does make the code
        -- much shorter.
        name <- (liftM (uncurry PropString) 
                       (liftM (\(StringLit p s) -> (p,s)) parseStringLit))
                <|> (liftM2 PropId getPosition identifier)
                <|> (liftM2 PropNum getPosition decimal)
        colon
        val <- parseAssignExpr
        return (name,val)
    in do pos <- getPosition
          props <- braces (parseProp `sepBy` comma) <?> "object literal"
          return $ ObjectLit pos props

--{{{ Parsing numbers.  From pg. 17-18 of ECMA-262.

hexLit = do
  try (string "0x")
  digits <- many1 (oneOf "0123456789abcdefABCDEF")
  [(hex,"")] <- return $ Numeric.readHex digits
  return hex

decimalDigit = oneOf "0123456789"

-- Creates a decimal value from a whole, fractional and exponent part.
mkDecimal:: Double -> Double -> Int -> Double
mkDecimal w f e =
  if (f >= 1.0)
    then mkDecimal w (f / 10.0) e
    else (w+f) * (10.0^e)

decimalInteger = do
  xs <- many1 decimalDigit
  spaces
  return $ (fst.head.readDec) xs

{-  (try (char '0' >> spaces >> return 0)) <|>
  (do xs <- many1 decimalDigit
      return $ (fst.head.readDec) xs) -}

exponentPart = do
  oneOf "eE"
  ((char '+' >> many1 decimalDigit >>= return.fst.head.readDec) <|> 
   (char '-' >> many1 decimalDigit >>= return.negate.fst.head.readDec) <|>
   (many1 decimalDigit >>= return.fst.head.readDec))
  
decLit =
  (do whole <- decimalInteger
      frac <- option 0 (char '.' >> decimalInteger)
      exp <- option 0  exponentPart
      return $ mkDecimal (fromIntegral whole) (fromIntegral frac) exp) <|>
  (do frac <- char '.' >> decimalInteger
      exp <- option 0  exponentPart
      return $ mkDecimal 0.0 (fromIntegral frac) exp)

parseNumLit:: ExpressionParser st
parseNumLit = 
  liftM2 NumLit getPosition
    (do x <- hexLit <|> decLit
        (notFollowedBy identifierStart <?> "whitespace") 
        spaces
        return x)

--}}}

------------------------------------------------------------------------------
-- Position Helper
------------------------------------------------------------------------------

withPos cstr p = do { pos <- getPosition; e <- p; return $ cstr pos e }

-------------------------------------------------------------------------------
-- Compound Expression Parsers
-------------------------------------------------------------------------------

dotRef e = (reservedOp "." >> withPos cstr identifier) <?> "property.ref"
    where cstr pos key = DotRef pos e key

funcApp e = (parens $ withPos cstr (parseExpression `sepBy` comma)) <?> "(function application)"
    where cstr pos args = CallExpr pos e args

bracketRef e = (brackets $ withPos cstr parseExpression) <?> "[property-ref]"
    where cstr pos key = BracketRef pos e key

-------------------------------------------------------------------------------
-- Expression Parsers
-------------------------------------------------------------------------------

parseParenExpr:: ExpressionParser st
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
      arguments <- (try (parens (parseExpression `sepBy` comma))) <|> (return [])
      return (NewExpr pos constructor arguments)) <|>
  parseSimpleExpr'

parseSimpleExpr (Just e) = (do
    e' <- dotRef e <|> funcApp e <|> bracketRef e
    parseSimpleExpr $ Just e') <|> (return e)
parseSimpleExpr Nothing = do
  e <- parseNewExpr <?> "expression (3)"
  parseSimpleExpr (Just e)

parseSimpleExprForNew (Just e) = (do
    e' <- dotRef e <|> bracketRef e
    parseSimpleExprForNew $ Just e') <|> (return e)
parseSimpleExprForNew Nothing = do
  e <- parseNewExpr <?> "expression (3)"
  parseSimpleExprForNew (Just e)
    
--}}}

makeInfixExpr str constr = Infix parser AssocLeft where
  parser:: CharParser st (Expression SourcePos -> Expression SourcePos -> Expression SourcePos)
  parser = do
    pos <- getPosition
    reservedOp str
    return (InfixExpr pos constr)  -- points-free, returns a function

makePrefixExpr str constr = Prefix parser where
  parser = do
    pos <- getPosition
    (reservedOp str <|> reserved str)
    return (PrefixExpr pos constr) -- points-free, returns a function
    
mkPrefix operator constr = Prefix $ do
  pos <- getPosition
  operator
  return (\operand -> PrefixExpr pos constr operand)

makePostfixExpr str constr = Postfix parser where
  parser = do
    pos <- getPosition
    (reservedOp str <|> reserved str)
    return (PostfixExpr pos constr) -- points-free, returns a function

-- apparently, expression tables can't handle multiple prefixes (which makes sense, since chaining ++ and -- doesn't 
-- make any sense)
parsePrefixedExpr = do
  pos <- getPosition 
  op <- optionMaybe $ (reservedOp "!" >> return PrefixLNot) <|> (reservedOp "~" >> return PrefixBNot)
  case op of
    Nothing -> parseSimpleExpr Nothing  -- new is treated as a simple expr
    Just op -> do
      innerExpr <- parsePrefixedExpr
      return (PrefixExpr pos op innerExpr)
    
exprTable:: [[Operator Char st ParsedExpression]]
exprTable = 
  [
   [makePrefixExpr "++" PrefixInc,
    makePostfixExpr "++" PostfixInc],
   [makePrefixExpr "--" PrefixDec,
    makePostfixExpr "--" PostfixDec],
   -- What we really need here is a good, old-fashioned lexer.
   [mkPrefix (try $ char '+' >> notFollowedBy (char '+')) PrefixPlus,
    mkPrefix (try $ char '-' >> notFollowedBy (char '-')) PrefixMinus],
    -- makePrefixExpr "-" PrefixMinus],
   [makePrefixExpr "typeof" PrefixTypeof,
    makePrefixExpr "void" PrefixVoid,
    makePrefixExpr "delete" PrefixDelete],
    
   [makeInfixExpr "*" OpMul, makeInfixExpr "/" OpDiv, makeInfixExpr "%" OpMod],
   [makeInfixExpr "+" OpAdd, makeInfixExpr "-" OpSub],
   [makeInfixExpr "<<" OpLShift, makeInfixExpr ">>" OpSpRShift,
    makeInfixExpr ">>>" OpZfRShift],
   [makeInfixExpr "<" OpLT, makeInfixExpr "<=" OpLEq, makeInfixExpr ">" OpGT,
    makeInfixExpr ">=" OpGEq, 
    makeInfixExpr "instanceof" OpInstanceof, makeInfixExpr "in" OpIn],
   [makeInfixExpr "&" OpBAnd], 
   [makeInfixExpr "^" OpBXor], 
   [makeInfixExpr "|" OpBOr],
   [makeInfixExpr "&&" OpLAnd],
   [makeInfixExpr "||" OpLOr],  
   [makeInfixExpr "==" OpEq, makeInfixExpr "!=" OpNEq,
    makeInfixExpr "===" OpStrictEq, makeInfixExpr "!==" OpStrictNEq]
    ]
  
parseExpression' = 
  buildExpressionParser exprTable parsePrefixedExpr <?> "simple expression"

--{{{ Parsing ternary operators: left factored

parseTernaryExpr':: CharParser st (Maybe (ParsedExpression,ParsedExpression))
parseTernaryExpr' =
  (do reservedOp "?"
      l <- parseTernaryExpr
      colon
      r <- parseTernaryExpr
      return $ Just (l,r)) <|>
  (return Nothing)

parseTernaryExpr:: ExpressionParser st
parseTernaryExpr = do
  e <- parseExpression'
  e' <- parseTernaryExpr'
  case e' of
    Nothing -> return e
    Just (l,r) -> do p <- getPosition
                     return $ CondExpr p e l r
--}}}

-- Parsing assignment operations.
makeAssignExpr str constr = Infix parser AssocRight where
  parser:: CharParser st (ParsedExpression -> ParsedExpression -> ParsedExpression)
  parser = do
    pos <- getPosition
    reservedOp str
    return (AssignExpr pos constr)

assignTable:: [[Operator Char st ParsedExpression]]
assignTable = [
  [makeAssignExpr "=" OpAssign, makeAssignExpr "+=" OpAssignAdd, 
    makeAssignExpr "-=" OpAssignSub, makeAssignExpr "*=" OpAssignMul,
    makeAssignExpr "/=" OpAssignDiv, makeAssignExpr "%=" OpAssignMod,
    makeAssignExpr "<<=" OpAssignLShift, makeAssignExpr ">>=" OpAssignSpRShift,
    makeAssignExpr ">>>=" OpAssignZfRShift, makeAssignExpr "&=" OpAssignBAnd,
    makeAssignExpr "^=" OpAssignBXor, makeAssignExpr "|=" OpAssignBOr
  ]]


parseAssignExpr:: ExpressionParser st
parseAssignExpr = buildExpressionParser assignTable parseTernaryExpr  

parseExpression:: ExpressionParser st
parseExpression = parseAssignExpr

parseListExpr =
  liftM2 ListExpr getPosition (parseAssignExpr `sepBy1` comma)

--}}}

parseScript:: CharParser state (JavaScript SourcePos)
parseScript = do
  whiteSpace
  liftM2 Script getPosition (parseStatement `sepBy` whiteSpace)
  
parseJavaScriptFromFile :: MonadIO m => String -> m [Statement SourcePos]
parseJavaScriptFromFile filename = do
  chars <- liftIO $ readFile filename
  case parse parseScript filename chars of
    Left err               -> fail (show err)
    Right (Script _ stmts) -> return stmts


parseScriptFromString:: String -> String -> Either ParseError (JavaScript SourcePos)
parseScriptFromString src script = parse parseScript src script

emptyParsedJavaScript = 
  Script (error "Parser.emptyParsedJavaScript--no annotation") []
