module WebBits.JavaScript.Lexer(lexeme,identifier,reserved,operator,reservedOp,charLiteral,
                        stringLiteral,natural,integer,float,naturalOrFloat,
                        decimal,hexadecimal,octal,symbol,whiteSpace,parens,
                        braces,brackets,squares,semi,comma,colon,dot,
                        identifierStart) where

import Prelude hiding (lex)
import Control.Monad
import qualified Data.List as L
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Char
identifierStart = (letter <|> oneOf "$_")


lineComment :: CharParser st String
lineComment = do
  try $ string "//"
  manyTill anyChar (char '\n' <|> (eof >> return ' '))

blockComment :: CharParser st String
blockComment = do
  try $ string "/*"
  manyTill anyChar (try $ string "*/")

comment :: CharParser st String
comment = lineComment <|> blockComment

comments :: CharParser st [String]
comments = do
  spaces
  (liftM2 (:) comment comments) <|> (return [])

-- |Parse whitespace and returns the last comment in the block of whitespace,
-- if any.
whiteSpace :: CharParser st (Maybe String)
whiteSpace = do
  r <- comments
  case r of
    [] -> return Nothing
    xs -> return (Just $ L.last r)

reservedWords :: [String]
reservedWords = 
  ["break", "case", "catch", "const", "continue", "debugger", 
   "default", "delete", "do", "else", "enum", "false", "finally",
   "for", "function", "if", "instanceof", "in", "let", "new", 
   "null", "return", "switch", "this", "throw", "true", "try", 
   "typeof", "var", "void", "while", "with"]
-- reserved

operatorRest :: CharParser st Char
operatorRest = oneOf "=<>|&+"

identifierRest :: CharParser st Char
identifierRest = alphaNum <|> oneOf "$_" -- identifier rest

reserved :: String -> CharParser st (String,Maybe String)
reserved word = do
  try $ string word >> notFollowedBy identifierRest
  c <- whiteSpace
  return (word,c)

reservedOp :: String -> CharParser st (String,Maybe String)
reservedOp op = do
  try $ string op >> notFollowedBy operatorRest
  c <- whiteSpace
  return (op,c)

javascriptDef =
  T.LanguageDef "/*"
                "*/"
                "//"
                False -- no nested comments
                {- Adapted from syntax/regexps.ss in Dave's code. -}
                identifierStart
                identifierRest
                (oneOf "{}<>()~.,?:|&^=!+-*/%!") -- operator start
                operatorRest
                reservedWords
                ["|=", "^=", "&=", "<<=", ">>=", ">>>=", "+=", "-=", "*=", "/=", 
                 "%=", "=", ";", ",", "?", ":", "||", "&&", "|", "^", "&", 
                 "===", "==", "=", "!==", "!=", "<<", "<=", "<", ">>>", ">>", 
                 ">=", ">", "++", "--", "+", "-", "*", "/", "%", "!", "~", ".", 
                 "[", "]", "{", "}", "(", ")","</","instanceof"]
                 True -- case-sensitive
            
lex = T.makeTokenParser javascriptDef

-- everything but commaSep and semiSep
identifier = T.identifier	 lex
operator = T.operator	 lex
charLiteral = T.charLiteral lex	
stringLiteral = T.stringLiteral lex	
natural = T.natural lex	
integer = T.integer lex	
float = T.float lex	
naturalOrFloat = T.naturalOrFloat lex	
decimal = T.decimal lex	
hexadecimal = T.hexadecimal lex	
octal = T.octal lex	
symbol = T.symbol lex	
-- whiteSpace = T.whiteSpace lex	
parens = T.parens	 lex
braces = T.braces	 lex
squares = T.squares lex	
semi = T.semi	 lex
comma = T.comma	 lex
colon = T.colon lex	
dot = T.dot lex
brackets = T.brackets lex
lexeme = T.lexeme lex
