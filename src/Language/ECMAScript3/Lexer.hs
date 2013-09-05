-- | This isn't a lexer in the sense that it provides a JavaScript
-- token-stream. This module provides character-parsers for various
-- JavaScript tokens.

module Language.ECMAScript3.Lexer(lexeme,identifier,reserved,operator,reservedOp,charLiteral,
                        stringLiteral,natural,integer,float,naturalOrFloat,
                        decimal,hexadecimal,octal,symbol,whiteSpace,parens,
                        braces,brackets,squares,semi,comma,colon,dot,
                        identifierStart) where

import Prelude hiding (lex)
import Text.Parsec
import qualified Text.Parsec.Token as T
import Language.ECMAScript3.Parser.State
import Language.ECMAScript3.Parser.Type
import Control.Monad.Identity

identifierStart :: Stream s Identity Char => Parser s Char
identifierStart = letter <|> oneOf "$_"

javascriptDef :: Stream s Identity Char =>T.GenLanguageDef s ParserState Identity
javascriptDef =
  T.LanguageDef "/*"
                "*/"
                "//"
                False -- no nested comments
                identifierStart
                (alphaNum <|> oneOf "$_") -- identifier rest
                (oneOf "{}<>()~.,?:|&^=!+-*/%!") -- operator start
                (oneOf "=<>|&+") -- operator rest
                ["break", "case", "catch", "const", "continue", "debugger", 
                 "default", "delete", "do", "else", "enum", "false", "finally",
                 "for", "function", "if", "instanceof", "in", "let", "new", 
                 "null", "return", "switch", "this", "throw", "true", "try", 
                 "typeof", "var", "void", "while", "with"]
                ["|=", "^=", "&=", "<<=", ">>=", ">>>=", "+=", "-=", "*=", "/=", 
                 "%=", "=", ";", ",", "?", ":", "||", "&&", "|", "^", "&", 
                 "===", "==", "=", "!==", "!=", "<<", "<=", "<", ">>>", ">>", 
                 ">=", ">", "++", "--", "+", "-", "*", "/", "%", "!", "~", ".", 
                 "[", "]", "{", "}", "(", ")","</","instanceof"]
                 True -- case-sensitive
            
lex :: Stream s Identity Char => T.GenTokenParser s ParserState Identity
lex = T.makeTokenParser javascriptDef

-- everything but commaSep and semiSep
identifier :: Stream s Identity Char => Parser s String
identifier = T.identifier	 lex
reserved :: Stream s Identity Char => String -> Parser s ()
reserved = T.reserved	 lex
operator :: Stream s Identity Char => Parser s String
operator = T.operator	 lex
reservedOp :: Stream s Identity Char => String -> Parser s ()
reservedOp = T.reservedOp lex	
charLiteral :: Stream s Identity Char => Parser s Char
charLiteral = T.charLiteral lex	
stringLiteral :: Stream s Identity Char => Parser s String
stringLiteral = T.stringLiteral lex
natural :: Stream s Identity Char => Parser s Integer
natural = T.natural lex	
integer :: Stream s Identity Char => Parser s Integer
integer = T.integer lex	
float :: Stream s Identity Char => Parser s Double
float = T.float lex
naturalOrFloat :: Stream s Identity Char => Parser s (Either Integer Double)
naturalOrFloat = T.naturalOrFloat lex
decimal :: Stream s Identity Char => Parser s Integer
decimal = T.decimal lex	
hexadecimal :: Stream s Identity Char => Parser s Integer
hexadecimal = T.hexadecimal lex	
octal :: Stream s Identity Char => Parser s Integer
octal = T.octal lex
symbol :: Stream s Identity Char => String -> Parser s String
symbol = T.symbol lex
whiteSpace :: Stream s Identity Char => Parser s ()
whiteSpace = T.whiteSpace lex	
parens :: Stream s Identity Char => Parser s a -> Parser s a
parens = T.parens	 lex
braces :: Stream s Identity Char => Parser s a -> Parser s a
braces = T.braces	 lex
squares :: Stream s Identity Char => Parser s a -> Parser s a
squares = T.squares lex	
semi :: Stream s Identity Char => Parser s String
semi = T.semi	 lex
comma :: Stream s Identity Char => Parser s String
comma = T.comma	 lex
colon :: Stream s Identity Char => Parser s String
colon = T.colon lex
dot :: Stream s Identity Char => Parser s String
dot = T.dot lex
brackets :: Stream s Identity Char => Parser s a -> Parser s a
brackets = T.brackets lex
lexeme :: Stream s Identity Char => Parser s a -> Parser s a
lexeme = T.lexeme lex
