{- This isn't a lexer in the sense that it provides a JavaScript token-stream.
 - This module provides character-parsers for various JavaScript tokens.
 -}
module BrownPLT.JavaScript.Lexer(lexeme,identifier,reserved,operator,reservedOp,charLiteral,
                        stringLiteral,natural,integer,float,naturalOrFloat,
                        decimal,hexadecimal,octal,symbol,whiteSpace,parens,
                        braces,brackets,squares,semi,comma,colon,dot,
                        identifierStart) where

import Prelude hiding (lex)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

identifierStart = (letter <|> oneOf "$_")

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
            
lex = T.makeTokenParser javascriptDef

-- everything but commaSep and semiSep
identifier = T.identifier	 lex
reserved = T.reserved	 lex
operator = T.operator	 lex
reservedOp = T.reservedOp lex	
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
whiteSpace = T.whiteSpace lex	
parens = T.parens	 lex
braces = T.braces	 lex
squares = T.squares lex	
semi = T.semi	 lex
comma = T.comma	 lex
colon = T.colon lex	
dot = T.dot lex
brackets = T.brackets lex
lexeme = T.lexeme lex
