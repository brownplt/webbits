-- |A structure-recovering parser for malformed documents.
--
-- Copyright 2007-2008 Arjun Guha.
-- Based on HtmlPrag 0.16 Copyright (C) 2003 - 2005 Neil W. Van Dyke.  
--
-- This program is Free Software; you can redistribute it and/or modify it under
-- the terms of the GNU Lesser General Public License as published by the Free
-- Software Foundation; either version 2.1 of the License, or (at your option)
-- any later version.  This program is distributed in the hope that it will be
-- useful, but without any warranty; without even the implied warranty of
-- merchantability or fitness for a particular purpose.  See 
-- <http://www.gnu.org/copyleft/lesser.html> for details.  For other license
-- options and consulting, contact the author.
module WebBits.Html.PermissiveParser
  ( html
  , parseHtmlFromFile
  , parseHtmlFromString
  -- tokenizer is exported primarily for testing
  , tokens
  , Token
  ) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (token,tokens)
import qualified Text.ParserCombinators.Parsec as Parsec
import Data.Char (toLower)
import Data.List (intersperse)

import qualified WebBits.Html.Syntax as Html
import WebBits.Html.Syntax (HtmlId,Attribute,Script(..))

type ParsedHtml s = Html.Html SourcePos s
type ParsedAttribute s = Html.Attribute SourcePos s

--------------------------------------------------------------------------------
-- Parsers generate warnings

data Warning = StringWarning SourcePos String

instance Show Warning where
  show (StringWarning p s) = "Warning parsing HTML: " ++ s
  showList [] s = s
  showList (x:xs) s = show x ++ ('\n':showList xs s)

warn:: String -> GenParser tok [Warning] ()
warn s = do
  p <- getPosition
  updateState ((StringWarning p s):)

noWarnings:: [Warning]
noWarnings = []
  

-- A structure-recovering parser for malformed documents, derived from
-- Neil W. Van Dyke's htmlprag library for PLT Scheme

-- The elements in the list can legally enclose the 1st element of the pair.
parentConstraints:: [(HtmlId,[HtmlId])]
parentConstraints =
  [("area",["map"]),
   ("body",["html"]),
   ("caption", ["table"]),
   ("colgroup", ["table"]),
   ("dd", ["dl"]),
   ("dt", ["dl"]),
   ("frame", ["frameset"]),
   ("head", ["html"]),
   ("isindex", ["head"]),
   ("li", ["dir", "menu", "ol", "ul"]),
   ("meta", ["head"]),
   ("noframes", ["frameset"]),
   ("option", ["select"]),
   ("p", ["body", "td", "th"]),
   ("param", ["applet"]),
   ("tbody", ["table"]),
   ("td", ["tr"]),
   ("th", ["tr"]),
   ("thead", ["table"]),
   ("title", ["head"]),
   ("tr", ["table", "tbody", "thead"])]

-- |List of HTML elements that are empty.
emptyElements:: [HtmlId]
emptyElements = 
  ["area", "base", "br", "frame", "hr", "img", "input", "isindex", "keygen", 
   "link", "meta", "object", "param", "spacer", "wbr"]

isLegalChildOf:: HtmlId -> HtmlId -> Bool
isLegalChildOf child parent =
  case lookup child parentConstraints of
    Nothing             -> True
    (Just legalParents) -> parent `elem` legalParents
    
isEmptyElement:: HtmlId -> Bool
isEmptyElement element = element `elem` emptyElements

--}}}

--------------------------------------------------------------------------------
-- Parses an HTML file into a stream of tokens.

-- The auxillary parsing functions return values of this type.
data Script s  => Token s
  = Text SourcePos String
  | EntityToken SourcePos String
  | EntityInt SourcePos Int
  | Tag SourcePos HtmlId [Attribute SourcePos s] Bool {-closed?-}
  | Script SourcePos s
  | Inline SourcePos s String
  | EndTag SourcePos HtmlId
  | Comment SourcePos String
  | DoctypeToken SourcePos String String String (Maybe String)


token:: Script s => Bool -> [Attribute SourcePos s] 
     -> CharParser [Warning] (Token s)
token expectedScript prevAttrs = case expectedScript of
  True  -> (liftM2 Script getPosition (parseScriptBlock prevAttrs)) 
             <?> "expected a script after a <script> tag"
  False -> doctype <|> comment <|> tag <|> endTag <|> inlineScript <|> entity 
             <|> text

tokens:: Script s => CharParser [Warning] [Token s]
tokens = (eof >> return []) <|> tokens' where
  tokens' = do 
    t <- token False []
    case t of
      (Tag _ "script" attrs False) -> do s <- token True attrs
                                         ts <- tokens
                                         return (t:s:ts)
      _ -> tokens >>= return.(t:)

--------------------------------------------------------------------------------
-- Parsers for various components of HTML

qname:: CharParser st String
qname = do
  x <- letter
  xs <- many (noneOf "/*=<>\"\'  \v\f\t\r\n")
  return (x:xs)

-- |We do not permit spaces between the hyphens and the right-angle in the
-- terminating '-->'.
comment:: Script s => CharParser st (Token s) 
comment =
  let notDoubleHyphen = try (char '-' >> notFollowedBy (char '-') >> return '-')
      notHyphen = noneOf "-"
    in do try (string "<!--")
          pos <- getPosition
          msg <- many (notHyphen <|> notDoubleHyphen)
          string "-->"
          return (Comment pos msg)

endTag:: Script s => CharParser [Warning] (Token s)
endTag  = do
  pos <- getPosition
  string "</"
  name <- qname <?> "closing tag\'s name"
  junk <- manyTill anyChar (char '>') -- permits junk between tag name and '>'
  unless (null junk) (warn $ "extra characters: " ++ junk ++ 
                             "; assuming tag name is " ++ name)
  return (EndTag pos name)
  
doctype:: Script s => CharParser [Warning] (Token s)
doctype = do
  p <- getPosition
  try (string "<!DOCTYPE")
  spaces
  top <- qname <?> "top-element name"
  spaces
  avail <- qname <?> "availability"
  spaces
  regEtc <- quotedString <?> "registration, etc."
  spaces
  uri <- optionMaybe quotedString
  spaces
  string ">"
  return (DoctypeToken p top avail regEtc uri)
  
entity:: Script s => CharParser [Warning] (Token s)
entity = do
  char '&'
  pos <- getPosition
  name <- many alphaNum <|> (char '#' >> many1 digit)
  when (null name) (warn "no identifer or number after &")
  (char ';' >> return ()) <|> (warn "expected semi-colon after entity") 
  return (EntityToken pos name)
  
notScript:: CharParser a Char
notScript = try (char '{' >> notFollowedBy (char '!') >> return '{')

-- Parses raw text, upto an opening angle-bracket ('<').
text:: Script s => CharParser st (Token s)
text = do
  pos  <- getPosition
  cs <- many1 (noneOf "<{" <|> notScript) -- Doesn't consume the terminating angle bracket.
  return (Text pos cs)

-- Strings that are either double-quoted or single-quoted.  Note that HTML
-- strings contain no escape sequences.
quotedString:: CharParser a String
quotedString =
  (char '"' >> manyTill anyChar (char '"')) <|>
  (char '\'' >> manyTill anyChar (char '\'')) <?>
  "quoted string (double-quotes or single quotes)"

-- Parses text to the right of a triple-stick in an inline expression.  We have
-- to ensure that if we read a `!,' it isn't immediately followed by `}.'
initText =
  let notEnd = try (char '!' >> notFollowedBy (char '}') >> return '!')
    in many1 (notEnd <|> noneOf "!")

scriptValue:: Script s => CharParser a (s,String)
scriptValue =
  case parseAttributeScript of
    Nothing -> fail "attribute-script parser not defined"
    (Just parser) -> do string "{!"
                        script <- parser
                        init <- (string "!}" >> return "")  <|> 
                                (string "|||" >> initText >>= 
                                 (\s -> string "!}" >> return s))
                        return (script,init)

number:: CharParser a String
number = many1 digit

-- 
nonquotedAttribute:: CharParser [Warning]  String
nonquotedAttribute = do
  x <- alphaNum <|> oneOf "_"
  xs <- many (noneOf "/*=<>\"\'  \v\f\t\r\n")
  warn $ "non-quoted attribute value: " ++ (x:xs)
  return (x:xs)


attribute:: Script s => CharParser [Warning] (Html.Attribute SourcePos s)
attribute = do
  pos  <- getPosition
  name <- qname <?> "attribute name"
  spaces
  value <- (do char '='
               spaces
               (liftM Right scriptValue)
                 <|> (liftM Left (quotedString <|> nonquotedAttribute)) 
                 <?> "attribute value")
           <|> (return $ Left "") -- Unspecified values are empty values.
  case value of
    (Left v)      -> return $ Html.Attribute name v pos
    (Right (s,d)) -> return $ Html.AttributeExpr pos name s d

-- Takes the name of the immediately-enclosing parent tag as an argument.
tag:: Script s => CharParser [Warning] (Token s)
tag = do
  try (char '<' >> notFollowedBy (char '/'))
  pos <- getPosition
  name <- qname <?>  "opening tag\'s name"
  spaces
  attributes <- (attribute `sepEndBy` spaces)
  (char '>' >> return (Tag pos name attributes False))
    <|> (string "/>" >> return (Tag pos name attributes True))
    <?> "end of tag (i.e. \">\")"


-- Parses an inline (curly-banged) script, if an inline-script parser has been
-- specified (i.e. not Nothing).  Two-character lookahead for the ``{!,'' after
-- which control is passed to the inline script parser.  Once control returns,
-- the parser expects to see the closing ``!}.''
inlineScript:: Script s => CharParser a (Token s)
inlineScript =
  case parseInlineScript of
    Nothing -> fail "no inline script parser specified." -- TODO: appropriate?
    (Just parser) -> do string "{!" <?> "{! script !}"
                        pos <- getPosition
                        script <- parser
                        spaces
                        init <- (string "!}" >> return "") <|>
                                (string "|||" >> initText >>= 
                                 (\s -> string "!}" >> return s))
                        return $ Inline pos script init


-- Parses a stream of tokens, with a script parser, s and returns values of 
-- type a.
type TokenParser s a = GenParser (Token s) [Warning] a

instance Script s => Show (Token s) where
  show = tokenShow

tokenShow token = case token of
  (Text _ s)        -> s
  (EntityToken _ s) -> "&" ++ s ++ ";"
  (EntityInt _ n)   -> "&#" ++ show n ++ ";"
  (Tag _ id attrs closed) -> 
    "<" ++ id ++ " ... " ++ closing where
      closing = if closed then "/>" else ">"
  (Script _ s)  -> "/* script body omitted */"
  (Inline _ s _)-> "{! /* script */ !}"
  (EndTag _ id) -> "</" ++ show id ++ ">"
  (Comment _ s) -> "<!-- " ++ show s ++ " -->"
  (DoctypeToken _ top avail desc Nothing) ->
    "<!DOCTYPE " ++ top ++ " " ++ avail ++ " " ++ show desc ++ ">"
  (DoctypeToken _ top avail desc (Just uri)) ->
    "<!DOCTYPE " ++ top ++ " " ++ avail ++ " " ++ show desc ++ " " 
      ++ show uri ++ ">" 

tokenPos tok = case tok of
  (Text p _) -> p
  (EntityToken p _) -> p
  (EntityInt p _) -> p
  (Tag p _ _ _) -> p
  (Script p _) -> p
  (Inline p _ _) -> p
  (EndTag p _) -> p
  (Comment p _) -> p
  (DoctypeToken p _ _ _ _) -> p
    
textToken :: Script s => TokenParser s (ParsedHtml s)
textToken = Parsec.token tokenShow tokenPos $ \t -> case t of
  Text _ s -> Just (Html.Text s (tokenPos t))
  otherwise -> Nothing
             
entityToken :: Script s => TokenParser s (ParsedHtml s)
entityToken =
  Parsec.token tokenShow tokenPos
    (\t -> case t of
             (EntityToken p s) -> Just (Html.Text ("&" ++ s ++ ";") p)
             (EntityInt p n) -> Just (Html.Text ("&#" ++ show n ++ ";") p)
             otherwise -> Nothing)
           
commentToken :: Script s => TokenParser s (ParsedHtml s)
commentToken = 
  Parsec.token tokenShow tokenPos
    (\t -> case t of
             (Comment _ s) -> Just (Html.Comment s (tokenPos t))
             otherwise -> Nothing)

scriptToken :: Script s => TokenParser s (ParsedHtml s) 
scriptToken = 
  Parsec.token tokenShow tokenPos
    (\t -> case t of
             (Script p s) -> Just (Html.Script s p)
             otherwise    -> Nothing)

inlineToken :: Script s => TokenParser s (ParsedHtml s)
inlineToken = 
  Parsec.token tokenShow tokenPos
    (\t -> case t of
             (Inline p s d) -> Just (Html.InlineScript s p d)
             otherwise    -> Nothing)
             
endToken :: Script s => TokenParser s HtmlId
endToken = 
  Parsec.token tokenShow tokenPos
    (\t -> case t of
             (EndTag p s) -> Just s
             otherwise    -> Nothing)

tagToken :: Script s => TokenParser s (Token s)
tagToken = 
  Parsec.token tokenShow tokenPos
  (\t -> case t of
           (Tag p id attrs closed) -> Just t
           otherwise               -> Nothing)

doctypeToken :: Script s => TokenParser s (Token s)
doctypeToken = Parsec.token tokenShow tokenPos $ \t -> case t of
  DoctypeToken _ _ _ _ _ -> Just t
  otherwise -> Nothing

--------------------------------------------------------------------------------
-- HTML fragments

-- |An HTML fragment represents an HTML element with a sequence of children.
data HtmlFragment s = Fragment {
  fragmentPosition   :: SourcePos,
  fragmentName       :: HtmlId,
  fragmentAttributes :: [ParsedAttribute s],
  fragmentChildren   :: [ParsedHtml s]
}

closeFragment:: HtmlFragment s -> ParsedHtml s
closeFragment (Fragment p htmlId attrs children) =  
  Html.Element htmlId attrs (reverse children) p

appendChildToFragment child fragment =
  fragment { fragmentChildren = child : (fragmentChildren fragment) }
             
atomic :: Script s => TokenParser s (ParsedHtml s)
atomic =
  textToken <|> commentToken <|> scriptToken <|> inlineToken <|> entityToken


maybeClose :: Script s => [HtmlFragment s] -> TokenParser s [HtmlFragment s] 
maybeClose es = do
  htmlId <- endToken
  let close [] = do 
        warn $ "unmatched closing tag \"" ++ htmlId ++ "\"; ignoring"
        return es
      close (e:e2:es) | fragmentName e == htmlId = do
        return $ (appendChildToFragment (closeFragment e) e2) : es
      close (e:e2:es) | otherwise = 
        let e2' = appendChildToFragment (closeFragment e) e2
          in close (e2':es)
      close [e] | fragmentName e == htmlId = do
        return [e]
      close [e] | otherwise = do
        warn $ "unmatched closing tag \"" ++ htmlId ++ "\"; ignoring"
        return [e]
    in close es

open :: Script s => [HtmlFragment s] -> TokenParser s [HtmlFragment s]
open [] = fail "PermissiveParser.open: invalid state (1)"
open (e:es) = do
  (Tag p id attrs isClosed) <- tagToken
  case isClosed of
    True -> return $ (appendChildToFragment (Html.Element id attrs [] p) e) : es
    False ->
      case isEmptyElement id of
        True -> do warn $ "empty-element, \"" ++ id 
                            ++ "\" was not immediately closed"
                   return $ (appendChildToFragment (Html.Element id attrs [] p)
                               e) : es
        False -> do
          let fragment = Fragment p id attrs []
              -- If we reach the root, simply make 'fragment' a child.
          let insert [root] = [fragment,root]
              -- The <html> element is the only child of the root.  This
              -- catches all other elements and makes them children of <html>.
              insert [html,root] = [fragment,html,root]
              insert (e1:e2:es) | id `isLegalChildOf` (fragmentName e1) =
                fragment:e1:e2:es
              insert (e1:e2:es) | otherwise =
                insert $ (appendChildToFragment (closeFragment e1) e2):es 
              insert [] = fail "PermissiveParser.open: invalid state (2)"
          return $ insert (e:es)

structured es = do
  maybeClose es <|> open es

html' :: Script s => [HtmlFragment s] -> TokenParser s [HtmlFragment s]
html' [] = fail "PermissiveParser.html': invalid state (1)"
html' (e:es) =
  (do a <- atomic
      html' $ (appendChildToFragment a e):es) <|>
  (structured (e:es) >>= html') <|>
  (return (e:es))

parseRoot = Fragment (error "parseRoot: position is meaningless") 
                     "DOCROOT"  (error "parseRoot: attributes are meaningless")
                     []

html :: Script s => CharParser [Warning] (Html.Html SourcePos s)
html = do
  let parser = do
        -- optional doctypeToken -- ignored
        fragments <- html' [parseRoot]
        case fragments of
          [fragment] -> case closeFragment fragment of 
                          Html.Element _ _ (c:_) _ -> do
                            ws <- getState
                            return (ws,head [c])
                          otherwise -> fail "root element not found"
          otherwise -> fail "no root / multiple roots"
  toks <- tokens -- a stream of HTML entities
  pos <- Parsec.getPosition
  warnings <- Parsec.getState
  case Parsec.runParser parser warnings (Parsec.sourceName pos) toks of
    Left e -> fail  $ "unparsable HTML: " ++ (show e)
    Right (ws,html) -> Parsec.setState ws >> return html 

htmlWithWarnings :: Script s 
                 => CharParser [Warning] (Html.Html SourcePos s,[Warning])
htmlWithWarnings = do
  h <- html
  ws <- getState
  return (h,ws)

parseHtmlFromString :: Script s => String -> String -> Either Parsec.ParseError (Html.Html SourcePos s,[Warning])
parseHtmlFromString sourceName sourceText = 
  case Parsec.runParser htmlWithWarnings noWarnings sourceName sourceText of
    Left err -> Left err
    Right (html,ws) -> Right (html,ws) 

parseHtmlFromFile :: Script s 
                  => String 
                  -> IO (Either Parsec.ParseError 
                                (Html.Html SourcePos s,[Warning]))
parseHtmlFromFile filename = do
  chars <- readFile filename
  case Parsec.runParser htmlWithWarnings noWarnings filename chars of
    Left err -> return (Left err)
    Right (html,ws) -> return $ Right (html,ws)
