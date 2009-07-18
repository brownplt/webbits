-- |Pretty-printing JavaScript.
module BrownPLT.JavaScript.PrettyPrint
  ( stmt
  , expr
  , javaScript
  , renderStatements
  , renderExpression
  ) where

import Text.PrettyPrint.HughesPJ
import BrownPLT.JavaScript.Syntax


renderStatements :: [Statement a] -> String
renderStatements ss = render (semiSep ss)


renderExpression :: Expression a -> String
renderExpression e = render (expr e)


-- Displays the statement in { ... }, unless it already is in a block.
inBlock:: (Statement a) -> Doc
inBlock s@(BlockStmt _ ss) = stmt s
inBlock s                 = lbrace $+$ nest 2 (stmt s) $+$ rbrace


-- Displays the expression in ( ... ), unless it already is in parens.
inParens:: (Expression a) -> Doc
inParens e@(ParenExpr _ _) = expr e
inParens e                 = parens (expr e)

semiSep :: [Statement a] -> Doc
semiSep ss = vcat $ map (\s -> stmt s <> semi) ss


pp (Id _ str) = text str


forInit :: ForInit a -> Doc
forInit t = case t of
  NoInit     -> empty
  VarInit vs -> text "var" <+> (cat $ punctuate comma $ (map varDecl vs))
  ExprInit e -> expr e
  

forInInit :: ForInInit a -> Doc  
forInInit t = case t of
  ForInVar id   -> text "var" <+> pp id
  ForInNoVar id -> pp id


caseClause :: CaseClause a -> Doc
caseClause (CaseClause _ e ss) =
  text "case" $+$ expr e <+> colon $$ (nest 2 (semiSep ss))
caseClause (CaseDefault _ ss) =
  text "default:" $$ (nest 2 (semiSep ss))


catchClause :: CatchClause a -> Doc
catchClause (CatchClause _ id s) = text "catch" <+> (parens.pp) id <+> inBlock s


varDecl :: VarDecl a -> Doc
varDecl (VarDecl _ id Nothing) = pp id
varDecl (VarDecl _ id (Just e)) = pp id <+> equals <+> expr e


stmt :: Statement a -> Doc
stmt s = case s of
  BlockStmt _ ss -> lbrace $+$ (nest 2 (semiSep ss)) $$ rbrace
  EmptyStmt _ -> semi
  ExprStmt _ e -> expr e
  IfSingleStmt _ test cons -> text "if" <+> inParens test $$ stmt cons
  IfStmt _ test cons alt ->
    text "if" <+> inParens test $$ stmt cons $$ text "else" <+> stmt alt
  SwitchStmt _ e cases ->
    text "switch" <+> inParens e $$ 
    braces (nest 2 (vcat (map caseClause cases)))
  WhileStmt _ test body -> text "while" <+> inParens test $$ (stmt body)
  ReturnStmt _ Nothing -> text "return"
  ReturnStmt _ (Just e) -> text "return" <+> expr e
  DoWhileStmt _ s e -> text "do" $$ (stmt s <+> text "while" <+> inParens e)
  BreakStmt _ Nothing ->  text "break"
  BreakStmt _ (Just label) -> text "break" <+> pp label
  ContinueStmt _ Nothing -> text "continue"
  ContinueStmt _ (Just label) -> text"continue" <+> pp label
  LabelledStmt _ label s -> pp label <> colon $$ stmt s
  ForInStmt p init e body -> 
    text "for" <+> 
    parens (forInInit init <+> text "in" <+> expr e) $+$ stmt body
  ForStmt _ init incr test body ->
    text "for" <+> 
    parens (forInit init <> semi <+> mexpr incr <> semi <+> mexpr test) $$ 
    stmt body
  TryStmt _ stmt catches finally ->
    text "try" $$ inBlock stmt $$ (vcat (map catchClause catches)) $$
    ppFinally where 
       ppFinally = case finally of
        Nothing -> empty
        Just stmt -> text "finally" <> inBlock stmt
  ThrowStmt _ e -> text "throw" <+> expr e
  WithStmt _ expr s ->  text "with" <+> inParens expr $$ stmt s
  VarDeclStmt _ decls ->
    text "var" <+> (cat $ punctuate comma (map varDecl decls))
  FunctionStmt _ name args s ->
    text "function" <+> pp name <> 
    (parens $ cat $ punctuate comma (map pp args)) $$ 
    inBlock s


prop :: Prop a -> Doc
prop p = case p of
  PropId _ id -> pp id
  PropString _ str -> doubleQuotes (text (jsEscape str))
  PropNum _ n -> text (show n)


infixOp op = text $ case op of
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%" 
  OpAdd -> "+" 
  OpSub -> "-"
  OpLShift -> "<<"
  OpSpRShift -> ">>"
  OpZfRShift -> ">>>"
  OpLT -> "<"
  OpLEq -> "<="
  OpGT -> ">"
  OpGEq -> ">="
  OpIn -> "in"
  OpInstanceof -> "instanceof"
  OpEq -> "=="
  OpNEq -> "!="
  OpStrictEq -> "==="
  OpStrictNEq -> "!=="
  OpBAnd -> "&"
  OpBXor -> "^"
  OpBOr -> "|"
  OpLAnd -> "&&"
  OpLOr -> "||"


prefixOp op = text $ case op of
  PrefixLNot -> "!"
  PrefixBNot -> "~"
  PrefixPlus -> "+"
  PrefixMinus -> "-"
  PrefixTypeof -> "typeof"
  PrefixVoid -> "void"
  PrefixDelete -> "delete"


assignOp op = text $ case op of
  OpAssign -> "="
  OpAssignAdd -> "+="
  OpAssignSub -> "-="
  OpAssignMul -> "*="
  OpAssignDiv -> "/="
  OpAssignMod -> "%="
  OpAssignLShift -> "<<="
  OpAssignSpRShift -> ">>="
  OpAssignZfRShift -> ">>>="
  OpAssignBAnd -> "&="
  OpAssignBXor -> "^="
  OpAssignBOr -> "|="


-- Based on:
--   http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Guide:Literals
jsEscape:: String -> String
jsEscape "" = ""
jsEscape (ch:chs) = (sel ch) ++ jsEscape chs where
    sel '\b' = "\\b"
    sel '\f' = "\\f"
    sel '\n' = "\\n"
    sel '\r' = "\\r"
    sel '\t' = "\\t"
    sel '\v' = "\\v"
    sel '\'' = "\\'"
    sel '\"' = "\\\""
    sel '\\' = "\\\\"
    sel x    = [x]
    -- We don't have to do anything about \X, \x and \u escape sequences.


lvalue :: LValue a -> Doc
lvalue (LVar _ x) = text x
lvalue (LDot _ e x) = expr e <> text "." <> text x
lvalue (LBracket _ e1 e2) = expr e1 <> brackets (expr e2)

 
expr :: Expression a -> Doc
expr e = case e of 
  StringLit _ str ->  doubleQuotes (text (jsEscape str))
  RegexpLit _ re global ci -> 
    text "/" <> text re <> text "/" <> g <> i where
      g = if global then text "g" else empty
      i = if ci then text "i" else empty
  NumLit _ n ->  text (show n)
  IntLit _ n ->  text (show n)
  BoolLit _ True ->  text "true"
  BoolLit _ False ->  text "false"
  NullLit _ ->  text "null"
  ArrayLit _ es ->  brackets $ cat $ punctuate comma (map expr es)
  ObjectLit _ xs ->  
    braces (hsep (punctuate comma (map pp' xs))) where
      pp' (n,v) =  prop n <> colon <+> expr v
  ThisRef _ ->  text "this"
  VarRef _ id ->  pp id
  DotRef _ e' id -> expr e' <> text "." <> pp id
  BracketRef _ container key -> expr container <> brackets (expr key)
  NewExpr _ constr args -> 
    text "new" <+> expr constr <> 
    (parens $ cat $ punctuate comma (map expr args))
  PrefixExpr _ op e' -> prefixOp op <+> expr e'
  InfixExpr _ op left right -> expr left <+> infixOp op <+> expr right
  CondExpr _ test cons alt -> 
    expr test <+> text "?" <+> expr cons <+> colon <+> expr alt
  AssignExpr _ op l r ->  lvalue l <+> assignOp op <+> expr r
  UnaryAssignExpr _ op e' -> case op of
    PrefixInc -> text "++" <> lvalue e'
    PrefixDec -> text "--" <> lvalue e'
    PostfixInc -> lvalue e' <> text "++"
    PostfixDec -> lvalue e' <> text "--"
  ParenExpr _ e' ->  parens (expr e')
  ListExpr _ es ->  cat $ punctuate comma (map expr es)
  CallExpr _ f args -> 
    expr f <> (parens $ cat $ punctuate comma (map expr args))
  FuncExpr _ args body -> 
    text "function" <+> 
    (parens $ cat $ punctuate comma (map pp args)) $$ 
    inBlock body


mexpr :: Maybe (Expression a) -> Doc
mexpr Nothing = empty
mexpr (Just e) = expr e

javaScript :: JavaScript a -> Doc
javaScript (Script _ ss) = semiSep ss
