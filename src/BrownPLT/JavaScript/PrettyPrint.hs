-- |Pretty-printing JavaScript.  This module doesn't export any names, but
-- importing it declares PrettyPrintable instances for JavaScript.Syntax.
module BrownPLT.JavaScript.PrettyPrint (stmt) where

import Text.PrettyPrint.HughesPJ
import BrownPLT.JavaScript.Syntax
import BrownPLT.JavaScript.Combinators
import BrownPLT.Common

--{{{ Helper functions for common printing patterns

-- Displays the statement in { ... }, unless it already is in a block.
inBlock:: (Statement a) -> Doc
inBlock s@(BlockStmt _ _) = stmt s
inBlock s                 = lbrace $+$ nest 2 (stmt s) $+$ rbrace

-- Displays the expression in ( ... ), unless it already is in parens.
inParens:: (Expression a) -> Doc
inParens e@(ParenExpr _ _) = expr e
inParens e                 = parens (expr e)

commaSep:: (PrettyPrintable a) => [a] -> Doc
commaSep = hsep.(punctuate comma).(map pp)

--}}}

instance PrettyPrintable (Id a) where
  pp (Id _ str) = text str

instance PrettyPrintable (ForInit a) where
  pp NoInit       = empty
  pp (VarInit vs) = text "var" <+> (cat $ punctuate comma $ (map varDecl vs))
  pp (ExprInit e) = expr e
  
instance PrettyPrintable (ForInInit a) where
  pp (ForInVar id)   = text "var" <+> pp id
  pp (ForInNoVar id) = pp id


caseClause :: CaseClause a -> Doc
caseClause (CaseClause _ e ss) =
  text "case" $+$ expr e <+> colon $$ (nest 2 (vcat (map stmt ss)))
caseClause (CaseDefault _ ss) =
  text "default:" $$ (nest 2 (vcat (map stmt ss)))

catchClause :: CatchClause a -> Doc
catchClause (CatchClause _ id s) = text "catch" <+> (parens.pp) id <+> inBlock s


varDecl :: VarDecl a -> Doc
varDecl (VarDecl _ id Nothing) = pp id
varDecl (VarDecl _ id (Just e)) = pp id <+> equals <+> expr e


stmt :: Statement a -> Doc
stmt s = case s of
  BlockStmt _ ss -> lbrace $+$ nest 2 (vcat (map stmt ss)) $+$ rbrace
  EmptyStmt _ -> semi
  ExprStmt _ e -> expr e <> semi
  IfSingleStmt _ test cons -> text "if" <+> inParens test $$ stmt cons
  IfStmt _ test cons alt ->
    text "if" <+> inParens test $$ stmt cons $$ text "else" <+> stmt alt
  SwitchStmt _ e cases ->
    text "switch" <+> inParens e $$ 
    braces (nest 2 (vcat (map caseClause cases)))
  WhileStmt _ test body -> text "while" <+> inParens test $$ (stmt body)
  ReturnStmt _ Nothing -> text "return" <> semi
  ReturnStmt _ (Just e) -> text "return" <+> expr e <> semi
  DoWhileStmt _ s e -> text "do" $$ (stmt s <+> text "while" <+> inParens e)
  BreakStmt _ Nothing ->  text "break;"
  BreakStmt _ (Just label) -> text "break" <+> pp label <> semi
  ContinueStmt _ Nothing -> text "continue;"
  ContinueStmt _ (Just label) -> text"continue" <+> pp label <> semi
  LabelledStmt _ label s -> pp label <> colon $$ stmt s
  ForInStmt p init e body -> 
    text "for" <+> parens (pp init <+> text "in" <+> expr e) $+$ stmt body
  ForStmt _ init incr test body ->
    text "for" <+> parens (pp init <> semi <+> mexpr incr <> semi <+> mexpr test)
      $$ (stmt body)
  TryStmt _ stmt catches finally ->
    text "try" $$ inBlock stmt $$ (vcat (map catchClause catches)) $$
    ppFinally where 
       ppFinally = case finally of
        Nothing -> empty
        Just stmt -> text "finally" <> inBlock stmt
  ThrowStmt _ e -> text "throw" <+> expr e <> semi
  WithStmt _ expr s ->  text "with" <+> inParens expr $$ stmt s
  VarDeclStmt _ decls ->
    text "var" <+> (cat $ punctuate comma (map varDecl decls))
  FunctionStmt _ name args s ->
    text "function" <+> pp name <> (parens.commaSep) args $$ inBlock s


prop :: Prop a -> Doc
prop p = case p of
  PropId _ id -> pp id
  PropString _ str -> doubleQuotes (text (jsEscape str))
  PropNum _ n -> text (show n)


--{{{ infix-operators

showInfix op =
  case op of
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

instance PrettyPrintable InfixOp where
  pp op = text (showInfix op)
--}}}

--{{{ prefix operators

showPrefix PrefixInc = "++"
showPrefix PrefixDec = "--"
showPrefix PrefixLNot = "!"
showPrefix PrefixBNot = "~"
showPrefix PrefixPlus = "+"
showPrefix PrefixMinus = "-"
showPrefix PrefixTypeof = "typeof"
showPrefix PrefixVoid = "void"
showPrefix PrefixDelete = "delete"

instance PrettyPrintable PrefixOp where
  pp op = text (showPrefix op)

--}}}

--{{{ postfix operators

instance PrettyPrintable PostfixOp where
  pp PostfixInc = text "++"
  pp PostfixDec = text "--"

--}}}

--{{{ assignment operators

showAssignOp OpAssign = "="
showAssignOp OpAssignAdd = "+="
showAssignOp OpAssignSub = "-="
showAssignOp OpAssignMul = "*="
showAssignOp OpAssignDiv = "/="
showAssignOp OpAssignMod = "%="
showAssignOp OpAssignLShift = "<<="
showAssignOp OpAssignSpRShift = ">>="
showAssignOp OpAssignZfRShift = ">>>="
showAssignOp OpAssignBAnd = "&="
showAssignOp OpAssignBXor = "^="
showAssignOp OpAssignBOr = "|="

instance PrettyPrintable AssignOp where
  pp = text.showAssignOp

--}}}

--{{{ expressions

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
  PrefixExpr _ op e' -> pp op <+> expr e'
  PostfixExpr _ op e' -> expr e' <+> pp op
  InfixExpr _ op left right -> expr left <+> pp op <+> expr right
  CondExpr _ test cons alt -> 
    expr test <+> text "?" <+> expr cons <+> colon <+> expr alt
  AssignExpr _ op l r ->  expr l <+> pp op <+> expr r
  ParenExpr _ e' ->  parens (expr e')
  ListExpr _ es ->  cat $ punctuate comma (map expr es)
  CallExpr _ f args -> 
    expr f <> (parens $ cat $ punctuate comma (map expr args))
  FuncExpr _ args body -> 
    text "function" <+> (parens.commaSep) args $$ inBlock body

mexpr :: Maybe (Expression a) -> Doc
mexpr Nothing = empty
mexpr (Just e) = expr e

instance PrettyPrintable (JavaScript a) where
  pp (Script _ ss) =
    vcat (map stmt ss)

--}}}

