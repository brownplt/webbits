-- |Pretty-printing JavaScript.  This module doesn't export any names, but
-- importing it declares PrettyPrintable instances for JavaScript.Syntax.
module WebBits.JavaScript.PrettyPrint() where

import Text.PrettyPrint.HughesPJ
import WebBits.JavaScript.Syntax
import WebBits.JavaScript.Combinators
import WebBits.Common

--{{{ Helper functions for common printing patterns

-- Displays the statement in { ... }, unless it already is in a block.
inBlock:: (Statement a) -> Doc
inBlock stmt@(BlockStmt _ _) = pp stmt
inBlock stmt                 = text "{" $+$ pp stmt $+$ text "}"

-- Displays the expression in ( ... ), unless it already is in parens.
inParens:: (Expression a) -> Doc
inParens expr@(ParenExpr _ _) = pp expr
inParens expr                 = parens (pp expr)

commaSep:: (PrettyPrintable a) => [a] -> Doc
commaSep = hsep.(punctuate comma).(map pp)

--}}}

instance PrettyPrintable (Id a) where
  pp (Id _ str) = text str

instance PrettyPrintable (ForInit a) where
  pp NoInit         = empty
  pp (VarInit vs) = text "var" <+> commaSep vs 
  pp (ExprInit e) = pp e
  
instance PrettyPrintable (ForInInit a) where
  pp (ForInVar id)   = text "var" <+> pp id
  pp (ForInNoVar id) = pp id

--{{{ Pretty-printing statements

instance PrettyPrintable (CaseClause a) where
  pp (CaseClause _ expr stmts) =
    pp expr <+> colon $$ (nest 2 (vcat (map pp stmts)))
  pp (CaseDefault _ stmts) =
    text "default:" $$ (nest 2 (vcat (map pp stmts)))

instance PrettyPrintable (CatchClause a) where
  pp (CatchClause _ id stmt) =
    text "catch" <+> (parens.pp) id <+> pp stmt


instance PrettyPrintable (VarDecl a) where
  pp (VarDecl _ id Nothing) =
    pp id
  pp (VarDecl _ id (Just expr)) =
    pp id <+> equals <+> pp expr

instance PrettyPrintable (Statement a) where
  pp (BlockStmt _ stmts) =
    text "{" $+$ nest 2 (vcat (map pp stmts)) $+$ text "}"
  pp (EmptyStmt _) =
    semi
  pp (ExprStmt _ expr) =
    pp expr <> semi
  pp (IfSingleStmt _ test cons) =
    text "if" <+> inParens test $$ (nest 2 (pp cons))
  pp (IfStmt _ test cons alt) =
    text "if" <+> inParens test $$ (nest 2 $ pp cons) $$ text "else"
      $$ (nest 2 $ pp alt)
  pp (SwitchStmt _ expr cases) =
    text "switch" <+> inParens expr $$ braces (nest 2 (vcat (map pp cases)))
  pp (WhileStmt _ test body) =
    text "while" <+> inParens test $$ (nest 2 (pp body))
  pp (ReturnStmt _ expr) =
    text "return" <+> pp expr <> semi
  pp (DoWhileStmt _ stmt expr) =
    text "do" $$ (nest 2 (pp stmt <+> inParens expr))
  pp (BreakStmt _ Nothing) =
    text "break;"
  pp (BreakStmt _ (Just label)) =
    text "break" <+> pp label <> semi
  pp (ContinueStmt _ Nothing) =
    text "continue;"
  pp (ContinueStmt _ (Just label)) =
    text"continue" <+> pp label <> semi
  pp (LabelledStmt _ label stmt) =
    pp label <> colon $$ pp stmt
  pp (ForInStmt p init expr body) =
    text "for" <+> parens (pp init <+> text "in" <+> pp expr)
      $$ (nest 2 (pp body))
  pp (ForStmt _ init incr test body) =
    text "for" <+> parens (pp init <> semi <+> pp incr <> semi <+> pp test)
      $$ (nest 2 (pp body))
  pp (TryStmt _ stmt catches finally) =
    (text "try" $$ pp stmt $$ (vcat (map pp catches)) $$ ppFinally) where
      ppFinally = 
        case finally of
          Nothing -> empty
	  (Just stmt) -> text "finally" <> pp stmt
  pp (ThrowStmt _ expr) =
    text "throw" <+> pp expr <> semi
  pp (WithStmt _ expr stmt) | isParenExpr expr =
    (text "with" <+> inParens expr $$ pp stmt)
  pp (WithStmt _ expr stmt) | otherwise =
      (text "with" <+> inParens expr $$ pp stmt)
  pp (VarDeclStmt _ decls) =
    text "var" <+> commaSep decls <> semi
  pp (FunctionStmt _ name args stmt) =
    text "function" <+> pp name <> (parens.commaSep) args $$ inBlock stmt

--}}}

instance PrettyPrintable (Prop a) where
  pp (PropId _ id) = pp id
  pp (PropString _ str) = text str
  pp (PropNum _ n) = text (show n)


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
jsEscape str =
  if str == ""  then "" else (sel (head str)) ++ (jsEscape (tail str)) where
    sel '\b' = "\\b"
    sel '\f' = "\\f"
    sel '\n' = "\\n"
    sel '\r' = "\\r"
    sel '\t' = "\\t"
    sel '\v' = "\\v"
    sel '\'' = "\\'"
    sel '\"' = "\\\""
    sel x    = [x]
    -- We don't have to do anything about \X, \x and \u escape sequences.

  
instance PrettyPrintable (Expression a) where
  pp (StringLit _ str) = doubleQuotes (text (jsEscape str))
  pp (RegexpLit _ re global ci) =
    text "/" <> text re <> text "/" <> g <> i where
      g = if global then text "g" else empty
      i = if ci then text "i" else empty
  pp (NumLit _ n) =  text (show n)
  pp (BoolLit _ True) = text "true"
  pp (BoolLit _ False) = text "false"
  pp (NullLit _) = text "null"
  pp (ArrayLit _ xs) = 
    (brackets.commaSep) xs
  pp (ObjectLit _ xs) = 
    braces (hsep (punctuate comma (map pp' xs))) where
      pp' (n,v) = pp n <> colon <+> pp v
  pp (ThisRef _) = text "this"
  pp (VarRef _ id) = pp id
  pp (DotRef _ expr id) =
    pp expr <> text "." <> pp id
  pp (BracketRef _ container key) =
    pp container <> brackets (pp key)
  pp (NewExpr _ constr args) =
    text "new" <+> pp constr <> (parens.commaSep) args
  pp (PrefixExpr _ op expr) =
    pp op <+> pp expr
  pp (PostfixExpr _ op expr) =
    pp expr <+> pp op
  pp (InfixExpr _ op left right) = 
    pp left <+> pp op <+> pp right
  pp (CondExpr _ test cons alt) =
    inParens test <+> text "?" <+> inParens cons <+> colon <+> inParens alt
  pp (AssignExpr _ op l r) =
    pp l <+> pp op <+> pp r
  pp (ParenExpr _ expr) =
    parens (pp expr)
  pp (ListExpr _ exprs) = commaSep exprs
  pp (CallExpr _ f args) =
    pp f <> (parens.commaSep) args
  pp (FuncExpr _ args body) =
    text "function" <+> (parens.commaSep) args $$ inBlock body

instance PrettyPrintable (JavaScript a) where
  pp (Script _ stmts) =
    vcat (map pp stmts)

--}}}

