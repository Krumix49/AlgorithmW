module Syntax
  ( Exp(..)
  , Lit(..)
  , BinOp(..)
  , Stmt(..)
  ) where

-- Exp 是这个小语言的表达式 AST：解析器把源码变成它，类型推断器再检查它。
data Exp
  = EVar String
  | ELit Lit
  | EApp Exp Exp
  | EAbs String Exp
  | ELet String Exp Exp
  | EIf Exp Exp Exp
  | EBin BinOp Exp Exp
  | EList [Exp]
  | EBlock [Stmt] String
  deriving (Eq, Ord)

-- Stmt 表示伪代码文件里的语句。它们只出现在 EBlock 里，最后通过输出变量取结果类型。
data Stmt
  = SAssign String Exp
  | SIfStmt Exp [Stmt] [Stmt]
  | SSwitchStmt Exp [(Exp, [Stmt])] [Stmt]
  | SFor String Exp [Stmt]
  | SWhile Exp [Stmt]
  deriving (Eq, Ord)

-- Lit 是不用查环境就能知道类型的字面量。
data Lit
  = LInt Integer
  | LBool Bool
  deriving (Eq, Ord)

-- BinOp 只记录运算符种类；每个运算符需要什么类型由 Infer.hs 决定。
data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  deriving (Eq, Ord)

instance Show Exp where
  show = showExp 0

instance Show Lit where
  show (LInt n) = show n
  show (LBool True) = "True"
  show (LBool False) = "False"

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Eq = "=="
  show Ne = "~="
  show Lt = "<"
  show Le = "<="
  show Gt = ">"
  show Ge = ">="
  show And = "&&"
  show Or = "||"

-- Show 实例把内部 AST 重新打印成接近源码的形式，方便调试和 verbose 输出。
showExp :: Int -> Exp -> String
showExp _ (EVar name) = name
showExp _ (ELit lit) = show lit
showExp p (EApp f x) =
  parensIf (p > 10) (showExp 10 f ++ " " ++ showExp 11 x)
showExp p (EAbs name body) =
  parensIf (p > 0) ("\\" ++ name ++ " -> " ++ showExp 0 body)
showExp p (ELet name value body) =
  parensIf (p > 0) ("let " ++ name ++ " = " ++ showExp 0 value ++ " in " ++ showExp 0 body)
showExp p (EIf cond yes no) =
  parensIf (p > 0) ("if " ++ showExp 0 cond ++ " then " ++ showExp 0 yes ++ " else " ++ showExp 0 no)
showExp p (EBin op left right) =
  parensIf (p > prec) (showExp prec left ++ " " ++ show op ++ " " ++ showExp (prec + 1) right)
  where
    prec = binPrec op
showExp _ (EList items) =
  "[" ++ joinWith ", " (map (showExp 0) items) ++ "]"
showExp _ (EBlock stmts outputName) =
  "block { " ++ joinWith "; " (map showStmt stmts) ++ "; return " ++ outputName ++ " }"

binPrec :: BinOp -> Int
binPrec Or = 1
binPrec And = 2
binPrec Eq = 3
binPrec Ne = 3
binPrec Lt = 3
binPrec Le = 3
binPrec Gt = 3
binPrec Ge = 3
binPrec Add = 4
binPrec Sub = 4
binPrec Mul = 5
binPrec Div = 5

-- 根据父表达式优先级决定是否补括号，避免打印结果改变原表达式含义。
parensIf :: Bool -> String -> String
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s

-- 伪代码语句也有自己的打印形式，用来展示 EBlock 的规范化结果。
showStmt :: Stmt -> String
showStmt (SAssign name expr) =
  name ++ " = " ++ showExp 0 expr
showStmt (SIfStmt cond yes no) =
  "if " ++ showExp 0 cond ++ " then { " ++ joinWith "; " (map showStmt yes)
  ++ " } else { " ++ joinWith "; " (map showStmt no) ++ " }"
showStmt (SSwitchStmt subject cases otherwiseBranch) =
  "switch " ++ showExp 0 subject ++ " { "
  ++ joinWith "; " (map showCase cases)
  ++ "; otherwise { " ++ joinWith "; " (map showStmt otherwiseBranch) ++ " } }"
showStmt (SFor name items body) =
  "for " ++ name ++ " in " ++ showExp 0 items ++ " { " ++ joinWith "; " (map showStmt body) ++ " }"
showStmt (SWhile cond body) =
  "while " ++ showExp 0 cond ++ " { " ++ joinWith "; " (map showStmt body) ++ " }"

showCase :: (Exp, [Stmt]) -> String
showCase (value, body) =
  "case " ++ showExp 0 value ++ " { " ++ joinWith "; " (map showStmt body) ++ " }"

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs
