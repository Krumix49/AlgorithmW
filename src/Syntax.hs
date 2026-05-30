module Syntax
  ( Exp(..)
  , Lit(..)
  , BinOp(..)
  ) where

data Exp
  = EVar String  -- 变量
  | ELit Lit  -- 常量
  | EApp Exp Exp  -- 函数应用
  | EAbs String Exp  -- lambda表达式
  | ELet String Exp Exp  -- let表达式
  | EIf Exp Exp Exp  -- if表达式
  | EBin BinOp Exp Exp  -- 二元运算
  deriving (Eq, Ord)

data Lit
  = LInt Integer  -- 整数
  | LBool Bool  -- 布尔值
  deriving (Eq, Ord)

data BinOp  -- 二元运算符类型
  = Add
  | Sub
  | Mul
  | Eq
  | And
  | Or
  deriving (Eq, Ord)

instance Show Exp where
  show :: Exp -> String  -- 显示表达式（默认最外层，优先级0）
  show = showExp 0

instance Show Lit where
  show :: Lit -> String  -- 显示常量
  show (LInt n) = show n
  show (LBool True) = "True"
  show (LBool False) = "False"

instance Show BinOp where
  show :: BinOp -> String  -- 显示二元运算符
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Eq = "=="
  show And = "&&"
  show Or = "||"

showExp :: Int -> Exp -> String  -- 显示表达式（优先级用于判断是否有括号）
showExp _ (EVar name) = name
showExp _ (ELit lit) = show lit
showExp p (EApp f x) =
  parensIf (p > 10) (showExp 10 f ++ " " ++ showExp 11 x)  -- 函数运算优先级很高，一般不用括号
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

binPrec :: BinOp -> Int  -- 二元运算符的优先级
binPrec Or = 1
binPrec And = 2
binPrec Eq = 3
binPrec Add = 4
binPrec Sub = 4
binPrec Mul = 5

parensIf :: Bool -> String -> String  -- 是否添加括号
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s
