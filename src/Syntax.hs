module Syntax
  ( Exp(..)
  , Lit(..)
  , BinOp(..)
  ) where

data Exp
  = EVar String
  | ELit Lit
  | EApp Exp Exp
  | EAbs String Exp
  | ELet String Exp Exp
  | EIf Exp Exp Exp
  | EBin BinOp Exp Exp
  deriving (Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Eq, Ord)

data BinOp
  = Add
  | Sub
  | Mul
  | Eq
  | And
  | Or
  deriving (Eq, Ord)

instance Show Exp where
  show :: Exp -> String
  show = showExp 0

instance Show Lit where
  show :: Lit -> String
  show (LInt n) = show n
  show (LBool True) = "True"
  show (LBool False) = "False"

instance Show BinOp where
  show :: BinOp -> String
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Eq = "=="
  show And = "&&"
  show Or = "||"

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

binPrec :: BinOp -> Int
binPrec Or = 1
binPrec And = 2
binPrec Eq = 3
binPrec Add = 4
binPrec Sub = 4
binPrec Mul = 5

parensIf :: Bool -> String -> String
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s
