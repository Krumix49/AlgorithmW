-- 基本语法定义, 包括表达式, 字面量, 类型和类型方案
module Syntax where

-- Exp 表示待推断类型的表达式.
-- 这里的 E 是 Expression 的缩写.
data Exp = EVar String
            -- EVar 表示变量表达式, 例如 x 或 id.
            -- String 保存变量名.
            | ELit Lit
            -- ELit 表示字面量表达式, 例如 2 或 True.
            | EApp Exp Exp
            -- EApp 表示函数应用, 例如 f x.
            -- 第一个 Exp 是函数位置, 第二个 Exp 是参数位置.
            | EAbs String Exp
            -- EAbs 表示 lambda 抽象, 例如 \x -> x.
            -- String 是参数名, Exp 是函数体.
            | ELet String Exp Exp
            -- ELet 表示 let 绑定, 例如 let id = \x -> x in id.
            -- String 是被绑定的变量名, 第一个 Exp 是绑定表达式,
            -- 第二个 Exp 是 let 的主体表达式.
            deriving (Eq, Ord)

-- Lit 表示语言中的字面量.
data Lit = LInt Integer
            -- LInt 表示整数字面量.
            | LBool Bool
            -- LBool 表示布尔字面量.
            deriving (Eq, Ord)

-- Type 表示类型推断系统中的类型.
data Type = TVar String
            -- TVar 表示类型变量, 例如 a0 或 a1.
            | TInt
            -- TInt 表示整数类型 Int.
            | TBool
            -- TBool 表示布尔类型 Bool.
            | TFun Type Type
            -- TFun 表示函数类型, 例如 Int -> Bool.
            -- 第一个 Type 是参数类型, 第二个 Type 是返回类型.
            deriving (Eq, Ord)

-- Scheme 表示类型方案, 用于 let 多态.
-- [String] 保存被 forall 量化的类型变量名.
-- Type 是类型方案的主体类型.
data Scheme = Scheme [String] Type
