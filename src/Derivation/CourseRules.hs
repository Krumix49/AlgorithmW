module Derivation.CourseRules
  ( CourseRule(..)
  , isExtension
  , ruleLabel
  , ruleSource
  ) where

import Syntax (BinOp(..)) -- 导入BinOp类型，BinOp是二元运算符类型。

data CourseRule
  = AppRule       -- 函数应用规则
  | LitIntRule    -- 整数字面量规则（字面量是常量，不需要推断）
  | LitBoolRule   -- 布尔字面量规则
  | IfRule        -- 条件表达式规则
  | VarRule       -- 变量查找规则（从类型环境查找类型）
  | LambdaRule    -- Lambda抽象规则
  | LetRule       -- let绑定规则
  | UnifyRule     -- 类型合一规则
  | BinOpRule BinOp -- 二元运算规则（二元运算有很多种） 
  deriving (Eq, Show)   -- 自动支持Eq和Show类型类，Eq表示相等性，Show表示显示。

isExtension :: CourseRule -> Bool
isExtension AppRule = False
isExtension LitIntRule = False
isExtension LitBoolRule = False
isExtension IfRule = False
isExtension VarRule = False
isExtension _ = True

ruleLabel :: CourseRule -> String
ruleLabel AppRule = "函数应用规则"
ruleLabel LitIntRule = "整数字面量"
ruleLabel LitBoolRule = "布尔字面量"
ruleLabel IfRule = "条件表达式"
ruleLabel VarRule = "变量查找"
ruleLabel LambdaRule = "Lambda 抽象"
ruleLabel LetRule = "let 绑定"
ruleLabel UnifyRule = "类型合一"
ruleLabel (BinOpRule op) = "二元运算 " ++ show op

ruleSource :: CourseRule -> String
ruleSource AppRule =
  "f :: A -> B, e :: A => f e :: B"
ruleSource LitIntRule =
  "整数字面量 n :: Int"
ruleSource LitBoolRule =
  "True/False :: Bool"
ruleSource IfRule =
  "if cond then e1 else e2，条件为 Bool，两分支同类型"
ruleSource VarRule =
  "表达式 e :: t（来自类型环境）"
ruleSource LambdaRule =
  "Currying + lambda；Algorithm W lambda 引入"
ruleSource LetRule =
  "Algorithm W let-多态（generalize/instantiate）"
ruleSource UnifyRule =
  "Algorithm W 合一（mgu）"
ruleSource (BinOpRule Add) =
  "(+) :: Int -> Int -> Int"
ruleSource (BinOpRule Sub) =
  ":: Int -> Int -> Int"
ruleSource (BinOpRule Mul) =
  "(*) :: Int -> Int -> Int"
ruleSource (BinOpRule Div) =
  "(/) :: Int -> Int -> Int"
ruleSource (BinOpRule Eq) =
  "(==) 结果 :: Bool"
ruleSource (BinOpRule Ne) =
  "不等比较结果 :: Bool"
ruleSource (BinOpRule Lt) =
  "(<) :: Int -> Int -> Bool"
ruleSource (BinOpRule Le) =
  "(<=) :: Int -> Int -> Bool"
ruleSource (BinOpRule Gt) =
  "(>) :: Int -> Int -> Bool"
ruleSource (BinOpRule Ge) =
  "(>=) :: Int -> Int -> Bool"
ruleSource (BinOpRule And) =
  "(&&) :: Bool -> Bool -> Bool"
ruleSource (BinOpRule Or) =
  "(||) :: Bool -> Bool -> Bool"
