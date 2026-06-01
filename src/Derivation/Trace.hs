module Derivation.Trace   -- 模块名，下方是倒出列表
  ( InferenceResult(..)   -- 导出类型+构造子。case当中需要构造子进行模式匹配。
  , InferenceOutcome(..)
  , inferSource
  , inferPseudoFunctionSource
  , inferParsed
  , inferTopTrace
  , inferTopTraceWithEnv
  ) where

import Derivation.Render (renderTrace)
import Infer (infer, inferWithPrelude, runInferMTrace)
import Parser (expressionOfFunction, parseExp, parsePseudoFileTrace)
import Syntax (Exp)
import TraceEvents (TraceEvent)
import Types (Scheme, TypeEnv, TypeError(..), generalize, apply)

data InferenceResult = InferenceOk
  { irExpr :: Exp
  , irScheme :: Scheme
  , irEvents :: [TraceEvent]
  }

data InferenceOutcome
  = OutcomeOk InferenceResult
  | OutcomeInferErr Exp TypeError [TraceEvent]

inferSource :: String -> Either TypeError InferenceOutcome  -- Either A B 表示 Either 是一个类型构造器，A 和 B 是类型参数。A 表示左边的类型，B 表示右边的类型
inferSource src =
  case parseExp src of
    Left err -> Left err
    Right expr -> inferExpressionOutcome expr

inferPseudoFunctionSource :: String -> Either TypeError InferenceOutcome
inferPseudoFunctionSource src =
  case parsePseudoFileTrace src of
    Left err -> Left (ParseFailure err)
    Right [] -> Left (ParseFailure "没有找到可展示的 pseudo function")
    Right (fn:_) -> inferExpressionOutcome (expressionOfFunction fn)

inferExpressionOutcome :: Exp -> Either TypeError InferenceOutcome
inferExpressionOutcome expr =
  case runInferMTrace (inferWithPrelude 0 expr) 0 of
    Left (err, traceLog) -> Right (OutcomeInferErr expr err traceLog)
    Right (((env, subst, t), _, traceLog)) ->
      Right
        ( OutcomeOk
            InferenceOk
              { irExpr = expr
              , irScheme = generalize (apply subst env) (apply subst t)
              , irEvents = traceLog
              }
        )

inferParsed :: Exp -> Either TypeError InferenceResult
inferParsed expr =
  case runInferMTrace (inferWithPrelude 0 expr) 0 of
    Left (err, _) -> Left err
    Right (((env, subst, t), _, traceLog)) ->
      Right
        InferenceOk
          { irExpr = expr
          , irScheme = generalize (apply subst env) (apply subst t)
          , irEvents = traceLog
          }

inferParsedWithEnv :: TypeEnv -> Exp -> Either TypeError InferenceResult
inferParsedWithEnv env expr =
  case runInferMTrace (infer 0 env expr) 0 of
    Left (err, _) -> Left err
    Right ((subst, t), _, traceLog) ->
      Right
        InferenceOk  -- 匹配InferenceOk的返回类型
          { irExpr = expr
          , irScheme = generalize (apply subst env) (apply subst t)  -- 泛化
          , irEvents = traceLog  -- 记录推断过程
          }

inferTopTrace :: Exp -> Either TypeError (Scheme, String)
inferTopTrace expr =
  case inferParsed expr of
    Left err -> Left err
    Right ok@(InferenceOk _ _ _) ->
      Right (irScheme ok, renderTrace (irEvents ok))

inferTopTraceWithEnv :: TypeEnv -> Exp -> Either TypeError (Scheme, String)
inferTopTraceWithEnv env expr =
  case inferParsedWithEnv env expr of
    Left err -> Left err
    Right ok@(InferenceOk _ _ _) -> -- 把InferenceOk的构造子解构出来，赋值给ok
      Right (irScheme ok, renderTrace (irEvents ok))
