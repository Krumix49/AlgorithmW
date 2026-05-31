module TraceEvents
  ( TraceEvent(..)
  ) where

import Derivation.CourseRules (CourseRule)
import Syntax (Exp)
import Types (Scheme, Subst, Type, TypeEnv)

data TraceEvent
  = EnterInfer              -- 要推断的表达式是expr，环境是env，深度是depth
      { depth :: Int
      , expr :: Exp
      , env :: TypeEnv
      }
  | ExitInfer               -- 推断结果是typ，深度是depth
      { depth :: Int
      , subst :: Subst  
      , typ :: Type  
      }
  | FreshVar                -- 生成新变量，深度是depth，变量是tv
      { depth :: Int
      , tv :: Type  
      }
  | InstantiateE            -- 实例化，深度是depth，类型是typ，方案是scheme
      { depth :: Int
      , scheme :: Scheme  
      , typ :: Type  
      }
  | GeneralizeE             -- 泛化，深度是depth，类型是typ，方案是scheme
      { depth :: Int
      , typ :: Type  
      , scheme :: Scheme  
      }
  | UnifyStart              -- 合一，深度是depth，类型是t1，类型是t2
      { depth :: Int
      , t1 :: Type  
      , t2 :: Type  
      }
  | UnifyDone               -- 合一结果，深度是depth，替换是subst
      { depth :: Int
      , subst :: Subst  
      }
  | RuleNote                -- 规则注释，深度是depth，规则是rule，细节是detail
      { depth :: Int
      , rule :: CourseRule  
      , detail :: String  
      }
  | AlgoStep                -- Algorithm W 分步说明（中文叙述 + show 类型/替换）
      { depth :: Int
      , message :: String
      }
  deriving (Eq, Show)  
