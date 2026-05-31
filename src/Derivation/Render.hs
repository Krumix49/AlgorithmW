module Derivation.Render
  ( renderTrace
  ) where

import Derivation.CourseRules
  ( CourseRule(..)
  , ruleLabel
  , ruleSource
  )
import Syntax ()
import TraceEvents hiding (depth, env, expr, subst, typ, tv, scheme, t1, t2, rule, detail, message)
import Types ()

renderTrace :: [TraceEvent] -> String
renderTrace events = unlines (map renderEvent events)

renderEvent :: TraceEvent -> String
renderEvent (EnterInfer d e env) =
  indent d ++ "推断 " ++ show e ++ "  环境: " ++ show env
renderEvent (ExitInfer d s t) =
  indent d ++ "=> 替换: " ++ show s ++ ", 类型: " ++ show t
renderEvent (FreshVar d tv) =
  indent d ++ "fresh：生成新类型变量 " ++ show tv
renderEvent (InstantiateE d scheme typ) =
  indent d ++ "instantiate：Scheme " ++ show scheme ++ " => Type " ++ show typ
renderEvent (GeneralizeE d typ scheme) =
  indent d ++ "generalize：泛化类型Type " ++ show typ ++ "，得到多态方案Scheme " ++ show scheme
renderEvent (UnifyStart d t1 t2) =
  indent d ++ "mgu：合一 " ++ show t1 ++ " ~ " ++ show t2
renderEvent (UnifyDone d subst) =
  indent d ++ "mgu 结果：S = " ++ show subst
renderEvent (AlgoStep d msg) =
  indent d ++ msg
renderEvent (RuleNote d AppRule detail) =
  unlines
    [ indent d ++ "函数应用规则 (" ++ ruleSource AppRule ++ ")"
    , indent (d + 1) ++ detail
    , renderAppFraction (d + 1) detail
    ]
renderEvent (RuleNote d rule detail) =
  prefix d rule ++ ruleLabel rule ++ ": " ++ detail

prefix :: Int -> CourseRule -> String
prefix d rule =
  indent d ++ "(" ++ ruleSource rule ++ ") "

indent :: Int -> String
indent n = replicate (n * 2) ' '

renderAppFraction :: Int -> String -> String
renderAppFraction d detail =
  case splitOnce " => " detail of       -- 按照=>分隔detail，得到premise和conclusion
    Just (premise, conclusion) ->       -- 找到=>
      case splitOnce ", " premise of    -- 再按照,分割
        Just (left, right) ->           -- 找到,
          unlines                       -- 把字符串列表拼成一个字符串
            [ indent d ++ left
            , indent d ++ right
            , indent d ++ replicate (max (length left) (length right)) '─'
            , indent d ++ conclusion
            ]
        Nothing ->
          indent d ++ conclusion
    Nothing ->
      indent d ++ "(应用)"

splitOnce :: String -> String -> Maybe (String, String)   -- 在字符串里找分隔符第一次出现的位置，只切一刀，切成前后两段。
splitOnce sep s = walk 0 s
  where               -- 局部定义函数
    walk idx str
      | sep `isPrefixOf` str = Just (take idx s, drop (length sep) str) -- isPrefixOf sep str
      | null str = Nothing
      | otherwise = walk (idx + 1) (tail str)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys -- 递归判断前缀是否相同
