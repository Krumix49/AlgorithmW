{-# LANGUAGE OverloadedStrings #-}

module Web.TraceHtml
  ( renderTraceHtml
  ) where

import Derivation.CourseRules
  ( CourseRule(..)
  , ruleLabel
  , ruleSource
  )
import Syntax ()
import Text.Blaze.Html (Html, toValue)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import TraceEvents hiding (depth, env, expr, subst, typ, tv, scheme, t1, t2, rule, detail, message)
import Types ()

renderTraceHtml :: [TraceEvent] -> Html
renderTraceHtml events =
  H.ol
    ! A.class_ "trace-log"
    ! H.customAttribute "aria-label" "推断过程"
    $ mapM_ renderTraceItem events
  where
    renderTraceItem event =
      H.li ! A.class_ "trace-item" $ renderEvent event

renderEvent :: TraceEvent -> Html
renderEvent (EnterInfer d e env) =
  depthDiv d "trace-enter" $ do
    H.span ! A.class_ "trace-label" $ "推断 "
    codeText (show e)
    H.span ! A.class_ "trace-env" $ do
      "  环境: "
      codeText (show env)
renderEvent (ExitInfer d s t) =
  depthDiv d "trace-exit" $ do
    H.span ! A.class_ "trace-arrow" $ "=> 替换: "
    codeText (show s)
    H.span ! A.class_ "trace-arrow" $ ", 类型: "
    codeText (show t)
renderEvent (FreshVar d tv) =
  depthDiv d "trace-step" $ H.toHtml ("fresh：生成新类型变量 " ++ show tv)
renderEvent (InstantiateE d sch typ) =
  depthDiv d "trace-step" $
    H.toHtml ("instantiate：Scheme " ++ show sch ++ " => Type " ++ show typ)
renderEvent (GeneralizeE d typ sch) =
  depthDiv d "trace-step" $
    H.toHtml ("generalize：从类型 " ++ show typ ++ " 泛化得到多态方案Scheme " ++ show sch)
renderEvent (UnifyStart d t1 t2) =
  depthDiv d "trace-step" $ H.toHtml ("mgu：合一 " ++ show t1 ++ " ~ " ++ show t2)
renderEvent (UnifyDone d s) =
  depthDiv d "trace-step" $ H.toHtml ("mgu 结果：S = " ++ show s)
renderEvent (AlgoStep d msg) =
  depthDiv d "trace-step" $ H.toHtml msg
renderEvent (RuleNote d AppRule detail) =
  depthDiv d "trace-rule" $ do
    H.div ! A.class_ "trace-rule-title" $ do
      "函数应用规则 ("
      H.toHtml (ruleSource AppRule)
      ")"
    H.div ! A.class_ "trace-detail" $ codeText detail
    renderAppFraction detail
renderEvent (RuleNote d r detail) =
  ruleLine d r (ruleLabel r ++ ": " ++ detail)

depthDiv :: Int -> String -> Html -> Html
depthDiv d cls content =
  H.div
    ! A.class_ (toValue ("trace-step " ++ cls))
    ! H.customAttribute "data-depth" (toValue (show d))
    $ content

ruleLine :: Int -> CourseRule -> String -> Html
ruleLine d r text =
  depthDiv d "trace-rule" $ do
    H.span ! A.class_ "trace-meta" $ H.toHtml ("(" ++ ruleSource r ++ ") ")
    codeText text

codeText :: String -> Html
codeText s = H.code ! A.class_ "mono" $ H.toHtml s

renderAppFraction :: String -> Html
renderAppFraction detail =
  case splitOnce " => " detail of
    Just (premise, conclusion) ->
      case splitOnce ", " premise of
        Just (left, right) ->
          H.div ! A.class_ "app-fraction" $ do
            H.div ! A.class_ "frac-line" $ codeText left
            H.div ! A.class_ "frac-line" $ codeText right
            H.div ! A.class_ "frac-bar" $
              H.toHtml (replicate (max (length left) (length right)) '─')
            H.div ! A.class_ "frac-line" $ codeText conclusion
        Nothing ->
          H.div ! A.class_ "frac-line" $ codeText conclusion
    Nothing ->
      H.div ! A.class_ "frac-line" $ "(应用)"

splitOnce :: String -> String -> Maybe (String, String)
splitOnce sep s = walk 0 s
  where
    walk idx str
      | sep `isPrefixOf` str = Just (take idx s, drop (length sep) str)
      | null str = Nothing
      | otherwise = walk (idx + 1) (tail str)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
