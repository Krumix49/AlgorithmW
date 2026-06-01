{-# LANGUAGE OverloadedStrings #-}

module Web.Page
  ( Example(..)
  , examples
  , examplesByGroup
  , renderPage
  , pageHtml
  ) where

import Derivation.InferExamples (InferUnit(..), groupLabel, inferUnitsByGroup)
import Derivation.Trace
  ( InferenceOutcome(..)
  , InferenceResult(..)
  )
import Syntax (Exp)
import Text.Blaze.Html (Html, preEscapedToHtml, toHtml, toValue)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Types (TypeError)
import Web.TraceHtml (renderTraceHtml)

data Example = Example
  { exLabel :: String
  , exSource :: String
  , exMode :: ExampleMode
  }

data ExampleMode
  = ExpressionExample
  | PseudoFunctionExample

examples :: [Example]
examples = map toExample (concatMap snd inferUnitsByGroup)

examplesByGroup :: [(String, [Example])]
examplesByGroup =
  map (\(group, units) -> (groupLabel group, map toExample units)) inferUnitsByGroup
    ++ [("伪代码函数", [chooseExample])]

toExample :: InferUnit -> Example
toExample unit =
  Example
    { exLabel = iuLabel unit
    , exSource = iuSource unit
    , exMode = ExpressionExample
    }

chooseExample :: Example
chooseExample =
  Example
    { exLabel = "choose(flag, a, b)"
    , exSource =
        unlines
          [ "function y = choose(flag, a, b)"
          , "  if flag"
          , "    y = a"
          , "  else"
          , "    y = b"
          , "  end"
          , "end"
          ]
    , exMode = PseudoFunctionExample
    }

renderPage :: String -> Maybe (Either TypeError InferenceOutcome) -> Html
renderPage source outcome =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.title "Algorithm W 类型推断"
      H.style ! A.type_ "text/css" $ toHtml pageCss
    H.body $ do
      H.header ! A.class_ "page-header" $ do
        H.h1 "Algorithm W 类型推断"
        H.p ! A.class_ "subtitle" $
          "输入表达式，查看推断类型与逐步推导"
      H.main ! A.class_ "page-main" $ do
        renderInputSection source
        case outcome of
          Nothing -> renderWelcome
          Just (Left err) -> renderParseError err
          Just (Right (OutcomeInferErr expr err _events)) -> renderInferError source expr err
          Just (Right (OutcomeOk result)) -> renderSuccess source result
      H.script ! A.type_ "text/javascript" $ preEscapedToHtml pageScript

pageHtml :: String -> Maybe (Either TypeError InferenceOutcome) -> String
pageHtml source outcome =
  renderHtml (renderPage source outcome)

renderInputSection :: String -> Html
renderInputSection source =
  H.section ! A.class_ "card input-card" $ do
    H.h2 "表达式"
    H.form ! A.method "post" ! A.action "/infer" ! A.class_ "infer-form" $ do
      H.label ! A.for "source-input" ! A.class_ "input-label" $ "ML 风格表达式"
      H.textarea
        ! A.id "source-input"
        ! A.name "source"
        ! A.class_ "source-input"
        ! A.rows "4"
        ! A.placeholder "例如: let id = \\x -> x in id 3"
        $ toHtml source
      H.div ! A.class_ "form-actions" $
        H.button ! A.type_ "submit" ! A.class_ "btn-primary" $ "推断"
    renderExamples

renderExamples :: Html
renderExamples =
  H.div ! A.class_ "examples" $ do
    H.span ! A.class_ "examples-label" $ "快速示例（按构造分组）"
    mapM_ renderExampleGroup examplesByGroup
  where
    renderExampleGroup (groupName, groupExamples) =
      H.div ! A.class_ "examples-group" $ do
        H.span ! A.class_ "examples-group-label" $ toHtml groupName
        H.div ! A.class_ "examples-list" $ mapM_ renderExampleBtn groupExamples
    renderExampleBtn (Example label src mode) =
      H.form ! A.method "post" ! A.action "/infer" ! A.class_ "example-form" $ do
        H.input ! A.type_ "hidden" ! A.name "source" ! A.value (toValue src)
        H.input ! A.type_ "hidden" ! A.name "mode" ! A.value (toValue (modeValue mode))
        H.button
          ! A.type_ "submit"
          ! A.class_ "btn-example"
          ! A.title (toValue src)
          $ toHtml label

modeValue :: ExampleMode -> String
modeValue ExpressionExample = "expr"
modeValue PseudoFunctionExample = "pseudo-function"

renderWelcome :: Html
renderWelcome =
  H.section ! A.class_ "card welcome-card" ! H.customAttribute "role" "status" $ do
    H.h2 "开始推断"
    H.p ! A.class_ "welcome-text" $
      "在上方输入表达式，或点击快速示例按钮，查看推断类型与逐步推导过程。"

renderParseError :: TypeError -> Html
renderParseError err =
  H.section
    ! A.class_ "card error-card"
    ! H.customAttribute "role" "alert"
    ! H.customAttribute "aria-live" "polite"
    $ do
      H.h2 "解析错误"
      H.p ! A.class_ "error-msg" $ do
        H.strong $ "错误："
        toHtml (show err)
      renderEmptyResults

renderInferError :: String -> Exp -> TypeError -> Html
renderInferError _source _expr err =
  H.section
    ! A.class_ "card error-card"
    ! H.customAttribute "role" "alert"
    ! H.customAttribute "aria-live" "polite"
    $ do
      H.h2 "推断错误"
      H.p ! A.class_ "error-msg" $ do
        H.strong $ "错误："
        toHtml (show err)
      renderResultGrid Nothing
      renderTraceSection (H.p ! A.class_ "trace-empty" $ "推断失败，无完整推导记录。")

renderSuccess :: String -> InferenceResult -> Html
renderSuccess _source r = do
  renderResultGrid (Just (show (irScheme r)))
  renderTraceSection (renderTraceHtml (irEvents r))

renderTraceSection :: Html -> Html
renderTraceSection content =
  H.section ! A.class_ "card trace-card" $ do
    H.h2 "推断过程"
    H.div ! A.class_ "trace-panel" $ content

renderResultGrid :: Maybe String -> Html
renderResultGrid mType =
  H.section ! A.class_ "results-grid" $ do
    H.div ! A.class_ "card result-card" $ do
      H.h3 "推断类型"
      case mType of
        Just t ->
          H.div ! A.class_ "type-ok" $ codeBlock t
        Nothing -> H.p ! A.class_ "placeholder" $ "—"

renderEmptyResults :: Html
renderEmptyResults = renderResultGrid Nothing

codeBlock :: String -> Html
codeBlock s = H.pre ! A.class_ "code-block" $ toHtml s

pageCss :: String
pageCss =
  unlines
    ( pageCssTokens
    ++ pageCssBase
    ++ pageCssLayout
    ++ pageCssComponents
    ++ pageCssTrace
    ++ traceDepthCss
    ++ traceDepthCssMobile
    ++ pageCssResponsive
    )

pageCssTokens :: [String]
pageCssTokens =
  [ ":root {"
  , "  --color-bg: #f8fafc;"
  , "  --color-surface: #ffffff;"
  , "  --color-text: #0f172a;"
  , "  --color-text-muted: #64748b;"
  , "  --color-border: #e2e8f0;"
  , "  --color-accent: #0f766e;"
  , "  --color-accent-hover: #0d9488;"
  , "  --color-success: #047857;"
  , "  --color-error: #b91c1c;"
  , "  --color-code-bg: #f1f5f9;"
  , "  --color-trace-enter: #94a3b8;"
  , "  --color-trace-exit: #047857;"
  , "  --color-trace-rule: #64748b;"
  , "  --radius: 6px;"
  , "  --space-1: 0.25rem;"
  , "  --space-2: 0.5rem;"
  , "  --space-3: 0.75rem;"
  , "  --space-4: 1rem;"
  , "  --space-5: 1.25rem;"
  , "  --space-6: 1.5rem;"
  , "  --space-8: 2rem;"
  , "  --focus-ring: 2px solid var(--color-accent);"
  , "}"
  ]

pageCssBase :: [String]
pageCssBase =
  [ "* { box-sizing: border-box; }"
  , "body {"
  , "  font-family: system-ui, -apple-system, 'Segoe UI', sans-serif;"
  , "  margin: 0;"
  , "  background: var(--color-bg);"
  , "  color: var(--color-text);"
  , "  line-height: 1.5;"
  , "}"
  , ".page-header {"
  , "  background: var(--color-surface);"
  , "  border-bottom: 1px solid var(--color-border);"
  , "  padding: var(--space-6) var(--space-4);"
  , "}"
  , ".page-header h1 {"
  , "  margin: 0 0 var(--space-1);"
  , "  font-size: 1.5rem;"
  , "  font-weight: 600;"
  , "}"
  , ".subtitle {"
  , "  margin: 0;"
  , "  color: var(--color-text-muted);"
  , "  font-size: 0.95rem;"
  , "}"
  , ".page-main {"
  , "  max-width: 60rem;"
  , "  margin: 0 auto;"
  , "  padding: var(--space-6) var(--space-4);"
  , "}"
  ]

pageCssLayout :: [String]
pageCssLayout =
  [ ".card {"
  , "  background: var(--color-surface);"
  , "  border: 1px solid var(--color-border);"
  , "  border-radius: var(--radius);"
  , "  padding: var(--space-5);"
  , "  margin-bottom: var(--space-4);"
  , "}"
  , ".input-card h2, .trace-card h2, .welcome-card h2, .error-card h2 {"
  , "  margin: 0 0 var(--space-3);"
  , "  font-size: 1.1rem;"
  , "  font-weight: 600;"
  , "}"
  , ".results-grid {"
  , "  display: grid;"
  , "  grid-template-columns: 1fr;"
  , "  gap: var(--space-4);"
  , "  margin-bottom: var(--space-4);"
  , "}"
  , ".result-card h3 {"
  , "  margin: 0 0 var(--space-2);"
  , "  font-size: 0.95rem;"
  , "  color: var(--color-text-muted);"
  , "  font-weight: 500;"
  , "}"
  ]

pageCssComponents :: [String]
pageCssComponents =
  [ ".input-label {"
  , "  display: block;"
  , "  margin-bottom: var(--space-2);"
  , "  font-size: 0.9rem;"
  , "  font-weight: 500;"
  , "}"
  , ".source-input {"
  , "  width: 100%;"
  , "  font-family: ui-monospace, 'Cascadia Code', monospace;"
  , "  font-size: 0.95rem;"
  , "  padding: var(--space-3);"
  , "  border: 1px solid var(--color-border);"
  , "  border-radius: var(--radius);"
  , "  background: var(--color-surface);"
  , "  color: var(--color-text);"
  , "  resize: vertical;"
  , "}"
  , ".source-input:focus-visible {"
  , "  outline: var(--focus-ring);"
  , "  outline-offset: 2px;"
  , "  border-color: var(--color-accent);"
  , "}"
  , ".infer-form { display: flex; flex-direction: column; gap: var(--space-3); }"
  , ".form-actions { display: flex; justify-content: flex-end; }"
  , ".btn-primary {"
  , "  background: var(--color-accent);"
  , "  color: #fff;"
  , "  border: none;"
  , "  padding: var(--space-2) var(--space-5);"
  , "  border-radius: var(--radius);"
  , "  font-size: 1rem;"
  , "  font-weight: 500;"
  , "  cursor: pointer;"
  , "}"
  , ".btn-primary:hover { background: var(--color-accent-hover); }"
  , ".btn-primary:focus-visible { outline: var(--focus-ring); outline-offset: 2px; }"
  , ".btn-primary:active { transform: translateY(1px); }"
  , ".examples {"
  , "  margin-top: var(--space-4);"
  , "  padding-top: var(--space-4);"
  , "  border-top: 1px solid var(--color-border);"
  , "}"
  , ".examples-label {"
  , "  display: block;"
  , "  font-size: 0.85rem;"
  , "  color: var(--color-text-muted);"
  , "  margin-bottom: var(--space-3);"
  , "}"
  , ".examples-group {"
  , "  margin-bottom: var(--space-3);"
  , "}"
  , ".examples-group:last-child {"
  , "  margin-bottom: 0;"
  , "}"
  , ".examples-group-label {"
  , "  display: block;"
  , "  font-size: 0.8rem;"
  , "  font-weight: 600;"
  , "  color: var(--color-text);"
  , "  margin-bottom: var(--space-2);"
  , "}"
  , ".examples-list { display: flex; flex-wrap: wrap; gap: var(--space-2); }"
  , ".example-form { display: inline; margin: 0; }"
  , ".btn-example {"
  , "  background: var(--color-surface);"
  , "  color: var(--color-text);"
  , "  border: 1px solid var(--color-border);"
  , "  padding: var(--space-1) var(--space-3);"
  , "  border-radius: var(--radius);"
  , "  font-size: 0.85rem;"
  , "  cursor: pointer;"
  , "}"
  , ".btn-example:hover {"
  , "  border-color: var(--color-accent);"
  , "  color: var(--color-accent);"
  , "}"
  , ".btn-example:focus-visible { outline: var(--focus-ring); outline-offset: 2px; }"
  , ".code-block, .mono {"
  , "  font-family: ui-monospace, 'Cascadia Code', monospace;"
  , "  font-size: 0.9rem;"
  , "}"
  , ".code-block {"
  , "  background: var(--color-code-bg);"
  , "  padding: var(--space-3);"
  , "  border-radius: var(--radius);"
  , "  overflow-x: auto;"
  , "  margin: 0;"
  , "  white-space: pre-wrap;"
  , "  word-break: break-word;"
  , "}"
  , ".type-ok {"
  , "  font-family: ui-monospace, monospace;"
  , "  font-size: 1rem;"
  , "  color: var(--color-success);"
  , "  font-weight: 600;"
  , "  margin: 0;"
  , "}"
  , ".placeholder { color: var(--color-text-muted); margin: 0; }"
  , ".welcome-text { margin: 0; color: var(--color-text-muted); }"
  , ".error-card { border-left: 4px solid var(--color-error); }"
  , ".error-msg { color: var(--color-error); margin: 0 0 var(--space-4); }"
  , ".trace-empty { color: var(--color-text-muted); font-style: italic; margin: 0; }"
  ]

pageCssTrace :: [String]
pageCssTrace =
  [ ".trace-panel {"
  , "  max-height: 28rem;"
  , "  overflow-y: auto;"
  , "  border: 1px solid var(--color-border);"
  , "  border-radius: var(--radius);"
  , "  padding: var(--space-3);"
  , "  background: var(--color-code-bg);"
  , "}"
  , ".trace-log {"
  , "  list-style: none;"
  , "  margin: 0;"
  , "  padding: 0;"
  , "  font-size: 0.88rem;"
  , "}"
  , ".trace-item { margin: 0; padding: 0; }"
  , ".trace-children { list-style: none; margin: 0; padding: 0; }"
  , ".trace-block.is-collapsed > .trace-children > .trace-detail-item { display: none; }"
  , ".trace-step {"
  , "  margin-bottom: var(--space-2);"
  , "  padding: var(--space-2) var(--space-3);"
  , "  border-radius: var(--radius);"
  , "  border-left: 3px solid transparent;"
  , "}"
  , ".trace-enter { display: flex; align-items: flex-start; gap: var(--space-2); }"
  , ".trace-toggle {"
  , "  width: 1.5rem;"
  , "  height: 1.5rem;"
  , "  flex: 0 0 auto;"
  , "  border: 1px solid var(--color-border);"
  , "  border-radius: var(--radius);"
  , "  background: var(--color-surface);"
  , "  color: var(--color-text-muted);"
  , "  cursor: pointer;"
  , "  font-size: 0.8rem;"
  , "  line-height: 1;"
  , "  margin-left: auto;"
  , "}"
  , ".trace-toggle:hover { border-color: var(--color-accent); color: var(--color-accent); }"
  , ".trace-toggle:focus-visible { outline: var(--focus-ring); outline-offset: 2px; }"
  , ".trace-content { flex: 1 1 auto; min-width: 0; }"
  , ".trace-enter { border-left-color: var(--color-trace-enter); }"
  , ".trace-exit {"
  , "  border-left-color: var(--color-trace-exit);"
  , "  color: var(--color-success);"
  , "  font-weight: 500;"
  , "}"
  , ".trace-rule { border-left-color: var(--color-trace-rule); }"
  , ".trace-env { color: var(--color-text-muted); font-size: 0.85em; }"
  , ".trace-meta { font-size: 0.8em; color: var(--color-text-muted); }"
  , ".trace-detail { margin: var(--space-1) 0; }"
  , ".trace-rule-title { font-weight: 500; margin-bottom: var(--space-1); }"
  , ".app-fraction { margin: var(--space-2) 0 var(--space-2) var(--space-4); }"
  , ".frac-line { font-family: ui-monospace, monospace; }"
  , ".frac-bar { color: var(--color-text-muted); letter-spacing: -0.05em; }"
  , "script { display: none; }"
  ]

traceDepthCss :: [String]
traceDepthCss =
  [ ".trace-step[data-depth=\"" ++ show n ++ "\"] { margin-left: "
      ++ show (fromIntegral n * (1.5 :: Double))
      ++ "em; }"
  | n <- ([0 .. 15] :: [Int])
  ]

traceDepthCssMobile :: [String]
traceDepthCssMobile =
  ( "@media (max-width: 640px) {"
    : [ "  .trace-step[data-depth=\"" ++ show n ++ "\"] { margin-left: "
          ++ show (fromIntegral n * (0.75 :: Double))
          ++ "em; }"
      | n <- ([0 .. 15] :: [Int])
      ]
    )
    ++ ["}"]

pageCssResponsive :: [String]
pageCssResponsive =
  [ "@media (max-width: 640px) {"
  , "  .results-grid { grid-template-columns: 1fr; }"
  , "  .page-main { padding: var(--space-4) var(--space-3); }"
  , "  .page-header { padding: var(--space-4) var(--space-3); }"
  , "}"
  , "@media (max-width: 320px) {"
  , "  .page-main { padding: var(--space-3) var(--space-2); }"
  , "  .page-header { padding: var(--space-3) var(--space-2); }"
  , "  .page-header h1 { font-size: 1.25rem; }"
  , "}"
  , "@media (min-width: 1024px) {"
  , "  .page-main { max-width: 72rem; }"
  , "  .trace-panel { max-height: 32rem; }"
  , "}"
  ]

pageScript :: String
pageScript =
  unlines
    [ "document.addEventListener('click', function (event) {"
    , "  var button = event.target.closest('[data-trace-toggle]');"
    , "  if (!button) return;"
    , "  var block = button.closest('.trace-block');"
    , "  if (!block) return;"
    , "  var collapsed = block.classList.toggle('is-collapsed');"
    , "  button.setAttribute('aria-expanded', collapsed ? 'false' : 'true');"
    , "  button.textContent = collapsed ? '>' : 'V';"
    , "});"
    ]
