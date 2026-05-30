module Main where

import Data.Char (toLower)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)

import Infer
import Parser
import Syntax
import Types

data Options = Options
  { optVerbose :: Bool
  , optHelp :: Bool
  , optFile :: Maybe FilePath
  , optExpressionParts :: [String]
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose = False
  , optHelp = False
  , optFile = Nothing
  , optExpressionParts = []
  }

examples :: [(String, String)]
examples =
  [ ("identity", "let id = \\x -> x in id")
  , ("identity applied to itself", "let id = \\x -> x in id id")
  , ("lambda application", "(\\x -> x + 1) 41")
  , ("if expression", "if True then 1 else 2")
  , ("partial application", "let func = \\x y -> x + y in func 4")
  , ("occurs check failure", "let bad = \\x -> x x in bad")
  , ("calling an integer", "2 2")
  ]

main :: IO ()
main = do
  args <- getArgs
  case parseOptions args of
    Left err -> putStrLn ("参数错误：" ++ err) >> usage >> exitFailure
    Right options
      | optHelp options -> usage
      | Just path <- optFile options -> do
          source <- readFile path
          ok <- analyzeFile (optVerbose options) path source
          if ok then pure () else exitFailure
      | null (optExpressionParts options) -> do
          usage
          repl True
      | otherwise -> do
          ok <- analyzeExpression (optVerbose options) (unwords (optExpressionParts options))
          if ok then pure () else exitFailure

parseOptions :: [String] -> Either String Options
parseOptions = go defaultOptions
  where
    go options [] = Right options
    go options ("--help":rest) = go options { optHelp = True } rest
    go options ("--verbose":rest) = go options { optVerbose = True } rest
    go options ("--file":path:rest) = go options { optFile = Just path } rest
    go options ("file":path:rest) = go options { optFile = Just path } rest
    go _ ["--file"] = Left "--file 后面需要文件路径"
    go _ ["file"] = Left "file 后面需要文件路径"
    go options ("expr":rest) = go options rest
    go _ (flag:_) | "--" `prefixOf` flag = Left ("未知选项：" ++ flag)
    go options (part:rest) =
      go options { optExpressionParts = optExpressionParts options ++ [part] } rest

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

repl :: Bool -> IO ()
repl verbose = do
  putStrLn "请输入表达式。输入 :quit 退出，:help 查看帮助，:examples 运行示例。"
  putStrLn "REPL 默认会展示详细解析过程。"
  loop
  where
    loop = do
      putStr "Pseudo-W> "
      hFlush stdout
      source <- getLine
      case source of
        ":quit" -> putStrLn "已退出。"
        ":q" -> putStrLn "已退出。"
        ":help" -> usage >> loop
        ":examples" -> mapM_ runExample examples >> loop
        "" -> loop
        _ -> do
          _ <- analyzeExpression verbose source
          putStrLn ""
          loop

runExample :: (String, String) -> IO ()
runExample (label, source) = do
  putStrLn ("== 示例：" ++ label ++ " ==")
  _ <- analyzeExpression True source
  putStrLn ""

analyzeExpression :: Bool -> String -> IO Bool
analyzeExpression verbose source =
  case parseExpressionTrace source of
    Left err -> putStrLn ("解析失败：" ++ err) >> pure False
    Right trace -> do
      if verbose then printExpressionTrace trace else pure ()
      putStrLn ("原始输入：" ++ source)
      putStrLn ("规范表达式：" ++ renderExp (traceExp trace))
      case inferTop (traceExp trace) of
        Left err -> putStrLn ("类型检查失败：" ++ show err) >> pure False
        Right scheme -> putStrLn ("推断类型：" ++ show scheme) >> pure True

analyzeFile :: Bool -> FilePath -> String -> IO Bool
analyzeFile verbose path source
  | ".pseudo" `suffixOfCI` path = analyzePseudoFile verbose path source
  | ".aw" `suffixOfCI` path = do
      putStrLn ("正在分析文件：" ++ path)
      analyzeExpression verbose source
  | otherwise = do
      putStrLn ("不支持的文件类型：" ++ path)
      putStrLn "  .pseudo 文件会按伪代码 function block 解析。"
      putStrLn "  .aw 文件会按 REPL 风格标准表达式解析。"
      pure False

suffixOfCI :: String -> String -> Bool
suffixOfCI suffix value = reverse (lower suffix) `prefixOf` reverse (lower value)
  where
    lower = map toLower

analyzePseudoFile :: Bool -> FilePath -> String -> IO Bool
analyzePseudoFile verbose path source =
  case parsePseudoFileTrace source of
    Left err -> do
      putStrLn ("文件解析失败：" ++ path)
      putStrLn ("原因：" ++ err)
      pure False
    Right functions -> do
      putStrLn ("正在分析文件：" ++ path)
      putStrLn ("发现函数数量：" ++ show (length functions))
      putStrLn ""
      analyzeFunctions verbose preludeEnv functions True

analyzeFunctions :: Bool -> TypeEnv -> [PseudoFunction] -> Bool -> IO Bool
analyzeFunctions _ _ [] ok = pure ok
analyzeFunctions verbose env (fn:fns) ok = do
  let expr = expressionOfFunction fn
  if verbose then printFunctionTrace fn expr else pure ()
  case inferTopWithEnv env expr of
    Left err -> do
      putStrLn ("[TYPE ERROR] " ++ pseudoName fn ++ "（从第 " ++ show (pseudoStartLine fn) ++ " 行开始）")
      putStrLn "  源代码："
      putStr (indent (pseudoSource fn))
      putStrLn ("  类型错误：" ++ show err)
      putStrLn ""
      analyzeFunctions verbose env fns False
    Right scheme -> do
      let env' = extend env (pseudoName fn) scheme
      putStrLn ("[OK] " ++ pseudoName fn ++ "（第 " ++ show (pseudoStartLine fn) ++ " 行）")
      putStrLn ("  参数：" ++ show (pseudoArgs fn))
      putStrLn ("  函数体：" ++ pseudoBodySource fn)
      putStrLn ("  推断类型：" ++ show scheme)
      putStrLn ""
      analyzeFunctions verbose env' fns ok

printExpressionTrace :: ParseTrace -> IO ()
printExpressionTrace trace = do
  putStrLn "[详细解析] 第 1 步：我先读到这段输入"
  putStr (indent (traceSource trace))
  putStrLn "[详细解析] 第 2 步：我把它拆成这些词法单元"
  putStrLn ("  " ++ renderTokens (traceTokens trace))
  putStrLn "[详细解析] 第 3 步：我按语法这样理解它"
  putStr (describeExp (traceExp trace))
  putStrLn "[详细解析] 第 4 步：整理成内部统一使用的表达式"
  putStrLn ("  " ++ renderExp (traceExp trace))
  putStrLn "[详细解析] 第 5 步：接下来把这个表达式交给 Algorithm W 推断类型"

printFunctionTrace :: PseudoFunction -> Exp -> IO ()
printFunctionTrace fn expr = do
  putStrLn ("[详细解析] 函数：" ++ pseudoName fn ++ "，从第 " ++ show (pseudoStartLine fn) ++ " 行开始")
  putStrLn "  源代码："
  putStr (indent (pseudoSource fn))
  putStrLn ("  参数：" ++ show (pseudoArgs fn))
  putStrLn ("  选中的函数体：" ++ pseudoBodySource fn)
  putStrLn "  我对这个函数的理解："
  putStr (indent (describeExp expr))
  putStrLn ("  规范化函数表达式：" ++ renderExp expr)

describeExp :: Exp -> String
describeExp = unlines . describe 0
  where
    describe level expr =
      case expr of
        EVar name ->
          [pad level ++ "- 这里用到了变量 `" ++ name ++ "`，它的类型会从当前环境里查。"]
        ELit lit ->
          [pad level ++ "- 这里是字面量 `" ++ show lit ++ "`，它本身就能确定基础类型。"]
        EApp fun arg ->
          [pad level ++ "- 这里是在调用一个函数。我会先确认左边确实是函数，再检查右边参数能不能喂给它。"]
          ++ describe (level + 1) fun
          ++ describe (level + 1) arg
        EAbs name body ->
          [pad level ++ "- 这里定义了一个匿名函数，参数叫 `" ++ name ++ "`。参数类型暂时未知，后面会由函数体约束出来。"]
          ++ describe (level + 1) body
        ELet name value body ->
          [pad level ++ "- 这里有一个局部定义：先计算 `" ++ name ++ "` 的类型。"]
          ++ describe (level + 1) value
          ++ [pad level ++ "- 然后带着这个定义继续检查后面的表达式。"]
          ++ describe (level + 1) body
        EIf cond yes no ->
          [pad level ++ "- 这里是条件表达式。我会要求条件部分是 Bool，并要求两个分支最后给出同一种类型。"
          ,pad level ++ "  我先看条件："]
          ++ describe (level + 1) cond
          ++ [pad level ++ "  条件为真时走这里："]
          ++ describe (level + 1) yes
          ++ [pad level ++ "  条件为假时走这里："]
          ++ describe (level + 1) no
        EBin op left right ->
          [pad level ++ "- 这里是二元运算 `" ++ show op ++ "`。我会分别看左右两边，再根据这个运算符要求它们的类型。"
          ,pad level ++ "  左边是："]
          ++ describe (level + 1) left
          ++ [pad level ++ "  右边是："]
          ++ describe (level + 1) right
        EList items ->
          [pad level ++ "- 这里是一个列表。我会检查每个元素，并要求所有元素类型一致。"]
          ++ concatMap (describe (level + 1)) items
        EBlock stmts outputName ->
          [pad level ++ "- 这里是一个伪代码语句块。我会从上到下检查每条语句，最后读取输出变量 `" ++ outputName ++ "` 的类型。"]
          ++ concatMap (describeStmt (level + 1)) stmts

    pad level = replicate (level * 2) ' '

    describeStmt level stmt =
      case stmt of
        SAssign name expr ->
          [pad level ++ "- 给变量 `" ++ name ++ "` 赋值；如果它之前出现过，这次赋值必须保持同一种类型。"]
          ++ describe (level + 1) expr
        SIfStmt cond yes no ->
          [pad level ++ "- 这里是 block if。条件必须是 Bool；then 和 else 两边对变量的类型约束必须兼容。"
          ,pad level ++ "  条件是："]
          ++ describe (level + 1) cond
          ++ [pad level ++ "  条件为真时执行："]
          ++ concatMap (describeStmt (level + 1)) yes
          ++ [pad level ++ "  条件为假时执行："]
          ++ concatMap (describeStmt (level + 1)) no
        SSwitchStmt subject cases otherwiseBranch ->
          [pad level ++ "- 这里是 switch。我会检查 switch 对象和每个 case 值能否比较，并合并各分支的类型约束。"
          ,pad level ++ "  switch 对象是："]
          ++ describe (level + 1) subject
          ++ concatMap (describeCase level) cases
          ++ [pad level ++ "  otherwise 分支："]
          ++ concatMap (describeStmt (level + 1)) otherwiseBranch
        SFor itemName items body ->
          [pad level ++ "- 这里是 for 循环。`" ++ itemName ++ "` 每次从列表里取一个元素，所以被循环的对象必须是列表。"
          ,pad level ++ "  被遍历的列表是："]
          ++ describe (level + 1) items
          ++ [pad level ++ "  循环体中执行："]
          ++ concatMap (describeStmt (level + 1)) body
        SWhile cond body ->
          [pad level ++ "- 这里是 while 循环。循环条件必须是 Bool，循环体里的赋值需要保持类型稳定。"
          ,pad level ++ "  循环条件是："]
          ++ describe (level + 1) cond
          ++ [pad level ++ "  循环体中执行："]
          ++ concatMap (describeStmt (level + 1)) body

    describeCase level (caseValue, body) =
      [pad level ++ "  case 值："]
      ++ describe (level + 1) caseValue
      ++ [pad level ++ "  case 分支执行："]
      ++ concatMap (describeStmt (level + 1)) body

usage :: IO ()
usage = do
  putStrLn "Algorithm W / Pseudo-W"
  putStrLn ""
  putStrLn "用法："
  putStrLn "  cabal run algorithm-w"
  putStrLn "  cabal run algorithm-w -- expr \"let id = \\x -> x in id 3\""
  putStrLn "  cabal run algorithm-w -- --verbose \"if 1 < 2 then 10 else 20\""
  putStrLn "  cabal run algorithm-w -- --file examples/id.aw             # REPL 风格标准表达式"
  putStrLn "  cabal run algorithm-w -- --file examples/main-branch.pseudo # MATLAB-like 伪代码"
  putStrLn ""
  putStrLn "表达式能力："
  putStrLn "  变量、Int/Bool 字面量、lambda、let、if/then/else"
  putStrLn "  运算符：+, -, *, /, <, <=, >, >=, ==, !=, ~=, &&, ||, not, ~"
  putStrLn ""
  putStrLn "MATLAB-like 伪代码："
  putStrLn "  function z = affine(x)"
  putStrLn "    shifted = x + 1"
  putStrLn "    scaled = shifted * 2"
  putStrLn "    z = scaled + 3"
  putStrLn "  end"
  putStrLn ""
  putStrLn "  function y = clampSign(x)"
  putStrLn "    if x < 0"
  putStrLn "      y = -1"
  putStrLn "    elseif x == 0"
  putStrLn "      y = 0"
  putStrLn "    else"
  putStrLn "      y = 1"
  putStrLn "    end"
  putStrLn "  end"
  putStrLn ""
  putStrLn "  function y = codeScore(code)"
  putStrLn "    switch code"
  putStrLn "      case 0"
  putStrLn "        base = 90"
  putStrLn "        y = base + 10"
  putStrLn "      otherwise"
  putStrLn "        y = 0"
  putStrLn "    end"
  putStrLn "  end"

indent :: String -> String
indent = unlines . map ("  " ++) . lines
