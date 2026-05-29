import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)

import Builtins
import Inference
import MonadStack
import Parser
import Pretty ()
import Syntax
import TypeEnv

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

main :: IO ()
main = do
    args <- getArgs
    case parseOptions args of
        Left err -> do
            putStrLn $ "参数错误：" ++ err
            putStrLn "使用 --help 查看用法。"
            exitFailure
        Right options
            | optHelp options -> printTutorial
            | Just path <- optFile options -> do
                source <- readFile path
                ok <- analyzePseudoFile (optVerbose options) path source
                if ok then return () else exitFailure
            | null (optExpressionParts options) -> do
                printTutorial
                repl (optVerbose options)
            | otherwise -> do
                ok <- analyzeExpression (optVerbose options) (unwords (optExpressionParts options))
                if ok then return () else exitFailure

parseOptions :: [String] -> Either String Options
parseOptions = go defaultOptions
  where
    go options [] = Right options
    go options ("--help":rest) = go options {optHelp = True} rest
    go options ("--verbose":rest) = go options {optVerbose = True} rest
    go options ("--file":path:rest) = go options {optFile = Just path} rest
    go _ ["--file"] = Left "--file 后面缺少文件路径。"
    go _ (flag:_) | "--" `prefixOf` flag = Left $ "未知选项：" ++ flag
    go options (part:rest) =
        go options {optExpressionParts = optExpressionParts options ++ [part]} rest

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

repl :: Bool -> IO ()
repl verbose = do
    putStrLn "请输入表达式，然后按 Enter。输入 :quit 退出，输入 :help 查看教程。"
    loop
  where
    loop = do
        putStr "Pseudo-W> "
        hFlush stdout
        source <- getLine
        case source of
            ":quit" -> putStrLn "已退出。"
            ":q" -> putStrLn "已退出。"
            ":help" -> printTutorial >> loop
            "" -> loop
            _ -> do
                _ <- analyzeExpression verbose source
                putStrLn ""
                loop

analyzeExpression :: Bool -> String -> IO Bool
analyzeExpression verbose source =
    case parseExpressionTrace source of
        Left err -> do
            putStrLn "输入被拒绝：表达式不合法或不规范。"
            putStrLn $ "原因：" ++ err
            return False
        Right trace -> do
            whenVerbose verbose $ printExpressionTrace trace
            putStrLn "解析成功。"
            putStrLn "转换为 Main.hs 风格的表达式："
            putStrLn $ "  " ++ renderExp (traceExp trace)
            putStrLn "漂亮输出："
            putStrLn $ indent (show (traceExp trace))
            (res, _) <- runTI (typeInference builtinEnv (traceExp trace))
            case res of
                Left err -> do
                    putStrLn "类型检查失败。"
                    putStrLn "原因："
                    putStr $ indent err
                    return False
                Right typ -> do
                    putStrLn "类型检查通过。"
                    putStrLn $ "推断出的类型：" ++ show typ
                    return True

analyzePseudoFile :: Bool -> FilePath -> String -> IO Bool
analyzePseudoFile verbose path source =
    case parsePseudoFileTrace source of
        Left err -> do
            putStrLn $ "文件解析失败：" ++ path
            putStrLn $ "原因：" ++ err
            return False
        Right functions -> do
            putStrLn $ "分析文件：" ++ path
            putStrLn $ "发现函数数量：" ++ show (length functions)
            putStrLn ""
            analyzeFunctions verbose builtinEnv functions True

analyzeFunctions :: Bool -> Map.Map String Scheme -> [PseudoFunction] -> Bool -> IO Bool
analyzeFunctions _ _ [] ok = return ok
analyzeFunctions verbose env (fn:fns) ok = do
    let expr = expressionOfFunction fn
    whenVerbose verbose (printFunctionTrace fn expr)
    (res, _) <- runTI (typeInference env expr)
    case res of
        Left err -> do
            putStrLn $ "[TYPE ERROR] " ++ pseudoName fn
            putStrLn "  源代码："
            putStr $ indent (pseudoSource fn)
            putStrLn "  原因："
            putStr $ indent err
            putStrLn ""
            analyzeFunctions verbose env fns False
        Right typ -> do
            let scheme = generalize (TypeEnv env) typ
                env' = Map.insert (pseudoName fn) scheme env
            putStrLn $ "[OK] " ++ pseudoName fn
            putStrLn $ "  参数：" ++ show (pseudoArgs fn)
            putStrLn $ "  函数体：" ++ pseudoBodySource fn
            putStrLn $ "  类型：" ++ show typ
            putStrLn ""
            analyzeFunctions verbose env' fns ok

printExpressionTrace :: ParseTrace -> IO ()
printExpressionTrace trace = do
    putStrLn "[verbose] 原始输入："
    putStr $ indent (traceSource trace)
    putStrLn "[verbose] 词法单元："
    putStrLn $ "  " ++ renderTokens (traceTokens trace)
    putStrLn "[verbose] 内部表达式："
    putStrLn $ "  " ++ renderExp (traceExp trace)

printFunctionTrace :: PseudoFunction -> Exp -> IO ()
printFunctionTrace fn expr = do
    putStrLn $ "[verbose] function " ++ pseudoName fn
    putStrLn "  源代码："
    putStr $ indent (pseudoSource fn)
    putStrLn $ "  参数：" ++ show (pseudoArgs fn)
    putStrLn $ "  选中的函数体表达式：" ++ pseudoBodySource fn
    putStrLn $ "  函数表达式：" ++ renderExp expr
    case parseExpressionTrace (pseudoBodySource fn) of
        Left _ -> return ()
        Right trace ->
            putStrLn $ "  函数体词法单元：" ++ renderTokens (traceTokens trace)

whenVerbose :: Bool -> IO () -> IO ()
whenVerbose verbose action =
    if verbose then action else return ()

printTutorial :: IO ()
printTutorial = do
    putStrLn "Pseudo-W / Algorithm W 用户输入教程"
    putStrLn ""
    putStrLn "单表达式模式："
    putStrLn "  cabal run AlgorithmW-user -- \"let id = \\x -> x in id id\""
    putStrLn "  cabal run AlgorithmW-user -- --verbose \"if 1 < 2 then 10 else 20\""
    putStrLn ""
    putStrLn "文件模式："
    putStrLn "  cabal run AlgorithmW-user -- --file examples.pseudo"
    putStrLn "  cabal run AlgorithmW-user -- --verbose --file examples.pseudo"
    putStrLn ""
    putStrLn "当前支持的表达式："
    putStrLn "  变量：x, id, flag"
    putStrLn "  字面量：2, True, False, true, false"
    putStrLn "  函数：\\x -> x"
    putStrLn "  多参数函数：\\x y -> x"
    putStrLn "  let 绑定：let id = \\x -> x in id"
    putStrLn "  条件：if x > 0 then x else 0"
    putStrLn "  整数运算：+, -, *, /"
    putStrLn "  比较：<, <=, >, >=, ==, !=, ~="
    putStrLn "  布尔运算：&&, ||, not, ~"
    putStrLn ""
    putStrLn "MATLAB-like pseudocode 文件格式："
    putStrLn "  % comment"
    putStrLn "  function y = addOne(x)"
    putStrLn "    y = x + 1"
    putStrLn "  end"
    putStrLn ""
    putStrLn "  function z = affine(x)"
    putStrLn "    shifted = x + 1"
    putStrLn "    scaled = shifted * 2"
    putStrLn "    z = scaled + 3"
    putStrLn "  end"
    putStrLn ""
    putStrLn "  function m = max2(a, b)"
    putStrLn "    m = if a > b then a else b"
    putStrLn "  end"
    putStrLn ""
    putStrLn "  function y = absVal(x)"
    putStrLn "    if x < 0"
    putStrLn "      flipped = -x;"
    putStrLn "      y = flipped;"
    putStrLn "    else"
    putStrLn "      y = x;"
    putStrLn "    end"
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
    putStrLn "      case 1"
    putStrLn "        y = 80"
    putStrLn "      otherwise"
    putStrLn "        y = 0"
    putStrLn "    end"
    putStrLn "  end"
    putStrLn ""
    putStrLn "也支持更短的单行函数："
    putStrLn "  function identity(x) = x"
    putStrLn ""
    putStrLn "当前限制：多语句只支持局部变量赋值，最后一行必须给输出变量赋值。"
    putStrLn "block if/switch 的每个分支也遵循同样规则。"
    putStrLn "暂不支持递归、多语句赋值流程、数组、矩阵、循环或 fall-through case。"
    putStrLn ""

indent :: String -> String
indent = unlines . map ("  " ++) . lines
