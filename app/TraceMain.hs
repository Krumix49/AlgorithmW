module Main where

import System.Environment (getArgs)       -- 括号表示选择性导入
import System.Exit (exitFailure)

import Derivation.InferExamples (InferUnit(..), inferUnits)
import Derivation.Trace (inferTopTrace)
import Parser (parseExp)
import Types (TypeError)

examples :: [(String, String)]
examples =
  [ (iuLabel unit, iuSource unit) | unit <- inferUnits ]

main :: IO ()                           -- main 是程序的入口点，IO 表示这是一个 IO 操作。IO操作是Haskell中用于处理输入输出的操作。
main = do                               -- do 表示一个代码块，从上往下顺序执行。
  args <- getArgs                       -- getArgs 表示获取命令行参数。args是一个字符串列表。
  case args of
    [] -> mapM_ runExample examples     -- mapM_ :: (a -> IO ()) -> [a] -> IO ()。map表示对每个元素应用一个函数，_表示忽略结果，M表示Monad。
    ["expr", source] -> inferAndPrint source
    ["file", path] -> readFile path >>= inferAndPrint    -- >>= 表示将左边的结果作为右边的参数。
    _ -> usage >> exitFailure                            -- _ 表示忽略结果。

runExample :: (String, String) -> IO ()                   -- (String, String) 表示一个元组，String 表示字符串。
runExample (label, source) = do
  putStrLn ("== " ++ label ++ " ==")
  inferAndPrint source
  putStrLn ""

inferAndPrint :: String -> IO ()
inferAndPrint source =
  case parseExp source of                   -- case 表示模式匹配，parseExp 是一个函数，返回一个 Either TypeError Exp。
    Left err -> printError err              -- 解析出错，打印错误。
    Right expr -> do
      putStrLn ("source: " ++ source)
      putStrLn ("ast:    " ++ show expr)
      case inferTopTrace expr of              -- inferTopTrace 是一个函数，返回一个 Either TypeError (Scheme, String)。
        Left err -> printError err
        Right (scheme, trace) -> do
          putStrLn "--- derivation ---"
          putStrLn trace
          putStrLn "--- result ---"
          putStrLn ("type:   " ++ show scheme)

printError :: TypeError -> IO ()            -- 
printError err = putStrLn ("error:  " ++ show err)

usage :: IO ()
usage = do
  putStrLn "Algorithm W reimplementation (with derivation trace)"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  cabal run algorithm-w-trace"
  putStrLn "  cabal run algorithm-w-trace -- expr \"let id = \\x -> x in id 3\""
  putStrLn "  cabal run algorithm-w-trace -- file examples/id.aw"
