module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Infer
import Parser
import Types

examples :: [(String, String)]
examples =
  [ ("identity", "let id = \\x -> x in id")
  , ("identity applied to itself", "let id = \\x -> x in id id")
  , ("identity applied to int", "let id = \\x -> x in id 2")
  , ("lambda application", "(\\x -> x + 1) 41")
  , ("if expression", "if True then 1 else 2")
  , ("occurs check failure", "let bad = \\x -> x x in bad")
  , ("calling an integer", "2 2")
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> mapM_ runExample examples
    ["expr", source] -> inferAndPrint source
    ["file", path] -> readFile path >>= inferAndPrint
    _ -> usage >> exitFailure

runExample :: (String, String) -> IO ()
runExample (label, source) = do
  putStrLn ("== " ++ label ++ " ==")
  inferAndPrint source
  putStrLn ""

inferAndPrint :: String -> IO ()
inferAndPrint source =
  case parseExp source of
    Left err -> printError err
    Right expr -> do
      putStrLn ("source: " ++ source)
      putStrLn ("ast:    " ++ show expr)
      case inferTop expr of
        Left err -> printError err
        Right scheme -> putStrLn ("type:   " ++ show scheme)

printError :: TypeError -> IO ()
printError err = putStrLn ("error:  " ++ show err)

usage :: IO ()
usage = do
  putStrLn "Algorithm W reimplementation"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  cabal run algorithm-w"
  putStrLn "  cabal run algorithm-w -- expr \"let id = \\x -> x in id 3\""
  putStrLn "  cabal run algorithm-w -- file examples/id.aw"
