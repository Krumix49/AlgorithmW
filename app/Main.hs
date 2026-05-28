-- 测试demo
import qualified Data.Map as Map
import Syntax
import Inference
import MonadStack
import Pretty ()

e0 = ELet "id" (EAbs "x" (EVar "x")) (EVar "id")
e1 = ELet "id" (EAbs "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
e2 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EVar "id") (EVar "id"))
e3 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EApp (EVar "id") (EVar "id")) (ELit (LInt 2)))
e4 = ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x"))) (EVar "id")
e5 = EAbs "m" (ELet "y" (EVar "m") (ELet "x" (EApp (EVar "y") (ELit (LBool True))) (EVar "x")))
e6 = EApp (ELit (LInt 2)) (ELit (LInt 2))

examples :: [(String, Exp)]
examples =
    [ ("恒等函数", e0)
    , ("多态恒等函数应用到自身", e1)
    , ("通过内层 let 返回参数的恒等函数", e2)
    , ("恒等函数链应用到整数", e3)
    , ("自应用，预期会失败", e4)
    , ("接收 Bool 参数的函数", e5)
    , ("把整数当作函数调用，预期会失败", e6)
    ]

indent :: String -> String
indent = unlines . map ("  " ++) . lines

test :: Int -> (String, Exp) -> IO ()
test n (label, e) = do
    (res, _) <- runTI (typeInference Map.empty e)
    putStrLn $ "示例 " ++ show n ++ "：" ++ label
    putStrLn "表达式："
    putStr $ indent (show e)
    case res of
        Left err -> do
            putStrLn "结果：类型错误"
            putStrLn "原因："
            putStr $ indent err
        Right t -> do
            putStrLn "结果：推断出的类型"
            putStrLn $ "  " ++ show t
    putStrLn ""

main :: IO ()
main = mapM_ (uncurry test) (zip [0 :: Int ..] examples)
-- Collecting Constraints
-- main = mapM _ test' [e0, e1, e2, e3, e4, e5]
