import Control.Applicative (Alternative (..))
import Control.Monad (when)
import qualified Data.Map as Map
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Inference
import MonadStack
import Pretty ()
import Syntax

data Token
    = TokIdent String
    | TokInt Integer
    | TokBool Bool
    | TokLet
    | TokIn
    | TokLambda
    | TokArrow
    | TokEquals
    | TokLParen
    | TokRParen
    deriving (Eq, Show)

newtype Parser a = Parser {runParser :: [Token] -> Either String (a, [Token])}

instance Functor Parser where
    fmap f p = Parser $ \tokens -> do
        (x, rest) <- runParser p tokens
        return (f x, rest)

instance Applicative Parser where
    pure x = Parser $ \tokens -> Right (x, tokens)
    pf <*> px = Parser $ \tokens -> do
        (f, rest) <- runParser pf tokens
        (x, rest') <- runParser px rest
        return (f x, rest')

instance Monad Parser where
    p >>= f = Parser $ \tokens -> do
        (x, rest) <- runParser p tokens
        runParser (f x) rest

instance Alternative Parser where
    empty = Parser $ const (Left "没有可用的语法分支")
    p <|> q = Parser $ \tokens ->
        case runParser p tokens of
            Left _ -> runParser q tokens
            ok -> ok

main :: IO ()
main = do
    args <- getArgs
    source <- case args of
        ["--help"] -> do
            printTutorial
            return ""
        [] -> do
            printTutorial
            putStrLn "请输入表达式，然后按 Enter："
            getLine
        _ -> return (unwords args)
    unlessEmpty source (runUserInput source)

unlessEmpty :: String -> IO () -> IO ()
unlessEmpty source action =
    when (not (null source)) action

printTutorial :: IO ()
printTutorial = do
    putStrLn "Algorithm W 用户输入教程"
    putStrLn ""
    putStrLn "当前支持的表达式："
    putStrLn "  变量：x, id, f"
    putStrLn "  字面量：2, True, False"
    putStrLn "  函数：\\x -> x"
    putStrLn "  多参数函数：\\x y -> x   会被解析成 \\x -> \\y -> x"
    putStrLn "  函数应用：f x, f x y"
    putStrLn "  let 绑定：let id = \\x -> x in id"
    putStrLn "  括号：用于明确优先级，例如 (\\x -> x) 2"
    putStrLn ""
    putStrLn "注意："
    putStrLn "  let 和 lambda 作为函数或参数出现时需要加括号。"
    putStrLn "  目前不支持 if、算术运算符、比较运算符、列表、元组、类型标注或递归定义。"
    putStrLn ""
    putStrLn "可以尝试："
    putStrLn "  \\x -> x"
    putStrLn "  \\x y -> x"
    putStrLn "  let id = \\x -> x in id id"
    putStrLn "  let const = \\x y -> x in const 2 True"
    putStrLn "  let apply = \\f x -> f x in apply (\\n -> n) 2"
    putStrLn "  2 2"
    putStrLn ""

runUserInput :: String -> IO ()
runUserInput source =
    case parseInput source of
        Left err -> do
            putStrLn "输入被拒绝：表达式不合法或不规范。"
            putStrLn $ "原因：" ++ err
            exitFailure
        Right expr -> do
            putStrLn "解析成功。"
            putStrLn "转换为 Main.hs 风格的表达式："
            putStrLn $ "  " ++ renderExp expr
            putStrLn "漂亮输出："
            putStrLn $ indent (show expr)
            (res, _) <- runTI (typeInference Map.empty expr)
            case res of
                Left err -> do
                    putStrLn "类型检查失败。"
                    putStrLn "原因："
                    putStr $ indent err
                    exitFailure
                Right typ -> do
                    putStrLn "类型检查通过。"
                    putStrLn $ "推断出的类型：" ++ show typ

parseInput :: String -> Either String Exp
parseInput source = do
    tokens <- tokenize source
    when (null tokens) (Left "输入为空。")
    (expr, rest) <- runParser parseExpr tokens
    case rest of
        [] -> return expr
        unexpected -> Left $ "表达式已经结束，但后面仍有多余内容：" ++ show unexpected

tokenize :: String -> Either String [Token]
tokenize = go
  where
    go [] = Right []
    go (c:cs)
        | isSpace c = go cs
        | isAlpha c || c == '_' = scanIdent (c:cs)
        | isDigit c = scanInt (c:cs)
        | c == '\\' = (TokLambda :) <$> go cs
        | c == '-' =
            case cs of
                ('>':rest) -> (TokArrow :) <$> go rest
                _ -> Left "发现 '-'；本语言不支持负数字面量或中缀运算符，只能在 lambda 中使用 '->'。"
        | c == '=' = (TokEquals :) <$> go cs
        | c == '(' = (TokLParen :) <$> go cs
        | c == ')' = (TokRParen :) <$> go cs
        | otherwise = Left $ "发现不支持的字符：" ++ [c]

    scanIdent input =
        let (name, rest) = span isIdentChar input
        in case name of
            "let" -> (TokLet :) <$> go rest
            "in" -> (TokIn :) <$> go rest
            "True" -> (TokBool True :) <$> go rest
            "False" -> (TokBool False :) <$> go rest
            _ | validIdent name -> (TokIdent name :) <$> go rest
              | otherwise -> Left $ "非法变量名：" ++ name

    scanInt input =
        let (digits, rest) = span isDigit input
        in (TokInt (read digits) :) <$> go rest

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

validIdent :: String -> Bool
validIdent [] = False
validIdent name@(c:_) =
    (isAlpha c || c == '_') &&
    all isIdentChar name &&
    name `notElem` ["let", "in", "True", "False"]

parseExpr :: Parser Exp
parseExpr = Parser $ \tokens ->
    case tokens of
        TokLet:_ -> runParser parseLet tokens
        TokLambda:_ -> runParser parseLambda tokens
        [] -> Left "表达式不完整。"
        _ -> runParser parseApp tokens

parseLet :: Parser Exp
parseLet = do
    expect TokLet "期望 let"
    name <- ident "let 后面必须跟变量名。"
    expect TokEquals "let 绑定中缺少 '='。"
    bound <- parseExpr
    expect TokIn "let 绑定中缺少 in。"
    body <- parseExpr
    return (ELet name bound body)

parseLambda :: Parser Exp
parseLambda = do
    expect TokLambda "期望 lambda。"
    names <- some (ident "lambda 后面必须跟至少一个参数名。")
    expect TokArrow "lambda 参数后缺少 '->'。"
    body <- parseExpr
    return (foldr EAbs body names)

parseApp :: Parser Exp
parseApp = do
    atoms <- some parseAtom
    return (foldl1 EApp atoms)

parseAtom :: Parser Exp
parseAtom = Parser $ \tokens ->
    case tokens of
        TokIdent name:rest -> Right (EVar name, rest)
        TokInt n:rest -> Right (ELit (LInt n), rest)
        TokBool b:rest -> Right (ELit (LBool b), rest)
        TokLParen:_ -> runParser parseParen tokens
        TokLet:_ -> Left "let 表达式作为函数或参数使用时必须加括号。"
        TokLambda:_ -> Left "lambda 表达式作为函数或参数使用时必须加括号。"
        TokIn:_ -> Left "遇到 in，当前子表达式结束。"
        TokRParen:_ -> Left "遇到右括号，当前子表达式结束。"
        TokArrow:_ -> Left "意外的 '->'。"
        TokEquals:_ -> Left "意外的 '='。"
        [] -> Left "表达式不完整。"

parseParen :: Parser Exp
parseParen = do
    expect TokLParen "期望 '('。"
    expr <- parseExpr
    expect TokRParen "括号表达式缺少 ')'。"
    return expr

ident :: String -> Parser String
ident message = Parser $ \tokens ->
    case tokens of
        TokIdent name:rest -> Right (name, rest)
        _ -> Left message

expect :: Token -> String -> Parser ()
expect token message = Parser $ \tokens ->
    case tokens of
        t:rest | t == token -> Right ((), rest)
        _ -> Left message

renderExp :: Exp -> String
renderExp (EVar name) = "EVar " ++ show name
renderExp (ELit lit) = "ELit (" ++ renderLit lit ++ ")"
renderExp (EApp f x) = "EApp (" ++ renderExp f ++ ") (" ++ renderExp x ++ ")"
renderExp (EAbs name body) = "EAbs " ++ show name ++ " (" ++ renderExp body ++ ")"
renderExp (ELet name bound body) =
    "ELet " ++ show name ++ " (" ++ renderExp bound ++ ") (" ++ renderExp body ++ ")"

renderLit :: Lit -> String
renderLit (LInt n) = "LInt " ++ show n
renderLit (LBool True) = "LBool True"
renderLit (LBool False) = "LBool False"

indent :: String -> String
indent = unlines . map ("  " ++) . lines
