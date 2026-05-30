module Parser
  ( parseExp
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

import Syntax
import Types (TypeError(..))

data Token
  = TokIdent String
  | TokInt Integer
  | TokBool Bool
  | TokLet
  | TokIn
  | TokIf
  | TokThen
  | TokElse
  | TokLambda
  | TokArrow
  | TokEquals
  | TokOp String
  | TokLParen  -- 左括号
  | TokRParen  -- 右括号
  | TokEOF  -- 文件结束
  deriving (Eq, Show)

newtype Parser a = Parser { runParser :: [Token] -> Either String (a, [Token]) }

-- 这三个和Infer里的基本一致
instance Functor Parser where
  fmap f parser = Parser $ \tokens ->
    case runParser parser tokens of
      Left err -> Left err
      Right (value, rest) -> Right (f value, rest)

instance Applicative Parser where
  pure value = Parser $ \tokens -> Right (value, tokens)
  pf <*> px = Parser $ \tokens ->
    case runParser pf tokens of
      Left err -> Left err
      Right (f, rest) ->
        case runParser px rest of
          Left err -> Left err
          Right (x, rest') -> Right (f x, rest')

instance Monad Parser where
  parser >>= next = Parser $ \tokens ->
    case runParser parser tokens of
      Left err -> Left err
      Right (value, rest) -> runParser (next value) rest

parseExp :: String -> Either TypeError Exp
parseExp input =
  case tokenize input of
    Left err -> Left (ParseFailure err)
    Right tokens ->
      case runParser (parseExpr <* expectEOF) tokens of
        Left err -> Left (ParseFailure err)
        Right (expr, _) -> Right expr

tokenize :: String -> Either String [Token]
tokenize [] = Right [TokEOF]
tokenize (c:cs)
  | isSpace c = tokenize cs  -- 跳过空格
  | isAlpha c = tokenizeIdent (c:cs)
  | isDigit c = tokenizeInt (c:cs)
  | c == '\\' = cons TokLambda <$> tokenize cs  -- <$>就是fmap，cons TokLambda :: [Token] -> [Token]
  | c == '(' = cons TokLParen <$> tokenize cs
  | c == ')' = cons TokRParen <$> tokenize cs
  | c == '-' && take 1 cs == ">" = cons TokArrow <$> tokenize (drop 1 cs)
  | c == '=' && take 1 cs == "=" = cons (TokOp "==") <$> tokenize (drop 1 cs)  -- 判断优先级大于单个=
  | c == '=' = cons TokEquals <$> tokenize cs
  | c == '&' && take 1 cs == "&" = cons (TokOp "&&") <$> tokenize (drop 1 cs)
  | c == '|' && take 1 cs == "|" = cons (TokOp "||") <$> tokenize (drop 1 cs)
  | c `elem` "+-*" = cons (TokOp [c]) <$> tokenize cs
  | otherwise = Left ("unexpected character: " ++ [c])
  where
    cons token tokens = token : tokens

tokenizeIdent :: String -> Either String [Token]
tokenizeIdent input =
  let (name, rest) = span isIdentChar input  -- span根据要求切列表
      token = case name of
        "let" -> TokLet
        "in" -> TokIn
        "if" -> TokIf
        "then" -> TokThen
        "else" -> TokElse
        "True" -> TokBool True
        "False" -> TokBool False
        _ -> TokIdent name  -- 非关键字就生成普通变量名
  in (token :) <$> tokenize rest

tokenizeInt :: String -> Either String [Token]
tokenizeInt input =
  let (digits, rest) = span isDigit input
  in (TokInt (read digits) :) <$> tokenize rest  -- read是把字符串转成整数

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''  -- 字母、数字、下划线、单引号

parseExpr :: Parser Exp
parseExpr = parseLet

parseLet :: Parser Exp  -- 优先级低的先解析
parseLet = do  -- let name = value in body
  token <- peek
  case token of
    TokLet -> do
      advance
      name <- parseIdentifier
      expect TokEquals
      value <- parseExpr
      expect TokIn
      body <- parseExpr
      pure (ELet name value body)
    _ -> parseLambda

parseLambda :: Parser Exp
parseLambda = do
  token <- peek
  case token of
    TokLambda -> do
      advance
      name <- parseIdentifier
      expect TokArrow
      EAbs name <$> parseExpr
    _ -> parseIf

parseIf :: Parser Exp
parseIf = do
  token <- peek
  case token of
    TokIf -> do
      advance
      cond <- parseExpr
      expect TokThen
      yes <- parseExpr
      expect TokElse
      EIf cond yes <$> parseExpr
    _ -> parseOr

parseOr :: Parser Exp
parseOr = chainLeft parseAnd [("||", Or)]

parseAnd :: Parser Exp
parseAnd = chainLeft parseEq [("&&", And)]

parseEq :: Parser Exp
parseEq = chainLeft parseAdd [("==", Eq)]

parseAdd :: Parser Exp
parseAdd = chainLeft parseMul [("+", Add), ("-", Sub)]

parseMul :: Parser Exp
parseMul = chainLeft parseApp [("*", Mul)]

parseApp :: Parser Exp
parseApp = do
  firstAtom <- parseAtom
  rest <- manyAtoms  -- 函数可能有多个参数
  pure (foldl EApp firstAtom rest)

manyAtoms :: Parser [Exp]
manyAtoms = do
  token <- peek
  case token of
    TokIdent _ -> consumeOne
    TokInt _ -> consumeOne
    TokBool _ -> consumeOne
    TokLParen -> consumeOne
    _ -> pure []
  where
    consumeOne = do
      atom <- parseAtom
      atoms <- manyAtoms
      pure (atom : atoms)

parseAtom :: Parser Exp
parseAtom = do
  token <- peek
  case token of
    TokIdent name -> advance >> pure (EVar name)
    TokInt n -> advance >> pure (ELit (LInt n))
    TokBool b -> advance >> pure (ELit (LBool b))
    TokLParen -> do
      advance
      expr <- parseExpr
      expect TokRParen
      pure expr
    _ -> Parser $ \_ -> Left ("expected expression atom, got " ++ show token)

chainLeft :: Parser Exp -> [(String, BinOp)] -> Parser Exp  -- 解析左结合的二元运算，右边应该是更高优先级的表达式
chainLeft parseTerm ops = do
  left <- parseTerm
  continue left
  where
    continue left = do
      token <- peek
      case token of
        TokOp opText ->
          case lookup opText ops of
            Just op -> do
              advance
              right <- parseTerm
              continue (EBin op left right)  -- 就是continue left，把(EBin op left right)视为left
            Nothing -> pure left
        _ -> pure left

parseIdentifier :: Parser String
parseIdentifier = do
  token <- peek
  case token of
    TokIdent name -> advance >> pure name
    _ -> Parser $ \_ -> Left ("expected identifier, got " ++ show token)

expect :: Token -> Parser ()
expect expected = do
  token <- peek
  if token == expected
    then advance
    else Parser $ \_ -> Left ("expected " ++ show expected ++ ", got " ++ show token)

expectEOF :: Parser ()
expectEOF = expect TokEOF

peek :: Parser Token
peek = Parser $ \tokens ->
  case tokens of
    [] -> Right (TokEOF, [])
    token:_ -> Right (token, tokens)  -- 只取第一个但不消费

advance :: Parser ()
advance = Parser $ \tokens ->
  case tokens of
    [] -> Right ((), [])
    _:rest -> Right ((), rest)  -- 消费当前 token，但不关心它是什么
