module Parser
  ( ParseTrace(..)
  , PseudoFunction(..)
  , Token(..)
  , expressionOfFunction
  , parseExp
  , parseExpression
  , parseExpressionTrace
  , parsePseudoFile
  , parsePseudoFileTrace
  , renderExp
  , renderTokens
  ) where

import Control.Applicative (Alternative(..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.List (isInfixOf, isPrefixOf)

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
  | TokLParen
  | TokRParen
  | TokLBracket
  | TokRBracket
  | TokComma
  | TokEOF
  deriving (Eq, Show)

data ParseTrace = ParseTrace
  { traceSource :: String
  , traceTokens :: [Token]
  , traceExp :: Exp
  }

data PseudoFunction = PseudoFunction
  { pseudoName :: String
  , pseudoArgs :: [String]
  , pseudoBodySource :: String
  , pseudoBody :: Exp
  , pseudoSource :: String
  , pseudoStartLine :: Int
  }

data SourceLine = SourceLine
  { sourceLineNumber :: Int
  , sourceLineText :: String
  }

newtype Parser a = Parser { runParser :: [Token] -> Either String (a, [Token]) }

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

instance Alternative Parser where
  empty = Parser $ \_ -> Left "没有匹配的语法分支"
  p <|> q = Parser $ \tokens ->
    case runParser p tokens of
      Left _ -> runParser q tokens
      ok -> ok

parseExp :: String -> Either TypeError Exp
parseExp input =
  case parseExpressionTrace input of
    Left err -> Left (ParseFailure err)
    Right trace -> Right (traceExp trace)

parseExpression :: String -> Either String Exp
parseExpression input = traceExp <$> parseExpressionTrace input

parseExpressionTrace :: String -> Either String ParseTrace
parseExpressionTrace input =
  case tokenize input of
    Left err -> Left err
    Right tokens ->
      case runParser (parseExpr <* expectEOF) tokens of
        Left err -> Left err
        Right (expr, _) -> Right (ParseTrace input tokens expr)

parsePseudoFile :: String -> Either TypeError [PseudoFunction]
parsePseudoFile input =
  case parsePseudoFileTrace input of
    Left err -> Left (ParseFailure err)
    Right functions -> Right functions

parsePseudoFileTrace :: String -> Either String [PseudoFunction]
parsePseudoFileTrace source = parseBlocks (cleanLines source)

expressionOfFunction :: PseudoFunction -> Exp
expressionOfFunction fn = foldr EAbs (pseudoBody fn) (pseudoArgs fn)

tokenize :: String -> Either String [Token]
tokenize [] = Right [TokEOF]
tokenize (c:cs)
  | isSpace c = tokenize cs
  | c == '#' = tokenize (dropLine cs)
  | c == '%' = tokenize (dropLine cs)
  | isAlpha c || c == '_' = tokenizeIdent (c:cs)
  | isDigit c = tokenizeInt (c:cs)
  | c == '\\' = cons TokLambda <$> tokenize cs
  | c == '(' = cons TokLParen <$> tokenize cs
  | c == ')' = cons TokRParen <$> tokenize cs
  | c == '[' = cons TokLBracket <$> tokenize cs
  | c == ']' = cons TokRBracket <$> tokenize cs
  | c == ',' = cons TokComma <$> tokenize cs
  | c == '-' && take 1 cs == ">" = cons TokArrow <$> tokenize (drop 1 cs)
  | c == '-' && take 1 cs == "-" = tokenize (dropLine (drop 1 cs))
  | c == '=' && take 1 cs == "=" = cons (TokOp "==") <$> tokenize (drop 1 cs)
  | c == '=' = cons TokEquals <$> tokenize cs
  | c == '!' && take 1 cs == "=" = cons (TokOp "!=") <$> tokenize (drop 1 cs)
  | c == '~' && take 1 cs == "=" = cons (TokOp "~=") <$> tokenize (drop 1 cs)
  | c == '~' = cons (TokOp "~") <$> tokenize cs
  | c == '<' && take 1 cs == "=" = cons (TokOp "<=") <$> tokenize (drop 1 cs)
  | c == '>' && take 1 cs == "=" = cons (TokOp ">=") <$> tokenize (drop 1 cs)
  | c == '<' = cons (TokOp "<") <$> tokenize cs
  | c == '>' = cons (TokOp ">") <$> tokenize cs
  | c == '&' && take 1 cs == "&" = cons (TokOp "&&") <$> tokenize (drop 1 cs)
  | c == '|' && take 1 cs == "|" = cons (TokOp "||") <$> tokenize (drop 1 cs)
  | c `elem` "+-*/" = cons (TokOp [c]) <$> tokenize cs
  | otherwise = Left ("发现不支持的字符：" ++ [c])
  where
    cons token tokens = token : tokens

tokenizeIdent :: String -> Either String [Token]
tokenizeIdent input =
  let (name, rest) = span isIdentChar input
      lname = lower name
      token = case lname of
        "let" -> TokLet
        "in" -> TokIn
        "if" -> TokIf
        "then" -> TokThen
        "else" -> TokElse
        "true" -> TokBool True
        "false" -> TokBool False
        _ -> TokIdent name
  in (token :) <$> tokenize rest

tokenizeInt :: String -> Either String [Token]
tokenizeInt input =
  let (digits, rest) = span isDigit input
  in (TokInt (read digits) :) <$> tokenize rest

dropLine :: String -> String
dropLine = dropWhile (/= '\n')

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

validIdent :: String -> Bool
validIdent [] = False
validIdent name@(c:_) =
  (isAlpha c || c == '_') &&
  all isIdentChar name &&
  lower name `notElem`
    [ "let", "in", "if", "then", "else", "function", "end"
    , "true", "false", "elseif", "switch", "case", "otherwise"
    ]

parseExpr :: Parser Exp
parseExpr = parseLet

parseLet :: Parser Exp
parseLet = do
  token <- peek
  case token of
    TokLet -> do
      advance
      name <- parseIdentifier
      expect TokEquals
      value <- parseExpr
      expect TokIn
      ELet name value <$> parseExpr
    _ -> parseLambda

parseLambda :: Parser Exp
parseLambda = do
  token <- peek
  case token of
    TokLambda -> do
      advance
      names <- some parseIdentifier
      expect TokArrow
      body <- parseExpr
      pure (foldr EAbs body names)
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
parseEq = chainLeft parseCompare [("==", Eq), ("!=", Ne), ("~=", Ne)]

parseCompare :: Parser Exp
parseCompare =
  chainLeft parseAdd
    [ ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge) ]

parseAdd :: Parser Exp
parseAdd = chainLeft parseMul [("+", Add), ("-", Sub)]

parseMul :: Parser Exp
parseMul = chainLeft parseUnary [("*", Mul), ("/", Div)]

parseUnary :: Parser Exp
parseUnary = do
  token <- peek
  case token of
    TokIdent "not" -> advance >> EApp (EVar "not") <$> parseUnary
    TokOp "~" -> advance >> EApp (EVar "not") <$> parseUnary
    TokOp "-" -> advance >> EApp (EVar "negate") <$> parseUnary
    _ -> parseApp

parseApp :: Parser Exp
parseApp = do
  firstAtom <- parseAtom
  rest <- manyAtoms
  pure (foldl EApp firstAtom rest)

manyAtoms :: Parser [Exp]
manyAtoms = do
  token <- peek
  case token of
    TokIdent _ -> consumeOne
    TokInt _ -> consumeOne
    TokBool _ -> consumeOne
    TokLParen -> consumeOne
    TokLBracket -> consumeOne
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
    TokIdent name -> do
      advance
      next <- peek
      case next of
        TokLParen -> do
          advance
          args <- parseCallArgs
          pure (foldl EApp (EVar name) args)
        _ -> pure (EVar name)
    TokInt n -> advance >> pure (ELit (LInt n))
    TokBool b -> advance >> pure (ELit (LBool b))
    TokLParen -> do
      advance
      expr <- parseExpr
      expect TokRParen
      pure expr
    TokLBracket -> do
      advance
      EList <$> parseListItems
    _ -> Parser $ \_ -> Left ("这里需要一个表达式原子，但实际看到：" ++ show token)

parseListItems :: Parser [Exp]
parseListItems = do
  token <- peek
  case token of
    TokRBracket -> advance >> pure []
    _ -> do
      firstItem <- parseExpr
      parseMore [firstItem]
  where
    parseMore items = do
      token <- peek
      case token of
        TokComma -> advance >> parseExpr >>= \item -> parseMore (items ++ [item])
        TokRBracket -> advance >> pure items
        _ -> Parser $ \_ -> Left "列表字面量需要右括号 ']'，多个元素之间需要逗号 ','"

parseCallArgs :: Parser [Exp]
parseCallArgs = do
  token <- peek
  case token of
    TokRParen -> advance >> pure []
    _ -> do
      firstArg <- parseExpr
      parseMore [firstArg]
  where
    parseMore args = do
      token <- peek
      case token of
        TokComma -> advance >> parseExpr >>= \arg -> parseMore (args ++ [arg])
        TokRParen -> advance >> pure args
        _ -> Parser $ \_ -> Left "函数调用需要右括号 ')'，多个参数之间需要逗号 ','"

chainLeft :: Parser Exp -> [(String, BinOp)] -> Parser Exp
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
              continue (EBin op left right)
            Nothing -> pure left
        _ -> pure left

parseIdentifier :: Parser String
parseIdentifier = do
  token <- peek
  case token of
    TokIdent name -> advance >> pure name
    _ -> Parser $ \_ -> Left ("这里需要变量名，但实际看到：" ++ show token)

expect :: Token -> Parser ()
expect expected = do
  token <- peek
  if token == expected
    then advance
    else Parser $ \_ -> Left ("这里需要 " ++ show expected ++ "，但实际看到：" ++ show token)

expectEOF :: Parser ()
expectEOF = expect TokEOF

peek :: Parser Token
peek = Parser $ \tokens ->
  case tokens of
    [] -> Right (TokEOF, [])
    token:_ -> Right (token, tokens)

advance :: Parser ()
advance = Parser $ \tokens ->
  case tokens of
    [] -> Right ((), [])
    _:rest -> Right ((), rest)

parseBlocks :: [SourceLine] -> Either String [PseudoFunction]
parseBlocks [] = Right []
parseBlocks (line:rest)
  | "function " `isPrefixOf` lower text = do
      let header = trim (drop (length "function") text)
      case splitTopLevelEquals header of
        Just (left, _) | looksLikeSignature left -> do
          fn <- parseInlineFunction line header
          (fn :) <$> parseBlocks rest
        Nothing -> do
          (bodyLines, remaining) <- collectFunctionBody line rest
          fn <- parseBlockFunction line header bodyLines
          (fn :) <$> parseBlocks remaining
        _ -> do
          (bodyLines, remaining) <- collectFunctionBody line rest
          fn <- parseBlockFunction line header bodyLines
          (fn :) <$> parseBlocks remaining
  | otherwise = Left (lineLabel line ++ "这里应该是 function block，但实际看到：" ++ text)
  where
    text = sourceLineText line

isEndLine :: SourceLine -> Bool
isEndLine line = lower (trim (sourceLineText line)) == "end"

isElseLine :: SourceLine -> Bool
isElseLine line = lower (trim (sourceLineText line)) == "else"

isElseIfLine :: SourceLine -> Bool
isElseIfLine line =
  startsKeyword "elseif" text || startsTwoKeywords "else" "if" text
  where
    text = sourceLineText line

isIfLine :: SourceLine -> Bool
isIfLine line = startsKeyword "if" text && not (" then " `isWordPart` text)
  where
    text = sourceLineText line

isSwitchLine :: SourceLine -> Bool
isSwitchLine = startsKeyword "switch" . sourceLineText

isForLine :: SourceLine -> Bool
isForLine = startsKeyword "for" . sourceLineText

isWhileLine :: SourceLine -> Bool
isWhileLine = startsKeyword "while" . sourceLineText

isCaseLine :: SourceLine -> Bool
isCaseLine = startsKeyword "case" . sourceLineText

isOtherwiseLine :: SourceLine -> Bool
isOtherwiseLine = startsKeyword "otherwise" . sourceLineText

isWordPart :: String -> String -> Bool
isWordPart needle line = needle `isInfixOf` (" " ++ lower (trim line) ++ " ")

startsKeyword :: String -> String -> Bool
startsKeyword keyword line =
  case words (lower (trim line)) of
    first:_ -> first == keyword
    [] -> False

startsTwoKeywords :: String -> String -> String -> Bool
startsTwoKeywords first second line =
  case words (lower (trim line)) of
    x:y:_ -> x == first && y == second
    _ -> False

looksLikeSignature :: String -> Bool
looksLikeSignature text =
  case parseSignature (trim text) of
    Right _ -> True
    Left _ -> False

collectFunctionBody :: SourceLine -> [SourceLine] -> Either String ([SourceLine], [SourceLine])
collectFunctionBody headerLine = go 0 []
  where
    go :: Int -> [SourceLine] -> [SourceLine] -> Either String ([SourceLine], [SourceLine])
    go _ _ [] = Left (lineLabel headerLine ++ "函数缺少 end：" ++ sourceLineText headerLine)
    go depth acc (line:rest)
      | isEndLine line && depth == 0 = Right (reverse acc, rest)
      | isEndLine line = go (depth - 1) (line : acc) rest
      | isIfLine line = go (depth + 1) (line : acc) rest
      | isSwitchLine line = go (depth + 1) (line : acc) rest
      | isForLine line = go (depth + 1) (line : acc) rest
      | isWhileLine line = go (depth + 1) (line : acc) rest
      | otherwise = go depth (line : acc) rest

parseInlineFunction :: SourceLine -> String -> Either String PseudoFunction
parseInlineFunction original header = do
  (signature, bodyPart) <- maybeToEither "不是单行 function" (splitTopLevelEquals header)
  let bodySource = trim bodyPart
  (name, args) <- parseSignature (trim signature)
  body <- withLine original (parseExpression bodySource)
  pure (PseudoFunction name args bodySource body (sourceLineText original) (sourceLineNumber original))

parseBlockFunction :: SourceLine -> String -> [SourceLine] -> Either String PseudoFunction
parseBlockFunction original header bodyLines = do
  (outputName, name, args) <- parseMatlabHeader header
  (bodySource, body) <- selectBody outputName bodyLines
  pure (PseudoFunction name args bodySource body (unlines (map sourceLineText (original : bodyLines ++ [SourceLine 0 "end"]))) (sourceLineNumber original))

parseMatlabHeader :: String -> Either String (Maybe String, String, [String])
parseMatlabHeader header =
  case splitTopLevelEquals header of
    Just (left, right) -> do
      outputName <- requireName (trim left) "函数输出变量名不合法"
      (name, args) <- parseSignature (trim right)
      pure (Just outputName, name, args)
    Nothing -> do
      (name, args) <- parseSignature header
      pure (Nothing, name, args)

parseSignature :: String -> Either String (String, [String])
parseSignature signature =
  case break (== '(') signature of
    (namePart, '(':rest) -> do
      let name = trim namePart
      _ <- requireName name "函数名不合法"
      argsPart <- case reverse rest of
        ')':revArgs -> Right (reverse revArgs)
        _ -> Left ("函数参数列表缺少右括号 ')'：" ++ signature)
      args <- parseArgs argsPart
      pure (name, args)
    _ -> Left ("函数签名必须形如 name(arg1, arg2)：" ++ signature)

parseArgs :: String -> Either String [String]
parseArgs raw
  | null (trim raw) = Right []
  | otherwise = mapM parseArg (splitCommas raw)
  where
    parseArg arg = requireName (trim arg) ("参数名不合法：" ++ arg)

selectBody :: Maybe String -> [SourceLine] -> Either String (String, Exp)
selectBody _ [] = Left "函数体为空"
selectBody Nothing [single] = do
  expr <- withLine single (parseExpression (sourceLineText single))
  Right (sourceLineText single, expr)
selectBody (Just outputName) linesIn = do
  stmts <- parseStatements linesIn
  let body = EBlock stmts outputName
  Right (renderExp body, body)
selectBody Nothing linesIn
  | startsWithBlockIf linesIn = Left "block if 需要 MATLAB 风格输出变量，例如 function y = f(x)"
  | startsWithSwitch linesIn = Left "switch block 需要 MATLAB 风格输出变量，例如 function y = f(x)"
  | startsWithFor linesIn = Left "for block 需要 MATLAB 风格输出变量，例如 function y = f(xs)"
  | startsWithWhile linesIn = Left "while block 需要 MATLAB 风格输出变量，例如 function y = f(x)"
selectBody Nothing _ = Left "没有输出变量时，只支持单表达式函数体"

startsWithBlockIf :: [SourceLine] -> Bool
startsWithBlockIf (line:_) = isIfLine line
startsWithBlockIf [] = False

startsWithSwitch :: [SourceLine] -> Bool
startsWithSwitch (line:_) = isSwitchLine line
startsWithSwitch [] = False

startsWithFor :: [SourceLine] -> Bool
startsWithFor (line:_) = isForLine line
startsWithFor [] = False

startsWithWhile :: [SourceLine] -> Bool
startsWithWhile (line:_) = isWhileLine line
startsWithWhile [] = False

parseStatements :: [SourceLine] -> Either String [Stmt]
parseStatements linesIn = do
  (stmts, rest) <- parseStatementsUntil (const False) linesIn
  case rest of
    [] -> Right stmts
    line:_ -> Left (lineLabel line ++ "这里出现了没有对应开头的 block 结束语句：" ++ sourceLineText line)

parseStatementsUntil :: (SourceLine -> Bool) -> [SourceLine] -> Either String ([Stmt], [SourceLine])
parseStatementsUntil _ [] = Right ([], [])
parseStatementsUntil stop linesIn@(line:rest)
  | stop line = Right ([], linesIn)
  | isIfLine line = do
      (stmt, remaining) <- parseIfStmt line rest
      (stmts, finalRest) <- parseStatementsUntil stop remaining
      Right (stmt : stmts, finalRest)
  | isSwitchLine line = do
      (stmt, remaining) <- parseSwitchStmt line rest
      (stmts, finalRest) <- parseStatementsUntil stop remaining
      Right (stmt : stmts, finalRest)
  | isForLine line = do
      (stmt, remaining) <- parseForStmt line rest
      (stmts, finalRest) <- parseStatementsUntil stop remaining
      Right (stmt : stmts, finalRest)
  | isWhileLine line = do
      (stmt, remaining) <- parseWhileStmt line rest
      (stmts, finalRest) <- parseStatementsUntil stop remaining
      Right (stmt : stmts, finalRest)
  | isEndLine line || isElseLine line || isElseIfLine line || isCaseLine line || isOtherwiseLine line =
      Right ([], linesIn)
  | otherwise = do
      stmt <- parseAssignmentStmt line
      (stmts, finalRest) <- parseStatementsUntil stop rest
      Right (stmt : stmts, finalRest)

parseAssignmentStmt :: SourceLine -> Either String Stmt
parseAssignmentStmt line =
  case splitTopLevelEquals (sourceLineText line) of
    Just (left, right) -> do
      name <- requireName (trim left) (lineLabel line ++ "赋值左侧不是合法变量名：" ++ trim left)
      expr <- withLine line (parseExpression (trim right))
      Right (SAssign name expr)
    Nothing -> Left (lineLabel line ++ "这里只支持赋值语句或 block 语句，实际看到：" ++ sourceLineText line)

parseIfStmt :: SourceLine -> [SourceLine] -> Either String (Stmt, [SourceLine])
parseIfStmt ifLine rest = do
  let condSource = trim (drop (length "if") (trim (sourceLineText ifLine)))
  if null condSource then Left (lineLabel ifLine ++ "if 后面缺少条件") else pure ()
  cond <- withLine ifLine (parseExpression condSource)
  parseIfTail cond rest

parseIfTail :: Exp -> [SourceLine] -> Either String (Stmt, [SourceLine])
parseIfTail cond rest = do
  (thenStmts, afterThen) <- parseStatementsUntil ifStop rest
  case afterThen of
    [] -> Left "if block 缺少 end"
    marker:afterMarker
      | isElseIfLine marker -> do
          let condSource = elseIfCondition marker
          nestedCond <- withLine marker (parseExpression condSource)
          (nested, remaining) <- parseIfTail nestedCond afterMarker
          Right (SIfStmt cond thenStmts [nested], remaining)
      | isElseLine marker -> do
          (elseStmts, afterElse) <- parseStatementsUntil isEndLine afterMarker
          case afterElse of
            endLine:remaining | isEndLine endLine -> Right (SIfStmt cond thenStmts elseStmts, remaining)
            _ -> Left "if block 缺少 end"
      | isEndLine marker -> Right (SIfStmt cond thenStmts [], afterMarker)
      | otherwise -> Left (lineLabel marker ++ "if block 结构不完整：" ++ sourceLineText marker)
  where
    ifStop line = isElseIfLine line || isElseLine line || isEndLine line

parseSwitchStmt :: SourceLine -> [SourceLine] -> Either String (Stmt, [SourceLine])
parseSwitchStmt switchLine rest = do
  let subjectSource = trim (drop (length "switch") (trim (sourceLineText switchLine)))
  if null subjectSource then Left (lineLabel switchLine ++ "switch 后面缺少待匹配表达式") else pure ()
  subject <- withLine switchLine (parseExpression subjectSource)
  (cases, otherwiseBranch, remaining) <- parseSwitchCases rest
  Right (SSwitchStmt subject cases otherwiseBranch, remaining)

parseSwitchCases :: [SourceLine] -> Either String ([(Exp, [Stmt])], [Stmt], [SourceLine])
parseSwitchCases [] = Left "switch block 缺少 case"
parseSwitchCases (line:rest)
  | isCaseLine line = go [] line rest
  | otherwise = Left (lineLabel line ++ "switch block 必须先出现 case")
  where
    go cases caseLine afterCase = do
      caseExpr <- withLine caseLine (parseExpression (caseCondition caseLine))
      (caseBody, afterBody) <- parseStatementsUntil switchStop afterCase
      case afterBody of
        [] -> Left "switch block 缺少 end"
        marker:afterMarker
          | isCaseLine marker -> go (cases ++ [(caseExpr, caseBody)]) marker afterMarker
          | isOtherwiseLine marker -> do
              (otherwiseBody, afterOtherwise) <- parseStatementsUntil isEndLine afterMarker
              case afterOtherwise of
                endLine:remaining | isEndLine endLine -> Right (cases ++ [(caseExpr, caseBody)], otherwiseBody, remaining)
                _ -> Left "switch block 缺少 end"
          | isEndLine marker -> Left "switch block 缺少 otherwise"
          | otherwise -> Left (lineLabel marker ++ "switch block 结构不完整：" ++ sourceLineText marker)
    switchStop marker = isCaseLine marker || isOtherwiseLine marker || isEndLine marker

parseForStmt :: SourceLine -> [SourceLine] -> Either String (Stmt, [SourceLine])
parseForStmt forLine rest = do
  (itemName, itemsSource) <- parseForHeader forLine
  items <- withLine forLine (parseExpression itemsSource)
  (body, afterBody) <- parseStatementsUntil isEndLine rest
  case afterBody of
    endLine:remaining | isEndLine endLine -> Right (SFor itemName items body, remaining)
    _ -> Left (lineLabel forLine ++ "for block 缺少 end")

parseForHeader :: SourceLine -> Either String (String, String)
parseForHeader line =
  let body = trim (drop (length "for") (trim (sourceLineText line)))
      parts = words body
  in case parts of
    name:"in":rest | validIdent name && not (null rest) -> Right (name, unwords rest)
    _ ->
      case splitTopLevelEquals body of
        Just (left, right) | validIdent (trim left) && not (null (trim right)) -> Right (trim left, trim right)
        _ -> Left (lineLabel line ++ "for 需要形如 `for x in xs` 或 `for x = xs`")

parseWhileStmt :: SourceLine -> [SourceLine] -> Either String (Stmt, [SourceLine])
parseWhileStmt whileLine rest = do
  let condSource = trim (drop (length "while") (trim (sourceLineText whileLine)))
  if null condSource then Left (lineLabel whileLine ++ "while 后面缺少条件") else pure ()
  cond <- withLine whileLine (parseExpression condSource)
  (body, afterBody) <- parseStatementsUntil isEndLine rest
  case afterBody of
    endLine:remaining | isEndLine endLine -> Right (SWhile cond body, remaining)
    _ -> Left (lineLabel whileLine ++ "while block 缺少 end")

elseIfCondition :: SourceLine -> String
elseIfCondition line
  | startsKeyword "elseif" text = trim (drop (length "elseif") (trim text))
  | startsTwoKeywords "else" "if" text = trim (drop (length "else if") (trim text))
  | otherwise = ""
  where
    text = sourceLineText line

caseCondition :: SourceLine -> String
caseCondition line = trim (drop (length "case") (trim (sourceLineText line)))

splitTopLevelEquals :: String -> Maybe (String, String)
splitTopLevelEquals input = go 0 "" input
  where
    go :: Int -> String -> String -> Maybe (String, String)
    go _ _ [] = Nothing
    go depth before (c:cs)
      | c == '(' = go (depth + 1) (before ++ [c]) cs
      | c == ')' = go (depth - 1) (before ++ [c]) cs
      | c == '=' && depth == 0 =
          case cs of
            '=':_ -> go depth (before ++ [c]) cs
            _ -> Just (before, cs)
      | otherwise = go depth (before ++ [c]) cs

splitCommas :: String -> [String]
splitCommas input = go 0 "" input
  where
    go :: Int -> String -> String -> [String]
    go _ current [] = [current]
    go depth current (c:cs)
      | c == '(' = go (depth + 1) (current ++ [c]) cs
      | c == ')' = go (depth - 1) (current ++ [c]) cs
      | c == ',' && depth == 0 = current : go depth "" cs
      | otherwise = go depth (current ++ [c]) cs

cleanLines :: String -> [SourceLine]
cleanLines source =
  [ SourceLine lineNo cleaned
  | (lineNo, raw) <- zip [1 :: Int ..] (lines source)
  , let cleaned = trim (stripSemicolon (stripComment raw))
  , not (null cleaned)
  ]

lineLabel :: SourceLine -> String
lineLabel line = "第 " ++ show (sourceLineNumber line) ++ " 行："

withLine :: SourceLine -> Either String a -> Either String a
withLine line =
  either (Left . (lineLabel line ++)) Right

stripComment :: String -> String
stripComment [] = []
stripComment ('%':_) = []
stripComment ('#':_) = []
stripComment ('-':'-':_) = []
stripComment (c:cs) = c : stripComment cs

stripSemicolon :: String -> String
stripSemicolon line =
  case reverse (dropWhile isSpace line) of
    ';':rest -> reverse rest
    _ -> line

requireName :: String -> String -> Either String String
requireName name message
  | validIdent name = Right name
  | otherwise = Left message

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

lower :: String -> String
lower = map toLower

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just value) = Right value

renderTokens :: [Token] -> String
renderTokens = unwords . map renderToken . filter (/= TokEOF)

renderToken :: Token -> String
renderToken token =
  case token of
    TokIdent name -> name
    TokInt n -> show n
    TokBool True -> "True"
    TokBool False -> "False"
    TokLet -> "let"
    TokIn -> "in"
    TokIf -> "if"
    TokThen -> "then"
    TokElse -> "else"
    TokLambda -> "\\"
    TokArrow -> "->"
    TokEquals -> "="
    TokOp op -> op
    TokLParen -> "("
    TokRParen -> ")"
    TokLBracket -> "["
    TokRBracket -> "]"
    TokComma -> ","
    TokEOF -> "<eof>"

renderExp :: Exp -> String
renderExp = show
