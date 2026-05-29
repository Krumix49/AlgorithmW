module Parser
    ( ParseTrace(..)
    , PseudoFunction(..)
    , Token(..)
    , expressionOfFunction
    , parseExpression
    , parseExpressionTrace
    , parsePseudoFile
    , parsePseudoFileTrace
    , renderExp
    , renderTokens
    ) where

import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.List (isInfixOf, isPrefixOf)

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
    | TokComma
    | TokIf
    | TokThen
    | TokElse
    | TokFunction
    | TokEnd
    | TokOp String
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
    }

data IfMarker
    = IfElseIf String
    | IfElse
    | IfEnd

data SwitchMarker
    = SwitchCase String
    | SwitchOtherwise
    | SwitchEnd

newtype P a = P {runP :: [Token] -> Either String (a, [Token])}

instance Functor P where
    fmap f p = P $ \tokens -> do
        (x, rest) <- runP p tokens
        return (f x, rest)

instance Applicative P where
    pure x = P $ \tokens -> Right (x, tokens)
    pf <*> px = P $ \tokens -> do
        (f, rest) <- runP pf tokens
        (x, rest') <- runP px rest
        return (f x, rest')

instance Monad P where
    p >>= f = P $ \tokens -> do
        (x, rest) <- runP p tokens
        runP (f x) rest

instance Alternative P where
    empty = P $ const (Left "没有可用的语法分支")
    p <|> q = P $ \tokens ->
        case runP p tokens of
            Left _ -> runP q tokens
            ok -> ok

parseExpression :: String -> Either String Exp
parseExpression source = traceExp <$> parseExpressionTrace source

parseExpressionTrace :: String -> Either String ParseTrace
parseExpressionTrace source = do
    tokens <- tokenize source
    case tokens of
        [] -> Left "输入为空。"
        _ -> do
            (expr, rest) <- runP parseExpr tokens
            case rest of
                [] -> Right (ParseTrace source tokens expr)
                unexpected -> Left $ "表达式已经结束，但后面仍有多余内容：" ++ renderTokens unexpected

parsePseudoFile :: String -> Either String [PseudoFunction]
parsePseudoFile = parsePseudoFileTrace

parsePseudoFileTrace :: String -> Either String [PseudoFunction]
parsePseudoFileTrace source = parseBlocks (cleanLines source)

expressionOfFunction :: PseudoFunction -> Exp
expressionOfFunction fn = foldr EAbs (pseudoBody fn) (pseudoArgs fn)

tokenize :: String -> Either String [Token]
tokenize = go
  where
    go [] = Right []
    go (c:cs)
        | isSpace c = go cs
        | c == '#' = go (dropLine cs)
        | c == '%' = go (dropLine cs)
        | isAlpha c || c == '_' = scanIdent (c:cs)
        | isDigit c = scanInt (c:cs)
        | c == '\\' = (TokLambda :) <$> go cs
        | c == '-' =
            case cs of
                ('>':rest) -> (TokArrow :) <$> go rest
                ('-':rest) -> go (dropLine rest)
                _ -> (TokOp "-" :) <$> go cs
        | c == '=' =
            case cs of
                ('=':rest) -> (TokOp "==" :) <$> go rest
                _ -> (TokEquals :) <$> go cs
        | c == '!' =
            case cs of
                ('=':rest) -> (TokOp "!=" :) <$> go rest
                _ -> Left "发现 '!'；如果要表示不等于，请使用 '!='。"
        | c == '~' =
            case cs of
                ('=':rest) -> (TokOp "~=" :) <$> go rest
                _ -> (TokOp "~" :) <$> go cs
        | c == '<' =
            case cs of
                ('=':rest) -> (TokOp "<=" :) <$> go rest
                _ -> (TokOp "<" :) <$> go cs
        | c == '>' =
            case cs of
                ('=':rest) -> (TokOp ">=" :) <$> go rest
                _ -> (TokOp ">" :) <$> go cs
        | c == '&' =
            case cs of
                ('&':rest) -> (TokOp "&&" :) <$> go rest
                _ -> Left "发现 '&'；布尔与请使用 '&&'。"
        | c == '|' =
            case cs of
                ('|':rest) -> (TokOp "||" :) <$> go rest
                _ -> Left "发现 '|'；布尔或请使用 '||'。"
        | c == '+' = (TokOp "+" :) <$> go cs
        | c == '*' = (TokOp "*" :) <$> go cs
        | c == '/' = (TokOp "/" :) <$> go cs
        | c == '(' = (TokLParen :) <$> go cs
        | c == ')' = (TokRParen :) <$> go cs
        | c == ',' = (TokComma :) <$> go cs
        | otherwise = Left $ "发现不支持的字符：" ++ [c]

    scanIdent input =
        let (name, rest) = span isIdentChar input
            lname = lower name
        in case lname of
            "let" -> (TokLet :) <$> go rest
            "in" -> (TokIn :) <$> go rest
            "if" -> (TokIf :) <$> go rest
            "then" -> (TokThen :) <$> go rest
            "else" -> (TokElse :) <$> go rest
            "function" -> (TokFunction :) <$> go rest
            "end" -> (TokEnd :) <$> go rest
            "true" -> (TokBool True :) <$> go rest
            "false" -> (TokBool False :) <$> go rest
            _ | validIdent name -> (TokIdent name :) <$> go rest
              | otherwise -> Left $ "非法变量名：" ++ name

    scanInt input =
        let (digits, rest) = span isDigit input
        in (TokInt (read digits) :) <$> go rest

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
        [ "let", "in", "if", "then", "else", "function", "end", "true", "false" ]

parseExpr :: P Exp
parseExpr = P $ \tokens ->
    case tokens of
        TokLet:_ -> runP parseLet tokens
        TokLambda:_ -> runP parseLambda tokens
        TokIf:_ -> runP parseIf tokens
        [] -> Left "表达式不完整。"
        _ -> runP parseOr tokens

parseLet :: P Exp
parseLet = do
    expect TokLet "期望 let。"
    name <- ident "let 后面必须跟变量名。"
    expect TokEquals "let 绑定中缺少 '='。"
    bound <- parseExpr
    expect TokIn "let 绑定中缺少 in。"
    body <- parseExpr
    return (ELet name bound body)

parseLambda :: P Exp
parseLambda = do
    expect TokLambda "期望 lambda。"
    names <- some (ident "lambda 后面必须跟至少一个参数名。")
    expect TokArrow "lambda 参数后缺少 '->'。"
    body <- parseExpr
    return (foldr EAbs body names)

parseIf :: P Exp
parseIf = do
    expect TokIf "期望 if。"
    cond <- parseExpr
    expect TokThen "if 条件后缺少 then。"
    trueBranch <- parseExpr
    expect TokElse "if 表达式缺少 else。"
    falseBranch <- parseExpr
    return (applyMany (EVar "if") [cond, trueBranch, falseBranch])

parseOr :: P Exp
parseOr = chainl parseAnd ["||"]

parseAnd :: P Exp
parseAnd = chainl parseEquality ["&&"]

parseEquality :: P Exp
parseEquality = chainl parseCompare ["==", "!=", "~="]

parseCompare :: P Exp
parseCompare = chainl parseAdd ["<", "<=", ">", ">="]

parseAdd :: P Exp
parseAdd = chainl parseMul ["+", "-"]

parseMul :: P Exp
parseMul = chainl parseUnary ["*", "/"]

parseUnary :: P Exp
parseUnary = P $ \tokens ->
    case tokens of
        TokIdent "not":rest -> runP (EApp (EVar "not") <$> parseUnary) rest
        TokOp "~":rest -> runP (EApp (EVar "not") <$> parseUnary) rest
        TokOp "-":rest -> runP (EApp (EVar "negate") <$> parseUnary) rest
        _ -> runP parseApp tokens

parseApp :: P Exp
parseApp = do
    first <- parseAtom
    rest <- many parseAtom
    return (foldl EApp first rest)

parseAtom :: P Exp
parseAtom = P $ \tokens ->
    case tokens of
        TokIdent name:TokLParen:rest -> do
            (args, rest') <- parseCallArgs rest
            Right (applyMany (EVar name) args, rest')
        TokIdent name:rest -> Right (EVar name, rest)
        TokInt n:rest -> Right (ELit (LInt n), rest)
        TokBool b:rest -> Right (ELit (LBool b), rest)
        TokLParen:_ -> runP parseParen tokens
        TokLet:_ -> Left "let 表达式作为函数或参数使用时必须加括号。"
        TokLambda:_ -> Left "lambda 表达式作为函数或参数使用时必须加括号。"
        TokIf:_ -> Left "if 表达式作为函数或参数使用时必须加括号。"
        TokIn:_ -> Left "遇到 in，当前子表达式结束。"
        TokThen:_ -> Left "遇到 then，当前子表达式结束。"
        TokElse:_ -> Left "遇到 else，当前子表达式结束。"
        TokRParen:_ -> Left "遇到右括号，当前子表达式结束。"
        TokComma:_ -> Left "遇到逗号，当前子表达式结束。"
        TokArrow:_ -> Left "意外的 '->'。"
        TokEquals:_ -> Left "意外的 '='。"
        TokOp op:_ -> Left $ "运算符 '" ++ op ++ "' 缺少左操作数。"
        [] -> Left "表达式不完整。"

parseCallArgs :: [Token] -> Either String ([Exp], [Token])
parseCallArgs tokens =
    case tokens of
        TokRParen:rest -> Right ([], rest)
        _ -> do
            (first, rest) <- runP parseExpr tokens
            parseMore [first] rest
  where
    parseMore args rest =
        case rest of
            TokComma:afterComma -> do
                (arg, rest') <- runP parseExpr afterComma
                parseMore (args ++ [arg]) rest'
            TokRParen:afterParen -> Right (args, afterParen)
            _ -> Left "函数调用参数列表缺少 ')'，或参数之间缺少逗号。"

parseParen :: P Exp
parseParen = do
    expect TokLParen "期望 '('。"
    expr <- parseExpr
    expect TokRParen "括号表达式缺少 ')'。"
    return expr

chainl :: P Exp -> [String] -> P Exp
chainl p ops = do
    first <- p
    P $ parseRest first
  where
    parseRest left tokens =
        case tokens of
            TokOp op:rest | op `elem` ops -> do
                (right, rest') <- runP p rest
                parseRest (applyMany (EVar op) [left, right]) rest'
            _ -> Right (left, tokens)

ident :: String -> P String
ident message = P $ \tokens ->
    case tokens of
        TokIdent name:rest -> Right (name, rest)
        _ -> Left message

expect :: Token -> String -> P ()
expect token message = P $ \tokens ->
    case tokens of
        t:rest | t == token -> Right ((), rest)
        _ -> Left message

applyMany :: Exp -> [Exp] -> Exp
applyMany = foldl EApp

parseBlocks :: [String] -> Either String [PseudoFunction]
parseBlocks [] = Right []
parseBlocks (line:rest)
    | "function " `isPrefixOf` lower line = do
        let header = trim (drop (length "function") line)
        case parseInlineFunction line header of
            Right fn -> (fn :) <$> parseBlocks rest
            Left _ -> do
                (bodyLines, remaining) <- collectFunctionBody line rest
                fn <- parseBlockFunction line header bodyLines
                (fn :) <$> parseBlocks remaining
    | otherwise = Left $ "文件中发现不属于 function block 的内容：" ++ line

isEndLine :: String -> Bool
isEndLine line = lower (trim line) == "end"

isElseLine :: String -> Bool
isElseLine line = lower (trim line) == "else"

isElseIfLine :: String -> Bool
isElseIfLine line =
    startsKeyword "elseif" line || startsTwoKeywords "else" "if" line

isIfLine :: String -> Bool
isIfLine line = startsKeyword "if" line && not (" then " `isPrefixOfWordTail` line)

isSwitchLine :: String -> Bool
isSwitchLine = startsKeyword "switch"

isCaseLine :: String -> Bool
isCaseLine = startsKeyword "case"

isOtherwiseLine :: String -> Bool
isOtherwiseLine = startsKeyword "otherwise"

isPrefixOfWordTail :: String -> String -> Bool
isPrefixOfWordTail needle line =
    needle `isInfixOf` (" " ++ lower (trim line) ++ " ")

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

collectFunctionBody :: String -> [String] -> Either String ([String], [String])
collectFunctionBody headerLine = go 0 []
  where
    go _ _ [] = Left $ "函数缺少 end：" ++ headerLine
    go depth acc (line:rest)
        | isEndLine line && depth == 0 = Right (reverse acc, rest)
        | isEndLine line = go (depth - 1) (line : acc) rest
        | isIfLine line = go (depth + 1) (line : acc) rest
        | isSwitchLine line = go (depth + 1) (line : acc) rest
        | otherwise = go depth (line : acc) rest

parseInlineFunction :: String -> String -> Either String PseudoFunction
parseInlineFunction original header = do
    (signature, bodyPart) <- case splitTopLevelEquals header of
        Nothing -> Left "不是单行 function。"
        Just parts -> Right parts
    let bodySource = trim bodyPart
    (name, args) <- parseSignature (trim signature)
    body <- parseExpression bodySource
    return (PseudoFunction name args bodySource body original)

parseBlockFunction :: String -> String -> [String] -> Either String PseudoFunction
parseBlockFunction original header bodyLines = do
    (outputName, name, args) <- parseMatlabHeader header
    bodySource <- selectBody outputName bodyLines
    body <- parseExpression bodySource
    return (PseudoFunction name args bodySource body (unlines (original : bodyLines ++ ["end"])))

parseMatlabHeader :: String -> Either String (Maybe String, String, [String])
parseMatlabHeader header =
    case splitTopLevelEquals header of
        Just (left, right) -> do
            outputName <- requireName (trim left) "function 输出变量名不合法。"
            (name, args) <- parseSignature (trim right)
            return (Just outputName, name, args)
        Nothing -> do
            (name, args) <- parseSignature header
            return (Nothing, name, args)

parseSignature :: String -> Either String (String, [String])
parseSignature signature =
    case break (== '(') signature of
        (namePart, '(':rest) -> do
            let name = trim namePart
            requireName name "函数名不合法。"
            argsPart <- case reverse rest of
                ')':revArgs -> Right (reverse revArgs)
                _ -> Left $ "函数参数列表缺少右括号：" ++ signature
            args <- parseArgs argsPart
            return (name, args)
        _ -> Left $ "函数签名必须形如 name(arg1, arg2)：" ++ signature

parseArgs :: String -> Either String [String]
parseArgs raw
    | null (trim raw) = Right []
    | otherwise = mapM parseArg (splitCommas raw)
  where
    parseArg arg = requireName (trim arg) ("函数参数名不合法：" ++ arg)

selectBody :: Maybe String -> [String] -> Either String String
selectBody _ [] = Left "函数体为空。"
selectBody Nothing [single] = Right single
selectBody (Just outputName) linesIn
    | startsWithBlockIf linesIn = parseOutputBlockIf outputName linesIn
    | startsWithSwitch linesIn = parseOutputSwitch outputName linesIn
    | otherwise = assignmentSequence outputName "函数体" linesIn
selectBody Nothing linesIn
    | startsWithBlockIf linesIn =
        Left "受限 if/else/end block 需要 MATLAB 风格输出变量，例如 function y = f(x)。"
    | startsWithSwitch linesIn =
        Left "受限 switch/case/otherwise/end block 需要 MATLAB 风格输出变量，例如 function y = f(x)。"
selectBody Nothing _ =
    Left "暂时只支持单表达式函数体。"

startsWithBlockIf :: [String] -> Bool
startsWithBlockIf (line:_) = isIfLine line
startsWithBlockIf [] = False

startsWithSwitch :: [String] -> Bool
startsWithSwitch (line:_) = isSwitchLine line
startsWithSwitch [] = False

parseOutputBlockIf :: String -> [String] -> Either String String
parseOutputBlockIf outputName (ifLine:rest) = do
    let cond = trim (drop (length "if") (trim ifLine))
    if null cond
        then Left "if 后面缺少条件表达式。"
        else return ()
    (branches, elseLines, afterIf) <- collectIfBranches cond rest
    case afterIf of
        [] -> return ()
        extra -> Left $ "if/end block 后面还有暂不支持的语句：" ++ unwords extra
    branchExprs <- mapM branchToExpr branches
    falseExpr <- assignmentSequence outputName "else" elseLines
    return (buildNestedIf branchExprs falseExpr)
  where
    branchToExpr (branchCond, branchLines) = do
        expr <- assignmentSequence outputName ("条件 " ++ branchCond) branchLines
        return (branchCond, expr)
parseOutputBlockIf _ [] = Left "函数体为空。"

collectIfBranches :: String -> [String] -> Either String ([(String, [String])], [String], [String])
collectIfBranches firstCond linesIn = go [(firstCond, [])] linesIn
  where
    go branches linesRest = do
        (branchLines, marker, afterBranch) <- collectUntilIfMarker linesRest
        let branches' = attachBranchLines branchLines branches
        case marker of
            IfElseIf cond -> go (branches' ++ [(cond, [])]) afterBranch
            IfElse -> do
                (elseLines, afterIf) <- collectElseBranch afterBranch
                return (branches', elseLines, afterIf)
            IfEnd -> Left "if block 缺少 else。"

attachBranchLines :: [String] -> [(String, [String])] -> [(String, [String])]
attachBranchLines branchLines [] = [("", branchLines)]
attachBranchLines branchLines branches =
    init branches ++ [(cond, branchLines)]
  where
    (cond, _) = last branches

collectUntilIfMarker :: [String] -> Either String ([String], IfMarker, [String])
collectUntilIfMarker = go 0 []
  where
    go _ _ [] = Left "if block 缺少 end。"
    go depth acc (line:rest)
        | isElseIfLine line && depth == 0 = Right (reverse acc, IfElseIf (elseIfCondition line), rest)
        | isElseLine line && depth == 0 = Right (reverse acc, IfElse, rest)
        | isEndLine line && depth == 0 = Right (reverse acc, IfEnd, rest)
        | isEndLine line = go (depth - 1) (line : acc) rest
        | isIfLine line = go (depth + 1) (line : acc) rest
        | isSwitchLine line = go (depth + 1) (line : acc) rest
        | otherwise = go depth (line : acc) rest

collectElseBranch :: [String] -> Either String ([String], [String])
collectElseBranch = go 0 []
  where
    go _ _ [] = Left "if block 缺少 end。"
    go depth acc (line:rest)
        | isEndLine line && depth == 0 = Right (reverse acc, rest)
        | isElseLine line && depth == 0 = Left "暂不支持同一个 if block 中出现多个 else。"
        | isElseIfLine line && depth == 0 = Left "elseif 必须出现在 else 之前。"
        | isEndLine line = go (depth - 1) (line : acc) rest
        | isIfLine line = go (depth + 1) (line : acc) rest
        | isSwitchLine line = go (depth + 1) (line : acc) rest
        | otherwise = go depth (line : acc) rest

elseIfCondition :: String -> String
elseIfCondition line
    | startsKeyword "elseif" line = trim (drop (length "elseif") (trim line))
    | startsTwoKeywords "else" "if" line = trim (drop (length "else if") (trim line))
    | otherwise = ""

buildNestedIf :: [(String, String)] -> String -> String
buildNestedIf [] elseExpr = elseExpr
buildNestedIf ((cond, expr):rest) elseExpr =
    "if " ++ cond ++ " then " ++ expr ++ " else " ++ buildNestedIf rest elseExpr

parseOutputSwitch :: String -> [String] -> Either String String
parseOutputSwitch outputName (switchLine:rest) = do
    let subject = trim (drop (length "switch") (trim switchLine))
    if null subject
        then Left "switch 后面缺少待匹配表达式。"
        else return ()
    (cases, otherwiseLines, afterSwitch) <- collectSwitchCases rest
    case afterSwitch of
        [] -> return ()
        extra -> Left $ "switch/end block 后面还有暂不支持的语句：" ++ unwords extra
    caseExprs <- mapM caseToExpr cases
    otherwiseExpr <- assignmentSequence outputName "otherwise" otherwiseLines
    return (buildNestedSwitch subject caseExprs otherwiseExpr)
  where
    caseToExpr (caseValue, branchLines) = do
        expr <- assignmentSequence outputName ("case " ++ caseValue) branchLines
        return (caseValue, expr)
parseOutputSwitch _ [] = Left "函数体为空。"

collectSwitchCases :: [String] -> Either String ([(String, [String])], [String], [String])
collectSwitchCases [] = Left "switch block 缺少 case。"
collectSwitchCases (line:rest)
    | isCaseLine line = go [(caseCondition line, [])] rest
    | otherwise = Left "switch block 必须先出现 case。"
  where
    go cases linesRest = do
        (branchLines, marker, afterBranch) <- collectUntilSwitchMarker linesRest
        let cases' = attachCaseLines branchLines cases
        case marker of
            SwitchCase value -> go (cases' ++ [(value, [])]) afterBranch
            SwitchOtherwise -> do
                (otherwiseLines, afterSwitch) <- collectOtherwiseBranch afterBranch
                return (cases', otherwiseLines, afterSwitch)
            SwitchEnd -> Left "switch block 缺少 otherwise。"

attachCaseLines :: [String] -> [(String, [String])] -> [(String, [String])]
attachCaseLines branchLines [] = [("", branchLines)]
attachCaseLines branchLines cases =
    init cases ++ [(value, branchLines)]
  where
    (value, _) = last cases

collectUntilSwitchMarker :: [String] -> Either String ([String], SwitchMarker, [String])
collectUntilSwitchMarker = go 0 []
  where
    go _ _ [] = Left "switch block 缺少 end。"
    go depth acc (line:rest)
        | isCaseLine line && depth == 0 = Right (reverse acc, SwitchCase (caseCondition line), rest)
        | isOtherwiseLine line && depth == 0 = Right (reverse acc, SwitchOtherwise, rest)
        | isEndLine line && depth == 0 = Right (reverse acc, SwitchEnd, rest)
        | isEndLine line = go (depth - 1) (line : acc) rest
        | isIfLine line = go (depth + 1) (line : acc) rest
        | isSwitchLine line = go (depth + 1) (line : acc) rest
        | otherwise = go depth (line : acc) rest

collectOtherwiseBranch :: [String] -> Either String ([String], [String])
collectOtherwiseBranch = go 0 []
  where
    go _ _ [] = Left "switch block 缺少 end。"
    go depth acc (line:rest)
        | isEndLine line && depth == 0 = Right (reverse acc, rest)
        | isCaseLine line && depth == 0 = Left "case 必须出现在 otherwise 之前。"
        | isOtherwiseLine line && depth == 0 = Left "switch block 中只能出现一个 otherwise。"
        | isEndLine line = go (depth - 1) (line : acc) rest
        | isIfLine line = go (depth + 1) (line : acc) rest
        | isSwitchLine line = go (depth + 1) (line : acc) rest
        | otherwise = go depth (line : acc) rest

caseCondition :: String -> String
caseCondition line = trim (drop (length "case") (trim line))

buildNestedSwitch :: String -> [(String, String)] -> String -> String
buildNestedSwitch _ [] otherwiseExpr = otherwiseExpr
buildNestedSwitch subject ((caseValue, expr):rest) otherwiseExpr =
    "if (" ++ subject ++ ") == (" ++ caseValue ++ ") then "
    ++ expr ++ " else " ++ buildNestedSwitch subject rest otherwiseExpr

assignmentSequence :: String -> String -> [String] -> Either String String
assignmentSequence outputName context linesIn =
    case linesIn of
        [] -> Left $ context ++ " 为空；需要最后给 " ++ outputName ++ " 赋值。"
        _ -> do
            assignments <- mapM (parseAssignmentLine context) linesIn
            buildAssignmentSequence outputName context assignments

parseAssignmentLine :: String -> String -> Either String (String, String)
parseAssignmentLine context line =
    case splitTopLevelEquals line of
        Just (left, right) -> do
            name <- requireName (trim left) (context ++ " 中的赋值左侧不是合法变量名：" ++ trim left)
            let expr = trim right
            if null expr
                then Left $ context ++ " 中的赋值缺少右侧表达式：" ++ line
                else Right (name, expr)
        Nothing ->
            Left $ context ++ " 中只支持赋值语句 name = expression，发现：" ++ line

buildAssignmentSequence :: String -> String -> [(String, String)] -> Either String String
buildAssignmentSequence outputName context assignments =
    let locals = init assignments
        (lastName, resultExpr) = last assignments
    in if lastName /= outputName
        then Left $ context ++ " 的最后一条语句必须给输出变量 " ++ outputName ++ " 赋值。"
        else if outputName `elem` map fst locals
            then Left $ context ++ " 中输出变量 " ++ outputName ++ " 只能在最后赋值一次。"
            else Right (foldr localLet resultExpr locals)

localLet :: (String, String) -> String -> String
localLet (name, expr) body =
    "let " ++ name ++ " = " ++ expr ++ " in " ++ body

splitTopLevelEquals :: String -> Maybe (String, String)
splitTopLevelEquals input = go 0 "" input
  where
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
    go _ current [] = [current]
    go depth current (c:cs)
        | c == '(' = go (depth + 1) (current ++ [c]) cs
        | c == ')' = go (depth - 1) (current ++ [c]) cs
        | c == ',' && depth == 0 = current : go depth "" cs
        | otherwise = go depth (current ++ [c]) cs

cleanLines :: String -> [String]
cleanLines = filter (not . null) . map (trim . stripSemicolon . stripComment) . lines

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

renderTokens :: [Token] -> String
renderTokens = unwords . map renderToken

renderToken :: Token -> String
renderToken token =
    case token of
        TokIdent name -> name
        TokInt n -> show n
        TokBool True -> "True"
        TokBool False -> "False"
        TokLet -> "let"
        TokIn -> "in"
        TokLambda -> "\\"
        TokArrow -> "->"
        TokEquals -> "="
        TokLParen -> "("
        TokRParen -> ")"
        TokComma -> ","
        TokIf -> "if"
        TokThen -> "then"
        TokElse -> "else"
        TokFunction -> "function"
        TokEnd -> "end"
        TokOp op -> op

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
