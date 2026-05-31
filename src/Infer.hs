module Infer
  ( inferTop
  , inferTopWithEnv
  , infer
  , runInferM
  , runInferMTrace
  , mgu
  , instantiate
  , preludeEnv
  , inferWithPrelude
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Syntax
import TraceEvents hiding (depth, env, expr, subst, typ, tv, scheme, t1, t2, rule, detail)
import Types

newtype Infer a =
  Infer { unInfer :: Int -> [TraceEvent] -> Either (TypeError, [TraceEvent]) (a, Int, [TraceEvent]) }

instance Functor Infer where
  fmap f (Infer action) = Infer $ \supply traceLog ->
    case action supply traceLog of
      Left errTrace -> Left errTrace
      Right (value, supply', traceLog') -> Right (f value, supply', traceLog')

instance Applicative Infer where
  pure value = Infer $ \supply traceLog -> Right (value, supply, traceLog)
  Infer sf <*> Infer sx = Infer $ \supply traceLog ->
    case sf supply traceLog of
      Left errTrace -> Left errTrace
      Right (f, supply1, traceLog1) ->
        case sx supply1 traceLog1 of
          Left errTrace -> Left errTrace
          Right (x, supply2, traceLog2) -> Right (f x, supply2, traceLog2)

instance Monad Infer where
  Infer action >>= next = Infer $ \supply traceLog ->
    case action supply traceLog of
      Left errTrace -> Left errTrace
      Right (value, supply', traceLog') -> unInfer (next value) supply' traceLog'

throwInfer :: TypeError -> Infer a
throwInfer err = Infer $ \_supply traceLog -> Left (err, traceLog)

logTrace :: TraceEvent -> Infer ()
logTrace ev = Infer $ \supply traceLog -> Right ((), supply, traceLog ++ [ev])

logStep :: Int -> String -> Infer ()
logStep d msg = logTrace (AlgoStep d msg)

showEnv :: TypeEnv -> String
showEnv env = "环境：" ++ show env

inferPairMsg :: String -> Subst -> Type -> String
inferPairMsg context s t =
  context ++ "得到替换" ++ show s ++ "和类型" ++ show t

composeResultMsg :: Subst -> Type -> String
composeResultMsg s t =
  "复合替换" ++ show s ++ "和类型" ++ show t

runInferM :: Infer a -> Int -> Either TypeError (a, Int, [TraceEvent])
runInferM (Infer action) supply =
  case action supply [] of
    Left (err, _) -> Left err
    Right (value, supply', traceLog) -> Right (value, supply', traceLog)

runInferMTrace :: Infer a -> Int -> Either (TypeError, [TraceEvent]) (a, Int, [TraceEvent])
runInferMTrace (Infer action) supply = action supply []

fresh :: Int -> Infer Type
fresh d = Infer $ \supply traceLog ->
  let tv = TVar ("a" ++ show supply)
  in Right (tv, supply + 1, traceLog ++ [FreshVar d tv])

instantiate :: Int -> Scheme -> Infer Type
instantiate depth (Scheme vars t) = do
  freshVars <- mapM (const (fresh depth)) vars
  let subst = Map.fromList (zip vars freshVars)
      typ = apply subst t
  logStep depth ("instantiate：生成新变量" ++ show freshVars ++ "并执行替换" ++ show subst ++ "，得到类型" ++ show typ)
  pure typ

inferTop :: Exp -> Either TypeError Scheme
inferTop expr =
  case runInferM (inferWithPrelude 0 expr) 0 of
    Left err -> Left err
    Right ((env, subst, t), _, _) ->
      Right (generalize (apply subst env) (apply subst t))

inferWithPrelude :: Int -> Exp -> Infer (TypeEnv, Subst, Type)
inferWithPrelude depth expr = do
  logStep depth "preludeEnv：预定义环境，包含not和negate函数"
  (subst, t) <- infer depth preludeEnv expr
  pure (preludeEnv, subst, t)

inferTopWithEnv :: TypeEnv -> Exp -> Either TypeError Scheme
inferTopWithEnv env expr =
  case runInferM (infer 0 env expr) 0 of
    Left err -> Left err
    Right ((subst, t), _, _) ->
      Right (generalize (apply subst env) (apply subst t))

infer :: Int -> TypeEnv -> Exp -> Infer (Subst, Type)
infer depth env expr = do
  logTrace (EnterInfer depth expr env)
  result <- inferExpr depth env expr
  let (subst, typ) = result
  logTrace (ExitInfer depth subst typ)
  pure result

inferExpr :: Int -> TypeEnv -> Exp -> Infer (Subst, Type)
inferExpr depth env expr =
  case expr of
    EVar name -> do
      logStep depth ("EVar：变量 " ++ name)
      case lookupEnv env name of
        Nothing -> do
          logStep depth ("未找到绑定，错误：未绑定变量 " ++ name)
          throwInfer (UnboundVariable name)
        Just sch -> do
          logStep depth ("lookup: 在环境中查找 " ++ name ++ "，得到多态方案scheme=" ++ show sch)
          t <- instantiate depth sch
          logStep depth (inferPairMsg ("变量 " ++ name ++ " ") nullSubst t)
          pure (nullSubst, t)

    ELit lit ->
      case lit of
        LInt n -> do
          logStep depth ("ELit：整数字面量 " ++ show n)
          logStep depth (inferPairMsg "字面量" nullSubst TInt)
          pure (nullSubst, TInt)
        LBool b -> do
          logStep depth ("ELit：布尔字面量 " ++ show b)
          logStep depth (inferPairMsg "字面量" nullSubst TBool)
          pure (nullSubst, TBool)

    EAbs name body -> do
      logStep depth ("EAbs：λ" ++ name ++ ". …")
      tv <- fresh depth
      let env' = extend (remove env name) name (Scheme [] tv)
      logStep depth ("extend: 用" ++ name ++ "和新变量" ++ show tv ++ "扩展环境。" ++ showEnv env')
      (s1, t1) <- infer (depth + 1) env' body
      let resultType = TFun (apply s1 tv) t1
      logStep depth (inferPairMsg "lambda " s1 resultType)
      pure (s1, resultType)

    EApp fun arg -> do
      logStep depth "EApp：函数应用"
      tv <- fresh depth
      (s1, tFun) <- infer (depth + 1) env fun
      logStep depth (inferPairMsg "推断函数 " s1 tFun)
      let env1 = apply s1 env
      logStep depth ("在" ++ showEnv env1 ++ " 下推断函数参数")
      (s2, tArg) <- infer (depth + 1) env1 arg
      logStep depth (inferPairMsg "推断函数参数" s2 tArg)
      let appliedFun = apply s2 tFun
          expected = TFun tArg tv
      logStep depth
        ( "合一约束：tFun ~ tArg -> tv，即 "
            ++ show appliedFun
            ++ " ~ "
            ++ show expected
        )
      s3 <- mgu depth appliedFun expected
      let resultType = apply s3 tv
          resultSubst = s3 `composeSubst` s2 `composeSubst` s1
      logStep depth (composeResultMsg resultSubst resultType)
      pure (resultSubst, resultType)

    ELet name value body -> do
      logStep depth ("ELet：let " ++ name ++ " = … in …")
      (s1, t1) <- infer (depth + 1) env value
      logStep depth (inferPairMsg "推断 value " s1 t1)
      let envAfterValue = apply s1 env
      logStep depth ("应用替换到环境，更新环境。" ++ showEnv envAfterValue)
      let scheme = generalize envAfterValue t1
      logTrace (GeneralizeE depth t1 scheme)
      let envForBody = extend (remove envAfterValue name) name scheme
      logStep depth ("extend: 用" ++ name ++ "和新变量" ++ show scheme ++ "扩展环境。" ++ showEnv envForBody)
      (s2, t2) <- infer (depth + 1) envForBody body
      logStep depth (inferPairMsg "推断函数体" s2 t2)
      let resultSubst = s2 `composeSubst` s1
      logStep depth (composeResultMsg resultSubst t2)
      pure (resultSubst, t2)

    EIf cond yes no -> do
      logStep depth "EIf：条件表达式"
      (s1, tCond) <- infer (depth + 1) env cond
      logStep depth (inferPairMsg "推断条件condition" s1 tCond)
      logStep depth ("合一约束：tCond ~ Bool，即 " ++ show tCond ++ " ~ Bool")
      sBool <- mgu depth tCond TBool
      let sCond = sBool `composeSubst` s1
      logStep depth ("复合替换sCond=" ++ show sCond)
      let envCond = apply sCond env
      logStep depth ("在" ++ showEnv envCond ++ " 下推断 then 分支")
      (s2, tYes) <- infer (depth + 1) envCond yes
      logStep depth (inferPairMsg "推断 then 分支 " s2 tYes)
      let envElse = apply (s2 `composeSubst` sCond) env
      logStep depth ("在" ++ showEnv envElse ++ " 下推断 else 分支")
      (s3, tNo) <- infer (depth + 1) envElse no
      logStep depth (inferPairMsg "推断 else 分支 " s3 tNo)
      logStep depth
        ( "合一约束：s3(tYes) ~ tNo，即 "
            ++ show (apply s3 tYes)
            ++ " ~ "
            ++ show tNo
        )
      s4 <- mgu depth (apply s3 tYes) tNo
      let resultSubst = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` sCond
          resultType = apply s4 tNo
      logStep depth (composeResultMsg resultSubst resultType)
      pure (resultSubst, resultType)

    EBin op left right ->
      inferBin depth env op left right

    EList items ->
      inferList depth env items

    EBlock stmts outputName -> do
      (s1, env') <- inferStmts depth env stmts
      case lookupEnv (apply s1 env') outputName of
        Nothing -> throwInfer (UnboundVariable outputName)
        Just scheme -> do
          t <- instantiate depth scheme
          pure (s1, t)

inferList :: Int -> TypeEnv -> [Exp] -> Infer (Subst, Type)
inferList depth _ [] = do
  tv <- fresh depth
  pure (nullSubst, TList tv)
inferList depth env (item:items) = do
  (s1, itemType) <- infer (depth + 1) env item
  inferListRest depth (apply s1 env) s1 itemType items

inferListRest :: Int -> TypeEnv -> Subst -> Type -> [Exp] -> Infer (Subst, Type)
inferListRest _ _ subst itemType [] =
  pure (subst, TList (apply subst itemType))
inferListRest depth env subst itemType (item:items) = do
  (sItem, nextType) <- infer (depth + 1) env item
  sSame <- mgu depth (apply sItem itemType) nextType
  let subst' = sSame `composeSubst` sItem `composeSubst` subst
      env' = apply subst' env
      itemType' = apply subst' itemType
  inferListRest depth env' subst' itemType' items

inferStmts :: Int -> TypeEnv -> [Stmt] -> Infer (Subst, TypeEnv)
inferStmts _ env [] = pure (nullSubst, env)
inferStmts depth env (stmt:stmts) = do
  (s1, env1) <- inferStmt depth env stmt
  (s2, env2) <- inferStmts depth (apply s1 env1) stmts
  pure (s2 `composeSubst` s1, env2)

inferStmt :: Int -> TypeEnv -> Stmt -> Infer (Subst, TypeEnv)
inferStmt depth env stmt =
  case stmt of
    SAssign name expr -> inferAssign depth env name expr

    SIfStmt cond yes no -> do
      (s1, tCond) <- infer (depth + 1) env cond
      sBool <- mgu depth tCond TBool
      let sCond = sBool `composeSubst` s1
          envCond = apply sCond env
      (sYes, envYes) <- inferStmts depth envCond yes
      (sNo, envNo) <- inferStmts depth envCond no
      sMerge <- mergeEnvs depth (apply sNo envYes) (apply sYes envNo)
      pure (sMerge `composeSubst` sNo `composeSubst` sYes `composeSubst` sCond, apply sMerge (apply sNo envYes))

    SSwitchStmt subject cases otherwiseBranch -> do
      (sSubject, tSubject) <- infer (depth + 1) env subject
      (sCases, envCases) <- inferCases depth (apply sSubject env) (apply sSubject tSubject) cases
      (sOtherwise, envOtherwise) <- inferStmts depth (apply sCases (apply sSubject env)) otherwiseBranch
      sMerge <- mergeEnvs depth (apply sOtherwise envCases) envOtherwise
      pure (sMerge `composeSubst` sOtherwise `composeSubst` sCases `composeSubst` sSubject, apply sMerge (apply sOtherwise envCases))

    SFor itemName items body -> do
      tv <- fresh depth
      (sItems, tItems) <- infer (depth + 1) env items
      sList <- mgu depth tItems (TList tv)
      let sLoop = sList `composeSubst` sItems
          itemType = apply sLoop tv
          envLoop = extend (remove (apply sLoop env) itemName) itemName (Scheme [] itemType)
      (sBody, envBody) <- inferStmts depth envLoop body
      pure (sBody `composeSubst` sLoop, remove envBody itemName)

    SWhile cond body -> do
      (sCond, tCond) <- infer (depth + 1) env cond
      sBool <- mgu depth tCond TBool
      let sLoop = sBool `composeSubst` sCond
      (sBody, envBody) <- inferStmts depth (apply sLoop env) body
      pure (sBody `composeSubst` sLoop, envBody)

inferAssign :: Int -> TypeEnv -> String -> Exp -> Infer (Subst, TypeEnv)
inferAssign depth env name expr = do
  (s1, tExpr) <- infer (depth + 1) env expr
  let envAfterExpr = apply s1 env
  case lookupEnv envAfterExpr name of
    Nothing ->
      pure (s1, extend envAfterExpr name (Scheme [] tExpr))
    Just scheme -> do
      tOld <- instantiate depth scheme
      sSame <- mgu depth tOld tExpr
      let subst = sSame `composeSubst` s1
      pure (subst, extend (apply subst env) name (Scheme [] (apply subst tExpr)))

inferCases :: Int -> TypeEnv -> Type -> [(Exp, [Stmt])] -> Infer (Subst, TypeEnv)
inferCases _ env _ [] = pure (nullSubst, env)
inferCases depth env subjectType ((caseExpr, body):cases) = do
  (sCase, tCase) <- infer (depth + 1) env caseExpr
  sSame <- mgu depth (apply sCase subjectType) tCase
  let sHead = sSame `composeSubst` sCase
      envHead = apply sHead env
  (sBody, envBody) <- inferStmts depth envHead body
  (sRest, envRest) <- inferCases depth (apply sBody envHead) (apply sBody (apply sHead subjectType)) cases
  sMerge <- mergeEnvs depth (apply sRest envBody) envRest
  pure (sMerge `composeSubst` sRest `composeSubst` sBody `composeSubst` sHead, apply sMerge (apply sRest envBody))

mergeEnvs :: Int -> TypeEnv -> TypeEnv -> Infer Subst
mergeEnvs depth (TypeEnv left) (TypeEnv right) =
  mergeNames nullSubst (Map.keysSet left `Set.union` Map.keysSet right)
  where
    mergeNames subst names =
      case Set.minView names of
        Nothing -> pure subst
        Just (name, rest) ->
          case (Map.lookup name left, Map.lookup name right) of
            (Just sLeft, Just sRight) -> do
              tLeft <- instantiate depth sLeft
              tRight <- instantiate depth sRight
              sSame <- mgu depth (apply subst tLeft) (apply subst tRight)
              mergeNames (sSame `composeSubst` subst) rest
            _ -> mergeNames subst rest

inferBin :: Int -> TypeEnv -> BinOp -> Exp -> Exp -> Infer (Subst, Type)
inferBin depth env op left right = do
  logStep depth ("EBin：二元运算 " ++ show op)
  (s1, tLeft) <- infer (depth + 1) env left
  logStep depth (inferPairMsg "推断左操作数 " s1 tLeft)
  let env1 = apply s1 env
  logStep depth ("在" ++ showEnv env1 ++ " 下推断右操作数")
  (s2, tRight) <- infer (depth + 1) env1 right
  logStep depth (inferPairMsg "推断右操作数 " s2 tRight)
  case op of
    Add -> inferIntBin depth s1 s2 tLeft tRight
    Sub -> inferIntBin depth s1 s2 tLeft tRight
    Mul -> inferIntBin depth s1 s2 tLeft tRight
    Div -> inferIntBin depth s1 s2 tLeft tRight
    And -> inferBoolBin depth s1 s2 tLeft tRight
    Or -> inferBoolBin depth s1 s2 tLeft tRight
    Eq -> inferEqBin depth s1 s2 tLeft tRight
    Ne -> inferEqBin depth s1 s2 tLeft tRight
    Lt -> inferIntCompare depth s1 s2 tLeft tRight
    Le -> inferIntCompare depth s1 s2 tLeft tRight
    Gt -> inferIntCompare depth s1 s2 tLeft tRight
    Ge -> inferIntCompare depth s1 s2 tLeft tRight

inferEqBin :: Int -> Subst -> Subst -> Type -> Type -> Infer (Subst, Type)
inferEqBin depth s1 s2 tLeft tRight = do
  logStep depth
    ( "(==) 合一约束：s2(tLeft) ~ tRight，即 "
        ++ show (apply s2 tLeft)
        ++ " ~ "
        ++ show tRight
    )
  s3 <- mgu depth (apply s2 tLeft) tRight
  let resultSubst = s3 `composeSubst` s2 `composeSubst` s1
  logStep depth (composeResultMsg resultSubst TBool)
  pure (resultSubst, TBool)

inferIntBin :: Int -> Subst -> Subst -> Type -> Type -> Infer (Subst, Type)
inferIntBin depth s1 s2 tLeft tRight = do
  logStep depth
    ("算术合一：s2(tLeft) ~ Int，即 " ++ show (apply s2 tLeft) ++ " ~ Int")
  s3 <- mgu depth (apply s2 tLeft) TInt
  logStep depth
    ("算术合一：s3(tRight) ~ Int，即 " ++ show (apply s3 tRight) ++ " ~ Int")
  s4 <- mgu depth (apply s3 tRight) TInt
  let resultSubst = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
  logStep depth (composeResultMsg resultSubst TInt)
  pure (resultSubst, TInt)

inferBoolBin :: Int -> Subst -> Subst -> Type -> Type -> Infer (Subst, Type)
inferBoolBin depth s1 s2 tLeft tRight = do
  logStep depth
    ("逻辑合一：s2(tLeft) ~ Bool，即 " ++ show (apply s2 tLeft) ++ " ~ Bool")
  s3 <- mgu depth (apply s2 tLeft) TBool
  logStep depth
    ("逻辑合一：s3(tRight) ~ Bool，即 " ++ show (apply s3 tRight) ++ " ~ Bool")
  s4 <- mgu depth (apply s3 tRight) TBool
  let resultSubst = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
  logStep depth (composeResultMsg resultSubst TBool)
  pure (resultSubst, TBool)

inferIntCompare :: Int -> Subst -> Subst -> Type -> Type -> Infer (Subst, Type)
inferIntCompare depth s1 s2 tLeft tRight = do
  logStep depth
    ("比较合一：s2(tLeft) ~ Int，即 " ++ show (apply s2 tLeft) ++ " ~ Int")
  s3 <- mgu depth (apply s2 tLeft) TInt
  logStep depth
    ("比较合一：s3(tRight) ~ Int，即 " ++ show (apply s3 tRight) ++ " ~ Int")
  s4 <- mgu depth (apply s3 tRight) TInt
  let resultSubst = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
  logStep depth (composeResultMsg resultSubst TBool)
  pure (resultSubst, TBool)

mgu :: Int -> Type -> Type -> Infer Subst
mgu depth t1 t2 = do
  logTrace (UnifyStart depth t1 t2)
  result <- mguImpl depth t1 t2
  logTrace (UnifyDone depth result)
  pure result

mguImpl :: Int -> Type -> Type -> Infer Subst
mguImpl depth (TFun left right) (TFun left' right') = do
  logStep depth "mgu：分解函数，输入、返回分别合一"
  s1 <- mgu (depth + 1) left left'
  s2 <- mgu (depth + 1) (apply s1 right) (apply s1 right')
  pure (s2 `composeSubst` s1)
mguImpl depth (TVar name) t = bindVar depth name t
mguImpl depth t (TVar name) = bindVar depth name t
mguImpl _ TInt TInt =
  pure nullSubst
mguImpl _ TBool TBool =
  pure nullSubst
mguImpl depth (TList left) (TList right) =
  mgu (depth + 1) left right
mguImpl _ t1 t2 = throwInfer (TypesDoNotUnify t1 t2)

bindVar :: Int -> String -> Type -> Infer Subst
bindVar depth name t
  | t == TVar name = do
      logStep depth ("varBind：" ++ name ++ " 与自身相同，得到空替换[]")
      pure nullSubst
  | name `Set.member` ftv t = do
      logStep depth
        ("occurs check 失败：" ++ name ++ " 出现在ftv(" ++ show t ++ ") 中")
      throwInfer (InfiniteType name t)
  | otherwise = do
      let s = Map.singleton name t
      logStep depth ("varBind：绑定 " ++ name ++ " -> " ++ show t ++ "，得到替换" ++ show s)
      pure s

preludeEnv :: TypeEnv
preludeEnv =
  foldl add emptyEnv
    [ ("not", Scheme [] (TFun TBool TBool))
    , ("negate", Scheme [] (TFun TInt TInt))
    ]
  where
    add env (name, scheme) = extend env name scheme
