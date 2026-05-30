module Infer
  ( inferTop
  , inferTopWithEnv
  , mgu
  , instantiate
  , preludeEnv
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Syntax
import Types

-- Infer a指的是算出a的推导动作，需要下标计数
newtype Infer a = Infer { runInfer :: Int -> Either TypeError (a, Int) }

-- Functor是Applicative的前提
instance Functor Infer where
  fmap f action = Infer $ \supply ->  -- 对成功结果做一次普通的函数变换
    case runInfer action supply of  -- action指的是Infer a的动作
      Left err -> Left err
      Right (value, supply') -> Right (f value, supply')

instance Applicative Infer where
  pure value = Infer $ \supply -> Right (value, supply)  -- pure的作用是把普通值放入Infer推导的上下文（不改计数）
  sf <*> sx = Infer $ \supply ->  -- (<*>) :: Infer (a -> b) -> Infer a -> Infer b
    case runInfer sf supply of
      Left err -> Left err
      Right (f, supply1) ->
        case runInfer sx supply1 of
          Left err -> Left err
          Right (x, supply2) -> Right (f x, supply2)

-- Monad 的核心是 >>=，读作 bind，(>>=) :: Infer a -> (a -> Infer b) -> Infer b
instance Monad Infer where
  action >>= next = Infer $ \supply ->
    case runInfer action supply of
      Left err -> Left err
      Right (value, supply') -> runInfer (next value) supply'

-- 输出报错，包装成推导动作
throwInfer :: TypeError -> Infer a
throwInfer err = Infer $ \_ -> Left err

-- 生成新的类型变量，下标迭代（一定成功）
fresh :: Infer Type
fresh = Infer $ \supply -> Right (TVar ("a" ++ show supply), supply + 1)

-- 多态的实例化，对每个变量执行一次fresh
instantiate :: Scheme -> Infer Type
instantiate (Scheme vars t) = do
  freshVars <- mapM (const fresh) vars  -- fresh不是函数（带有上下文），不能直接map fresh
  let subst = Map.fromList (zip vars freshVars)
  pure (apply subst t)  -- 把Type包装成Infer Type

-- 顶层推导
inferTop :: Exp -> Either TypeError Scheme
inferTop = inferTopWithEnv preludeEnv

-- infer env expr返回是一个推导动作，因此需要变量计数器supply
-- 最后要把推导得到的类型替换作用的类型t上和环境env上（环境里的类型也可能在推导过程中被进一步约束）
-- 泛化是因为结果可能是多态
inferTopWithEnv :: TypeEnv -> Exp -> Either TypeError Scheme
inferTopWithEnv env expr =
  case runInfer (infer env expr) 0 of
    Left err -> Left err
    Right ((subst, t), _) -> Right (generalize (apply subst env) (apply subst t))

-- Algorithm W.
-- 返回是一个推导动作（推导过程中会产生约束）
infer :: TypeEnv -> Exp -> Infer (Subst, Type)
infer env expr =
  case expr of  -- 不同表达式的推导不同
    -- Variable rule:
    -- 找不到就报错，找到了就实例化（可能有多次不同的应用）
    EVar name ->
      case lookupEnv env name of
        Nothing -> throwInfer (UnboundVariable name)
        Just scheme -> do
          t <- instantiate scheme
          pure (nullSubst, t)

    -- Literal rule:
    -- Integer literals have type Int, boolean literals have type Bool.
    ELit lit ->
      pure (nullSubst, inferLit lit)

    -- Lambda rule:
    -- For \x -> body:
    --   1. 给x生成一个新的类型变量
    --   2. 在新环境下推导body（先移除就约束再添加新约束）
    --   3. 返回 xType -> bodyType.
    EAbs name body -> do
      tv <- fresh
      let env' = extend (remove env name) name (Scheme [] tv)
      (s1, t1) <- infer env' body
      pure (s1, TFun (apply s1 tv) t1)  -- lambda类型规则就是（参数类型 -> 函数体类型）

    -- Application rule:
    -- For f x:
    --   1. 先推导函数部分（一定是函数类型）
    --   2. 再推导参数部分（推导参数时，环境已经更新）
    --   3. 确保函数类型和（参数类型->结果类型）一致
    EApp fun arg -> do
      tv <- fresh  -- 结果类型的类型变量
      (s1, tFun) <- infer env fun
      (s2, tArg) <- infer (apply s1 env) arg
      s3 <- mgu (apply s2 tFun) (TFun tArg tv)
      pure (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)

    -- Let rule:
    -- For let x = value in body:
    --   1. 先推导value;
    --   2. 泛化value的类型成一个多态的类型方案;
    --   3. 在body推导前更新环境，添加x的类型方案;
    ELet name value body -> do
      (s1, t1) <- infer env value
      let envAfterValue = apply s1 env  -- 在推导完value类型后更新环境
          scheme = generalize envAfterValue t1  -- 泛化便于后续实例化成不同类型
          envForBody = extend (remove envAfterValue name) name scheme  -- 在body推导前更新环境
      (s2, t2) <- infer envForBody body
      pure (s2 `composeSubst` s1, t2)

    -- If rule:
    -- 条件必须是Bool，且两个分支的类型必须一致
    EIf cond yes no -> do
      (s1, tCond) <- infer env cond
      sBool <- mgu tCond TBool
      let sCond = sBool `composeSubst` s1  -- 每次推导都可能产生新的约束，需要复合然后更新环境
      (s2, tYes) <- infer (apply sCond env) yes
      (s3, tNo) <- infer (apply (s2 `composeSubst` sCond) env) no
      s4 <- mgu (apply s3 tYes) tNo  -- 分支类型相同
      pure (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` sCond, apply s4 tNo)  -- 统一分支类型时可能有新的约束，最后选tNo和tYes一样

    -- Binary operators 
    EBin op left right ->
      inferBin env op left right

inferLit :: Lit -> Type
inferLit (LInt _) = TInt
inferLit (LBool _) = TBool

inferBin :: TypeEnv -> BinOp -> Exp -> Exp -> Infer (Subst, Type)
inferBin env op left right = do
  (s1, tLeft) <- infer env left
  (s2, tRight) <- infer (apply s1 env) right
  case op of
    Add -> inferIntBin s1 s2 tLeft tRight
    Sub -> inferIntBin s1 s2 tLeft tRight
    Mul -> inferIntBin s1 s2 tLeft tRight
    And -> inferBoolBin s1 s2 tLeft tRight
    Or -> inferBoolBin s1 s2 tLeft tRight
    Eq -> do
      s3 <- mgu (apply s2 tLeft) tRight
      pure (s3 `composeSubst` s2 `composeSubst` s1, TBool)

inferIntBin :: Subst -> Subst -> Type -> Type -> Infer (Subst, Type)
inferIntBin s1 s2 tLeft tRight = do
  s3 <- mgu (apply s2 tLeft) TInt
  s4 <- mgu (apply s3 tRight) TInt
  pure (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, TInt)

inferBoolBin :: Subst -> Subst -> Type -> Type -> Infer (Subst, Type)
inferBoolBin s1 s2 tLeft tRight = do
  s3 <- mgu (apply s2 tLeft) TBool
  s4 <- mgu (apply s3 tRight) TBool
  pure (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, TBool)

-- 合一两个类型
mgu :: Type -> Type -> Infer Subst
mgu (TFun left right) (TFun left' right') = do  -- 函数类型相等等价于参数和结果类型都相等
  s1 <- mgu left left'
  s2 <- mgu (apply s1 right) (apply s1 right')
  pure (s2 `composeSubst` s1)
mgu (TVar name) t = bindVar name t  -- 绑定类型变量
mgu t (TVar name) = bindVar name t
mgu TInt TInt = pure nullSubst
mgu TBool TBool = pure nullSubst
mgu t1 t2 = throwInfer (TypesDoNotUnify t1 t2)  -- 只有上面那些合一才成功

-- 把一个类型变量绑定到类型上
bindVar :: String -> Type -> Infer Subst
bindVar name t
  | t == TVar name = pure nullSubst  -- 绑定自身直接返回空替换
  | name `Set.member` ftv t = throwInfer (InfiniteType name t)  -- 类型变量出现在类型的自由变量中则报错
  | otherwise = pure (Map.singleton name t)  -- 正常绑定（创建一个替换）

-- A tiny built-in environment.
-- More predefined functions can be added here.
preludeEnv :: TypeEnv
preludeEnv =
  foldl add emptyEnv
    [ ("not", Scheme [] (TFun TBool TBool))
    ]
  where
    add env (name, scheme) = extend env name scheme
