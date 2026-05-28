-- 递归推断表达式类型
module Inference where
import Control.Monad.Except (catchError, throwError)
import qualified Data.Map as Map
import Types
import Syntax
import TypeEnv
import MonadStack
import Unification
import Pretty ()

tiLit :: Lit -> TI (Subst, Type)
tiLit (LInt _) = return (nullSubst, TInt)
tiLit (LBool _) = return (nullSubst, TBool)

ti :: TypeEnv -> Exp -> TI (Subst, Type)
ti (TypeEnv env) (EVar n) = 
    case Map.lookup n env of
        Nothing -> throwError $ "未绑定的变量：" ++ n
        Just sigma -> do t <- instantiate sigma
                         return (nullSubst, t)
ti _ (ELit l) = tiLit l
ti env (EAbs n e) = do 
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `Map.union` (Map.singleton n (Scheme [] tv)))
    (s1, t1) <- ti env'' e
    return (s1, TFun (apply s1 tv) t1)
ti env exp@(EApp e1 e2) = do
    tv <- newTyVar "a"
    (s1, t1) <- ti env e1
    (s2, t2) <- ti (apply s1 env) e2
    s3 <- mgu (apply s2 t1) (TFun t2 tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
    `catchError`
    \e -> throwError $ e ++ "\n检查以下表达式时发生错误：\n  " ++ show exp
ti env (ELet x e1 e2) = do
    (s1, t1) <- ti env e1
    let TypeEnv env' = remove env x
        t' = generalize (apply s1 env) t1
        env'' = TypeEnv (Map.insert x t' env')
    (s2, t2) <- ti (apply s1 env'') e2
    return (s2 `composeSubst` s1, t2)

typeInference :: Map.Map String Scheme -> Exp -> TI Type
typeInference env e = do
    (s, t) <- ti (TypeEnv env) e
    return (apply s t)
