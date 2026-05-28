-- 最一般合一算法
module Unification where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except (throwError)
import Types
import Syntax
import MonadStack
import Pretty ()
mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r') = do 
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s2 `composeSubst` s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt TInt = return nullSubst
mgu TBool TBool = return nullSubst
mgu t1 t2 = throwError $
    "无法统一两个类型：\n"
    ++ "  左侧：  " ++ show t1 ++ "\n"
    ++ "  右侧：  " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t | t == TVar u = return nullSubst
            | u `Set.member` ftv t = throwError $
                "不允许出现递归类型（occurs check 失败）：\n"
                ++ "  类型变量：" ++ u ++ "\n"
                ++ "  类型：    " ++ show t
            | otherwise = return (Map.singleton u t)
