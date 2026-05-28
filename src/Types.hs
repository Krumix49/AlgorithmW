-- 类型系统相关的类型定义
module Types where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Syntax
class Types a where
    ftv :: a -> Set.Set String
    apply :: Subst -> a -> a

instance Types Type where
    ftv (TVar n)            = Set.singleton n
    ftv TInt                = Set.empty
    ftv TBool               = Set.empty
    ftv (TFun t1 t2)        = ftv t1 `Set.union` ftv t2
    apply s (TVar n)        = case Map.lookup n s of
                            Nothing -> TVar n
                            Just t  -> t
    apply s (TFun t1 t2)    = TFun (apply s t1) (apply s t2)
    apply s t               = t

instance Types Scheme where
    ftv (Scheme vars t)     = ftv t `Set.difference` Set.fromList vars
    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv l = foldr Set.union Set.empty (map ftv l)

type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1