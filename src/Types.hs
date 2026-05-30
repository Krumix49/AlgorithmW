{-# LANGUAGE InstanceSigs #-}
module Types
  ( Type(..)
  , Scheme(..)
  , Subst
  , TypeEnv(..)
  , TypeError(..)
  , Types(..)
  , nullSubst
  , composeSubst
  , emptyEnv
  , extend
  , remove
  , lookupEnv
  , generalize
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Type  -- 类型
  = TVar String
  | TInt
  | TBool
  | TFun Type Type
  deriving (Eq, Ord)

data Scheme = Scheme [String] Type  -- 类型方案=带forall的多态类型，[string]是被forall量化的类型变量，便于后续实例化
  deriving (Eq, Ord)

type Subst = Map.Map String Type  -- 类型替换

newtype TypeEnv = TypeEnv (Map.Map String Scheme)  -- 类型环境=类型变量到类型方案的映射
  deriving (Eq, Show)

data TypeError  -- 类型错误
  = UnboundVariable String  -- 未绑定变量
  | TypesDoNotUnify Type Type  -- 变量不一致（比如then和else）
  | InfiniteType String Type  -- 无限类型
  | ParseFailure String  -- 源码的语法不合法
  deriving (Eq)

class Types a where  -- 不只是应用于Type
  ftv :: a -> Set.Set String  -- 找到自由类型变量
  apply :: Subst -> a -> a  -- 应用类型替换

instance Types Type where
  ftv :: Type -> Set.Set String
  ftv (TVar name) = Set.singleton name
  ftv TInt = Set.empty
  ftv TBool = Set.empty
  ftv (TFun left right) = ftv left `Set.union` ftv right

  apply :: Subst -> Type -> Type
  apply subst t@(TVar name) =  -- 在映射表里查询类型
    case Map.lookup name subst of
      Nothing -> t
      Just replacement -> replacement
  apply subst (TFun left right) = TFun (apply subst left) (apply subst right)
  apply _ TInt = TInt
  apply _ TBool = TBool

instance Types Scheme where
  ftv :: Scheme -> Set.Set String
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars  -- 自由变量要减去被forall绑定的
  apply :: Subst -> Scheme -> Scheme
  apply subst (Scheme vars t) =
    Scheme vars (apply substWithoutBoundVars t)  -- 先删除被绑定的变量再替换类型方案中的变量
    where
      substWithoutBoundVars = foldr Map.delete subst vars

instance Types a => Types [a] where  -- 扩展到[Type][Scheme]
  ftv :: Types a => [a] -> Set.Set String  -- 每个元素找完取并集
  ftv xs = foldr (Set.union . ftv) Set.empty xs
  apply :: Types a => Subst -> [a] -> [a]
  apply subst = map (apply subst)

instance Types TypeEnv where
  ftv :: TypeEnv -> Set.Set String
  ftv (TypeEnv env) = ftv (Map.elems env)  -- 对所有Scheme做ftv
  apply :: Subst -> TypeEnv -> TypeEnv
  apply subst (TypeEnv env) = TypeEnv (Map.map (apply subst) env)

nullSubst :: Subst  -- 空替换
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst  -- 复合替换
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

emptyEnv :: TypeEnv  -- 空类型环境
emptyEnv = TypeEnv Map.empty

extend :: TypeEnv -> String -> Scheme -> TypeEnv  -- 扩展类型环境
extend (TypeEnv env) name scheme = TypeEnv (Map.insert name scheme env)

remove :: TypeEnv -> String -> TypeEnv  -- 用于作用域解析时删除已绑定的变量
remove (TypeEnv env) name = TypeEnv (Map.delete name env)

lookupEnv :: TypeEnv -> String -> Maybe Scheme  -- key -> Scheme
lookupEnv (TypeEnv env) name = Map.lookup name env

generalize :: TypeEnv -> Type -> Scheme  -- 类型方案泛化
generalize env t = Scheme vars t
  where
    vars = Set.toList (ftv t `Set.difference` ftv env)  -- 泛化的类型变量不能来自外部环境

instance Show Type where
  show :: Type -> String
  show = showType 0

instance Show Scheme where
  show :: Scheme -> String
  show (Scheme [] t) = show t
  show (Scheme vars t) = "forall " ++ unwords vars ++ ". " ++ show t

instance Show TypeError where
  show :: TypeError -> String
  show (UnboundVariable name) =
    "unbound variable: " ++ name
  show (TypesDoNotUnify t1 t2) =
    "types do not unify: " ++ show t1 ++ " vs. " ++ show t2
  show (InfiniteType name t) =
    "occurs check failed: cannot construct infinite type " ++ name ++ " ~ " ++ show t
  show (ParseFailure msg) =
    "parse error: " ++ msg

showType :: Int -> Type -> String
showType _ (TVar name) = name
showType _ TInt = "Int"
showType _ TBool = "Bool"
showType p (TFun left right) =
  parensIf (p > 0) (showType 1 left ++ " -> " ++ showType 0 right)  -- 函数右结合，右边一般不用括号

parensIf :: Bool -> String -> String
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s
