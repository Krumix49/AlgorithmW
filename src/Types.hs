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

data Type
  = TVar String
  | TInt
  | TBool
  | TFun Type Type
  deriving (Eq, Ord)

data Scheme = Scheme [String] Type  -- 带forall的多态类型
  deriving (Eq, Ord)

type Subst = Map.Map String Type

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
  deriving (Eq, Show)

data TypeError
  = UnboundVariable String  -- 未绑定变量
  | TypesDoNotUnify Type Type  -- 变量不一致（比如then和else）
  | InfiniteType String Type  -- 无限类型
  | ParseFailure String  -- 源码的语法不合法
  deriving (Eq)

class Types a where
  ftv :: a -> Set.Set String
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TVar name) = Set.singleton name
  ftv TInt = Set.empty
  ftv TBool = Set.empty
  ftv (TFun left right) = ftv left `Set.union` ftv right

  apply subst t@(TVar name) =  -- 在映射表里查询类型
    case Map.lookup name subst of
      Nothing -> t
      Just replacement -> replacement
  apply subst (TFun left right) = TFun (apply subst left) (apply subst right)
  apply _ TInt = TInt
  apply _ TBool = TBool

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars  -- 自由变量要减去被forall绑定的
  apply subst (Scheme vars t) =
    Scheme vars (apply substWithoutBoundVars t)  -- 先删除被绑定的变量再替换
    where
      substWithoutBoundVars = foldr Map.delete subst vars

instance Types a => Types [a] where
  ftv xs = foldr (Set.union . ftv) Set.empty xs
  apply subst = map (apply subst)

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv (Map.elems env)
  apply subst (TypeEnv env) = TypeEnv (Map.map (apply subst) env)

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

extend :: TypeEnv -> String -> Scheme -> TypeEnv
extend (TypeEnv env) name scheme = TypeEnv (Map.insert name scheme env)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) name = TypeEnv (Map.delete name env)

lookupEnv :: TypeEnv -> String -> Maybe Scheme
lookupEnv (TypeEnv env) name = Map.lookup name env

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where
    vars = Set.toList (ftv t `Set.difference` ftv env)

instance Show Type where
  show = showType 0

instance Show Scheme where
  show (Scheme [] t) = show t
  show (Scheme vars t) = "forall " ++ unwords vars ++ ". " ++ show t

instance Show TypeError where
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
  parensIf (p > 0) (showType 1 left ++ " -> " ++ showType 0 right)

parensIf :: Bool -> String -> String
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s
