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

newtype Infer a = Infer { runInfer :: Int -> Either TypeError (a, Int) }

instance Functor Infer where
  fmap f action = Infer $ \supply ->
    case runInfer action supply of
      Left err -> Left err
      Right (value, supply') -> Right (f value, supply')

instance Applicative Infer where
  pure value = Infer $ \supply -> Right (value, supply)
  sf <*> sx = Infer $ \supply ->
    case runInfer sf supply of
      Left err -> Left err
      Right (f, supply1) ->
        case runInfer sx supply1 of
          Left err -> Left err
          Right (x, supply2) -> Right (f x, supply2)

instance Monad Infer where
  action >>= next = Infer $ \supply ->
    case runInfer action supply of
      Left err -> Left err
      Right (value, supply') -> runInfer (next value) supply'

throwInfer :: TypeError -> Infer a
throwInfer err = Infer $ \_ -> Left err

fresh :: Infer Type
fresh = Infer $ \supply -> Right (TVar ("a" ++ show supply), supply + 1)

instantiate :: Scheme -> Infer Type
instantiate (Scheme vars t) = do
  freshVars <- mapM (const fresh) vars
  let subst = Map.fromList (zip vars freshVars)
  pure (apply subst t)

inferTop :: Exp -> Either TypeError Scheme
inferTop = inferTopWithEnv preludeEnv

inferTopWithEnv :: TypeEnv -> Exp -> Either TypeError Scheme
inferTopWithEnv env expr =
  case runInfer (infer env expr) 0 of
    Left err -> Left err
    Right ((subst, t), _) -> Right (generalize (apply subst env) (apply subst t))

infer :: TypeEnv -> Exp -> Infer (Subst, Type)
infer env expr =
  case expr of
    EVar name ->
      case lookupEnv env name of
        Nothing -> throwInfer (UnboundVariable name)
        Just scheme -> do
          t <- instantiate scheme
          pure (nullSubst, t)

    ELit lit ->
      pure (nullSubst, inferLit lit)

    EAbs name body -> do
      tv <- fresh
      let env' = extend (remove env name) name (Scheme [] tv)
      (s1, t1) <- infer env' body
      pure (s1, TFun (apply s1 tv) t1)

    EApp fun arg -> do
      tv <- fresh
      (s1, tFun) <- infer env fun
      (s2, tArg) <- infer (apply s1 env) arg
      s3 <- mgu (apply s2 tFun) (TFun tArg tv)
      pure (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)

    ELet name value body -> do
      (s1, t1) <- infer env value
      let envAfterValue = apply s1 env
          scheme = generalize envAfterValue t1
          envForBody = extend (remove envAfterValue name) name scheme
      (s2, t2) <- infer envForBody body
      pure (s2 `composeSubst` s1, t2)

    EIf cond yes no -> do
      (s1, tCond) <- infer env cond
      sBool <- mgu tCond TBool
      let sCond = sBool `composeSubst` s1
      (s2, tYes) <- infer (apply sCond env) yes
      (s3, tNo) <- infer (apply (s2 `composeSubst` sCond) env) no
      s4 <- mgu (apply s3 tYes) tNo
      pure (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` sCond, apply s4 tNo)

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

mgu :: Type -> Type -> Infer Subst
mgu (TFun left right) (TFun left' right') = do
  s1 <- mgu left left'
  s2 <- mgu (apply s1 right) (apply s1 right')
  pure (s2 `composeSubst` s1)
mgu (TVar name) t = bindVar name t
mgu t (TVar name) = bindVar name t
mgu TInt TInt = pure nullSubst
mgu TBool TBool = pure nullSubst
mgu t1 t2 = throwInfer (TypesDoNotUnify t1 t2)

bindVar :: String -> Type -> Infer Subst
bindVar name t
  | t == TVar name = pure nullSubst
  | name `Set.member` ftv t = throwInfer (InfiniteType name t)
  | otherwise = pure (Map.singleton name t)

preludeEnv :: TypeEnv
preludeEnv =
  foldl add emptyEnv
    [ ("not", Scheme [] (TFun TBool TBool))
    ]
  where
    add env (name, scheme) = extend env name scheme
