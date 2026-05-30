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

    EList items ->
      inferList env items

    EBlock stmts outputName -> do
      (s1, env') <- inferStmts env stmts
      case lookupEnv (apply s1 env') outputName of
        Nothing -> throwInfer (UnboundVariable outputName)
        Just scheme -> do
          t <- instantiate scheme
          pure (s1, t)

inferLit :: Lit -> Type
inferLit (LInt _) = TInt
inferLit (LBool _) = TBool

inferList :: TypeEnv -> [Exp] -> Infer (Subst, Type)
inferList _ [] = do
  tv <- fresh
  pure (nullSubst, TList tv)
inferList env (item:items) = do
  (s1, itemType) <- infer env item
  inferListRest (apply s1 env) s1 itemType items

inferListRest :: TypeEnv -> Subst -> Type -> [Exp] -> Infer (Subst, Type)
inferListRest _ subst itemType [] =
  pure (subst, TList (apply subst itemType))
inferListRest env subst itemType (item:items) = do
  (sItem, nextType) <- infer env item
  sSame <- mgu (apply sItem itemType) nextType
  let subst' = sSame `composeSubst` sItem `composeSubst` subst
      env' = apply subst' env
      itemType' = apply subst' itemType
  inferListRest env' subst' itemType' items

inferStmts :: TypeEnv -> [Stmt] -> Infer (Subst, TypeEnv)
inferStmts env [] = pure (nullSubst, env)
inferStmts env (stmt:stmts) = do
  (s1, env1) <- inferStmt env stmt
  (s2, env2) <- inferStmts (apply s1 env1) stmts
  pure (s2 `composeSubst` s1, env2)

inferStmt :: TypeEnv -> Stmt -> Infer (Subst, TypeEnv)
inferStmt env stmt =
  case stmt of
    SAssign name expr -> inferAssign env name expr

    SIfStmt cond yes no -> do
      (s1, tCond) <- infer env cond
      sBool <- mgu tCond TBool
      let sCond = sBool `composeSubst` s1
          envCond = apply sCond env
      (sYes, envYes) <- inferStmts envCond yes
      (sNo, envNo) <- inferStmts envCond no
      sMerge <- mergeEnvs (apply sNo envYes) (apply sYes envNo)
      pure (sMerge `composeSubst` sNo `composeSubst` sYes `composeSubst` sCond, apply sMerge (apply sNo envYes))

    SSwitchStmt subject cases otherwiseBranch -> do
      (sSubject, tSubject) <- infer env subject
      (sCases, envCases) <- inferCases (apply sSubject env) (apply sSubject tSubject) cases
      (sOtherwise, envOtherwise) <- inferStmts (apply sCases (apply sSubject env)) otherwiseBranch
      sMerge <- mergeEnvs (apply sOtherwise envCases) envOtherwise
      pure (sMerge `composeSubst` sOtherwise `composeSubst` sCases `composeSubst` sSubject, apply sMerge (apply sOtherwise envCases))

    SFor itemName items body -> do
      tv <- fresh
      (sItems, tItems) <- infer env items
      sList <- mgu tItems (TList tv)
      let sLoop = sList `composeSubst` sItems
          itemType = apply sLoop tv
          envLoop = extend (remove (apply sLoop env) itemName) itemName (Scheme [] itemType)
      (sBody, envBody) <- inferStmts envLoop body
      pure (sBody `composeSubst` sLoop, remove envBody itemName)

    SWhile cond body -> do
      (sCond, tCond) <- infer env cond
      sBool <- mgu tCond TBool
      let sLoop = sBool `composeSubst` sCond
      (sBody, envBody) <- inferStmts (apply sLoop env) body
      pure (sBody `composeSubst` sLoop, envBody)

inferAssign :: TypeEnv -> String -> Exp -> Infer (Subst, TypeEnv)
inferAssign env name expr = do
  (s1, tExpr) <- infer env expr
  let envAfterExpr = apply s1 env
  case lookupEnv envAfterExpr name of
    Nothing ->
      pure (s1, extend envAfterExpr name (Scheme [] tExpr))
    Just scheme -> do
      tOld <- instantiate scheme
      sSame <- mgu tOld tExpr
      let subst = sSame `composeSubst` s1
      pure (subst, extend (apply subst env) name (Scheme [] (apply subst tExpr)))

inferCases :: TypeEnv -> Type -> [(Exp, [Stmt])] -> Infer (Subst, TypeEnv)
inferCases env _ [] = pure (nullSubst, env)
inferCases env subjectType ((caseExpr, body):cases) = do
  (sCase, tCase) <- infer env caseExpr
  sSame <- mgu (apply sCase subjectType) tCase
  let sHead = sSame `composeSubst` sCase
      envHead = apply sHead env
  (sBody, envBody) <- inferStmts envHead body
  (sRest, envRest) <- inferCases (apply sBody envHead) (apply sBody (apply sHead subjectType)) cases
  sMerge <- mergeEnvs (apply sRest envBody) envRest
  pure (sMerge `composeSubst` sRest `composeSubst` sBody `composeSubst` sHead, apply sMerge (apply sRest envBody))

mergeEnvs :: TypeEnv -> TypeEnv -> Infer Subst
mergeEnvs (TypeEnv left) (TypeEnv right) =
  mergeNames nullSubst (Map.keysSet left `Set.union` Map.keysSet right)
  where
    mergeNames subst names =
      case Set.minView names of
        Nothing -> pure subst
        Just (name, rest) ->
          case (Map.lookup name left, Map.lookup name right) of
            (Just sLeft, Just sRight) -> do
              tLeft <- instantiate sLeft
              tRight <- instantiate sRight
              sSame <- mgu (apply subst tLeft) (apply subst tRight)
              mergeNames (sSame `composeSubst` subst) rest
            _ -> mergeNames subst rest

inferBin :: TypeEnv -> BinOp -> Exp -> Exp -> Infer (Subst, Type)
inferBin env op left right = do
  (s1, tLeft) <- infer env left
  (s2, tRight) <- infer (apply s1 env) right
  case op of
    Add -> inferIntBin s1 s2 tLeft tRight
    Sub -> inferIntBin s1 s2 tLeft tRight
    Mul -> inferIntBin s1 s2 tLeft tRight
    Div -> inferIntBin s1 s2 tLeft tRight
    And -> inferBoolBin s1 s2 tLeft tRight
    Or -> inferBoolBin s1 s2 tLeft tRight
    Eq -> do
      s3 <- mgu (apply s2 tLeft) tRight
      pure (s3 `composeSubst` s2 `composeSubst` s1, TBool)
    Ne -> do
      s3 <- mgu (apply s2 tLeft) tRight
      pure (s3 `composeSubst` s2 `composeSubst` s1, TBool)
    Lt -> inferIntCompare s1 s2 tLeft tRight
    Le -> inferIntCompare s1 s2 tLeft tRight
    Gt -> inferIntCompare s1 s2 tLeft tRight
    Ge -> inferIntCompare s1 s2 tLeft tRight

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

inferIntCompare :: Subst -> Subst -> Type -> Type -> Infer (Subst, Type)
inferIntCompare s1 s2 tLeft tRight = do
  s3 <- mgu (apply s2 tLeft) TInt
  s4 <- mgu (apply s3 tRight) TInt
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
mgu (TList left) (TList right) = mgu left right
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
    , ("negate", Scheme [] (TFun TInt TInt))
    ]
  where
    add env (name, scheme) = extend env name scheme
