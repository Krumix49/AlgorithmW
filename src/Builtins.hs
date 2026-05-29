module Builtins where

import qualified Data.Map as Map

import Syntax

builtinEnv :: Map.Map String Scheme
builtinEnv = Map.fromList
    [ ("+", intBin TInt)
    , ("-", intBin TInt)
    , ("*", intBin TInt)
    , ("/", intBin TInt)
    , ("negate", Scheme [] (TFun TInt TInt))
    , ("<", intBin TBool)
    , ("<=", intBin TBool)
    , (">", intBin TBool)
    , (">=", intBin TBool)
    , ("==", sameTypeBool)
    , ("!=", sameTypeBool)
    , ("~=", sameTypeBool)
    , ("&&", boolBin)
    , ("||", boolBin)
    , ("not", Scheme [] (TFun TBool TBool))
    , ("if", Scheme ["a"] (TFun TBool (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))))
    ]

intBin :: Type -> Scheme
intBin result = Scheme [] (TFun TInt (TFun TInt result))

boolBin :: Scheme
boolBin = Scheme [] (TFun TBool (TFun TBool TBool))

sameTypeBool :: Scheme
sameTypeBool = Scheme ["a"] (TFun (TVar "a") (TFun (TVar "a") TBool))
