-- 提供Monad栈的相关操作
module MonadStack where
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Syntax
import Types

data TIEnv = TIEnv {}
data TIState = TIState {tiSupply :: Int}
type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = do 
    (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    return (res, st)
    where initTIEnv = TIEnv
          initTIState = TIState {tiSupply = 0}

newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s {tiSupply = tiSupply s + 1}
    return (TVar (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do 
    nvars <- mapM (const (newTyVar "a")) vars
    let s = Map.fromList (zip vars nvars)
    return $ apply s t
    