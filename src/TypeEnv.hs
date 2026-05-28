-- 类型环境
module TypeEnv where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Syntax
import Types

newtype TypeEnv = TypeEnv (Map.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = Set.toList (ftv t `Set.difference` ftv env)