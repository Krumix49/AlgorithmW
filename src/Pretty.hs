-- 定义漂亮输出相关的函数
-- 导入需要的模块
module Pretty where
import qualified Text.PrettyPrint as PP
import Syntax

-- 类型输出
instance Show Type where
    showsPrec _ x = shows (prType x)

prType :: Type -> PP.Doc
prType (TVar n)     = PP.text n
prType TInt         = PP.text "Int"
prType TBool        = PP.text "Bool"
prType (TFun t s)   = prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType :: Type -> PP.Doc
prParenType t = case t of 
                TFun _ _ -> PP.parens (prType t)
                _        -> prType t

-- 表达式输出
instance Show Exp where
    showsPrec _ x = shows (prExp x)

prExp :: Exp -> PP.Doc
prExp (EVar name)       = PP.text name
prExp (ELit lit)        = prLit lit
prExp (ELet x b body)   = PP.text "let" PP.<+>
                        PP.text x PP.<+> PP.text "=" PP.<+>
                        prExp b PP.<+> PP.text "in" PP.$$
                        PP.nest 2 (prExp body)
prExp (EApp e1 e2)      = prExp e1 PP.<+> prParenExp e2
prExp (EAbs n e)        = PP.char '\\' PP.<> PP.text n PP.<+>
                        PP.text "->" PP.<+>
                        prExp e

prParenExp :: Exp -> PP.Doc
prParenExp t = case t of
               ELet _ _ _ -> PP.parens (prExp t)
               EApp _ _    -> PP.parens (prExp t)
               EAbs _ _    -> PP.parens (prExp t)
               _           -> prExp t

-- 字面量输出
instance Show Lit where
    showsPrec _ x = shows (prLit x)

prLit :: Lit -> PP.Doc
prLit (LInt i)  = PP.integer i
prLit (LBool b) = if b then PP.text "True" else PP.text "False"

-- 模式输出
instance Show Scheme where
    showsPrec _ x = shows (prScheme x)
prScheme :: Scheme -> PP.Doc
prScheme (Scheme vars t) = PP.text "All" PP.<+>
                         PP.hcat (PP.punctuate PP.comma (map PP.text vars))
                         PP.<> PP.text "." PP.<+> prType t