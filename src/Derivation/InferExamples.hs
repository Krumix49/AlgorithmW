module Derivation.InferExamples
  ( InferGroup(..)
  , InferUnit(..)
  , ExpectOutcome(..)
  , inferUnits
  , inferUnitsByGroup
  , groupLabel
  , runUnit
  , renderedTrace
  , outcomeTrace
  ) where

import Derivation.Render (renderTrace)
import Derivation.Trace (InferenceOutcome(..), InferenceResult(..), inferSource)
import Types (TypeError)

data InferGroup
  = GroupEVar
  | GroupInstantiate
  | GroupELit
  | GroupEAbs
  | GroupEApp
  | GroupELet
  | GroupEIf
  | GroupEBin
  | GroupMgu
  | GroupError
  deriving (Eq, Ord, Show)

data ExpectOutcome
  = ExpectScheme String
  | ExpectInferErr String
  deriving (Eq, Show)

data InferUnit = InferUnit
  { iuGroup :: InferGroup
  , iuLabel :: String
  , iuSource :: String
  , iuExpect :: ExpectOutcome
  , iuTrace :: [String]
  }
  deriving (Eq, Show)

groupLabel :: InferGroup -> String
groupLabel group =
  case group of
    GroupEVar -> "变量 EVar"
    GroupInstantiate -> "实例化 instantiate"
    GroupELit -> "字面量 ELit"
    GroupEAbs -> "Lambda EAbs"
    GroupEApp -> "函数应用 EApp"
    GroupELet -> "Let 绑定 ELet"
    GroupEIf -> "条件 EIf"
    GroupEBin -> "二元运算 EBin"
    GroupMgu -> "合一 mgu"
    GroupError -> "错误路径"

inferUnits :: [InferUnit]
inferUnits =
  [ InferUnit
      { iuGroup = GroupEVar
      , iuLabel = "not True"
      , iuSource = "not True"
      , iuExpect = ExpectScheme "Bool"
      , iuTrace =
          [ "EVar：变量 not"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupInstantiate
      , iuLabel = "id id"
      , iuSource = "let id = \\x -> x in id id"
      , iuExpect = ExpectScheme "->"
      , iuTrace =
          [ "instantiate："
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupELit
      , iuLabel = "42"
      , iuSource = "42"
      , iuExpect = ExpectScheme "Int"
      , iuTrace =
          [ "ELit：整数字面量"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupELit
      , iuLabel = "True"
      , iuSource = "True"
      , iuExpect = ExpectScheme "Bool"
      , iuTrace =
          [ "ELit：布尔字面量"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupEAbs
      , iuLabel = "\\x -> x"
      , iuSource = "\\x -> x"
      , iuExpect = ExpectScheme "->"
      , iuTrace =
          [ "EAbs：λ"
          , "fresh：生成新类型变量"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupEApp
      , iuLabel = "(\\x -> x) 1"
      , iuSource = "(\\x -> x) 1"
      , iuExpect = ExpectScheme "Int"
      , iuTrace =
          [ "EApp：函数应用"
          , "mgu：分解函数，输入、返回分别合一"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupELet
      , iuLabel = "let id … id 2"
      , iuSource = "let id = \\x -> x in id 2"
      , iuExpect = ExpectScheme "Int"
      , iuTrace =
          [ "ELet：let"
          , "generalize："
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupEIf
      , iuLabel = "if … then … else …"
      , iuSource = "if True then 1 else 2"
      , iuExpect = ExpectScheme "Int"
      , iuTrace =
          [ "EIf：条件表达式"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupEBin
      , iuLabel = "1 + 2"
      , iuSource = "1 + 2"
      , iuExpect = ExpectScheme "Int"
      , iuTrace =
          [ "EBin：二元运算 +"
          , "算术合一"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupEBin
      , iuLabel = "3 - 1"
      , iuSource = "3 - 1"
      , iuExpect = ExpectScheme "Int"
      , iuTrace =
          [ "EBin：二元运算 -"
          , "算术合一"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupEBin
      , iuLabel = "2 * 3"
      , iuSource = "2 * 3"
      , iuExpect = ExpectScheme "Int"
      , iuTrace =
          [ "EBin：二元运算 *"
          , "算术合一"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupEBin
      , iuLabel = "True && False"
      , iuSource = "True && False"
      , iuExpect = ExpectScheme "Bool"
      , iuTrace =
          [ "逻辑合一"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupEBin
      , iuLabel = "True || False"
      , iuSource = "True || False"
      , iuExpect = ExpectScheme "Bool"
      , iuTrace =
          [ "EBin：二元运算 ||"
          , "逻辑合一"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupEBin
      , iuLabel = "1 == 1"
      , iuSource = "1 == 1"
      , iuExpect = ExpectScheme "Bool"
      , iuTrace =
          [ "(==) 合一约束"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupMgu
      , iuLabel = "0 + 0"
      , iuSource = "0 + 0"
      , iuExpect = ExpectScheme "Int"
      , iuTrace =
          [ "mgu：合一 Int ~ Int"
          , "推断 "
          , "=> 替换:"
          ]
      }
  , InferUnit
      { iuGroup = GroupError
      , iuLabel = "occurs check"
      , iuSource = "let bad = \\x -> x x in bad"
      , iuExpect = ExpectInferErr "occurs check 失败"
      , iuTrace =
          [ "occurs check 失败"
          , "推断 "
          ]
      }
  , InferUnit
      { iuGroup = GroupError
      , iuLabel = "if 分支类型不一致"
      , iuSource = "if True then 1 else False"
      , iuExpect = ExpectInferErr "类型无法统一"
      , iuTrace =
          [ "mgu：合一"
          , "推断 "
          ]
      }
  , InferUnit
      { iuGroup = GroupError
      , iuLabel = "2 2"
      , iuSource = "2 2"
      , iuExpect = ExpectInferErr "类型无法统一"
      , iuTrace =
          [ "EApp：函数应用"
          , "推断 "
          ]
      }
  ]

inferUnitsByGroup :: [(InferGroup, [InferUnit])]
inferUnitsByGroup =
  map (\g -> (g, filter ((== g) . iuGroup) inferUnits)) groups
  where
    groups =
      [ GroupEVar
      , GroupInstantiate
      , GroupELit
      , GroupEAbs
      , GroupEApp
      , GroupELet
      , GroupEIf
      , GroupEBin
      , GroupMgu
      , GroupError
      ]

runUnit :: InferUnit -> Either TypeError InferenceOutcome
runUnit unit = inferSource (iuSource unit)

renderedTrace :: InferenceResult -> String
renderedTrace result = renderTrace (irEvents result)

outcomeTrace :: InferenceOutcome -> String
outcomeTrace outcome =
  case outcome of
    OutcomeOk result -> renderedTrace result
    OutcomeInferErr _ _ events -> renderTrace events
