# Algorithm W 类型推断项目报告

## 1. Introduction

本项目实现了一个简化版的 Hindley-Milner 类型推断系统，核心算法是经典的 Algorithm W。该算法常用于解释 ML、OCaml、Haskell 等语言中“无需显式写出所有类型标注，编译器仍能推断表达式类型”的基本原理。

项目的输入不是完整 Haskell 程序，而是一组自定义的 Lambda Calculus 风格表达式。表达式支持变量、字面量、函数抽象、函数应用和 `let` 绑定，定义见 `src/Syntax.hs`：

```haskell
data Exp = EVar String
         | ELit Lit
         | EApp Exp Exp
         | EAbs String Exp
         | ELet String Exp Exp
```

类型系统也保持在教学项目所需的最小范围内，包括类型变量、整数、布尔值和函数类型：

```haskell
data Type = TVar String
          | TInt
          | TBool
          | TFun Type Type
```

项目的目标是：给定一个表达式，自动推断它的类型；如果表达式在类型上不合法，则给出错误信息。例如，恒等函数 `\x -> x` 会被推断为 `a -> a`，而 `2 2` 会因为试图把整数当作函数使用而产生类型错误。

## 2. Main Principles

### 2.1 Hindley-Milner 类型系统

Hindley-Milner 类型系统的一个重要特点是“参数多态”。例如恒等函数：

```haskell
\x -> x
```

它不关心 `x` 是整数、布尔值还是函数，因此其类型可以写作：

```text
a -> a
```

这里的 `a` 是类型变量，表示任意类型。项目中使用 `Scheme` 表示多态类型方案：

```haskell
data Scheme = Scheme [String] Type
```

它可以表示类似 `forall a. a -> a` 的类型。虽然程序输出中没有直接打印 `forall`，但内部会通过 `Scheme` 记录哪些类型变量是可泛化的。

### 2.2 类型变量、替换和自由类型变量

类型推断过程中会不断产生未知类型，例如 `a0`、`a1`。这些未知类型在后续推断中会逐渐被约束。项目用 `Subst` 表示类型替换：

```haskell
type Subst = Map.Map String Type
```

例如，如果推断过程中发现 `a0` 必须是 `Int`，就可以形成替换：

```text
a0 := Int
```

`src/Types.hs` 中的 `Types` 类型类定义了两个核心操作：

```haskell
class Types a where
    ftv :: a -> Set.Set String
    apply :: Subst -> a -> a
```

其中 `ftv` 用于计算自由类型变量，`apply` 用于把替换应用到类型、类型方案或类型环境上。

### 2.3 类型环境、泛化和实例化

类型环境记录变量名到类型方案的映射。例如，在表达式：

```haskell
let id = \x -> x in id id
```

推断 `id id` 时，环境中需要知道 `id` 的类型。项目中使用 `TypeEnv` 表示类型环境：

```haskell
newtype TypeEnv = TypeEnv (Map.Map String Scheme)
```

`let` 绑定是 Hindley-Milner 类型系统中多态能力的关键。当推断出 `id` 的类型为 `a -> a` 后，系统会调用 `generalize` 把它泛化为一个多态类型方案。这样，`id` 在不同位置使用时可以被实例化为不同的类型。

实例化由 `src/MonadStack.hs` 中的 `instantiate` 完成。它会把类型方案中的量化变量替换成新的 fresh type variable，避免不同使用位置互相干扰。

### 2.4 统一算法

统一算法用于解决“两个类型是否可以变成同一个类型”的问题。核心函数位于 `src/Unification.hs`：

```haskell
mgu :: Type -> Type -> TI Subst
```

例如：

```text
a0  与  Int
```

可以统一，结果是：

```text
a0 := Int
```

但：

```text
Int  与  Int -> a0
```

不能统一，因为整数类型不能同时是函数类型。

项目还实现了 occurs check，用于拒绝递归类型。例如表达式：

```haskell
\x -> x x
```

会要求 `x` 同时是某个函数，又是这个函数的参数，最终导致类似：

```text
a0 = a0 -> a1
```

这样的无限类型。`varBind` 中通过检查类型变量是否出现在目标类型的自由变量集合中，防止这种非法递归类型出现。

### 2.5 Algorithm W 的递归推断结构

项目的主推断函数是 `src/Inference.hs` 中的：

```haskell
ti :: TypeEnv -> Exp -> TI (Subst, Type)
```

它根据表达式结构递归推断类型：

- 变量 `EVar`：从类型环境中查找并实例化类型方案。
- 字面量 `ELit`：整数对应 `Int`，布尔值对应 `Bool`。
- 函数抽象 `EAbs`：为参数生成新类型变量，并推断函数体类型。
- 函数应用 `EApp`：推断函数和参数类型，并统一函数类型与参数类型。
- `let` 绑定 `ELet`：先推断绑定表达式，再泛化并加入环境，最后推断主体表达式。

最终入口函数是：

```haskell
typeInference :: Map.Map String Scheme -> Exp -> TI Type
```

它会调用 `ti`，并把最终替换应用到推断出的类型上。

## 3. Implementation and Test

### 3.1 模块结构

项目经过模块化整理后，主要模块如下：

```text
app/Main.hs          测试入口与输出格式
src/Syntax.hs        表达式、字面量、类型、类型方案定义
src/Types.hs         类型变量、替换、替换组合
src/TypeEnv.hs       类型环境、泛化
src/MonadStack.hs    类型推断所需的 monad 栈和 fresh type variable
src/Unification.hs   最一般统一算法
src/Inference.hs     Algorithm W 主体
src/Pretty.hs        表达式和类型的 pretty print
```

构建配置由 `AlgorithmW.cabal` 和 `cabal.project` 提供。项目依赖包括：

```text
base
containers
mtl
pretty
transformers
```

其中 `containers` 提供 `Map` 和 `Set`，`pretty` 用于格式化输出，`mtl` 和 `transformers` 用于异常、状态和环境传递。

### 3.2 测试表达式

`app/Main.hs` 中定义了七个测试表达式。它们覆盖了正常推断和错误推断两类情况。

第一个测试是普通恒等函数：

```haskell
e0 = ELet "id" (EAbs "x" (EVar "x")) (EVar "id")
```

输出类型为：

```text
a1 -> a1
```

说明 `id` 可以接收任意类型，并返回同一类型。

第二个测试是把多态恒等函数应用到自身：

```haskell
e1 = ELet "id" (EAbs "x" (EVar "x"))
     (EApp (EVar "id") (EVar "id"))
```

该表达式仍然可以成功推断，因为 `let` 绑定使 `id` 获得多态类型。

第四个测试包含自应用：

```haskell
e4 = ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x"))) (EVar "id")
```

它会触发 occurs check 错误，因为 `x x` 要求 `x` 的类型递归地包含自身。

最后一个测试：

```haskell
e6 = EApp (ELit (LInt 2)) (ELit (LInt 2))
```

它表示 `2 2`，即把整数 `2` 当成函数调用，因此会产生类型错误。

### 3.3 输出改进

最初程序输出直接把表达式、类型和错误信息拼接在一起，例如：

```text
let id = \x -> x in
  id :: a1 -> a1
```

对于已经理解类型推断的人来说可以阅读，但对学习者不够友好。因此当前版本在 `app/Main.hs` 中把输出改成了分区形式：

```text
示例 0：恒等函数
表达式：
  let id = \x -> x in
    id
结果：推断出的类型
  a1 -> a1
```

类型错误也会明确区分“结果”和“原因”：

```text
结果：类型错误
原因：
  无法统一两个类型：
    左侧：  Int
    右侧：  Int -> a0
```

这样可以帮助读者理解：错误不是程序崩溃，而是类型系统正确拒绝了不合法表达式。

### 3.4 运行方式

项目可以通过 Cabal 构建和运行：

```powershell
cabal run exe:AlgorithmW
```

如果只需要检查是否能编译，可以运行：

```powershell
cabal build exe:AlgorithmW
```

测试结果显示，合法表达式可以推断出类型，非法表达式会输出中文错误信息。因此项目已经实现了一个可运行、可观察、适合教学展示的 Algorithm W 原型。

## 4. Summary

本项目从一个简化表达式语言出发，实现了 Hindley-Milner 类型推断中的核心机制：类型变量、替换、自由类型变量、类型环境、泛化、实例化和统一算法。通过这些组件的组合，程序能够自动推断 Lambda 表达式和 `let` 表达式的类型。

在实现过程中，项目也展示了类型推断中的两个关键错误场景：类型无法统一和 occurs check 失败。前者说明某些表达式在类型结构上不匹配，例如把整数当函数使用；后者说明某些表达式会产生无限递归类型，例如自应用 `x x`。

最终版本不仅能够运行，还对输出进行了中文化和结构化改进，使测试结果更接近课程展示或实验报告中的说明形式。整体而言，该项目适合作为理解 Algorithm W 的实践样例：它规模较小，但包含了 Hindley-Milner 类型推断中最重要的算法步骤和错误处理逻辑。
