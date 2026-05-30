# Algorithm W / Pseudo-W

这是一个用 Haskell 实现的 Algorithm W 类型推断器。项目目标不是实现完整 Haskell 或完整 MATLAB，而是提供一个小而清楚的 Pseudo-W 语言：用户可以输入 lambda/let 表达式，或编写 MATLAB-like 的伪代码函数文件，程序会解析它、解释自己“看懂了什么”，并检查类型是否合规。

## 能做什么

当前支持两种输入方式：

- 单个表达式：适合在 REPL 中快速测试类型推断。
- `.pseudo` 文件：适合分析一组 MATLAB-like pseudo-functions。

表达式层面支持：

- 变量：`x`
- 整数和布尔字面量：`1`, `True`, `False`
- lambda：`\x -> x`
- 多参数 lambda：`\x y -> x + y`
- 函数应用：`f x` 或 pseudo 风格 `f(x)`
- `let` 局部定义：`let id = \x -> x in id 3`
- 条件表达式：`if x > 0 then x else 0`
- 运算符：`+`, `-`, `*`, `/`, `<`, `<=`, `>`, `>=`, `==`, `!=`, `~=`, `&&`, `||`, `not`, `~`

伪代码文件支持：

- `function y = f(x)` / `end`
- 单行函数：`function identity(x) = x`
- 多语句局部赋值
- `if / elseif / else / end`
- `switch / case / otherwise / end`
- `for / end`
- `while / end`
- 按文件顺序引用前面已经通过类型检查的函数

## 运行方式

启动 REPL：

```powershell
cabal run algorithm-w
```

REPL 默认展示详细解析过程。输入：

```text
1 + 2
```

会看到程序按自然语言解释：

```text
这是一个二元运算 `+`：分别检查左右两边。
左边读取字面量 `1`。
右边读取字面量 `2`。
```

退出 REPL：

```text
:quit
```

直接分析一个表达式：

```powershell
cabal run algorithm-w -- expr "let id = \x -> x in id 3"
```

对命令行表达式开启详细解析：

```powershell
cabal run algorithm-w -- --verbose "if 1 < 2 then 10 else 20"
```

分析标准表达式文件：

```powershell
cabal run algorithm-w -- --file examples/id.aw
```

分析 pseudo-function 文件：

```powershell
cabal run algorithm-w -- --file examples/main-branch.pseudo
```

## Pseudo 文件示例

简单函数：

```matlab
function y = addOne(x)
  y = x + 1
end
```

多语句局部赋值：

```matlab
function z = affine(x)
  shifted = x + 1
  scaled = shifted * 2
  z = scaled + 3
end
```

这会被理解成类似：

```haskell
let shifted = x + 1 in
let scaled = shifted * 2 in
scaled + 3
```

多分支条件：

```matlab
function y = clampSign(x)
  if x < 0
    y = -1
  elseif x == 0
    y = 0
  else
    y = 1
  end
end
```

`switch / case / otherwise`：

```matlab
function y = codeScore(code)
  switch code
    case 0
      base = 90
      y = base + 10
    case 1
      y = 80
    otherwise
      y = 0
  end
end
```

列表和循环：

```matlab
function total = sumList(xs)
  total = 0
  for x in xs
    total = total + x
  end
end
```

这里 `xs` 会被推断成 `[Int]`，因此整个函数类型是：

```text
[Int] -> Int
```

`while` 循环：

```matlab
function y = countDown(x)
  y = x
  while y > 0
    y = y - 1
  end
end
```

## 类型检查规则

程序会自动推断函数类型。例如：

```matlab
function y = choose(flag, a, b)
  if flag
    y = a
  else
    y = b
  end
end
```

会被推断为：

```text
Bool -> a -> a -> a
```

意思是：第一个参数必须是 `Bool`，后两个参数可以是任意同一类型，结果也是这个类型。

如果两个分支返回不同类型，程序会拒绝：

```matlab
function bad(x)
  if x then 1 else false
end
```

错误类似：

```text
类型无法统一：Int 与 Bool 不一致
```

## 行号错误定位

解析 `.pseudo` 文件时，程序会保留原始行号。语法错误会提示具体行，例如：

```text
第 12 行：函数参数列表缺少右括号 ')'
```

类型错误会标明出错函数从哪一行开始，例如：

```text
[TYPE ERROR] bad（从第 64 行开始）
```

目前类型错误定位到函数级别；语法错误通常可以定位到具体语句行。之后可以继续扩展到表达式内部列号。

## 详细解析输出

`--verbose` 和 REPL 默认详细模式会展示：

1. 原始输入
2. 词法单元
3. 程序如何用自然语言理解表达式
4. 规范化表达式
5. 类型推断结果

这个输出是面向用户的，不是完整 Algorithm W 教程。它的目标是让用户看懂：程序到底把输入解释成了什么。

## 当前限制

当前版本仍然是一个小型类型检查器，有意保留了一些边界：

- 不支持递归函数。
- `for` / `while` 只做类型检查，不执行循环体，也不计算运行结果。
- 支持基础列表，例如 `[1, 2, 3]`，但不支持数组下标、矩阵和字符串。
- 不支持 MATLAB 的完整语法。
- 多语句只支持 `name = expression`、`if`、`switch`、`for`、`while`。
- `switch` 必须有 `otherwise`。
- `case` 不支持 fall-through。

这些限制让核心类型推断保持清楚，也方便后续逐步扩展。

## 项目结构

```text
app/Main.hs      命令行、REPL、中文输出
src/Syntax.hs    表达式 AST 和二元运算
src/Parser.hs    手写 parser 和 pseudo 文件解析
src/Infer.hs     Algorithm W、合一和类型推断
src/Types.hs     类型、类型方案、类型环境和错误
examples/        示例输入
```

## 构建

```powershell
cabal build all
```

如果 Windows 上 `cabal run` 遇到临时文件锁，可以稍等后重试；通常是旧进程刚退出时可执行文件还没被系统释放。
