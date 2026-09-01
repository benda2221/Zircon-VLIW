# Zircon 浮点加法与乘法流水线

## 范围

本次融合以 [Zircon-FloatPoint](https://github.com/MAdrid1011/Zircon-FloatPoint) 的 `bc66c30` 版本为输入设计基线，只替换单精度 `FADD.S`、`FSUB.S` 和 `FMUL.S` 的算术数据通路。原实现只覆盖组合路径和主要 RNE 正常数场景，因此融合时补齐了流水控制、五种舍入模式、特殊值、非规格数和异常标志。浮点除法、开平方和融合乘加不属于该单元，也不在本次回归范围内。比较、分类继续复用原有模块；访存、搬运和格式转换的完整通路见 [RV32F 访存与整数转换](zircon-f-memory-convert.md)。

实现位于 `src/main/scala/Backend/FunctionUnit/ZirconFPAddMul.scala`，接口通过 `FPU.scala` 接入 0～2 号浮点流水线。

## 三级切分

| 流水级 | 浮点加/减 | 浮点乘 |
| --- | --- | --- |
| EX1 | 分类、比较幅值、对阶并生成 sticky 位 | 分类、指数预计算、24×24 位有效数乘积 |
| EX2 | 有效数加减、前导零规格化 | 乘积规格化、非规格数移位并生成 sticky 位 |
| EX3 | 按舍入模式舍入、打包、生成 `fflags` | 按舍入模式舍入、打包、生成 `fflags` |

两个段间寄存器分别受处理器的 EX2 和 EX3 `stall`/`flush` 控制。结果在 EX3 保持与 `InstructionPackage` 对齐，并在 EX3→WB 边界写入 `fpuResult` 和 `fflags`。流水线可以每周期接收一条新的加法、减法或乘法指令。

## IEEE 754 与 RISC-V 行为

- 支持 RNE、RTZ、RDN、RUP 和 RMM 五种静态舍入模式。
- 生成 `{NV, DZ, OF, UF, NX}` 异常标志；加法和乘法不会产生 DZ。
- 支持零、非规格数、无穷、quiet NaN 和 signaling NaN，并生成 RISC-V canonical NaN。
- underflow 使用舍入后 tininess 判定。
- 当前处理器尚未实现 FCSR/FRM。指令编码 `rm=111` 时使用架构复位舍入模式 RNE；静态编码 `rm=000`～`100` 均由数据通路直接支持。

## 验证

`ZirconFPAddMulTest` 覆盖以下内容：

- `riscv-tests` 的 `rv32uf/fadd` 中 FADD、FSUB 和 FMUL 指令向量；
- 1500 组由 Berkeley SoftFloat 生成的确定性随机向量，平均覆盖三种运算和五种舍入模式；
- NaN、无穷、溢出、下溢、非规格数、精确抵消和异常标志边界；
- 10000 组随机 RNE 位级比较；
- 连续每周期发射、stall 保持、flush 清除和 VLIW 流水线写回对齐。

运行命令：

```text
sbt -batch 'testOnly ZirconFPAddMulTest'
```
