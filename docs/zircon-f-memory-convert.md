# RV32F 访存与整数转换

## 支持范围

- 浮点访存：`FLW`、`FSW`
- 浮点转整数：`FCVT.W.S`、`FCVT.WU.S`
- 整数转浮点：`FCVT.S.W`、`FCVT.S.WU`
- 位搬运：`FMV.X.W`、`FMV.W.X`

`FLW` 和 `FSW` 使用 5、6 号 LSU 槽；转换和位搬运可以使用任意 0～2 号 FPU 槽。除法、开平方和融合乘加不属于本次范围。

## 数据通路

Decoder 分别标记 `rs1`、`rs2`、`rs3` 和 `rd` 的 GPR/FPR 类型。`FSW` 因而可以同时读取整数基址和浮点存储数据。Frontend 为两个 LSU 槽各增加一个 FPR 读口，Backend 将两个 LSU 的 FPR 写回接入浮点寄存器堆：

- `FLW`：`GPR rs1` → 地址计算 → LSU → `FPR rd`
- `FSW`：`GPR rs1` → 地址计算；`FPR rs2` → LSU 写数据

寄存器类型作为六位寄存器编号的最高位参与 Hazard 和 Forward 比较，因此 `FLW → FPU`、`FPU → FSW` 和 `FLW → FSW` 的相关不会与同编号的 GPR 混淆。

转换模块位于 FPU 内部，结果经过与其他简单 FPU 操作相同的两个段间寄存器，与 EX3 指令包对齐。四条 FCVT 指令支持 RNE、RTZ、RDN、RUP 和 RMM，生成 `{NV, DZ, OF, UF, NX}` 中适用的 NV/NX 标志。当前尚无 FCSR/FRM，动态舍入编码仍使用复位默认 RNE。

## 验证

`ZirconFMemConvertTest` 覆盖：

- `riscv-tests` 的 `rv32uf/fcvt` 和 `rv32uf/fcvt_w` 用例；
- 2000 组 Berkeley SoftFloat 确定性向量，覆盖四条 FCVT 和五种舍入模式；
- NaN、无穷、符号/无符号饱和、非法与不精确标志；
- FMV 对全部 32 位载荷的无损搬运；
- 两条 LSU 的 FPR 读写、混合寄存器译码、stall 和 WB 前递；
- 0 号 FPU 槽中的两个转换方向。
- 生成后 CPU Verilog 上的 `FLW → FADD → FSW → FCVT → FMV → FSW` 整机指令链。

运行命令：

```text
sbt -batch 'testOnly ZirconFMemConvertTest'
make test-fmem-verilator
```
