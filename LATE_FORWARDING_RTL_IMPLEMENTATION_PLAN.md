# 晚前递与嵌套浅依赖 RTL 最终实现方案

## 1. 最终架构

本方案扩展 Z-VLIW 现有同包浅依赖前递，使一个在 EX2 重算的 ALU 结果可以被
下一指令包在 EX2 使用，同时避免形成 `EX3 -> EX2 ALU -> EX2 ALU` 组合长路径。

最终模块分工如下：

```text
Forward：
    在EX1做依赖匹配和高lane优先选择
    输出每个源的selector与needsEX2Replay
    完成普通前递以及EX2阶段的数据选择

Pipeline：
    将Forward输出的selector写入InstructionPackage
    selector随EX1/EX2寄存器进入EX2
    解释ex2Stall/ex3Stall组合并在EX2入口插入气泡
    使用EX2 ALU重算

Hazard：
    读取Forward生成的selector
    检测嵌套依赖并产生stall组合
    保留needWB、分支和除法器控制
```

具体取舍：

1. lane 0 和 lane 1 增加 EX2 ALU，使 8 个 lane 的整数 ALU 都能在 EX2 重算；
2. EX1 选择信息由 `Forward` 产生，随流水进入 EX2；
3. EX3 到 EX2 的数据选择由 `Forward` 完成，EX2 不重新比较寄存器号；
4. 嵌套依赖在 EX1 停顿一拍；
5. 不新增 `ex2Bubble` 信号，复用 `ex2Stall`；
6. load、MUL/DIV、FPU/FDiv 等原本需要 WB 结果的 RAW 继续由 `needWB` 阻塞；
7. 不增加 EX3 ALU、WB 到 EX3 前递或新流水级。

## 2. 核心信号语义

### 2.1 `needsEX2Replay`

现有 `aluResultInvalid` 合并并重命名为：

```scala
val needsEX2Replay = Bool()
```

含义：

> 该指令的 EX1 ALU 结果不可用于普通 EX2 到 EX1 前递，必须在 EX2 使用修正后的
> 操作数重新执行；正确的 `aluResult` 从 EX3 开始可用。

设置条件：

```scala
needsEX2Replay :=
    shallowRs1Sel.orR ||
    shallowRs2Sel.orR ||
    lateRs1Sel.orR ||
    lateRs2Sel.orR
```

它是每个 lane 的指令属性，不是无条件停顿信号。

### 2.2 四个源选择信号

```scala
// 当前包低槽生产者；EX2从ex2Pkgs中选择。
val shallowRs1Sel = UInt(8.W)
val shallowRs2Sel = UInt(8.W)

// 前一包晚生产者；EX2从ex3Pkgs中选择。
val lateRs1Sel = UInt(8.W)
val lateRs2Sel = UInt(8.W)
```

每个源独立保存 one-hot，因为 `rs1`、`rs2` 可能来自不同生产者，也可能一个来自
当前包、另一个来自前一包。

## 3. 复用 `ex2Stall` 表示 EX2 气泡

不增加 `ex2Bubble`。现有 `ex2Stall` 和 `ex3Stall` 的组合决定 EX2 寄存器行为：

| `ex2Stall` | `ex3Stall` | 当前 EX2 是否进入 EX3 | EX2 下一状态 |
|---:|---:|---|---|
| 0 | 0 | 是 | 接收 EX1 新包 |
| 1 | 1 | 否 | 保持当前 EX2 包 |
| 1 | 0 | 是 | 写入气泡 |

不再定义 `ex2InsertBubble`。Pipeline 直接在寄存器控制中检查
`ex2Stall && !ex3Stall`。`Hazard` 必须保证只有“保持 EX1、排空 EX2”这一情况
才输出该组合；真正的全局停顿必须同时置位 `ex3Stall`。

嵌套依赖时 Hazard 输出：

```text
frontend.stall = 1  // 保持ID/取指
ex1Stall       = 1  // 保持当前EX1包
ex2Stall       = 1  // 当前EX1包不得覆盖EX2
ex3Stall       = 0  // 旧EX2包正常进入EX3
```

不得同时设置 `ex1Flush`、`ex2Flush` 或 `ex3Flush`。

## 4. `InstructionPackage.scala`

文件：`src/main/scala/Config/InstructionPackage.scala`

删除：

```scala
val shallowFwdMatch = UInt(8.W)
val aluResultInvalid = Bool()
```

增加：

```scala
val shallowRs1Sel = UInt(8.W)
val shallowRs2Sel = UInt(8.W)
val lateRs1Sel = UInt(8.W)
val lateRs2Sel = UInt(8.W)
val needsEX2Replay = Bool()
```

`IDUpdate` 初始化：

```scala
instPkg.shallowRs1Sel := 0.U
instPkg.shallowRs2Sel := 0.U
instPkg.lateRs1Sel := 0.U
instPkg.lateRs2Sel := 0.U
instPkg.needsEX2Replay := false.B
```

flush 和气泡使用 `0.U.asTypeOf(new InstructionPackage)` 清除所有字段。

## 5. `Backend.scala` 与 Backend/Hazard 接口

文件：`src/main/scala/Backend/Backend.scala`

### 5.1 `BackendHazardIO`

保留 Backend 到 Hazard 的阶段包：

```scala
val ex1Pkgs = Output(Vec(8, new InstructionPackage))
val ex2Pkgs = Output(Vec(8, new InstructionPackage))
```

删除：

```scala
val ex1AluResultInvalid = Output(Vec(8, Bool()))
```

增加 Backend/Forward 到 Hazard 的 EX1 选择结果。方向按 `BackendHazardIO` 位于
Backend 一侧书写，因此是 `Output`：

```scala
val ex1ShallowRs1Sel = Output(Vec(8, UInt(8.W)))
val ex1ShallowRs2Sel = Output(Vec(8, UInt(8.W)))
val ex1LateRs1Sel = Output(Vec(8, UInt(8.W)))
val ex1LateRs2Sel = Output(Vec(8, UInt(8.W)))
val ex1NeedsEX2Replay = Output(Vec(8, Bool()))
```

不增加 `ex2Bubble`。继续使用已有 `ex1Stall/ex2Stall/ex3Stall`。

### 5.2 Forward、Hazard 与 Pipeline 连接

Forward 的 selector 同时送往 Hazard 和对应 Pipeline：

```scala
io.hazard.ex1ShallowRs1Sel := forward.io.shallowRs1Sel
io.hazard.ex1ShallowRs2Sel := forward.io.shallowRs2Sel
io.hazard.ex1LateRs1Sel := forward.io.lateRs1Sel
io.hazard.ex1LateRs2Sel := forward.io.lateRs2Sel
io.hazard.ex1NeedsEX2Replay := forward.io.needsEX2Replay

p.io.forward.shallowRs1Sel := forward.io.shallowRs1Sel(idx)
p.io.forward.shallowRs2Sel := forward.io.shallowRs2Sel(idx)
p.io.forward.lateRs1Sel := forward.io.lateRs1Sel(idx)
p.io.forward.lateRs2Sel := forward.io.lateRs2Sel(idx)
p.io.forward.needsEX2Replay := forward.io.needsEX2Replay(idx)
```

Hazard 只读取 selector 进行嵌套判断，不修改 selector。Pipeline 直接使用 Forward
输出，避免 Forward→Hazard→Backend→Pipeline 的额外组合绕行。

## 6. `PipelineForwardIO`

定义位置：`src/main/scala/Backend/Pipeline/ALUBranchPipeline.scala`

将旧的单个 `shallowFwdMatch/aluResultInvalid` 接口替换为：

```scala
val shallowRs1Sel = Input(UInt(8.W))
val shallowRs2Sel = Input(UInt(8.W))
val lateRs1Sel = Input(UInt(8.W))
val lateRs2Sel = Input(UInt(8.W))
val needsEX2Replay = Input(Bool())
```

所有 Pipeline 在 EX1 将 Forward 结果写入 `ex1PkgOut`：

```scala
ex1PkgOut.shallowRs1Sel := io.forward.shallowRs1Sel
ex1PkgOut.shallowRs2Sel := io.forward.shallowRs2Sel
ex1PkgOut.lateRs1Sel := io.forward.lateRs1Sel
ex1PkgOut.lateRs2Sel := io.forward.lateRs2Sel
ex1PkgOut.needsEX2Replay := io.forward.needsEX2Replay
```

EX1 正常前进时，这些字段随 EX1/EX2 寄存器进入 EX2。发生嵌套保持时，EX2 写入
气泡，第一次选择结果不会进入 EX2；下一周期 Forward 根据新阶段位置重新计算。

`ex1Stall` 时虽然必须保持 `idEx1Pkgs` 的指令和 selector 元数据，但要用当拍
`fwdRs1/2/3Data` 更新其中的源数据。否则嵌套停顿第一拍命中的 WB 前递可能在下一拍
离开 WB，解除停顿时 EX1 会重新看到旧的寄存器堆数据。

## 7. `Hazard.scala`

文件：`src/main/scala/Hazard/Hazard.scala`

Hazard 不再生成 selector。它从 `io.backend` 读取 Forward 已经生成的：

```scala
io.backend.ex1ShallowRs1Sel
io.backend.ex1ShallowRs2Sel
io.backend.ex1LateRs1Sel
io.backend.ex1LateRs2Sel
io.backend.ex1NeedsEX2Replay
```

Hazard 新增的功能只有：

1. 根据 selector 检测嵌套依赖；
2. 产生 EX1 保持和 EX2 排空控制；
3. 在 ID RAW 中区分可进入晚前递路径的消费者。

### 7.1 嵌套检测

```scala
val laneNeedsLate = Wire(Vec(8, Bool()))

for (i <- 0 until 8) {
    laneNeedsLate(i) :=
        io.backend.ex1LateRs1Sel(i).orR ||
        io.backend.ex1LateRs2Sel(i).orR
}

val lateLaneMask = laneNeedsLate.asUInt
val nestedLateShallow = WireInit(false.B)

for (consumer <- 0 until 8) {
    val shallowProducer =
        io.backend.ex1ShallowRs1Sel(consumer) |
        io.backend.ex1ShallowRs2Sel(consumer)

    when((shallowProducer & lateLaneMask).orR) {
        nestedLateShallow := true.B
    }
}
```

### 7.2 控制优先级

```text
1. DIV/FDiv busy
2. EX3 分支重定向
3. EX2 分支重定向
4. EX1 嵌套保持
5. ID needWB RAW / 非shallowDepOK晚结果RAW
```

嵌套控制：

```scala
when(!divStall &&
     !ex3Redirect &&
     !ex2Redirect &&
     nestedLateShallow) {

    io.frontend.stall := true.B

    for (i <- 0 until 8) {
        io.backend.ex1Stall(i) := true.B
        io.backend.ex2Stall(i) := true.B

        // 必须保持false，使旧EX2包进入EX3。
        io.backend.ex3Stall(i) := false.B
    }
}
```

不设置任何 flush。

### 7.3 `needWB` RAW

现有 `needWB(op, pipelineIdx)` 保持，用于 load、MUL/DIV、FPU/FDiv 等必须等到
WB 的生产者。

旧逻辑不能再把所有 `needsEX2Replay` 无条件算作 ID stall。最终规则：

```text
needWB生产者RAW
    -> 原有ID停顿

晚生产者RAW且消费者shallowDepOK
    -> 允许进入EX1

晚生产者RAW且消费者非shallowDepOK
    -> 在生产者仍位于EX1或EX2时保持ID停顿，待其进入EX3后使用普通前递
```

这里的“晚生产者”包括 EX1 中 `ex1NeedsEX2Replay=1` 的写者以及 EX2 中已保存
`needsEX2Replay=1` 的写者。对 ID 包中每个消费者，EX1 和 EX2 都应用相同的消费者
能力检查：

```scala
val lateRs12Raw = ex1Pkg.rdValid &&
    io.backend.ex1NeedsEX2Replay(ex1Idx) &&
    ((idPkg.rs1ReadValid && idPkg.rs1 === ex1Pkg.rd) ||
     (idPkg.rs2ReadValid && idPkg.rs2 === ex1Pkg.rd))

val lateRawMustStall = lateRs12Raw && !idPkg.shallowDepOK
```

`rs3` 没有 EX2 replay 数据通路；其 RAW 仍按不可晚前递处理。`needWB` 的判断也不受
`shallowDepOK` 影响。因而 ID 的最终停顿条件是原有 `needWB` RAW、EX1/EX2 replay
生产者到非 replay-capable 消费者的 RAW、`rs3` 不可用 RAW 以及
`lateRawMustStall` 的或。这样 MUL/DIV/FPU/LSU 不会误用仅供整数 ALU/Branch 使用的
EX2 重算路径。

不新增 `canReplayInEX2`，直接复用 Decoder 已有的 `shallowDepOK`。

## 8. `Forward.scala`

文件：`src/main/scala/Backend/Bypass/Forward.scala`

Forward 负责所有数据相关匹配、selector 生成和数据前递，但不直接产生流水线
stall/flush。嵌套控制由 Hazard 根据 Forward 输出的 selector 生成。

### 8.1 高 lane 优先选择

```scala
def highestMatchOH(matches: Vec[Bool]): UInt = {
    MuxCase(
        0.U(8.W),
        (7 to 0 by -1).map { lane =>
            matches(lane) -> (BigInt(1) << lane).U(8.W)
        }
    )
}
```

必须先从全部匹配写者中选最高 lane，再判断被选写者是否需要 replay。不能先过滤
晚写者，否则可能绕过更高 lane 的普通同名写入。

### 8.2 EX1 同包浅依赖 selector

对 EX1 消费者 `consumer` 和更低的生产者 `producer`：

```scala
val producerOK =
    ex1Pkgs(producer).rdValid &&
    ex1Pkgs(producer).shallowDepOK

val consumerOK = ex1Pkgs(consumer).shallowDepOK

val rs1Match =
    ex1Pkgs(consumer).rs1ReadValid &&
    ex1Pkgs(producer).rd === ex1Pkgs(consumer).rs1

val rs2Match =
    ex1Pkgs(consumer).rs2ReadValid &&
    ex1Pkgs(producer).rd === ex1Pkgs(consumer).rs2
```

分别生成高 lane 优先的：

```scala
shallowRs1Sel(consumer)
shallowRs2Sel(consumer)
```

### 8.3 EX1 跨包晚依赖 selector

对 EX1 消费者的每个真实源，在全部 EX2 lane 中选择最高匹配写者：

```scala
val ex2Rs1Writer = highestMatchOH(ex2Rs1Matches)
val ex2Rs2Writer = highestMatchOH(ex2Rs2Matches)

val ex2ReplayMask =
    VecInit(ex2Pkgs.map(_.needsEX2Replay)).asUInt

val lateRs1Sel = Mux(
    ex1Pkg.shallowDepOK,
    ex2Rs1Writer & ex2ReplayMask,
    0.U
)
val lateRs2Sel = Mux(
    ex1Pkg.shallowDepOK,
    ex2Rs2Writer & ex2ReplayMask,
    0.U
)
```

ForwardIO 输出：

```scala
val shallowRs1Sel = Output(Vec(8, UInt(8.W)))
val shallowRs2Sel = Output(Vec(8, UInt(8.W)))
val lateRs1Sel = Output(Vec(8, UInt(8.W)))
val lateRs2Sel = Output(Vec(8, UInt(8.W)))
val needsEX2Replay = Output(Vec(8, Bool()))
```

```scala
io.needsEX2Replay(i) :=
    io.shallowRs1Sel(i).orR ||
    io.shallowRs2Sel(i).orR ||
    io.lateRs1Sel(i).orR ||
    io.lateRs2Sel(i).orR
```

这些输出同时送往 Pipeline 和 Hazard；Pipeline 将其随包保存，Hazard 用其检测嵌套。

### 8.4 普通 EX1 前递

优先级保持：

```text
EX2 > EX3 > WB
同阶段 lane 7 > ... > lane 0
```

EX2 中待重算结果必须屏蔽：

```scala
val ex2CanForward =
    io.ex2Pkgs(j).rdValid &&
    !io.ex2Pkgs(j).needsEX2Replay
```

EX3 中已经保存重算结果，可以普通前递：

```scala
val ex3CanForward = io.ex3Pkgs(j).rdValid
```

### 8.5 同包 EX2 前递

```scala
val shallowRs1Data = Mux1H(
    ex2Pkg.shallowRs1Sel,
    io.ex2Pkgs.map(_.aluResult)
)

val shallowRs2Data = Mux1H(
    ex2Pkg.shallowRs2Sel,
    io.ex2Pkgs.map(_.aluResult)
)
```

### 8.6 EX3 到 EX2 晚前递

EX1 已经选定生产者，因此 EX2 不重新比较寄存器号：

```scala
val lateRs1Data = Mux1H(
    ex2Pkg.lateRs1Sel,
    io.ex3Pkgs.map(_.aluResult)
)

val lateRs2Data = Mux1H(
    ex2Pkg.lateRs2Sel,
    io.ex3Pkgs.map(_.aluResult)
)
```

优先级为：

```text
当前包低槽EX2 > 前一包EX3 > EX1保存值
```

```scala
val rs1AfterLate = Mux(
    ex2Pkg.lateRs1Sel.orR,
    lateRs1Data,
    ex2Pkg.rs1Data
)

val replayRs1Data = Mux(
    ex2Pkg.shallowRs1Sel.orR,
    shallowRs1Data,
    rs1AfterLate
)
```

`rs2` 同理。ForwardIO 输出：

```scala
val replayRs1Data = Output(Vec(8, UInt(32.W)))
val replayRs2Data = Output(Vec(8, UInt(32.W)))
```

删除 Forward 中旧的单 selector 接口：

```text
shallowMatch回送接口
```

其功能由每源独立的 `shallowRs*Sel/lateRs*Sel/needsEX2Replay` 替代。

## 9. 所有 Pipeline 的共同修改

涉及：

- `FDivFPUPipeline.scala`
- `ALUFPUPipeline.scala`
- `ALUiMDPipeline.scala`
- `ALULSUPipeline.scala`
- `ALUBranchPipeline.scala`

### 9.1 EX1 保存 Forward 选择信息

```scala
ex1PkgOut.shallowRs1Sel := io.forward.shallowRs1Sel
ex1PkgOut.shallowRs2Sel := io.forward.shallowRs2Sel
ex1PkgOut.lateRs1Sel := io.forward.lateRs1Sel
ex1PkgOut.lateRs2Sel := io.forward.lateRs2Sel
ex1PkgOut.needsEX2Replay := io.forward.needsEX2Replay
```

### 9.2 EX2 寄存器控制

不创建 `ex2InsertBubble`。直接在寄存器控制中解释 stall 组合：

```scala
when(io.hazard.ex2Flush) {
    ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
}.elsewhen(io.hazard.ex2Stall && !io.hazard.ex3Stall) {
    ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
}.elsewhen(!io.hazard.ex2Stall) {
    ex2Pkg := ex1PkgOut
}
```

所有与 EX2 包对齐的辅助寄存器使用相同条件清零：

```scala
when(io.hazard.ex2Flush ||
     (io.hazard.ex2Stall && !io.hazard.ex3Stall)) {
    auxiliaryEX2Reg := 0.U
}.elsewhen(!io.hazard.ex2Stall) {
    auxiliaryEX2Reg := auxiliaryEX1Value
}
```

真正的 div stall 同时设置 `ex2Stall=1`、`ex3Stall=1`，因此 EX2 正常保持。

### 9.3 EX2 ALU重算

```scala
aluEX2.io.src1 := Mux(
    ex2Pkg.src1Sel === 0.U,
    io.forward.replayRs1Data,
    ex2Pkg.pc
)

aluEX2.io.src2 := Mux(
    ex2Pkg.src2Sel === 0.U,
    io.forward.replayRs2Data,
    ex2Pkg.imm
)

val ex2PkgOut = WireDefault(ex2Pkg)

when(ex2Pkg.needsEX2Replay) {
    ex2PkgOut.aluResult := aluEX2.io.res
}

ex2PkgOut.rs1Data := io.forward.replayRs1Data
ex2PkgOut.rs2Data := io.forward.replayRs2Data
```

### 9.4 EX1 有状态单元

嵌套时 EX1 包保持两周期。组合 ALU/Branch 无副作用，但 FDiv、SRT2、Multiply、
FPU 必须避免重复或错位启动。

不增加新的 hold 信号。有 valid/start 的单元直接使用同一控制条件：

```scala
unit.io.valid := originalValid &&
    !(io.hazard.ex1Stall &&
      io.hazard.ex2Stall &&
      !io.hazard.ex3Stall)
```

没有 valid 的全流水单元应注入 NOP，或证明第一次结果只对应 EX2 气泡、第二次结果
与真实包对齐。`ex2Stall` 不得接到功能单元 kill。

## 10. 各具体 Pipeline 修改

### 10.1 `FDivFPUPipeline`

- 增加 EX2 ALU；
- 只在 `needsEX2Replay` 时覆盖整数 `aluResult`；
- `fdiv.io.valid` 在 `ex1Stall && ex2Stall && !ex3Stall` 时禁止新启动；
- FPU/FDiv 专用结果路径不变；
- `fdiv.io.kill` 只响应真正 flush，不响应 `ex2Stall`。

### 10.2 `ALUFPUPipeline`

- lane 1 的 `enableShallowEX2` 改为 `true`；
- lane 2 保持启用；
- 两条 lane 使用 Forward 的 replay 数据；
- 转换结果和 fflags 与 EX2 气泡同步清零；
- 审计 FPU 在 EX1 hold 时的结果对齐。

### 10.3 `ALUiMDPipeline`

- 保留 EX2 ALU并使用统一 replay 数据；
- 审计 Multiply/SRT2 在 EX1 hold 时的启动；
- replay 只覆盖 ALU 结果，不替代 MUL/DIV 结果；
- Multiply 的内部流水只在 `ex3Stall=1` 的全局保持时冻结；嵌套控制的
  `ex2Stall=1, ex3Stall=0` 必须让已在 EX2 的乘法继续进入内部下一阶段；
- div busy 时 `ex3Stall=1`，不会误插 EX2 气泡。

### 10.4 `ALULSUPipeline`

- 保留 EX2 ALU并使用统一 replay 数据；
- load/store 不通过 `shallowDepOK` 进入本实验 replay；
- EX2 气泡不得产生 LSU 请求或 Store Buffer 入队；
- Store Buffer 的 store-to-load 前递不变。

### 10.5 `ALUBranchPipeline`

- 保留 EX2 ALU并使用统一 replay 数据；
- `needsEX2Replay` 时屏蔽旧 EX2 redirect：

  ```scala
  io.hazard.predFailEX2 :=
      ex2Pkg.predFail && !ex2Pkg.needsEX2Replay
  ```

- 保存修复后的 `rs1Data/rs2Data`；
- EX3 使用修复源重新执行 Branch/JALR；
- redirect 高于嵌套保持。

## 11. Frontend 修改

Frontend 无新增结构。继续响应：

```text
frontend.stall
frontend.flush
```

嵌套时 `frontend.stall=1` 保持 ID/取指，Backend 的 `idEx1Pkgs` 在
`ex1Stall=1` 时保持当前 EX1 包。

## 12. 嵌套依赖逐周期行为

设：

```text
包A：slot3在EX2重算后写x5

包B：
    slot2: ADD  x6, x5, x0
    slot3: ADDI x7, x6, 1

包C：下一包
```

### Cycle N：A 在 EX2，B 在 EX1，C 在 ID

Hazard 得到：

```text
B.slot2.lateRs1Sel = A.slot3
B.slot2.needsEX2Replay = 1

B.slot3.shallowRs1Sel = B.slot2
B.slot3.needsEX2Replay = 1

nestedLateShallow = 1
```

输出：

```text
frontend.stall = 1
ex1Stall       = 1
ex2Stall       = 1
ex3Stall       = 0
```

Pipeline 直接识别 `ex2Stall=1 && ex3Stall=0`，在 EX2 寄存器写入气泡。时钟沿后：

```text
A：EX2 -> EX3，结果已修正
B：保持在EX1
C：保持在ID
EX2：写入气泡
```

第一次生成的 B selector 没有进入 EX2。

### Cycle N+1：A 在 EX3，B 仍在 EX1

EX2 是气泡，所以：

```text
B.slot2.lateRs1Sel = 0
```

B.slot2 通过普通 Forward 取得 A.EX3 的正确 x5，并在 EX1 算出正确 x6。
B.slot3 仍浅依赖 B.slot2，因此：

```text
B.slot3.shallowRs1Sel = B.slot2
B.slot3.needsEX2Replay = 1
nestedLateShallow = 0
```

停顿解除。时钟沿后：

```text
A：EX3 -> WB
B：EX1 -> EX2，携带Forward选择信息
C：ID -> EX1
```

### Cycle N+2：B 在 EX2

Forward 使用 B.slot3 已寄存的 `shallowRs1Sel`：

```text
B.slot2.EX2寄存结果
    -> Forward浅前递MUX
    -> B.slot3.EX2 ALU
```

B.slot3 得到正确 x7。全程没有双 ALU 组合链。

## 13. 普通晚前递逐周期行为

若消费者没有成为本包其他指令的浅依赖生产者，则不停止：

```text
Cycle N：
    A在EX2重算x5
    B在EX1识别A为晚生产者
    Forward输出B.lateRs1Sel和needsEX2Replay

Cycle N结束：
    A进入EX3
    B进入EX2，selector随包进入EX2

Cycle N+1：
    Forward按B.lateRs1Sel选择A.EX3.aluResult
    B在EX2重算

Cycle N+1结束：
    B进入EX3，结果正确
```

## 14. 必须保持的设计不变量

1. EX2 中 `needsEX2Replay=1` 的旧结果不参与普通 EX2 到 EX1 前递；
2. EX3 中的 replay 结果有效，可以普通前递；
3. selector 只在 Forward 的 EX1 分析中产生，并由 Pipeline 随包保存；
4. Forward 的 EX3 到 EX2 前递只使用已寄存 selector；
5. WAW 始终选择最高 lane；
6. 当前包低槽结果优先于前一包同名结果；
7. `ex2Stall=1 && ex3Stall=0` 表示排空 EX2 并插气泡；
8. 全局 stall 必须设置 `ex3Stall=1`，从而保持 EX2；
9. 嵌套停顿不得设置 flush；
10. `ex2Stall` 不得连接到功能单元 kill；
11. EX1 hold 期间有状态单元不得重复产生可见启动；
12. selector 随 stall 保持，随 flush/气泡清零；
13. redirect 优先于嵌套保持；
14. `needWB` RAW 不得被晚前递绕过；
15. 无效源和 GPR x0 不得建立依赖。
16. EX1 保持时必须保存当拍普通 Forward 已解析出的源值，不能依赖下一拍仍存在的 WB；
17. 非 `shallowDepOK` 消费者不得生成 late selector，并须等待 replay 生产者进入 EX3；
18. EX2 排空气泡不得冻结与当前 EX2 指令对齐的 Multiply/FPU 后级。

## 15. 文件修改清单

| 文件 | 修改内容 |
|---|---|
| `Config/InstructionPackage.scala` | 增加每源 selector 和 `needsEX2Replay`，删除旧字段 |
| `Hazard/Hazard.scala` | 读取 Forward selector、检测嵌套、产生 stall 组合 |
| `Backend/Backend.scala` | 将 Forward selector 同时接到 Hazard 和各 Pipeline |
| `Backend/Bypass/Forward.scala` | 生成每源 selector，并按已寄存 selector 完成 EX3→EX2 前递 |
| `Backend/Pipeline/ALUBranchPipeline.scala` | 扩展 PipelineForwardIO、统一 EX2 气泡和分支重判断 |
| `Backend/Pipeline/FDivFPUPipeline.scala` | 增加 EX2 ALU，处理 hold/valid |
| `Backend/Pipeline/ALUFPUPipeline.scala` | lane1启用EX2 ALU，辅助状态与气泡对齐 |
| `Backend/Pipeline/ALUiMDPipeline.scala` | 使用统一 replay 数据，审计 MUL/DIV hold |
| `Backend/Pipeline/ALULSUPipeline.scala` | 使用统一 replay 数据，气泡不得发 LSU 请求 |
| `Frontend/Frontend.scala` | 无新增结构，确认 stall 时 ID 保持 |
| `src/test/scala/ShallowDependencyTest.scala` | 更新字段和接口，增加晚前递/嵌套测试 |

## 16. 验证要求

### Hazard

- 正确读取 Forward 生成的 shallow/late selector；
- 普通晚依赖不停止；
- 嵌套依赖输出 `frontend.stall=1, ex1Stall=1, ex2Stall=1, ex3Stall=0`；
- 嵌套周期不产生 flush；
- 下一周期 EX2 气泡使嵌套自动解除；
- div stall 时 `ex3Stall=1`，EX2 保持；
- redirect 高于嵌套；
- `needWB` RAW 行为不变。

### Forward

- 同包和跨包 selector 均满足高 lane 优先；
- 更高普通写者覆盖低槽晚写者时不产生错误 late selector；
- 普通 EX2 > EX3 > WB 优先级；
- `needsEX2Replay` 屏蔽 EX2 普通前递；
- replay 后 EX3 结果可普通前递；
- 每源 selector 正确选择 EX2/EX3 数据；
- 当前包浅数据优先于前一包晚数据；
- Forward 的 EX2 replay 路径不包含寄存器比较和优先编码。

### Pipeline/CPU

- lane 0、lane 1 EX2 ALU重算；
- `ex2Stall=1 && ex3Stall=0` 时旧 EX2 进入 EX3、EX2 写气泡；
- `ex2Stall=1 && ex3Stall=1` 时 EX2 保持；
- selector 随包移动并随气泡/flush 清零；
- EX1 hold 不重复启动 FDiv/SRT2 等状态单元；
- EX2 气泡不产生 LSU/Store Buffer 请求；
- 连续跨包晚前递无额外停顿；
- 嵌套情况 EX1 恰好保持一拍；
- Branch/JALR 使用修正源在 EX3 重定向。

## 17. 建议实施顺序

1. 修改 `InstructionPackage` 与 IO 字段；
2. 在 Forward 将 selector 扩展为每源独立的 shallow/late selector；
3. 将 Forward selector 经 Backend 同时接入 Hazard 和 Pipeline；
4. 在 Hazard 增加基于 selector 的嵌套检测和控制；
5. 在 Forward 按已寄存 selector 完成 EX2/EX3 数据选择；
6. 给 lane 0、lane 1 增加 EX2 ALU；
7. 修改所有 Pipeline 对 `ex2Stall/ex3Stall` 组合的解释；
8. 审计 EX1 有状态功能单元；
9. 更新 Branch 和定向测试；
10. 完成 RTL 生成、单元测试和软件回归。

## 18. 结论

最终方案不新增 `ex2Bubble`，也不保留局部 `ex2InsertBubble` 命名信号。Forward 在
EX1 完成全部依赖选择，Hazard 根据 selector 判断嵌套并使用：

```text
ex1Stall=1，ex2Stall=1，ex3Stall=0
```

编码“保持 EX1、排空旧 EX2、在 EX2 入口插入气泡”。选择信息由 Forward 输出给
Pipeline，并随流水进入 EX2；Forward 使用这些已寄存的信息完成 EX3 到 EX2 前递。

这样既避免在 EX2 关键路径重新进行寄存器比较，也避免在 ID 重复进行浅依赖判断，
并保证所有组合数据路径最多经过一个 ALU。
