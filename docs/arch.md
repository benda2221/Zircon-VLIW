# Zircon-VLIW 处理器设计文档

## 概述

Zircon-VLIW（以下简称 Zircon）是一款基于 RISC-V 的 VLIW 处理器，旨在高效处理 DSP 任务。该处理器采用 8 取指、8 译码、8 发射、8 提交的并行结构，能够处理由 VLIW 编译器生成的 RISC-V imf 程序。

在原型机阶段，Zircon 采用哈佛架构，假设拥有单周期 100% 命中的无限大 Cache——即所有访存行为（包括取指、加载、存储）完全由软件仿真环境 Emulator 在单周期内完成。

![RProject.drawio](./arch.assets/RProject.drawio.svg)

Zircon-VLIW 原型机的设计模块关系如下：

* **Frontend（前端）**
  * NPC（下一 PC 生成）
  * Decoder ×8（8 路译码器）
  * FPRegfile（浮点寄存器堆）
  * GPRegfile（通用寄存器堆）

* **Backend（后端）**
  * FDiv-FPU Pipeline
  * ALU-FPU Pipeline ×2
  * ALU-iMD Pipeline ×2（ALU-整数乘除法）
  * ALU-LSU Pipeline ×2（ALU-访存）
  * ALU-Branch Pipeline ×2

* **Hazard（冒险控制）**

---

## 指令传递范式

在 Zircon 中，指令以 `InstructionPackage` 类为基本单元，该类包含了一条指令从取指到写回全过程中可能携带的所有信息，包括指令码、读写数据等。这个包会随着流水线向后传递，并在编译时借助 Firrtl 的死代码删除优化，裁剪掉后续流水级中不再使用的成员。

`InstructionPackage` 的定义如下：

```scala
class InstructionPackage extends Bundle {
    /* IF Stage */
    val pc        = UInt(32.W)
    val inst      = UInt(32.W)
    /* ID Stage */
    // regfile
    val rs1       = UInt(6.W) // the highest bit to judge if it is float
    val rs2       = UInt(6.W) // the highest bit to judge if it is float
    val rs3       = UInt(6.W) // the highest bit to judge if it is float
    val rd        = UInt(6.W) // the highest bit to judge if it is float
    val rdValid   = Bool()
    val rs1Data   = UInt(32.W)
    val rs2Data   = UInt(32.W)
    val rs3Data   = UInt(32.W)
    // operation
    val op        = UInt(7.W) // [3:0]: operation; [4]: is branch or load or muldiv [5]: is store  [6]: is float (not fdiv) 
    val rm        = UInt(3.W) // rounding mode for FPU
    val imm       = UInt(32.W)
    val src1Sel   = UInt(1.W) // 0: rs1; 1: pc; 
    val src2Sel   = UInt(1.W) // 0: rs2; 1: imm;
    /* EX Stage */
    val aluResult = UInt(32.W)
    val fpuResult = UInt(32.W)
    val fflags    = UInt(5.W) // floating-point status flags (NV, DZ, OF, UF, NX)
    val branchTgt = UInt(32.W)
    val predFail  = Bool()
    val memResult = UInt(32.W)
    /* WB Stage */
    val rfWdata   = UInt(32.W)

    def IFUpdate(pc: UInt, inst: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.pc     := pc
        instPkg.inst   := inst
        instPkg
    }
    def IDUpdate(rs1: UInt, rs2: UInt, rs3: UInt, rd: UInt, rdValid: Bool, op: UInt, imm: UInt, src1Sel: UInt, src2Sel: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.rs1       := rs1
        instPkg.rs2       := rs2
        instPkg.rs3       := rs3
        instPkg.rd        := rd
        instPkg.rdValid   := rdValid
        instPkg.op        := op
        instPkg.imm       := imm
        instPkg.src1Sel   := src1Sel
        instPkg.src2Sel   := src2Sel
        instPkg
    }
    def IDUpdate(rs1Data: UInt, rs2Data: UInt, rs3Data: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.rs1Data   := rs1Data
        instPkg.rs2Data   := rs2Data
        instPkg.rs3Data   := rs3Data
        instPkg
    }
    def EX1Update(aluResult: UInt, branchTgt: UInt, predFail: Bool): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.aluResult := aluResult
        instPkg.branchTgt := branchTgt
        instPkg.predFail  := predFail
        instPkg
    }
    def EX2Update(memResult: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.memResult := memResult
        instPkg
    }
    def EX3Update(fpuResult: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.fpuResult := fpuResult
        instPkg
    }
    def WBUpdate(rfWdata: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.rfWdata := rfWdata
        instPkg
    }
}
```

以上定义中的几个 Update 方法，用于在流水线的不同流水级中，从不同的模块逐步"组装"数据到 `InstructionPackage` 中。除了 `Decoder` 会在模块内部进行组装之外，其余的方法都在 `Frontend.scala` 和 `Backend.scala` 中按照流水级进行调用。

---

## Frontend（前端）

前端的代码目录结构如下：

```
Frontend/
├── Frontend.scala
├── Fetch/
│   └── NPC.scala
├── Decode/
│   ├── Decoder.scala
│   └── RVISA.scala
└── Regfile/
    └── Regfile.scala
```

### Frontend 顶层模块

前端由两个流水级组成：**IF（取指）**和 **ID（指令译码）**。其中，NPC 和 PC 寄存器（复位值 `0x80000000`）位于 IF 段，而 Decoder 们和两个例化的寄存器堆位于 ID 段，流水级之间使用段间寄存器分割。此外，取指行为在 IF 端发起，同周期仿真环境会根据 PC 值返回 8 条地址连续的指令。

`FrontendIO` 包含 3 套接口：

* **FrontendMemIO**：将 PC 值发送给仿真环境，仿真环境返回以当前 PC 为起始地址的连续 8 条指令。
* **FrontendBackendIO**：将 ID 段组装好的 8 个 `InstPkg` 发送给后端，接收后端的重定向目标地址，以及后端的写回请求。
* **FrontendHazardIO**：接收 Hazard 给出的 `flush` 和 `stall` 信号，用来控制流水线寄存器的流动和 NPC 的生成，同时将 ID 阶段组装好的 `InstPkg` 发给 Hazard 用于冲突检测。

**控制信号响应**：
* 当 Hazard 给出 `flush` 信号时，IF-ID 段间寄存器应当被冲刷，NPC 应当基于后端的重定向目标地址重新生成 PC。
* 当 Hazard 给出 `stall` 信号时，IF-ID 段间寄存器应当被阻塞，NPC 应当保持 PC 不变。

### NPC（下一 PC 生成）

NPC 模块负责根据流水线的执行情况计算下一个 PC 并实现 PC 的重定向。`NPCIO` 包含 3 套接口：

* **NPCHazardIO**：接收 Hazard 的冲刷和阻塞信号。
* **NPCBackendIO**：接收后端计算单元给出的 PC 重定向地址。
* **NPCFrontendIO**：接收前端的当前 PC 值，向前端发送 `npc`（下一个 PC 值）。

### Decoder（译码器）

Decoder 将指令进行译码，转换为 `InstructionPackage` 中的部分字段。由于每条流水线只能接收部分固定类型的指令，因此 Decoder 采用模板化设计（通过输入参数控制能够译码哪些类型的指令）。8 条流水线的功能部件如下表所示（**需要注意**：0 号流水线的 FPU 不支持 FPToInt 和 IntToFP 转换，1 号和 2 号支持）：

| 流水线编号 | 0    | 1    | 2    | 3        | 4        | 5    | 6    | 7      |
| ---------- | ---- | ---- | ---- | -------- | -------- | ---- | ---- | ------ |
| 部件1      | FDiv | ALU  | ALU  | ALU      | ALU      | ALU  | ALU  | ALU    |
| 部件2      | FPU  | FPU  | FPU  | iMul/Div | iMul/Div | LSU  | LSU  | Branch |

具体部件的功能将在后文讲解。我们为每一个功能部件可以执行的指令创建了一个译码表（即指令码到操作微码的映射）。在执行时，Decoder 会查询自己支持的所有译码表，确定当前指令所需的操作微码，并将其打包到 `InstPkgOut` 中输出。

Decoder 只有一套 IO：

* **DecoderIO**：输入 `inst`，输出 `InstPkgOut`。

Decoder 基于 `RVISA.scala` 中使用 `BitPat` 定义的指令集编码进行工作，具体操作微码编码在 `Config.scala` 中。

**译码器实现细节**：

* **寄存器编号扩展**：指令集中的寄存器编号为 5 位，我们扩展了 1 位。如果是 GPR，则最高位为 0；如果是 FPR，则最高位为 1。
* **GPR 0 号寄存器处理**：对于 `rd` 编号为 0 且是 GPR 的情况，`rdValid` 将会被设置为 `false.B`。这是为了简化 Regfile 的写优先设计：
  * 如果向 GPR 0 号寄存器写入，该写入值不应该被写优先前递，因为 0 号寄存器读取值恒为 0。这样当初始化为 0 时，GPR 的写前递、读逻辑都不需要特判地址为 0 的情况。
  * FPR 并没有"0 号寄存器恒为 0"的规定。

### Regfile（寄存器堆）

Regfile 是一个写优先、长度为 32、宽度为 32 位的寄存器堆模板，读写端口数量可参数化配置。Frontend 将其例化为 `gpr` 和 `fpr`。由于 RISC-V 寄存器编号位于指令的固定位置，因此读地址无需经过译码就可以直接由 Frontend 连接到 Regfile。

前端并不是 8 个通道都需要同时读 GPR 和 FPR，后端也并不是每一条流水线都有可能写回 GPR 和 FPR。具体连线分配关系如下表所示：

| 流水线编号 | 0    | 1    | 2    | 3    | 4    | 5    | 6    | 7    |
| ---------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| 读GPR口    | 0    | 2    | 2    | 2    | 2    | 2    | 2    | 2    |
| 读FPR口    | 3    | 3    | 3    | 0    | 0    | 0    | 0    | 0    |
| 写GPR口    | 0    | 1    | 1    | 1    | 1    | 1    | 1    | 1    |
| 写FPR口    | 1    | 1    | 1    | 0    | 0    | 0    | 0    | 0    |

因此，GPR 有 14 个读口和 7 个写口，FPR 有 9 个读口和 3 个写口。每个寄存器堆内，所有读口对所有写口都需要判断写优先。

---

## Backend（后端）

后端的代码目录结构如下：

```
Backend/
├── Backend.scala
├── Pipeline/
│   ├── FDivFPUPipeline.scala
│   ├── ALUFPUPipeline.scala
│   ├── ALUiMDPipeline.scala
│   ├── ALULSUPipeline.scala
│   └── ALUBranchPipeline.scala
├── FunctionUnit/
│   ├── ALU.scala
│   ├── Adder.scala
│   ├── Shifter.scala
│   ├── Branch.scala
│   ├── Multiply.scala
│   ├── SRT2.scala
│   ├── LSU.scala
│   ├── FPU.scala          # 浮点算术单元
│   ├── FPUConvert.scala   # 类型转换单元
│   ├── FDiv.scala         # 浮点除法单元
│   └── fudian/            # Fudian 浮点库
│       ├── FADD.scala
│       ├── FMUL.scala
│       ├── FDIV.scala
│       ├── FCMP.scala
│       ├── FPToInt.scala
│       ├── IntToFP.scala
│       ├── FPToFP.scala
│       ├── FCMA.scala
│       ├── RoundingUnit.scala
│       ├── package.scala
│       └── utils/
└── Bypass/
    └── Forward.scala
```

### Backend 顶层模块

后端由多个 Pipeline 组成。在这些 Pipeline 之前，首先使用一个段间寄存器接收 Frontend 给出的 `InstructionPackage`——这也是 ID 和 EX1 阶段之间的段间寄存器：**ID-EX1 段间寄存器**。每个 Pipeline 内部都有 **EX1、EX2、EX3、WB** 四个阶段，也就是拥有 **EX1-EX2、EX2-EX3、EX3-WB** 三个段间寄存器。

每个 Pipeline 中拥有的功能单元如下表所示：

| 流水线编号 | 0    | 1    | 2    | 3        | 4        | 5    | 6    | 7      |
| ---------- | ---- | ---- | ---- | -------- | -------- | ---- | ---- | ------ |
| 部件1      | FDiv | ALU  | ALU  | ALU      | ALU      | ALU  | ALU  | ALU    |
| 部件2      | FPU  | FPU  | FPU  | iMul/Div | iMul/Div | LSU  | LSU  | Branch |

`BackendIO` 包含 3 套接口：

* **Flipped(FrontendBackendIO)**：将前端和后端的接口整体使用 `Flipped` 反转方向。
* **BackendHazardIO**：向 Hazard 输出 EX1 和 EX2 阶段各流水线的 `InstPkg` 用于 RAW 冲突判断，传递后端可能出现的分支预测失败 `flush`、除法器 `stall` 等信号，同时接收 Hazard 针对每个阶段寄存器的独立 `flush` 和 `stall` 信号（即，任何停顿信号都应该送入 Hazard 统一调度）。
* **BackendMemIO**：两个 ALULSUPipeline 中 LSU 与仿真环境的数据交互接口，根据地址进行数据读写。

**段间寄存器控制**：每个寄存器在 Hazard 给出属于该寄存器的 `flush` 或 `stall` 信号时，都应该做出相应的响应。注意当同时给出时，优先响应 `flush`（除了 ID-EX1 寄存器之外，其余都封装在 Pipeline 内部）。

### Arithmetic（算术功能单元）

#### Adder（加法器）

Adder 中包含多种经过测试的块间进位加法器，时序性能较好，供其他功能单元调用。

#### Shifter（移位器）

桶形移位器实现。

#### ALU（算术逻辑单元）

ALU 占据 EX1 阶段，通过实例化 Adder 和 Shifter，并补充一些辅助逻辑，可以实现 `EXEOp` 中所有标注的非乘除整型运算。其两个源操作数的选择在 Pipeline 中由 `InstPkg` 中的 `Src1Sel` 和 `Src2Sel` 决定，其结果可以在 EX2 阶段被前递。**特别注意**：

* 在执行 `JALR` 和 `JAL` 指令时，应该默认让其执行 `PC+4` 的操作。
* 如果选择了 `RS1` 或 `RS2` 作为源操作数，还需要检查该数据是否正在由 Forward 模块前递，以前递数据为准。

#### Branch（分支单元）

Branch 占据 EX1 阶段，可以判断分支是否跳转并计算分支跳转地址。注意其输入同样受到前递影响。

#### Multiply（乘法器）

该乘法器采用 3 级流水，使用 2-bit Booth 编码 + Wallace Tree + 全加器，占据 EX1、EX2、EX3 三级流水线。其结果必须等到 WB 段才可以前递。注意其内部有独立的段间寄存器，同样需要响应 Hazard 给出的 `stall` 或 `flush` 信号。其输入同样受到前递影响。

#### SRT2（SRT2 除法器）

SRT2 除法器占据 EX1、EX2、EX3 三级流水线，其结果必须等到 WB 段才可以前递。其 `stall` 信号在 EX3 阶段产生，计算周期不定，由被除数和除数的绝对值前导零之差决定。其输入同样受到前递影响。

#### FPU（浮点运算单元）

FPU 占据 EX1、EX2、EX3 三级流水。FADD、FSUB 和 FMUL 使用 Zircon-FloatPoint 数据通路，其他操作继续复用 Fudian 浮点模块，支持以下运算：

- **基本算术**：加法（FADD）、减法（FSUB）、乘法（FMUL）
- **符号注入**：FSGNJ、FSGNJN、FSGNJX
- **比较运算**：FEQ、FLT、FLE、FMIN、FMAX
- **分类指令**：FCLASS
- **寄存器移动**：FMV.X.W、FMV.W.X

加法/减法依次完成“分类与对阶、有效数运算与规格化、舍入与打包”，乘法依次完成“分类与有效数乘积、规格化、舍入与打包”。两个段间寄存器与处理器的 stall/flush 同步，结果在 EX3→WB 边界写入指令包，因此可以每周期接收一条新运算且不会与指令包错位。其结果在 WB 阶段才能前递。FPU 会输出 5 位的浮点状态标志（fflags）。详细设计与验证范围见 [Zircon 浮点加法与乘法流水线](zircon-fp-add-mul.md)。

**类型转换支持**：1-2 号流水线的 FPU 额外支持浮点-整数类型转换：
- 1 号流水线：FPToInt（FCVT.W.S、FCVT.WU.S）
- 2 号流水线：IntToFP（FCVT.S.W、FCVT.S.WU）

#### FDiv（浮点除法器）

FDiv 基于 SRT-2 算法实现，支持除法（FDIV.S）和开方（FSQRT.S）运算。计算周期不固定，由操作数决定。运算期间会产生 `busy` 信号阻塞流水线。

控制信号接口：
- `isSqrt`：置 1 进入开方模式
- `in_valid` / `in_ready`：输入握手信号
- `out_valid` / `out_ready`：输出握手信号
- `kill`：终止正在进行的运算

> **注意**：`out_valid` 置 1 后需多打一拍才能得到正确结果。

#### LSU（加载存储单元）

LSU 占据 EX2、EX3 阶段。在原型机阶段实现非常简单：将 `op`、由 ALU 计算并通过 EX1-EX2 寄存器传递的地址、在 EX2 阶段将经过前递修正的写数据（`rs2Data`）送出，然后将读回的数据按照加载指令的定义进行零扩展或符号扩展，送到后续阶段。这个过程在原型机阶段实际上在 EX2 阶段就全部完成了，但为了模拟真实 Cache 的行为，我们要求其空过 EX3 阶段，只有到 WB 阶段才能前递。

### Pipeline（流水线）

每个 Pipeline 都有以下几套基础接口：

* **PipelineForwardIO**：包括 EX1 阶段的 `instPkg`、EX2 阶段的 `InstPkg`、WB 阶段的 `InstPkg`，用于供 Forward 模块进行前递判断，并接收 Forward 的前递数据。
* **PipelineBackendIO**：接收 Backend 从 Frontend 送来的 `InstPkg`，每个 Pkg 按照流水线编号只会送入对应的一个 Pipeline。
* **PipelineFrontendIO**：写回前端两个寄存器堆的数据和请求，包括地址、写数据和两个寄存器堆的写使能。ALUBranchPipeline 还包括特有的分支跳转地址。
* **PipelineHazardIO**：将产生的 `stall` 和 `flush` 请求送到 Hazard 进行统一调度；同时需要将 EX1 和 EX2 阶段的 `instPkg` 送出，让 Hazard 判断 RAW 冲突，并接收 Hazard 给出的针对每个段间寄存器的 `stall` 和 `flush` 信号。

流水线的 WB 段需要一个多路选择器，基于 `op` 判断应该写回哪个功能单元产生的数据，并将更新后的 `InstPkg` 送到 Forward 模块。

#### FDivFPUPipeline

该流水线（0 号流水线）包含 FDiv 浮点除法器和 FPU 浮点运算单元。FDiv 在运算期间会产生 `busy` 信号，通过 `PipelineHazardIO` 送到 Hazard 模块进行流水线停顿控制。当分支预测失败时，需要通过 `kill` 信号终止正在进行的浮点除法运算。FPU 和 FDiv 的结果都需要等到 WB 阶段才能前递。

#### ALUFPUPipeline

该流水线（1-2 号流水线）包含 ALU 和 FPU。ALU 位于 EX1 阶段，其结果可以在 EX2 阶段前递。FPU 占据 EX1-EX3 三级流水，其结果在 WB 阶段前递。

该流水线通过 `convert` 参数控制类型转换方向：
- `convert = 1`（1 号流水线）：启用 FPToInt，支持 FCVT.W.S、FCVT.WU.S
- `convert = 0`（2 号流水线）：启用 IntToFP，支持 FCVT.S.W、FCVT.S.WU

#### ALUiMDPipeline

该流水线只有 EX1 阶段包含 ALU，EX1、EX2、EX3 阶段包含 Multiply 和 SRT2 除法器。该流水线需要将除法器产生的停顿信号送到 `PipelineHazardIO` 的 `stall` 中。ALU 结果在 EX2 前递，乘除法结果必须在 WB 前递。注意，EX3阶段将会对除法器和乘法器的结果进行选择。

#### ALULSUPipeline

该流水线 EX1 阶段包含 ALU，EX2 阶段包含 LSU。注意 LSU 需要使用 ALU 的加法结果进行访存，所以这里的 ALU 需要特殊判断：当前如果是 `load` 或 `store` 指令，默认执行加法操作。注意 EX3 阶段需要空过，等到 WB 才可以前递 LSU 的结果。ALU 的结果在 EX2 就可以前递。

#### ALUBranchPipeline

该流水线 EX1 阶段包含 ALU 和 Branch。ALU 结果在 EX2 阶段前递。注意 Branch 结果不可以在 EX1 阶段直接送出，而是为了时序考虑打一拍，在 EX2 阶段一起送出到 `PipelineHazardIO`。注意，我们目前完全采用静态预测，不需要给出 `predOffset`，如果发现需要跳转，就认为 `predFail`。

### Forward（数据前递）

Forward 模块接收每条流水线在 EX2、EX3 或 WB 阶段给出的 `InstPkg`，通过判断其写入使能、写入寄存器地址与 EX1 阶段的 `instPkg` 的 2-3 个源寄存器（注意只有前三条流水线有 3 个源寄存器，后面的流水线只有 2 个源寄存器），判断是否需要将 EX2、EX3 或 WB 阶段的数据进行前递。

这里的逻辑比较复杂：每一个 EX1 阶段的数据，都需要与所有 8 条流水线的 EX2、EX3、WB（如果可以前递）的 `rd` 进行比较（**特别注意**是带着第 6 位——判断是否为浮点寄存器的寄存器编号）。

**前递优先级规则**：
* 当同一流水级有多个数据同时可以前递时，以编号较大的流水线数据为准（因为最新）。
* 当不同流水级有多个数据可以同时前递时，以 EX2 为准（因为最新）。

所有流水线的 EX2、EX3、WB 是否可以前递如下表所示：

| 流水线 | 0        | 1       | 2       | 3        | 4        | 5       | 6       | 7    |
| ------ | -------- | ------- | ------- | -------- | -------- | ------- | ------- | ---- |
| EX2    | No       | ALU     | ALU     | ALU      | ALU      | ALU     | ALU     | ALU  |
| EX3    | No       | ALU     | ALU     | ALU      | ALU      | ALU      | ALU      | ALU   |
| WB     | FPU+FDiv | ALU+FPU | ALU+FPU | ALU+iMDU | ALU+iMDU | ALU+LSU | ALU+LSU | ALU  |

---

## Hazard（冒险控制）

Hazard 模块独立位于 Hazard 目录下，具有非常重要的流水线停顿和冲刷调控功能。

`HazardIO` 由以下几套接口构成：

* **Flipped(FrontendHazardIO)**：将前端和 Hazard 的接口反转方向。
* **Flipped(BackendHazardIO)**：将后端和 Hazard 的接口反转方向。

### 控制相关处理

控制相关是由于分支指令跳转带来的流水线冲刷。当 Branch 模块检测到跳转时，会通过后端和 Hazard 之间的接口将跳转使能传递给 Hazard。Hazard 需要执行以下操作：

* 给前端 `flush` 信号
* 给 ID-EX1 段间寄存器 `flush` 信号
* 给 EX1-EX2 段间寄存器 `flush` 信号

### 数据相关处理

数据相关是由于某些指令不能在 EX1 阶段产生结果，或者需要停顿多个周期来处理（如除法）。这些情况都在后端出现。

* **对于 load（包括 flw）、乘除法、浮点指令**：它们的结果需要在 WB 阶段才能访问。当 8 条后端流水线的 EX1 或 EX2 级中有这些指令，且处于 ID 级的那一组 8 条指令中有任意一条与这些指令存在数据相关，则需要：
  * 对前端发起停顿
  * 冲刷 ID-EX1 寄存器（**注意**：如果此时除法器给出了 `stall`，则优先执行 `stall`）

* **对于除法器给出的 stall 信号**，Hazard 需要：
  * 对前端发起停顿
  * 停顿所有的 ID-EX1、EX1-EX2、EX2-EX3 寄存器
  * 冲刷所有的 EX3-WB 寄存器

---
