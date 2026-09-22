import chisel3._
import chisel3.util._

class ALUiMDPipelineHazardIO extends PipelineHazardIO {
    // 除法器busy信号，传递给Hazard做阻塞判断
    val divBusy = Output(Bool())
}

class ALUiMDPipelineIO extends Bundle {
    val forward = new PipelineForwardIO
    val backend = new PipelineBackendIO
    val frontend = new PipelineFrontendIO
    val hazard = new ALUiMDPipelineHazardIO
}

class ALUiMDPipeline extends Module {
    val io = IO(new ALUiMDPipelineIO)
    
    // ========== EX1阶段 ==========
    // ID-EX1 段间寄存器在 Backend 中统一管理，这里直接使用传入的数据
    val ex1Pkg = io.backend.instPkgIn
    
    // 应用Forward前递
    val ex1Rs1Data = io.forward.fwdRs1Data
    val ex1Rs2Data = io.forward.fwdRs2Data
    
    // ALU实例化
    val alu = Module(new ALU)
    val aluSrc1 = Mux(ex1Pkg.src1Sel === 0.U, ex1Rs1Data, ex1Pkg.pc)
    val aluSrc2 = Mux(ex1Pkg.src2Sel === 0.U, ex1Rs2Data, ex1Pkg.imm)
    alu.io.src1 := aluSrc1
    alu.io.src2 := aluSrc2
    alu.io.op := ex1Pkg.op
    
    // SRT2除法器实例化
    val srt2 = Module(new SRT2)
    srt2.io.src1 := ex1Rs1Data
    srt2.io.src2 := ex1Rs2Data
    val replayHold = io.hazard.ex1Stall && io.hazard.ex2Stall &&
        !io.hazard.ex3Stall
    srt2.io.op := Mux(replayHold, 0.U, ex1Pkg.op(4, 0))
    
    // Multiply实例化（内部有3级流水，结果在EX3阶段有效）
    val multiply = Module(new MulBooth2Wallce)
    multiply.io.src1 := ex1Rs1Data
    multiply.io.src2 := ex1Rs2Data
    multiply.io.op := ex1Pkg.op(4, 0)
    // Freeze the multiplier only when EX3 is also held.  A nested replay uses
    // ex2Stall=1/ex3Stall=0 to drain the current EX2 packet; the multiplier
    // must advance with that packet on the drain cycle.
    multiply.io.divBusy := io.hazard.ex3Stall
    
    // EX1阶段：只保存ALU结果，乘除法器在内部流水
    val ex1PkgOut = WireDefault(ex1Pkg.EX1Update(alu.io.res, 0.U, false.B))
    ex1PkgOut.rs1Data := ex1Rs1Data
    ex1PkgOut.rs2Data := ex1Rs2Data
    ex1PkgOut.shallowRs1Sel := io.forward.shallowRs1Sel
    ex1PkgOut.shallowRs2Sel := io.forward.shallowRs2Sel
    ex1PkgOut.lateRs1Sel := io.forward.lateRs1Sel
    ex1PkgOut.lateRs2Sel := io.forward.lateRs2Sel
    ex1PkgOut.needsEX2Replay := io.forward.needsEX2Replay
    
    // 输出divBusy信号给Hazard
    io.hazard.divBusy := srt2.io.busy
    
    // ========== EX2阶段 ==========
    val ex2Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex2Flush) {
        ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(io.hazard.ex2Stall && !io.hazard.ex3Stall) {
        ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex2Stall) {
        ex2Pkg := ex1PkgOut
    }

    val aluEX2 = Module(new ALU)
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
    aluEX2.io.op := ex2Pkg.op

    val ex2PkgOut = WireDefault(ex2Pkg)
    ex2PkgOut.rs1Data := io.forward.replayRs1Data
    ex2PkgOut.rs2Data := io.forward.replayRs2Data
    when(ex2Pkg.needsEX2Replay) {
        ex2PkgOut.aluResult := aluEX2.io.res
    }
    
    // ========== EX3阶段 ==========
    // 乘法器内部有2级流水，结果在EX3阶段有效
    // 在EX3阶段捕获乘法器/除法器结果
    val ex3MulRes = multiply.io.res
    val ex3DivRes = srt2.io.res
    
    val ex3Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex3Flush) {
        ex3Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex3Stall) {
        ex3Pkg := ex2PkgOut
    }
    
    // 缓存EX3阶段的乘除法结果，与ex3Pkg同步传递到WB
    val ex3MulResReg = RegInit(0.U(32.W))
    val ex3DivResReg = RegInit(0.U(32.W))
    when(!io.hazard.ex3Stall) {
        ex3MulResReg := ex3MulRes
        ex3DivResReg := ex3DivRes
    }
    
    // ========== WB阶段 ==========
    val wbPkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.wbFlush) {
        wbPkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.wbStall) {
        wbPkg := ex3Pkg
    }
    
    // WB阶段：根据op选择ALU结果还是乘除法器结果
    val wbIsMulDiv = wbPkg.op(4)  // op[4]=1表示是mul/div指令
    val wbIsDiv = wbPkg.op(2)     // op[2]=1表示是div指令
    val wbMulDivRes = Mux(wbIsDiv, ex3DivResReg, ex3MulResReg)
    val wbData = Mux(wbIsMulDiv, wbMulDivRes, wbPkg.aluResult)
    val wbPkgOut = wbPkg.WBUpdate(wbData)
    
    // 写回到寄存器堆
    io.frontend.gprWen := wbPkgOut.rdValid && !wbPkgOut.rd(5)
    io.frontend.gprWaddr := wbPkgOut.rd(4, 0)
    io.frontend.gprWdata := wbPkgOut.rfWdata
    io.frontend.fprWen := wbPkgOut.rdValid && wbPkgOut.rd(5)
    io.frontend.fprWaddr := wbPkgOut.rd(4, 0)
    io.frontend.fprWdata := wbPkgOut.rfWdata
    
    // 输出到Forward和Hazard
    io.forward.ex1Pkg := ex1Pkg
    io.forward.ex2Pkg := ex2Pkg
    io.forward.ex3Pkg := ex3Pkg
    io.forward.wbPkg := wbPkgOut
    io.hazard.ex1Pkg := ex1Pkg
    io.hazard.ex2Pkg := ex2Pkg
}
