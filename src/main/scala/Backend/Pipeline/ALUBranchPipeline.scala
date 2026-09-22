import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._

// Pipeline的基础接口
class PipelineForwardIO extends Bundle {
    val ex1Pkg = Output(new InstructionPackage)  // EX1阶段
    val ex2Pkg = Output(new InstructionPackage)  // EX2阶段（用于前递）
    val ex3Pkg = Output(new InstructionPackage)  // EX3阶段（用于前递）
    val wbPkg  = Output(new InstructionPackage)  // WB阶段（用于前递）
    // 接收Forward的前递数据
    val fwdRs1Data = Input(UInt(32.W))
    val fwdRs2Data = Input(UInt(32.W))
    val fwdRs3Data = Input(UInt(32.W))
    val shallowRs1Sel = Input(UInt(8.W))
    val shallowRs2Sel = Input(UInt(8.W))
    val lateRs1Sel = Input(UInt(8.W))
    val lateRs2Sel = Input(UInt(8.W))
    val needsEX2Replay = Input(Bool())
    val replayRs1Data = Input(UInt(32.W))
    val replayRs2Data = Input(UInt(32.W))
}

class PipelineBackendIO extends Bundle {
    val instPkgIn = Input(new InstructionPackage)  // 从Backend接收的InstPkg
}

class PipelineFrontendIO extends Bundle {
    // 写回寄存器堆
    val gprWen   = Output(Bool())
    val gprWaddr = Output(UInt(5.W))
    val gprWdata = Output(UInt(32.W))
    val fprWen   = Output(Bool())
    val fprWaddr = Output(UInt(5.W))
    val fprWdata = Output(UInt(32.W))
}

class PipelineHazardIO extends Bundle {
    // 接收Hazard的控制信号
    val ex1Flush = Input(Bool())
    val ex1Stall = Input(Bool())
    val ex2Flush = Input(Bool())
    val ex2Stall = Input(Bool())
    val ex3Flush = Input(Bool())
    val ex3Stall = Input(Bool())
    val wbFlush  = Input(Bool())
    val wbStall  = Input(Bool())
    
    // 输出EX1和EX2的InstPkg给Hazard做RAW判断
    val ex1Pkg = Output(new InstructionPackage)
    val ex2Pkg = Output(new InstructionPackage)
}

// ALUBranchPipeline特有的IO
class ALUBranchPipelineHazardIO extends PipelineHazardIO {
    // EX2普通分支以及EX3浅依赖分支的重定向信号
    val predFailEX2   = Output(Bool())
    val branchTgtEX2  = Output(UInt(32.W))
    val predFailEX3   = Output(Bool())
    val branchTgtEX3  = Output(UInt(32.W))
}

class ALUBranchPipelineIO extends Bundle {
    val forward = new PipelineForwardIO
    val backend = new PipelineBackendIO
    val frontend = new PipelineFrontendIO
    val hazard = new ALUBranchPipelineHazardIO
}

class ALUBranchPipeline extends Module {
    val io = IO(new ALUBranchPipelineIO)
    
    // ========== EX1阶段 ==========
    // ID-EX1 段间寄存器在 Backend 中统一管理，这里直接使用传入的数据
    val ex1Pkg = io.backend.instPkgIn
    
    // 应用Forward前递
    val ex1Rs1Data = io.forward.fwdRs1Data
    val ex1Rs2Data = io.forward.fwdRs2Data
    
    // ALU实例化
    val alu = Module(new ALU)
    // ALU源操作数选择
    val aluSrc1 = Mux(ex1Pkg.src1Sel === 0.U, ex1Rs1Data, ex1Pkg.pc)
    val aluSrc2 = Mux(ex1Pkg.src2Sel === 0.U, ex1Rs2Data, ex1Pkg.imm)
    alu.io.src1 := aluSrc1
    alu.io.src2 := aluSrc2
    alu.io.op := ex1Pkg.op
    
    // Branch实例化
    val branch = Module(new Branch)
    branch.io.src1 := ex1Rs1Data
    branch.io.src2 := ex1Rs2Data
    branch.io.op := ex1Pkg.op
    branch.io.pc := ex1Pkg.pc
    branch.io.imm := ex1Pkg.imm
    
    // EX1阶段更新InstPkg
    val ex1PkgOut = WireDefault(
        ex1Pkg.EX1Update(alu.io.res, branch.io.branchTgt, branch.io.predFail)
    )
    ex1PkgOut.rs1Data := ex1Rs1Data
    ex1PkgOut.rs2Data := ex1Rs2Data
    ex1PkgOut.shallowRs1Sel := io.forward.shallowRs1Sel
    ex1PkgOut.shallowRs2Sel := io.forward.shallowRs2Sel
    ex1PkgOut.lateRs1Sel := io.forward.lateRs1Sel
    ex1PkgOut.lateRs2Sel := io.forward.lateRs2Sel
    ex1PkgOut.needsEX2Replay := io.forward.needsEX2Replay
    
    // ========== EX2阶段 ==========
    // EX1-EX2段间寄存器
    val ex2Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex2Flush) {
        ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(io.hazard.ex2Stall && !io.hazard.ex3Stall) {
        ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex2Stall) {
        ex2Pkg := ex1PkgOut
    }
    
    // 浅依赖消费者在EX2使用前递数据重新执行ALU。
    val aluEX2 = Module(new ALU)
    val ex2Rs1Data = io.forward.replayRs1Data
    val ex2Rs2Data = io.forward.replayRs2Data
    aluEX2.io.src1 := Mux(ex2Pkg.src1Sel === 0.U, ex2Rs1Data, ex2Pkg.pc)
    aluEX2.io.src2 := Mux(ex2Pkg.src2Sel === 0.U, ex2Rs2Data, ex2Pkg.imm)
    aluEX2.io.op := ex2Pkg.op

    val ex2PkgOut = WireDefault(ex2Pkg)
    ex2PkgOut.rs1Data := ex2Rs1Data
    ex2PkgOut.rs2Data := ex2Rs2Data
    when(ex2Pkg.needsEX2Replay) {
        ex2PkgOut.aluResult := aluEX2.io.res
    }

    // EX1结果无效的分支必须延迟到EX3重定向。
    io.hazard.predFailEX2 := ex2Pkg.predFail && !ex2Pkg.needsEX2Replay
    io.hazard.branchTgtEX2 := ex2Pkg.branchTgt
    
    // ========== EX3阶段 ==========
    // EX2-EX3段间寄存器
    val ex3Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex3Flush) {
        ex3Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex3Stall) {
        ex3Pkg := ex2PkgOut
    }

    // 浅依赖条件分支/JALR在EX3使用修正后的源操作数重新判断。
    val branchEX3 = Module(new Branch)
    branchEX3.io.src1 := ex3Pkg.rs1Data
    branchEX3.io.src2 := ex3Pkg.rs2Data
    branchEX3.io.op := ex3Pkg.op
    branchEX3.io.pc := ex3Pkg.pc
    branchEX3.io.imm := ex3Pkg.imm

    val isConditionalBranch = ex3Pkg.op === BEQ || ex3Pkg.op === BNE ||
        ex3Pkg.op === BLT || ex3Pkg.op === BGE ||
        ex3Pkg.op === BLTU || ex3Pkg.op === BGEU
    val isJALR = ex3Pkg.op === JALR
    val ex3ShallowBranchValid = ex3Pkg.needsEX2Replay &&
        (isConditionalBranch || isJALR)
    io.hazard.predFailEX3 := ex3ShallowBranchValid && branchEX3.io.predFail
    io.hazard.branchTgtEX3 := branchEX3.io.branchTgt
    
    // ========== WB阶段 ==========
    // EX3-WB段间寄存器
    val wbPkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.wbFlush) {
        wbPkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.wbStall) {
        wbPkg := ex3Pkg
    }
    
    // WB阶段：选择写回数据（ALU结果）
    val wbData = wbPkg.aluResult
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
