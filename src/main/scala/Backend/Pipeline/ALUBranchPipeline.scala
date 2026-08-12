import chisel3._
import chisel3.util._

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
    // 分支预测失败信号和跳转地址
    val predFail   = Output(Bool())
    val branchTgt  = Output(UInt(32.W))
    val branchUpdateValid = Output(Bool())
    val branchUpdatePC = Output(UInt(32.W))
    val branchUpdateInst = Output(UInt(32.W))
    val branchUpdateTaken = Output(Bool())
    val branchUpdateTarget = Output(UInt(32.W))
}

class ALUBranchPipelineCSRIO extends Bundle {
    val valid = Output(Bool())
    val address = Output(UInt(12.W))
    val command = Output(UInt(2.W))
    val source = Output(UInt(32.W))
    val readData = Input(UInt(32.W))
}

class ALUBranchPipelineIO extends Bundle {
    val forward = new PipelineForwardIO
    val backend = new PipelineBackendIO
    val frontend = new PipelineFrontendIO
    val hazard = new ALUBranchPipelineHazardIO
    val csr = new ALUBranchPipelineCSRIO
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
    branch.io.predTaken := ex1Pkg.predTaken
    branch.io.predTarget := ex1Pkg.predTarget
    
    // EX1阶段更新InstPkg
    val ex1PkgOut = WireDefault(ex1Pkg.EX1Update(
        alu.io.res,
        branch.io.branchTgt,
        branch.io.predFail,
        branch.io.actualTaken,
        branch.io.actualTarget
    ))
    // CSR寄存器形式必须保留前递后的源操作数直到WB提交。
    ex1PkgOut.rs1Data := ex1Rs1Data
    
    // ========== EX2阶段 ==========
    // EX1-EX2段间寄存器
    val ex2Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex2Flush) {
        ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex2Stall) {
        ex2Pkg := ex1PkgOut
    }
    
    // Branch结果在EX2阶段送出（为了时序打一拍）
    io.hazard.predFail := ex2Pkg.predFail
    io.hazard.branchTgt := ex2Pkg.branchTgt
    val ex2IsControl = ex2Pkg.op === ZirconConfig.EXEOp.BEQ ||
                       ex2Pkg.op === ZirconConfig.EXEOp.BNE ||
                       ex2Pkg.op === ZirconConfig.EXEOp.BLT ||
                       ex2Pkg.op === ZirconConfig.EXEOp.BGE ||
                       ex2Pkg.op === ZirconConfig.EXEOp.BLTU ||
                       ex2Pkg.op === ZirconConfig.EXEOp.BGEU ||
                       ex2Pkg.op === ZirconConfig.EXEOp.JAL ||
                       ex2Pkg.op === ZirconConfig.EXEOp.JALR
    io.hazard.branchUpdateValid := ex2IsControl && !io.hazard.ex3Stall
    io.hazard.branchUpdatePC := ex2Pkg.pc
    io.hazard.branchUpdateInst := ex2Pkg.inst
    io.hazard.branchUpdateTaken := ex2Pkg.branchTaken
    io.hazard.branchUpdateTarget := ex2Pkg.actualBranchTarget
    
    // ========== EX3阶段 ==========
    // EX2-EX3段间寄存器
    val ex3Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex3Flush) {
        ex3Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex3Stall) {
        ex3Pkg := ex2Pkg
    }
    
    // ========== WB阶段 ==========
    // EX3-WB段间寄存器
    val wbPkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.wbFlush) {
        wbPkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.wbStall) {
        wbPkg := ex3Pkg
    }
    
    val wbIsCSR = wbPkg.op === ZirconConfig.EXEOp.CSRRW ||
                  wbPkg.op === ZirconConfig.EXEOp.CSRRS ||
                  wbPkg.op === ZirconConfig.EXEOp.CSRRC
    val csrSource = Mux(wbPkg.inst(14), Cat(0.U(27.W), wbPkg.inst(19, 15)), wbPkg.rs1Data)

    io.csr.valid := wbIsCSR && wbPkg.inst =/= 0.U && !io.hazard.wbFlush
    io.csr.address := wbPkg.inst(31, 20)
    io.csr.command := MuxLookup(wbPkg.op, ZirconConfig.CSRCommand.WRITE)(Seq(
        ZirconConfig.EXEOp.CSRRW -> ZirconConfig.CSRCommand.WRITE,
        ZirconConfig.EXEOp.CSRRS -> ZirconConfig.CSRCommand.SET,
        ZirconConfig.EXEOp.CSRRC -> ZirconConfig.CSRCommand.CLEAR
    ))
    io.csr.source := csrSource

    // CSR指令向rd返回修改前的值。
    val wbData = Mux(wbIsCSR, io.csr.readData, wbPkg.aluResult)
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
