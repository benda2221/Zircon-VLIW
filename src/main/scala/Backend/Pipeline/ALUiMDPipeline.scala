import chisel3._
import chisel3.util._

class ALUiMDPipelineHazardIO extends PipelineHazardIO {
    // 除法器busy信号，传递给Hazard做阻塞判断
    val divBusy = Output(Bool())
    val divStart = Output(Bool())
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
    srt2.io.op := ex1Pkg.op(4, 0)
    val isDivOp = ex1Pkg.op(4) && ex1Pkg.op(2)
    val divActive = RegInit(false.B)
    val divComplete = divActive && srt2.io.ready
    // ex2Flush squashes the younger package currently in EX1 after a redirect.
    // Do not let a wrong-path DIV/REM request escape into the stateful divider.
    val divRequest = ex1Pkg.rdValid && isDivOp && !io.hazard.ex2Flush &&
        (!divActive || divComplete)
    srt2.io.valid := divRequest
    io.hazard.divStart := divRequest

    when(divRequest) {
        divActive := true.B
    }.elsewhen(divComplete) {
        divActive := false.B
    }
    
    // Multiply实例化（内部有3级流水，结果在EX3阶段有效）
    val multiply = Module(new MulBooth2Wallce)
    multiply.io.src1 := ex1Rs1Data
    multiply.io.src2 := ex1Rs2Data
    multiply.io.op := ex1Pkg.op(4, 0)
    multiply.io.stall := io.hazard.ex2Stall
    
    // EX1阶段：只保存ALU结果，乘除法器在内部流水
    val ex1PkgOut = ex1Pkg.EX1Update(alu.io.res, 0.U, false.B)
    
    // 输出divBusy信号给Hazard
    // Let the request enter EX2, then hold it until the result-ready cycle.
    io.hazard.divBusy := (divActive && !divComplete) || srt2.io.busy
    
    // ========== EX2阶段 ==========
    val ex2Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex2Flush) {
        ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex2Stall) {
        ex2Pkg := ex1PkgOut
    }
    
    // ========== EX3阶段 ==========
    // 乘法器内部有2级流水，结果在EX3阶段有效
    // 在EX3阶段捕获乘法器/除法器结果
    val ex3MulRes = multiply.io.res
    val completedDivRes = RegEnable(srt2.io.res, 0.U(32.W), srt2.io.ready)
    val ex3DivRes = Mux(srt2.io.ready, srt2.io.res, completedDivRes)
    
    val ex3Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex3Flush) {
        ex3Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex3Stall) {
        ex3Pkg := ex2Pkg
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

    // Keep the registered package independent from the combinational multiply
    // result. Consumers that permit this path use ex3GprData explicitly.
    val ex3IsMul = ex3Pkg.op(4) && !ex3Pkg.op(2)
    val ex3GprData = Mux(ex3IsMul, ex3MulRes, ex3Pkg.aluResult)
    
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
    io.forward.ex3GprData := ex3GprData
    io.forward.wbPkg := wbPkgOut
    io.hazard.ex1Pkg := ex1Pkg
    io.hazard.ex2Pkg := ex2Pkg
}
