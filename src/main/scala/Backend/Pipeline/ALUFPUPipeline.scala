import chisel3._
import chisel3.util._

class ALUFPUPipelineIO extends Bundle {
    val forward = new PipelineForwardIO
    val backend = new PipelineBackendIO
    val frontend = new PipelineFrontendIO
    val hazard = new PipelineHazardIO
}

// ALUFPUPipeline 支持类型转换
// convertType: 1 = FPToInt (1号流水线), 0 = IntToFP (2号流水线)
class ALUFPUPipeline(val convertType: Int = 0) extends Module {
    val io = IO(new ALUFPUPipelineIO)
    
    // ========== EX1阶段 ==========
    // ID-EX1 段间寄存器在 Backend 中统一管理，这里直接使用传入的数据
    val ex1Pkg = io.backend.instPkgIn
    
    // 应用Forward前递
    val ex1Rs1Data = io.forward.fwdRs1Data
    val ex1Rs2Data = io.forward.fwdRs2Data
    val ex1Rs3Data = io.forward.fwdRs3Data
    val ex1GprRs1Data = io.forward.fwdGprRs1Data
    val ex1GprRs2Data = io.forward.fwdGprRs2Data
    val ex1FprRs1Data = io.forward.fwdFprRs1Data
    
    // ALU实例化
    val alu = Module(new ALU)
    val aluSrc1 = Mux(ex1Pkg.src1Sel === 0.U, ex1GprRs1Data, ex1Pkg.pc)
    val aluSrc2 = Mux(ex1Pkg.src2Sel === 0.U, ex1GprRs2Data, ex1Pkg.imm)
    alu.io.src1 := aluSrc1
    alu.io.src2 := aluSrc2
    alu.io.op := ex1Pkg.op
    
    // FPU实例化
    val fpu = Module(new FPU)
    fpu.io.rs1Data := ex1Rs1Data
    fpu.io.rs2Data := ex1Rs2Data
    fpu.io.rs3Data := ex1Rs3Data
    fpu.io.op := ex1Pkg.op
    fpu.io.rm := ex1Pkg.rm
    fpu.io.inValid := ex1Pkg.rdValid
    fpu.io.ex2Advance := !io.hazard.ex2Stall
    fpu.io.ex2Flush := io.hazard.ex2Flush
    fpu.io.ex3Advance := !io.hazard.ex3Stall
    fpu.io.ex3Flush := io.hazard.ex3Flush
    fpu.io.wbAdvance := !io.hazard.wbStall
    fpu.io.wbFlush := io.hazard.wbFlush
    
    // 类型转换模块实例化
    val fpuConvert = Module(new FPUConvert(convertType))
    val conversionOperand = if (convertType == 1) ex1FprRs1Data else ex1GprRs1Data
    
    // EX1阶段更新InstPkg。组合浮点操作在这里保存结果；FADD和FMUL
    // 分别由后续阶段按其内部1级和2级流水延迟保存。
    val ex1PkgOut = ex1Pkg.EX1Update(alu.io.res, 0.U, false.B)
    ex1PkgOut.rs1Data := conversionOperand
    val ex1IsConvert = if (convertType == 1) {
        ex1Pkg.op === ZirconConfig.EXEOp.FCVT_W_S ||
        ex1Pkg.op === ZirconConfig.EXEOp.FCVT_WU_S
    } else {
        ex1Pkg.op === ZirconConfig.EXEOp.FCVT_S_W ||
        ex1Pkg.op === ZirconConfig.EXEOp.FCVT_S_WU
    }
    val ex1IsAdd = ex1Pkg.op === ZirconConfig.EXEOp.FADD_S ||
                   ex1Pkg.op === ZirconConfig.EXEOp.FSUB_S
    val ex1IsMul = ex1Pkg.op === ZirconConfig.EXEOp.FMUL_S
    val ex1IsImmediateFpu = ex1Pkg.op(6) && !ex1IsAdd && !ex1IsMul && !ex1IsConvert
    val ex1ImmediateRes = fpu.io.res
    val ex1ImmediateFlags = fpu.io.fflags
    val ex1PkgOutWithFpu = Mux(
        ex1IsImmediateFpu,
        ex1PkgOut.EX3Update(ex1ImmediateRes, ex1ImmediateFlags),
        ex1PkgOut
    )
    
    // ========== EX2阶段 ==========
    val ex2Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex2Flush) {
        ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex2Stall) {
        ex2Pkg := ex1PkgOutWithFpu
    }

    // Conversion uses EX2's registered operand and is captured at EX3. WB
    // latency is unchanged, while the forwarding network no longer shares a
    // cycle with the full conversion datapath.
    fpuConvert.io.rs1Data := ex2Pkg.rs1Data
    fpuConvert.io.op := ex2Pkg.op
    fpuConvert.io.rm := ex2Pkg.rm
    val ex2IsConvert = if (convertType == 1) {
        ex2Pkg.op === ZirconConfig.EXEOp.FCVT_W_S ||
        ex2Pkg.op === ZirconConfig.EXEOp.FCVT_WU_S
    } else {
        ex2Pkg.op === ZirconConfig.EXEOp.FCVT_S_W ||
        ex2Pkg.op === ZirconConfig.EXEOp.FCVT_S_WU
    }
    val ex2PkgWithConvert = Mux(
        ex2IsConvert,
        ex2Pkg.EX3Update(fpuConvert.io.res, fpuConvert.io.fflags),
        ex2Pkg
    )
    
    // ========== EX3阶段 ==========
    val ex3Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex3Flush) {
        ex3Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex3Stall) {
        ex3Pkg := ex2PkgWithConvert
    }
    
    // ========== WB阶段 ==========
    val wbPkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.wbFlush) {
        wbPkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.wbStall) {
        val ex3IsMul = ex3Pkg.op === ZirconConfig.EXEOp.FMUL_S
        wbPkg := Mux(
            ex3IsMul,
            ex3Pkg.EX3Update(fpu.io.fmulResult, fpu.io.fmulFflags),
            ex3Pkg
        )
    }
    
    // WB阶段：选择写回数据（FPU指令用fpuResult，ALU指令用aluResult）
    val wbIsAdd = wbPkg.op === ZirconConfig.EXEOp.FADD_S ||
                  wbPkg.op === ZirconConfig.EXEOp.FSUB_S
    val wbIsMul = wbPkg.op === ZirconConfig.EXEOp.FMUL_S
    val wbPkgWithArithmetic = MuxCase(wbPkg, Seq(
        wbIsAdd -> wbPkg.EX3Update(fpu.io.faddResult, fpu.io.faddFflags),
        wbIsMul -> wbPkg.EX3Update(fpu.io.fmulResult, fpu.io.fmulFflags)
    ))
    val isFPU = wbPkgWithArithmetic.op(6)  // op[6]表示是否是FPU指令
    val wbData = Mux(isFPU, wbPkgWithArithmetic.fpuResult, wbPkgWithArithmetic.aluResult)
    val wbPkgOut = wbPkgWithArithmetic.WBUpdate(wbData)
    val gprWbData = Mux(isFPU, wbPkg.fpuResult, wbPkg.aluResult)
    
    // 写回到寄存器堆
    io.frontend.gprWen := wbPkgOut.rdValid && !wbPkgOut.rd(5)
    io.frontend.gprWaddr := wbPkgOut.rd(4, 0)
    io.frontend.gprWdata := gprWbData
    io.frontend.fprWen := wbPkgOut.rdValid && wbPkgOut.rd(5)
    io.frontend.fprWaddr := wbPkgOut.rd(4, 0)
    io.frontend.fprWdata := wbPkgOut.rfWdata

    // 输出到Forward和Hazard
    io.forward.ex1Pkg := ex1Pkg
    io.forward.ex2Pkg := ex2Pkg
    io.forward.ex3Pkg := ex3Pkg
    io.forward.ex3GprData := ex3Pkg.aluResult
    io.forward.wbPkg := wbPkgOut
    io.hazard.ex1Pkg := ex1Pkg
    io.hazard.ex2Pkg := ex2Pkg
}
