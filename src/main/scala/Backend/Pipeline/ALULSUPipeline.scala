import chisel3._
import chisel3.util._

class ALULSUPipelineIO extends Bundle {
    val forward = new PipelineForwardIO
    val backend = new PipelineBackendIO
    val frontend = new PipelineFrontendIO
    val hazard = new PipelineHazardIO
    val lsu = new PipelineLSUIO
}

class ALULSUPipeline extends Module {
    val io = IO(new ALULSUPipelineIO)
    
    // ========== EX1阶段 ==========
    // ID-EX1 段间寄存器在 Backend 中统一管理，这里直接使用传入的数据
    val ex1Pkg = io.backend.instPkgIn
    
    // 应用Forward前递
    val ex1Rs1Data = io.forward.fwdRs1Data
    val ex1Rs2Data = io.forward.fwdRs2Data
    
    // ALU实例化（用于计算访存地址）
    val alu = Module(new ALU)
    val aluSrc1 = Mux(ex1Pkg.src1Sel === 0.U, ex1Rs1Data, ex1Pkg.pc)
    val aluSrc2 = Mux(ex1Pkg.src2Sel === 0.U, ex1Rs2Data, ex1Pkg.imm)
    alu.io.src1 := aluSrc1
    alu.io.src2 := aluSrc2
    alu.io.op := ex1Pkg.op
    
    // EX1阶段更新InstPkg（包含ALU结果和前递后的rs2Data供EX2阶段store使用）
    val ex1PkgOut = WireDefault(ex1Pkg.EX1Update(alu.io.res, 0.U, false.B))
    ex1PkgOut.rs1Data := ex1Rs1Data
    ex1PkgOut.rs2Data := ex1Rs2Data  // 保存前递后的rs2数据，供EX2阶段store使用
    ex1PkgOut.shallowRs1Sel := io.forward.shallowRs1Sel
    ex1PkgOut.shallowRs2Sel := io.forward.shallowRs2Sel
    ex1PkgOut.lateRs1Sel := io.forward.lateRs1Sel
    ex1PkgOut.lateRs2Sel := io.forward.lateRs2Sel
    ex1PkgOut.needsEX2Replay := io.forward.needsEX2Replay
    
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

    val ex2PkgWithAlu = WireDefault(ex2Pkg)
    ex2PkgWithAlu.rs1Data := io.forward.replayRs1Data
    ex2PkgWithAlu.rs2Data := io.forward.replayRs2Data
    when(ex2Pkg.needsEX2Replay) {
        ex2PkgWithAlu.aluResult := aluEX2.io.res
    }
    
    // LSU在EX2生成Load请求或Store Buffer入队请求。Store不再直接写内存。
    val lsu = Module(new LSU)
    lsu.io.valid := ex2Pkg.inst =/= 0.U
    lsu.io.op := ex2Pkg.op
    lsu.io.addr := ex2PkgWithAlu.aluResult
    lsu.io.wdata := ex2PkgWithAlu.rs2Data
    lsu.io.pc := ex2Pkg.pc
    lsu.io.inst := ex2Pkg.inst

    io.lsu <> lsu.io.pipeline
    
    // EX2阶段更新memResult
    val ex2PkgOut = ex2PkgWithAlu.EX2Update(lsu.io.res)
    
    // ========== EX3阶段 ==========
    val ex3Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex3Flush) {
        ex3Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex3Stall) {
        ex3Pkg := ex2PkgOut  // 使用包含memResult的ex2PkgOut
    }
    
    // ========== WB阶段 ==========
    val wbPkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.wbFlush) {
        wbPkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.wbStall) {
        wbPkg := ex3Pkg
    }
    
    // WB阶段：选择写回数据（load指令用memResult，其他用aluResult）
    val isLoad = wbPkg.op === ZirconConfig.EXEOp.LB || wbPkg.op === ZirconConfig.EXEOp.LH || 
                 wbPkg.op === ZirconConfig.EXEOp.LW || wbPkg.op === ZirconConfig.EXEOp.LBU || 
                 wbPkg.op === ZirconConfig.EXEOp.LHU || wbPkg.op === ZirconConfig.EXEOp.FLW
    val wbData = Mux(isLoad, wbPkg.memResult, wbPkg.aluResult)
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
