import chisel3._
import chisel3.util._

// Backend内存接口（两个LSU的内存接口）
class BackendMemIO extends Bundle {
    val lsu0 = new LSUMemIO  // 流水线5的LSU
    val lsu1 = new LSUMemIO  // 流水线6的LSU
}

// Backend与Frontend的接口
class BackendFrontendIO extends Bundle {
    // 从Frontend接收8个InstPkg
    val instPkgs = Input(Vec(8, new InstructionPackage))

    // 写回到Frontend寄存器堆（8条流水线的写回请求）
    val gprWen = Output(Vec(8, Bool()))      // 8个GPR写口（流水线0现在也支持ALU）
    val gprWaddr = Output(Vec(8, UInt(5.W)))
    val gprWdata = Output(Vec(8, UInt(32.W)))
    val fprWen = Output(Vec(5, Bool()))      // 流水线0-2 FPU和流水线5-6 FLW
    val fprWaddr = Output(Vec(5, UInt(5.W)))
    val fprWdata = Output(Vec(5, UInt(32.W)))
    val csrValid = Output(Bool())
    val csrAddress = Output(UInt(12.W))
    val csrCommand = Output(UInt(2.W))
    val csrSource = Output(UInt(32.W))
    val csrReadData = Input(UInt(32.W))
    
    // 分支预测失败信号和跳转地址
    val predFail = Output(Bool())
    val branchTgt = Output(UInt(32.W))
    val branchUpdateValid = Output(Bool())
    val branchUpdatePC = Output(UInt(32.W))
    val branchUpdateInst = Output(UInt(32.W))
    val branchUpdateTaken = Output(Bool())
    val branchUpdateTarget = Output(UInt(32.W))
}

// Backend与Hazard的接口
class BackendHazardIO extends Bundle {
    // 接收Hazard的控制信号（每个阶段）
    val ex1Flush = Input(Vec(8, Bool()))
    val ex1Stall = Input(Vec(8, Bool()))
    val ex2Flush = Input(Vec(8, Bool()))
    val ex2Stall = Input(Vec(8, Bool()))
    val ex3Flush = Input(Vec(8, Bool()))
    val ex3Stall = Input(Vec(8, Bool()))
    val wbFlush = Input(Vec(8, Bool()))
    val wbStall = Input(Vec(8, Bool()))
    
    // 输出各Pipeline的EX1和EX2阶段InstPkg给Hazard做RAW判断
    val ex1Pkgs = Output(Vec(8, new InstructionPackage))
    val ex2Pkgs = Output(Vec(8, new InstructionPackage))
    
    // 任意流水线需要保持时都会触发全局停顿
    val pipelineBusy = Output(Vec(8, Bool()))

    // 可变延迟单元在EX1接受请求，用于阻止更年轻的包进入EX1
    val pipelineStart = Output(Vec(8, Bool()))

    // 尚未提交的frm/fcsr写操作，用于阻止动态舍入指令读取旧frm。
    val frmWritePending = Output(Bool())

    // 当前访存接口为零等待；为后续带握手的内存系统预留
    val memBusy = Output(Bool())
    
    // 流水线7的分支预测失败信号
    val predFail = Output(Bool())
    val branchTgt = Output(UInt(32.W))
}

// Backend调试接口
class BackendDebugIO extends Bundle {
    // 8条流水线的WB阶段提交信息
    val wbValid = Output(Vec(8, Bool()))       // 是否有效提交
    val wbPC = Output(Vec(8, UInt(32.W)))      // 提交的PC
    val wbInst = Output(Vec(8, UInt(32.W)))    // 提交的指令
    val wbRd = Output(Vec(8, UInt(6.W)))       // 写回的寄存器（6位，最高位区分GPR/FPR）
    val wbData = Output(Vec(8, UInt(32.W)))    // 写回的数据
}

class BackendIO extends Bundle {
    val frontend = new BackendFrontendIO
    val hazard = new BackendHazardIO
    val mem = new BackendMemIO
    val debug = new BackendDebugIO
}

class Backend extends Module {
    val io = IO(new BackendIO)
    
    // ========== ID-EX1段间寄存器（集中管理）==========
    val idEx1Pkgs = RegInit(VecInit(Seq.fill(8)(0.U.asTypeOf(new InstructionPackage))))
    for (i <- 0 until 8) {
        when(io.hazard.ex1Flush(i)) {
            idEx1Pkgs(i) := 0.U.asTypeOf(new InstructionPackage)
        }.elsewhen(!io.hazard.ex1Stall(i)) {
            idEx1Pkgs(i) := io.frontend.instPkgs(i)
        }
    }
    
    // ========== 实例化8条Pipeline ==========
    val pipeline0 = Module(new FDivFPUPipeline)          // FDiv + FPU
    val pipeline1 = Module(new ALUFPUPipeline(1))        // ALU + FPU (FPToInt)
    val pipeline2 = Module(new ALUFPUPipeline(0))        // ALU + FPU (IntToFP)
    val pipeline3 = Module(new ALUiMDPipeline)     // ALU + iMulDiv
    val pipeline4 = Module(new ALUiMDPipeline)     // ALU + iMulDiv
    val pipeline5 = Module(new ALULSUPipeline)     // ALU + LSU
    val pipeline6 = Module(new ALULSUPipeline)     // ALU + LSU
    val pipeline7 = Module(new ALUBranchPipeline)  // ALU + Branch
    
    // ========== 实例化Forward模块 ==========
    val forward = Module(new Forward)
    
    // ========== 连接Forward输入（使用辅助函数折叠）==========
    def connectPipeToForward(idx: Int, pipe: Module): Unit = {
        forward.io.ex1Pkgs(idx) := pipe.asInstanceOf[{ def io: { def forward: { def ex1Pkg: InstructionPackage }}}].io.forward.ex1Pkg
        forward.io.ex2Pkgs(idx) := pipe.asInstanceOf[{ def io: { def forward: { def ex2Pkg: InstructionPackage }}}].io.forward.ex2Pkg
        forward.io.ex3Pkgs(idx) := pipe.asInstanceOf[{ def io: { def forward: { def ex3Pkg: InstructionPackage }}}].io.forward.ex3Pkg
        forward.io.wbPkgs(idx) := pipe.asInstanceOf[{ def io: { def forward: { def wbPkg: InstructionPackage }}}].io.forward.wbPkg
    }
    
    def connectForwardToPipe(idx: Int, pipe: Module): Unit = {
        pipe.asInstanceOf[{ def io: { def forward: { def fwdRs1Data: UInt; def fwdRs2Data: UInt; def fwdRs3Data: UInt }}}].io.forward.fwdRs1Data := forward.io.fwdRs1Data(idx)
        pipe.asInstanceOf[{ def io: { def forward: { def fwdRs1Data: UInt; def fwdRs2Data: UInt; def fwdRs3Data: UInt }}}].io.forward.fwdRs2Data := forward.io.fwdRs2Data(idx)
        pipe.asInstanceOf[{ def io: { def forward: { def fwdRs1Data: UInt; def fwdRs2Data: UInt; def fwdRs3Data: UInt }}}].io.forward.fwdRs3Data := forward.io.fwdRs3Data(idx)
    }
    
    def connectBackendToPipe(idx: Int, pipe: Module): Unit = {
        pipe.asInstanceOf[{ def io: { def backend: { def instPkgIn: InstructionPackage }}}].io.backend.instPkgIn := idEx1Pkgs(idx)
    }
    
    def connectHazardToPipe(idx: Int, pipe: Module): Unit = {
        val p = pipe.asInstanceOf[{ 
            def io: { 
                def hazard: { 
                    def ex1Flush: Bool; def ex1Stall: Bool
                    def ex2Flush: Bool; def ex2Stall: Bool
                    def ex3Flush: Bool; def ex3Stall: Bool
                    def wbFlush: Bool; def wbStall: Bool
                    def ex1Pkg: InstructionPackage
                    def ex2Pkg: InstructionPackage
                }
            }
        }]
        p.io.hazard.ex1Flush := io.hazard.ex1Flush(idx)
        p.io.hazard.ex1Stall := io.hazard.ex1Stall(idx)
        p.io.hazard.ex2Flush := io.hazard.ex2Flush(idx)
        p.io.hazard.ex2Stall := io.hazard.ex2Stall(idx)
        p.io.hazard.ex3Flush := io.hazard.ex3Flush(idx)
        p.io.hazard.ex3Stall := io.hazard.ex3Stall(idx)
        p.io.hazard.wbFlush := io.hazard.wbFlush(idx)
        p.io.hazard.wbStall := io.hazard.wbStall(idx)
        
        io.hazard.ex1Pkgs(idx) := p.io.hazard.ex1Pkg
        io.hazard.ex2Pkgs(idx) := p.io.hazard.ex2Pkg
    }
    
    // 应用连接（循环处理所有Pipeline）
    val pipelines = Seq(pipeline0, pipeline1, pipeline2, pipeline3, pipeline4, pipeline5, pipeline6, pipeline7)
    for (i <- 0 until 8) {
        connectPipeToForward(i, pipelines(i))
        connectForwardToPipe(i, pipelines(i))
        connectBackendToPipe(i, pipelines(i))
        connectHazardToPipe(i, pipelines(i))
    }
    
    // ========== 连接LSU的内存接口 ==========
    io.mem.lsu0 <> pipeline5.io.mem
    io.mem.lsu1 <> pipeline6.io.mem
    
    // ========== 汇总各流水线的可变延迟停顿请求 ==========
    io.hazard.pipelineBusy := VecInit(Seq(
        pipeline0.io.hazard.fdivBusy,
        false.B,
        false.B,
        pipeline3.io.hazard.divBusy,
        pipeline4.io.hazard.divBusy,
        false.B,
        false.B,
        false.B
    ))

    io.hazard.pipelineStart := VecInit(Seq(
        pipeline0.io.hazard.fdivStart,
        false.B,
        false.B,
        pipeline3.io.hazard.divStart,
        pipeline4.io.hazard.divStart,
        false.B,
        false.B,
        false.B
    ))

    // 现有组合访存接口在当前周期返回，不产生停顿。
    io.hazard.memBusy := false.B
    
    // ========== 连接分支预测失败信号 ==========
    io.hazard.predFail := pipeline7.io.hazard.predFail
    io.hazard.branchTgt := pipeline7.io.hazard.branchTgt
    io.frontend.predFail := pipeline7.io.hazard.predFail
    io.frontend.branchTgt := pipeline7.io.hazard.branchTgt
    pipeline7.io.csr.readData := io.frontend.csrReadData
    io.frontend.csrValid := pipeline7.io.csr.valid
    io.frontend.csrAddress := pipeline7.io.csr.address
    io.frontend.csrCommand := pipeline7.io.csr.command
    io.frontend.csrSource := pipeline7.io.csr.source

    def writesFrm(pkg: InstructionPackage): Bool = {
        val isCSR = pkg.op === ZirconConfig.EXEOp.CSRRW ||
                    pkg.op === ZirconConfig.EXEOp.CSRRS ||
                    pkg.op === ZirconConfig.EXEOp.CSRRC
        val targetsFrm = pkg.inst(31, 20) === ZirconConfig.FloatingCSRAddress.FRM ||
                         pkg.inst(31, 20) === ZirconConfig.FloatingCSRAddress.FCSR
        val writes = pkg.op === ZirconConfig.EXEOp.CSRRW || pkg.inst(19, 15) =/= 0.U
        isCSR && pkg.inst =/= 0.U && targetsFrm && writes
    }

    io.hazard.frmWritePending := Seq(
        pipeline7.io.forward.ex1Pkg,
        pipeline7.io.forward.ex2Pkg,
        pipeline7.io.forward.ex3Pkg,
        pipeline7.io.forward.wbPkg
    ).map(writesFrm).reduce(_ || _)

    io.frontend.branchUpdateValid := pipeline7.io.hazard.branchUpdateValid
    io.frontend.branchUpdatePC := pipeline7.io.hazard.branchUpdatePC
    io.frontend.branchUpdateInst := pipeline7.io.hazard.branchUpdateInst
    io.frontend.branchUpdateTaken := pipeline7.io.hazard.branchUpdateTaken
    io.frontend.branchUpdateTarget := pipeline7.io.hazard.branchUpdateTarget
    
    // ========== 汇总写回信号到Frontend（使用循环）==========
    // GPR写口分配：流水线0-7各1个，共8个
    for (i <- 0 until 8) {
        val p = pipelines(i).asInstanceOf[{ 
            def io: { 
                def frontend: { 
                    def gprWen: Bool
                    def gprWaddr: UInt
                    def gprWdata: UInt
                }
            }
        }]
        io.frontend.gprWen(i) := p.io.frontend.gprWen
        io.frontend.gprWaddr(i) := p.io.frontend.gprWaddr
        io.frontend.gprWdata(i) := p.io.frontend.gprWdata
    }
    
    // FPR写口分配：流水线0-2执行FPU，流水线5-6执行FLW
    val fprPipelines = Seq(pipeline0, pipeline1, pipeline2, pipeline5, pipeline6)
    for (i <- 0 until 5) {
        val p = fprPipelines(i).asInstanceOf[{
            def io: {
                def frontend: {
                    def fprWen: Bool
                    def fprWaddr: UInt
                    def fprWdata: UInt
                }
            }
        }]
        io.frontend.fprWen(i) := p.io.frontend.fprWen
        io.frontend.fprWaddr(i) := p.io.frontend.fprWaddr
        io.frontend.fprWdata(i) := p.io.frontend.fprWdata
    }
    
    // ========== 调试接口：暴露WB阶段提交信息 ==========
    // 从各流水线的forward接口获取wbPkg
    for (i <- 0 until 8) {
        val p = pipelines(i).asInstanceOf[{ 
            def io: { 
                def forward: { 
                    def wbPkg: InstructionPackage
                }
            }
        }]
        val wbPkg = p.io.forward.wbPkg
        // 使用 inst != 0 判断是否是有效指令
        io.debug.wbValid(i) := wbPkg.inst =/= 0.U
        io.debug.wbPC(i) := wbPkg.pc
        io.debug.wbInst(i) := wbPkg.inst
        // 如果 rdValid=false（如 store/branch），设置 rd=0 避免 difftest 检查寄存器
        io.debug.wbRd(i) := Mux(wbPkg.rdValid, wbPkg.rd, 0.U)
        io.debug.wbData(i) := wbPkg.rfWdata
    }
}
