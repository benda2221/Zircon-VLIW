import chisel3._
import chisel3.util._

// Backend与Frontend的接口
class BackendFrontendIO extends Bundle {
    // 从Frontend接收8个InstPkg
    val instPkgs = Input(Vec(8, new InstructionPackage))
    
    // 写回到Frontend寄存器堆（8条流水线的写回请求）
    val gprWen = Output(Vec(8, Bool()))      // 8个GPR写口（流水线0现在也支持ALU）
    val gprWaddr = Output(Vec(8, UInt(5.W)))
    val gprWdata = Output(Vec(8, UInt(32.W)))
    val fprWen = Output(Vec(5, Bool()))      // FPU 0-2 + LSU 5-6
    val fprWaddr = Output(Vec(5, UInt(5.W)))
    val fprWdata = Output(Vec(5, UInt(32.W)))
    
    // 分支预测失败信号和跳转地址
    val predFail = Output(Bool())
    val branchTgt = Output(UInt(32.W))
}

// Backend与Hazard的接口
class BackendHazardIO extends Bundle {
    // 接收Hazard的控制信号（每个阶段）
    val ex1Flush = Input(Vec(8, Bool()))
    val ex1RawFlush = Input(Vec(8, Bool()))    // insert an ID-EX1 bubble
    val ex1BranchFlush = Input(Vec(8, Bool())) // cancel younger branch-path work
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
    val ex1ShallowRs1Sel = Output(Vec(8, UInt(8.W)))
    val ex1ShallowRs2Sel = Output(Vec(8, UInt(8.W)))
    val ex1LateRs1Sel = Output(Vec(8, UInt(8.W)))
    val ex1LateRs2Sel = Output(Vec(8, UInt(8.W)))
    val ex1NeedsEX2Replay = Output(Vec(8, Bool()))
    
    // 流水线3、4的除法器busy信号
    val divBusy = Output(Vec(2, Bool()))
    
    // 流水线0的浮点除法器busy信号
    val fdivBusy = Output(Bool())
    
    // 流水线7的分阶段重定向信号
    val predFailEX2 = Output(Bool())
    val branchTgtEX2 = Output(UInt(32.W))
    val predFailEX3 = Output(Bool())
    val branchTgtEX3 = Output(UInt(32.W))
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
    
    // ========== 实例化8条Pipeline ==========
    val pipeline0 = Module(new FDivFPUPipeline)          // FDiv + FPU
    val pipeline1 = Module(new ALUFPUPipeline(enableShallowEX2 = true))  // ALU + FPU + replay EX2 ALU
    val pipeline2 = Module(new ALUFPUPipeline(enableShallowEX2 = true))  // ALU + FPU + shallow EX2 ALU
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
        val f = pipe.asInstanceOf[{
            def io: {
                def forward: {
                    def shallowRs1Sel: UInt; def shallowRs2Sel: UInt
                    def lateRs1Sel: UInt; def lateRs2Sel: UInt
                    def needsEX2Replay: Bool
                    def replayRs1Data: UInt; def replayRs2Data: UInt
                }
            }
        }].io.forward
        f.shallowRs1Sel := forward.io.shallowRs1Sel(idx)
        f.shallowRs2Sel := forward.io.shallowRs2Sel(idx)
        f.lateRs1Sel := forward.io.lateRs1Sel(idx)
        f.lateRs2Sel := forward.io.lateRs2Sel(idx)
        f.needsEX2Replay := forward.io.needsEX2Replay(idx)
        f.replayRs1Data := forward.io.replayRs1Data(idx)
        f.replayRs2Data := forward.io.replayRs2Data(idx)
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

    // A nested replay holds the EX1 instruction for one cycle. Preserve any
    // ordinary EX2/EX3/WB forwarding observed in that first cycle, because the
    // producing WB entry may be gone when the held instruction is released.
    for (i <- 0 until 8) {
        when(io.hazard.ex1Flush(i)) {
            idEx1Pkgs(i) := 0.U.asTypeOf(new InstructionPackage)
        }.elsewhen(io.hazard.ex1Stall(i)) {
            idEx1Pkgs(i).rs1Data := forward.io.fwdRs1Data(i)
            idEx1Pkgs(i).rs2Data := forward.io.fwdRs2Data(i)
            idEx1Pkgs(i).rs3Data := forward.io.fwdRs3Data(i)
        }.otherwise {
            idEx1Pkgs(i) := io.frontend.instPkgs(i)
        }
    }

    io.hazard.ex1ShallowRs1Sel := forward.io.shallowRs1Sel
    io.hazard.ex1ShallowRs2Sel := forward.io.shallowRs2Sel
    io.hazard.ex1LateRs1Sel := forward.io.lateRs1Sel
    io.hazard.ex1LateRs2Sel := forward.io.lateRs2Sel
    io.hazard.ex1NeedsEX2Replay := forward.io.needsEX2Replay
    pipeline0.io.hazard.branchFlush := io.hazard.ex1BranchFlush(0)

    // ========== 连接LSU、Store Buffer与内存接口 ==========
    val memoryOrder = Module(new LSUMemoryOrder)
    memoryOrder.io.pipeline(0) <> pipeline5.io.lsu
    memoryOrder.io.pipeline(1) <> pipeline6.io.lsu
    memoryOrder.io.ex3Flush(0) := io.hazard.ex3Flush(5)
    memoryOrder.io.ex3Flush(1) := io.hazard.ex3Flush(6)
    memoryOrder.io.ex3Stall(0) := io.hazard.ex3Stall(5)
    memoryOrder.io.ex3Stall(1) := io.hazard.ex3Stall(6)
    io.mem <> memoryOrder.io.mem
    
    // ========== 连接divBusy信号 ==========
    io.hazard.divBusy(0) := pipeline3.io.hazard.divBusy
    io.hazard.divBusy(1) := pipeline4.io.hazard.divBusy
    
    // ========== 连接fdivBusy信号 ==========
    io.hazard.fdivBusy := pipeline0.io.hazard.fdivBusy
    
    // ========== 连接分支预测失败信号 ==========
    io.hazard.predFailEX2 := pipeline7.io.hazard.predFailEX2
    io.hazard.branchTgtEX2 := pipeline7.io.hazard.branchTgtEX2
    io.hazard.predFailEX3 := pipeline7.io.hazard.predFailEX3
    io.hazard.branchTgtEX3 := pipeline7.io.hazard.branchTgtEX3

    io.frontend.predFail := pipeline7.io.hazard.predFailEX3 ||
        pipeline7.io.hazard.predFailEX2
    io.frontend.branchTgt := Mux(
        pipeline7.io.hazard.predFailEX3,
        pipeline7.io.hazard.branchTgtEX3,
        pipeline7.io.hazard.branchTgtEX2
    )
    
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
    
    // FPR写口分配：FPU流水线0-2和LSU流水线5-6各1个。
    val fprWritePipelines = Seq(pipeline0, pipeline1, pipeline2, pipeline5, pipeline6)
    for ((pipe, i) <- fprWritePipelines.zipWithIndex) {
        val p = pipe.asInstanceOf[{
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
