import chisel3._
import chisel3.util._

// 前端内存接口（用于取指）
class FrontendMemIO extends Bundle {
    val pc = Output(UInt(32.W))  // 发送PC到仿真环境
    val insts = Input(Vec(8, UInt(32.W)))  // 接收8条连续的指令
}

// 前端与后端接口
class FrontendBackendIO extends Bundle {
    val instPkg = Output(Vec(8, new InstructionPackage))  // 8个指令包输出
    val branchTgt = Input(UInt(32.W))  // 分支重定向目标地址
    val predFail = Input(Bool())  // 分支预测失败信号
    // 写回接口
    val gprWen = Input(Vec(8, Bool()))      // 8个GPR写口
    val gprWaddr = Input(Vec(8, UInt(5.W)))
    val gprWdata = Input(Vec(8, UInt(32.W)))
    val fprWen = Input(Vec(5, Bool()))
    val fprWaddr = Input(Vec(5, UInt(5.W)))
    val fprWdata = Input(Vec(5, UInt(32.W)))
}

// 前端与Hazard接口
class FrontendHazardIO extends Bundle {
    val flush = Input(Bool())  // 冲刷信号
    val stall = Input(Bool())  // 阻塞信号
    val idPkgs = Output(Vec(8, new InstructionPackage))  // ID阶段的指令包（用于RAW检测）
}

// 前端调试接口
class FrontendDebugIO extends Bundle {
    val gpr = Output(Vec(32, UInt(32.W)))  // GPR寄存器堆
    val fpr = Output(Vec(32, UInt(32.W)))  // FPR寄存器堆
}

class FrontendIO extends Bundle {
    val mem = new FrontendMemIO
    val backend = new FrontendBackendIO
    val hazard = new FrontendHazardIO
    val debug = new FrontendDebugIO
}

class Frontend extends Module {
    val io = IO(new FrontendIO)
    
    // ========== IF Stage ==========
    // PC寄存器（复位值0x80000000）
    val pc = RegInit(0x80000000L.U(32.W))
    
    // NPC模块
    val npc = Module(new NPC)
    npc.io.fetch.pc := pc
    npc.io.backend.branchTgt := io.backend.branchTgt
    npc.io.backend.predFail := io.backend.predFail
    npc.io.backend.stall := io.hazard.stall
    
    // 更新PC
    when(!io.hazard.stall) {
        pc := npc.io.fetch.npc
    }
    
    // 向仿真环境发送PC取指
    io.mem.pc := pc
    
    // IF段的8个InstructionPackage
    val ifInstPkgs = Wire(Vec(8, new InstructionPackage))
    for (i <- 0 until 8) {
        ifInstPkgs(i) := WireDefault(0.U.asTypeOf(new InstructionPackage)).IFUpdate(pc + (i * 4).U, io.mem.insts(i))
    }
    
    // ========== IF-ID 段间寄存器 ==========
    val idInstPkgs = RegInit(VecInit(Seq.fill(8)(0.U.asTypeOf(new InstructionPackage))))
    when(io.hazard.flush) {
        for (i <- 0 until 8) {
            idInstPkgs(i) := 0.U.asTypeOf(new InstructionPackage)
        }
    }.elsewhen(!io.hazard.stall) {
        idInstPkgs := ifInstPkgs
    }
    
    // ========== ID Stage ==========
    // 8个Decoder，根据流水线功能配置
    // 注意：所有流水线都支持ALU以提高灵活性
    val decoders = Seq(
        Module(new Decoder(ALU = true,  FPU = true,  Branch = false, Mem = false, IMulDiv = false, FDiv = true)),  // 0: ALU + FDiv + FPU
        Module(new Decoder(ALU = true,  FPU = true,  Branch = false, Mem = false, IMulDiv = false, FDiv = false)), // 1: ALU + FPU (FPToInt)
        Module(new Decoder(ALU = true,  FPU = true,  Branch = false, Mem = false, IMulDiv = false, FDiv = false)), // 2: ALU + FPU (IntToFP)
        Module(new Decoder(ALU = true,  FPU = false, Branch = false, Mem = false, IMulDiv = true,  FDiv = false)), // 3: ALU + iMulDiv
        Module(new Decoder(ALU = true,  FPU = false, Branch = false, Mem = false, IMulDiv = true,  FDiv = false)), // 4: ALU + iMulDiv
        Module(new Decoder(ALU = true,  FPU = false, Branch = false, Mem = true,  IMulDiv = false, FDiv = false)), // 5: ALU + LSU
        Module(new Decoder(ALU = true,  FPU = false, Branch = false, Mem = true,  IMulDiv = false, FDiv = false)), // 6: ALU + LSU
        Module(new Decoder(ALU = true,  FPU = false, Branch = true,  Mem = false, IMulDiv = false, FDiv = false))  // 7: ALU + Branch
    )
    
    // 连接Decoder输入
    for (i <- 0 until 8) {
        decoders(i).io.instPkgIn := idInstPkgs(i)
    }
    
    // 寄存器堆：所有执行槽均可读取 GPR；FPU 槽和两个 LSU 槽可读取 FPR。
    // LSU 的额外 FPR 读口用于 FSW，额外写口用于 FLW。
    val grf = Module(new Regfile(nr = 16, nw = 8))
    val frf = Module(new Regfile(nr = 11, nw = 5))
    
    // ========== 寄存器堆读端口连接 ==========
    // 流水线0-7: 各2个GPR (rs1, rs2)。0号槽需要这些端口以支持
    // FCVT.S.W[U]/FMV.W.X 被打包到任意 FPU 槽的情况。
    for (i <- 0 until 8) {
        grf.io.raddr(i * 2)     := idInstPkgs(i).inst(19, 15)
        grf.io.raddr(i * 2 + 1) := idInstPkgs(i).inst(24, 20)
    }
    
    // 流水线0-2: 各3个FPR (rs1, rs2, rs3)
    for (i <- 0 until 3) {
        frf.io.raddr(i * 3)     := idInstPkgs(i).inst(19, 15)
        frf.io.raddr(i * 3 + 1) := idInstPkgs(i).inst(24, 20)
        frf.io.raddr(i * 3 + 2) := idInstPkgs(i).inst(31, 27)
    }

    // 流水线5-6: FSW 的 rs2 浮点源。
    frf.io.raddr(9) := idInstPkgs(5).inst(24, 20)
    frf.io.raddr(10) := idInstPkgs(6).inst(24, 20)
    
    // ========== 寄存器堆写端口连接（来自后端）==========
    for (i <- 0 until 8) {
        grf.io.wen(i) := io.backend.gprWen(i)
        grf.io.waddr(i) := io.backend.gprWaddr(i)
        grf.io.wdata(i) := io.backend.gprWdata(i)
    }
    
    for (i <- 0 until 5) {
        frf.io.wen(i) := io.backend.fprWen(i)
        frf.io.waddr(i) := io.backend.fprWaddr(i)
        frf.io.wdata(i) := io.backend.fprWdata(i)
    }
    
    // ========== 组装寄存器数据到InstPkg ==========
    val decodedInstPkgs = Wire(Vec(8, new InstructionPackage))
    
    // 流水线0-2: ALU+FPU，根据每个源寄存器各自的类型位选择 GPR/FPR。
    for (i <- 0 until 3) {
        val gprBase = i * 2
        val fprBase = i * 3
        val rs1Data = Mux(decoders(i).io.instPkgOut.rs1(5), frf.io.rdata(fprBase),     grf.io.rdata(gprBase))
        val rs2Data = Mux(decoders(i).io.instPkgOut.rs2(5), frf.io.rdata(fprBase + 1), grf.io.rdata(gprBase + 1))
        val rs3Data = Mux(decoders(i).io.instPkgOut.rs3(5), frf.io.rdata(fprBase + 2), 0.U)
        decodedInstPkgs(i) := decoders(i).io.instPkgOut.IDUpdate(rs1Data, rs2Data, rs3Data)
    }
    
    // 流水线3、4、7: 只有GPR。
    for (i <- Seq(3, 4, 7)) {
        val gprBase = i * 2
        decodedInstPkgs(i) := decoders(i).io.instPkgOut.IDUpdate(
            grf.io.rdata(gprBase),
            grf.io.rdata(gprBase + 1),
            0.U
        )
    }

    // 流水线5-6: 地址基址来自 GPR；FSW 的 rs2 来自 FPR，普通 store/ALU
    // 仍从 GPR 读取。
    for (i <- 5 until 7) {
        val gprBase = i * 2
        val fprPort = 9 + (i - 5)
        val rs2Data = Mux(decoders(i).io.instPkgOut.rs2(5), frf.io.rdata(fprPort), grf.io.rdata(gprBase + 1))
        decodedInstPkgs(i) := decoders(i).io.instPkgOut.IDUpdate(
            grf.io.rdata(gprBase),
            rs2Data,
            0.U
        )
    }
    
    // 输出到后端和Hazard
    io.backend.instPkg := decodedInstPkgs
    io.hazard.idPkgs := decodedInstPkgs
    
    // 调试输出：寄存器堆的值
    io.debug.gpr := grf.io.dbgRegs
    io.debug.fpr := frf.io.dbgRegs
}
