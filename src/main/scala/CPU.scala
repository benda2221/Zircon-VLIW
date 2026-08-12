import chisel3._
import chisel3.util._

// CPU调试接口
class CPUDebugIO extends Bundle {
    // 寄存器堆
    val gpr = Output(Vec(32, UInt(32.W)))  // GPR寄存器堆
    val fpr = Output(Vec(32, UInt(32.W)))  // FPR寄存器堆
    
    // 8条流水线的WB阶段提交信息
    val wbValid = Output(Vec(8, Bool()))       // 是否有效提交
    val wbPC = Output(Vec(8, UInt(32.W)))      // 提交的PC
    val wbInst = Output(Vec(8, UInt(32.W)))    // 提交的指令
    val wbRd = Output(Vec(8, UInt(6.W)))       // 写回的寄存器（6位，最高位区分GPR/FPR）
    val wbData = Output(Vec(8, UInt(32.W)))    // 写回的数据

    // 处理器内部性能计数器，IPC由调试端用计数器比值计算
    val perfCycles = Output(UInt(64.W))
    val perfEffectiveInstructions = Output(UInt(64.W))
    val perfExecutedPackets = Output(UInt(64.W))
    val perfRawStallCycles = Output(UInt(64.W))
    val perfRawLoadCycles = Output(UInt(64.W))
    val perfRawIntegerMulCycles = Output(UInt(64.W))
    val perfRawFloatAddCycles = Output(UInt(64.W))
    val perfRawFloatMulCycles = Output(UInt(64.W))
    val perfRawFloatDivCycles = Output(UInt(64.W))
    val perfFalseRawStallCycles = Output(UInt(64.W))
    val perfRawOtherCycles = Output(UInt(64.W))
    val perfBranchRedirects = Output(UInt(64.W))
    val perfExecutionStallCycles = Output(UInt(64.W))
    val perfFloatDivStallCycles = Output(UInt(64.W))
    val perfIntegerDivStallCycles = Output(UInt(64.W))
    val perfIntegerDivStartEvents = Output(UInt(64.W))
    val perfIntegerDivStartOnRedirectEvents = Output(UInt(64.W))
    val perfConditionalBranches = Output(UInt(64.W))
    val perfJumps = Output(UInt(64.W))
    val perfIntegerMultiplyInstructions = Output(UInt(64.W))
    val perfIntegerDivideInstructions = Output(UInt(64.W))
    val perfFloatAddInstructions = Output(UInt(64.W))
    val perfFloatMultiplyInstructions = Output(UInt(64.W))
    val perfFloatDivideInstructions = Output(UInt(64.W))
    val perfFloatMacInstructions = Output(UInt(64.W))
    val perfLoadInstructions = Output(UInt(64.W))
    val perfStoreInstructions = Output(UInt(64.W))
    
    // 分支调试信号
    val predFail = Output(Bool())
    val branchTgt = Output(UInt(32.W))
    val hazardFlush = Output(Bool())
    val hazardStall = Output(Bool())
}

// CPU的顶层IO：对外提供仿真环境的内存接口
class CPUIO extends Bundle {
    val imem = new FrontendMemIO     // 取指内存接口
    val dmem = new BackendMemIO      // 数据内存接口（两个LSU）
    val debug = new CPUDebugIO       // 调试接口
}

class CPU extends Module {
    val io = IO(new CPUIO)
    
    // ========== 实例化三大模块 ==========
    val frontend = Module(new Frontend)
    val backend = Module(new Backend)
    val hazard = Module(new Hazard)
    val performanceMonitor = Module(new PerformanceMonitor)
    
    // ========== 连接Frontend和Backend ==========
    // Frontend -> Backend: 指令包
    backend.io.frontend.instPkgs := frontend.io.backend.instPkg
    
    // Backend -> Frontend: 写回信号
    frontend.io.backend.gprWen := backend.io.frontend.gprWen
    frontend.io.backend.gprWaddr := backend.io.frontend.gprWaddr
    frontend.io.backend.gprWdata := backend.io.frontend.gprWdata
    frontend.io.backend.fprWen := backend.io.frontend.fprWen
    frontend.io.backend.fprWaddr := backend.io.frontend.fprWaddr
    frontend.io.backend.fprWdata := backend.io.frontend.fprWdata
    frontend.io.backend.csrValid := backend.io.frontend.csrValid
    frontend.io.backend.csrAddress := backend.io.frontend.csrAddress
    frontend.io.backend.csrCommand := backend.io.frontend.csrCommand
    frontend.io.backend.csrSource := backend.io.frontend.csrSource
    backend.io.frontend.csrReadData := frontend.io.backend.csrReadData
    
    // Backend -> Frontend: 分支重定向
    frontend.io.backend.predFail := backend.io.frontend.predFail
    frontend.io.backend.branchTgt := backend.io.frontend.branchTgt
    frontend.io.backend.branchUpdateValid := backend.io.frontend.branchUpdateValid
    frontend.io.backend.branchUpdatePC := backend.io.frontend.branchUpdatePC
    frontend.io.backend.branchUpdateInst := backend.io.frontend.branchUpdateInst
    frontend.io.backend.branchUpdateTaken := backend.io.frontend.branchUpdateTaken
    frontend.io.backend.branchUpdateTarget := backend.io.frontend.branchUpdateTarget
    
    // ========== 连接Frontend和Hazard ==========
    frontend.io.hazard <> hazard.io.frontend
    
    // ========== 连接Backend和Hazard ==========
    backend.io.hazard <> hazard.io.backend
    
    // ========== 连接仿真环境内存接口 ==========
    // 取指接口
    io.imem <> frontend.io.mem
    
    // 数据访存接口（两个LSU）
    io.dmem <> backend.io.mem
    
    // ========== 连接调试接口 ==========
    io.debug.gpr := frontend.io.debug.gpr
    io.debug.fpr := frontend.io.debug.fpr
    io.debug.wbValid := backend.io.debug.wbValid
    io.debug.wbPC := backend.io.debug.wbPC
    io.debug.wbInst := backend.io.debug.wbInst
    io.debug.wbRd := backend.io.debug.wbRd
    io.debug.wbData := backend.io.debug.wbData
    performanceMonitor.io.wbValid := backend.io.debug.wbValid
    performanceMonitor.io.wbInst := backend.io.debug.wbInst
    performanceMonitor.io.rawStall := hazard.io.debug.rawStall
    performanceMonitor.io.rawLoad := hazard.io.debug.rawLoad
    performanceMonitor.io.rawIntegerMul := hazard.io.debug.rawIntegerMul
    performanceMonitor.io.rawFloatAdd := hazard.io.debug.rawFloatAdd
    performanceMonitor.io.rawFloatMul := hazard.io.debug.rawFloatMul
    performanceMonitor.io.rawFloatDiv := hazard.io.debug.rawFloatDiv
    performanceMonitor.io.falseRawStall := hazard.io.debug.falseRawStall
    performanceMonitor.io.branchRedirect := hazard.io.debug.branchRedirect
    performanceMonitor.io.executionStall := hazard.io.debug.executionStall
    performanceMonitor.io.floatDivStall := hazard.io.debug.floatDivStall
    performanceMonitor.io.integerDivStall := hazard.io.debug.integerDivStall
    performanceMonitor.io.integerDivStarts := hazard.io.debug.integerDivStarts
    performanceMonitor.io.integerDivStartsOnRedirect := hazard.io.debug.integerDivStartsOnRedirect
    io.debug.perfCycles := performanceMonitor.io.cycles
    io.debug.perfEffectiveInstructions := performanceMonitor.io.effectiveInstructions
    io.debug.perfExecutedPackets := performanceMonitor.io.executedPackets
    io.debug.perfRawStallCycles := performanceMonitor.io.rawStallCycles
    io.debug.perfRawLoadCycles := performanceMonitor.io.rawLoadCycles
    io.debug.perfRawIntegerMulCycles := performanceMonitor.io.rawIntegerMulCycles
    io.debug.perfRawFloatAddCycles := performanceMonitor.io.rawFloatAddCycles
    io.debug.perfRawFloatMulCycles := performanceMonitor.io.rawFloatMulCycles
    io.debug.perfRawFloatDivCycles := performanceMonitor.io.rawFloatDivCycles
    io.debug.perfFalseRawStallCycles := performanceMonitor.io.falseRawStallCycles
    io.debug.perfRawOtherCycles := performanceMonitor.io.rawOtherCycles
    io.debug.perfBranchRedirects := performanceMonitor.io.branchRedirects
    io.debug.perfExecutionStallCycles := performanceMonitor.io.executionStallCycles
    io.debug.perfFloatDivStallCycles := performanceMonitor.io.floatDivStallCycles
    io.debug.perfIntegerDivStallCycles := performanceMonitor.io.integerDivStallCycles
    io.debug.perfIntegerDivStartEvents := performanceMonitor.io.integerDivStartEvents
    io.debug.perfIntegerDivStartOnRedirectEvents := performanceMonitor.io.integerDivStartOnRedirectEvents
    io.debug.perfConditionalBranches := performanceMonitor.io.conditionalBranches
    io.debug.perfJumps := performanceMonitor.io.jumps
    io.debug.perfIntegerMultiplyInstructions := performanceMonitor.io.integerMultiplyInstructions
    io.debug.perfIntegerDivideInstructions := performanceMonitor.io.integerDivideInstructions
    io.debug.perfFloatAddInstructions := performanceMonitor.io.floatAddInstructions
    io.debug.perfFloatMultiplyInstructions := performanceMonitor.io.floatMultiplyInstructions
    io.debug.perfFloatDivideInstructions := performanceMonitor.io.floatDivideInstructions
    io.debug.perfFloatMacInstructions := performanceMonitor.io.floatMacInstructions
    io.debug.perfLoadInstructions := performanceMonitor.io.loadInstructions
    io.debug.perfStoreInstructions := performanceMonitor.io.storeInstructions
    
    // 分支调试信号
    io.debug.predFail := backend.io.frontend.predFail
    io.debug.branchTgt := backend.io.frontend.branchTgt
    io.debug.hazardFlush := hazard.io.frontend.flush
    io.debug.hazardStall := hazard.io.frontend.stall
}
