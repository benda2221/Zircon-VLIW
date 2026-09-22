import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._

class HazardIO extends Bundle {
    val frontend = Flipped(new FrontendHazardIO)
    val backend = Flipped(new BackendHazardIO)
}

class Hazard extends Module {
    val io = IO(new HazardIO)
    
    // ========== 默认不产生任何控制信号 ==========
    io.frontend.flush := false.B
    io.frontend.stall := false.B
    for (i <- 0 until 8) {
        io.backend.ex1Stall(i) := false.B
        io.backend.ex2Flush(i) := false.B
        io.backend.ex2Stall(i) := false.B
        io.backend.ex3Flush(i) := false.B
        io.backend.ex3Stall(i) := false.B
        io.backend.wbFlush(i) := false.B
        io.backend.wbStall(i) := false.B
    }
    
    // ========== 1. 除法器停顿处理（最高优先级）==========
    // 包括整数除法器（流水线3、4）和浮点除法器（流水线0）
    val divStall = io.backend.divBusy(0) || io.backend.divBusy(1) || io.backend.fdivBusy
    when(divStall) {
        // 对前端发起停顿
        io.frontend.stall := true.B
        // 停顿所有的ID-EX1、EX1-EX2、EX2-EX3寄存器
        for (i <- 0 until 8) {
            io.backend.ex1Stall(i) := true.B
            io.backend.ex2Stall(i) := true.B
            io.backend.ex3Stall(i) := true.B
        }
        // 冲刷所有的EX3-WB寄存器
        for (i <- 0 until 8) {
            io.backend.wbFlush(i) := true.B
        }
    }
    
    // ========== 2. 分支预测失败处理（次优先级）==========
    // EX3浅依赖分支比EX2普通分支更老，因此具有更高优先级。
    val ex3Redirect = io.backend.predFailEX3
    val ex2Redirect = io.backend.predFailEX2 && !ex3Redirect

    // A nested dependency exists when a lane that needs cross-packet late
    // forwarding is itself selected as a same-packet shallow producer.
    val laneNeedsLate = Wire(Vec(8, Bool()))
    for (i <- 0 until 8) {
        laneNeedsLate(i) := io.backend.ex1LateRs1Sel(i).orR ||
            io.backend.ex1LateRs2Sel(i).orR
    }
    val lateLaneMask = laneNeedsLate.asUInt
    val nestedLateShallow = WireInit(false.B)
    for (consumer <- 0 until 8) {
        val shallowProducer = io.backend.ex1ShallowRs1Sel(consumer) |
            io.backend.ex1ShallowRs2Sel(consumer)
        when((shallowProducer & lateLaneMask).orR) {
            nestedLateShallow := true.B
        }
    }

    when(!divStall && ex3Redirect) {
        // 给前端flush信号
        io.frontend.flush := true.B
        for (i <- 0 until 8) {
            io.backend.ex2Flush(i) := true.B
            // EX3重定向还必须清除当前EX2中更年轻的指令。
            io.backend.ex3Flush(i) := true.B
        }
    }.elsewhen(!divStall && ex2Redirect) {
        io.frontend.flush := true.B
        for (i <- 0 until 8) {
            io.backend.ex2Flush(i) := true.B
        }
    }
    
    // ========== 3. RAW数据相关处理 ==========
    // 判断指令是否需要WB阶段才能访问（load/mul/div/float）
    def needWB(op: UInt, pipelineIdx: Int): Bool = {
        val isLoad = op(4) && !op(5)  // op[4]=1表示branch/load/muldiv，但load没有op[5]
        val isMulDiv = op(4) && ((pipelineIdx == 3) || (pipelineIdx == 4)).B  // 乘除法在流水线3-4
        val isFloat = op(6)  // op[6]=1表示float（不包括fdiv）
        // FDiv 需要根据操作码判断，而不是流水线编号
        val isFDiv = (op === ZirconConfig.EXEOp.FDIV_S) || (op === ZirconConfig.EXEOp.FSQRT_S)
        isLoad || isMulDiv || isFloat || isFDiv
    }
    
    // 检查RAW冲突：ID阶段的指令依赖EX1或EX2阶段的指令
    val rawHazard = WireInit(false.B)
    
    for (idIdx <- 0 until 8) {  // ID阶段的8条指令
        val idPkg = io.frontend.idPkgs(idIdx)
        
        // 检查与EX1阶段的冲突
        for (ex1Idx <- 0 until 8) {
            val ex1Pkg = io.backend.ex1Pkgs(ex1Idx)
            val rs1Match = idPkg.rs1ReadValid && idPkg.rs1 === ex1Pkg.rd
            val rs2Match = idPkg.rs2ReadValid && idPkg.rs2 === ex1Pkg.rd
            val rs3Match = (idIdx < 3).B && (idPkg.rs3 === ex1Pkg.rd)
            val anyMatch = rs1Match || rs2Match || rs3Match
            val replayRs12MustStall = io.backend.ex1NeedsEX2Replay(ex1Idx) &&
                !idPkg.shallowDepOK && (rs1Match || rs2Match)
            val replayRs3MustStall = io.backend.ex1NeedsEX2Replay(ex1Idx) && rs3Match
            when(ex1Pkg.rdValid && needWB(ex1Pkg.op, ex1Idx) && anyMatch) {
                rawHazard := true.B
            }
            when(ex1Pkg.rdValid &&
                 (replayRs12MustStall || replayRs3MustStall)) {
                rawHazard := true.B
            }
        }
        
        // 检查与EX2阶段的冲突
        for (ex2Idx <- 0 until 8) {
            val ex2Pkg = io.backend.ex2Pkgs(ex2Idx)
            val rs1Match = idPkg.rs1ReadValid && idPkg.rs1 === ex2Pkg.rd
            val rs2Match = idPkg.rs2ReadValid && idPkg.rs2 === ex2Pkg.rd
            val rs3Match = (idIdx < 3).B && (idPkg.rs3 === ex2Pkg.rd)
            val anyMatch = rs1Match || rs2Match || rs3Match
            val replayRs12MustStall = ex2Pkg.needsEX2Replay &&
                !idPkg.shallowDepOK && (rs1Match || rs2Match)
            val replayRs3MustStall = ex2Pkg.needsEX2Replay && rs3Match
            when(ex2Pkg.rdValid && needWB(ex2Pkg.op, ex2Idx) && anyMatch) {
                rawHazard := true.B
            }
            when(ex2Pkg.rdValid &&
                 (replayRs12MustStall || replayRs3MustStall)) {
                rawHazard := true.B
            }
        }
    }
    
    // RAW冲突处理：如果没有除法器停顿和分支冲刷，则处理RAW冲突
    // 注意：分支冲刷优先于RAW stall，否则PC无法更新到正确的跳转地址
    val takeNested = !divStall && !ex3Redirect && !ex2Redirect &&
        nestedLateShallow
    when(takeNested) {
        io.frontend.stall := true.B
        for (i <- 0 until 8) {
            io.backend.ex1Stall(i) := true.B
            io.backend.ex2Stall(i) := true.B
            // ex3Stall remains false: drain the old EX2 packet into EX3.
        }
    }

    val takeRawFlush = !divStall && !ex3Redirect && !ex2Redirect &&
        !nestedLateShallow && rawHazard
    val takeBranchFlush = !divStall && (ex3Redirect || ex2Redirect)
    when(takeRawFlush) {
        // 对前端发起停顿
        io.frontend.stall := true.B
    }

    // Both causes clear the next ID-EX1 contents. Only a taken redirect
    // cancels current execution; a RAW bubble must let the EX1 producer run.
    for (i <- 0 until 8) {
        io.backend.ex1RawFlush(i) := takeRawFlush
        io.backend.ex1BranchFlush(i) := takeBranchFlush
        io.backend.ex1Flush(i) := takeRawFlush || takeBranchFlush
    }

}
