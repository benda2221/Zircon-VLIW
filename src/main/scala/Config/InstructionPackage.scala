import chisel3._
import chisel3.util._
// import ZirconConfig.RegisterFile._

class InstructionPackage extends Bundle {
    /* IF Stage */
    val pc        = UInt(32.W)
    val inst      = UInt(32.W)
    val predTaken = Bool()
    val predTarget = UInt(32.W)
    /* ID Stage */
    // regfile
    val rs1       = UInt(6.W) // the highest bit to judge if it is float
    val rs2       = UInt(6.W) // the highest bit to judge if it is float
    val rs3       = UInt(6.W) // the highest bit to judge if it is float
    val rd        = UInt(6.W) // the highest bit to judge if it is float
    val rdValid   = Bool()
    val rs1Data   = UInt(32.W)
    val rs2Data   = UInt(32.W)
    val rs3Data   = UInt(32.W)
    // operation
    val op        = UInt(7.W) // [3:0]: operation; [4]: is branch or load or muldiv [5]: is store  [6]: is float (not fdiv) 
    val rm        = UInt(3.W) // rounding mode for FPU (from inst[14:12])
    val imm       = UInt(32.W)
    val src1Sel   = UInt(1.W) // 0: rs1; 1: pc; 
    val src2Sel   = UInt(1.W) // 0: rs2; 1: imm;
    /* EX Stage */
    val aluResult = UInt(32.W)
    val fpuResult = UInt(32.W)
    val fflags    = UInt(5.W) // floating-point status flags (NV, DZ, OF, UF, NX)
    val branchTgt = UInt(32.W)
    val predFail  = Bool()
    val branchTaken = Bool()
    val actualBranchTarget = UInt(32.W)
    val memResult = UInt(32.W)
    /* WB Stage */
    val rfWdata   = UInt(32.W)

    def IFUpdate(pc: UInt, inst: UInt, predTaken: Bool = false.B, predTarget: UInt = 0.U): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.pc     := pc
        instPkg.inst   := inst
        instPkg.predTaken := predTaken
        instPkg.predTarget := predTarget
        instPkg
    }
    def IDUpdate(rs1: UInt, rs2: UInt, rs3: UInt, rd: UInt, rdValid: Bool, op: UInt, rm: UInt, imm: UInt, src1Sel: UInt, src2Sel: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.rs1       := rs1
        instPkg.rs2       := rs2
        instPkg.rs3       := rs3
        instPkg.rd        := rd
        instPkg.rdValid   := rdValid
        instPkg.op        := op
        instPkg.rm        := rm
        instPkg.imm       := imm
        instPkg.src1Sel   := src1Sel
        instPkg.src2Sel   := src2Sel
        instPkg
    }
    def IDUpdate(rs1Data: UInt, rs2Data: UInt, rs3Data: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.rs1Data   := rs1Data
        instPkg.rs2Data   := rs2Data
        instPkg.rs3Data   := rs3Data
        instPkg
    }
    def EX1Update(
        aluResult: UInt,
        branchTgt: UInt,
        predFail: Bool,
        branchTaken: Bool = false.B,
        actualBranchTarget: UInt = 0.U
    ): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.aluResult := aluResult
        instPkg.branchTgt := branchTgt
        instPkg.predFail  := predFail
        instPkg.branchTaken := branchTaken
        instPkg.actualBranchTarget := actualBranchTarget
        instPkg
    }
    def EX2Update(memResult: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.memResult := memResult
        instPkg
    }
    def EX3Update(fpuResult: UInt, fflags: UInt = 0.U): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.fpuResult := fpuResult
        instPkg.fflags    := fflags
        instPkg
    }
    def WBUpdate(rfWdata: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.rfWdata := rfWdata
        instPkg
    }
}
