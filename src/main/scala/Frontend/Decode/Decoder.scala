import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._
import ZirconConfig.InstructionType._
import ZirconConfig.Src1Sel._
import ZirconConfig.Src2Sel._
import ZirconConfig.Valid._
import ZirconUtil._

// Keep the target branch's independent register-file tags and add semantic
// register-use bits for shallow dependency and RAW detection.
object ALUDecodeMap {
    // op, immType, src1Sel, src2Sel, rdValid,
    // rs1IsFloat, rs2IsFloat, rs3IsFloat, rdIsFloat, instValid,
    // rs1ReadValid, rs2ReadValid
    val default = List(LUI, U_TYPE, PC, IMM, N, N, N, N, N, N, N, N)
    val map = Array(
        RVISA.LUI   -> List(LUI,  U_TYPE, PC,  IMM, Y, N, N, N, N, Y, N, N),
        RVISA.AUIPC -> List(ADD,  U_TYPE, PC,  IMM, Y, N, N, N, N, Y, N, N),
        RVISA.ADDI  -> List(ADD,  I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.SLTI  -> List(SLT,  I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.SLTIU -> List(SLTU, I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.XORI  -> List(XOR,  I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.ORI   -> List(OR,   I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.ANDI  -> List(AND,  I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.SLLI  -> List(SLL,  I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.SRLI  -> List(SRL,  I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.SRAI  -> List(SRA,  I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.ADD   -> List(ADD,  R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.SUB   -> List(SUB,  R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.SLL   -> List(SLL,  R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.SLT   -> List(SLT,  R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.SLTU  -> List(SLTU, R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.XOR   -> List(XOR,  R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.SRL   -> List(SRL,  R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.SRA   -> List(SRA,  R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.OR    -> List(OR,   R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.AND   -> List(AND,  R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
    )
}

object BranchDecodeMap {
    val default = List(BEQ, B_TYPE, PC, IMM, N, N, N, N, N, N, N, N)
    val map = Array(
        RVISA.JAL  -> List(JAL,  J_TYPE, PC, IMM, Y, N, N, N, N, Y, N, N),
        RVISA.JALR -> List(JALR, I_TYPE, PC, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.BEQ  -> List(BEQ,  B_TYPE, PC, IMM, N, N, N, N, N, Y, Y, Y),
        RVISA.BNE  -> List(BNE,  B_TYPE, PC, IMM, N, N, N, N, N, Y, Y, Y),
        RVISA.BLT  -> List(BLT,  B_TYPE, PC, IMM, N, N, N, N, N, Y, Y, Y),
        RVISA.BGE  -> List(BGE,  B_TYPE, PC, IMM, N, N, N, N, N, Y, Y, Y),
        RVISA.BLTU -> List(BLTU, B_TYPE, PC, IMM, N, N, N, N, N, Y, Y, Y),
        RVISA.BGEU -> List(BGEU, B_TYPE, PC, IMM, N, N, N, N, N, Y, Y, Y),
    )
}

object FPUDecodeMap {
    val default = List(FADD_S, R_TYPE, RS1, RS2, N, N, N, N, N, N, N, N)
    val map = Array(
        RVISA.FMADD_S   -> List(FMADD_S,   R_TYPE, RS1, RS2, Y, Y, Y, Y, Y, Y, Y, Y),
        RVISA.FMSUB_S   -> List(FMSUB_S,   R_TYPE, RS1, RS2, Y, Y, Y, Y, Y, Y, Y, Y),
        RVISA.FNMSUB_S  -> List(FNMSUB_S,  R_TYPE, RS1, RS2, Y, Y, Y, Y, Y, Y, Y, Y),
        RVISA.FNMADD_S  -> List(FNMADD_S,  R_TYPE, RS1, RS2, Y, Y, Y, Y, Y, Y, Y, Y),
        RVISA.FADD_S    -> List(FADD_S,    R_TYPE, RS1, RS2, Y, Y, Y, N, Y, Y, Y, Y),
        RVISA.FSUB_S    -> List(FSUB_S,    R_TYPE, RS1, RS2, Y, Y, Y, N, Y, Y, Y, Y),
        RVISA.FMUL_S    -> List(FMUL_S,    R_TYPE, RS1, RS2, Y, Y, Y, N, Y, Y, Y, Y),
        RVISA.FSGNJ_S   -> List(FSGNJ_S,   R_TYPE, RS1, RS2, Y, Y, Y, N, Y, Y, Y, Y),
        RVISA.FSGNJN_S  -> List(FSGNJN_S,  R_TYPE, RS1, RS2, Y, Y, Y, N, Y, Y, Y, Y),
        RVISA.FSGNJX_S  -> List(FSGNJX_S,  R_TYPE, RS1, RS2, Y, Y, Y, N, Y, Y, Y, Y),
        RVISA.FMIN_S    -> List(FMIN_S,    R_TYPE, RS1, RS2, Y, Y, Y, N, Y, Y, Y, Y),
        RVISA.FMAX_S    -> List(FMAX_S,    R_TYPE, RS1, RS2, Y, Y, Y, N, Y, Y, Y, Y),
        RVISA.FCVT_W_S  -> List(FCVT_W_S,  R_TYPE, RS1, RS2, Y, Y, N, N, N, Y, Y, N),
        RVISA.FCVT_WU_S -> List(FCVT_WU_S, R_TYPE, RS1, RS2, Y, Y, N, N, N, Y, Y, N),
        RVISA.FMV_X_W   -> List(FMV_X_W,   R_TYPE, RS1, RS2, Y, Y, N, N, N, Y, Y, N),
        RVISA.FEQ_S     -> List(FEQ_S,     R_TYPE, RS1, RS2, Y, Y, Y, N, N, Y, Y, Y),
        RVISA.FLT_S     -> List(FLT_S,     R_TYPE, RS1, RS2, Y, Y, Y, N, N, Y, Y, Y),
        RVISA.FLE_S     -> List(FLE_S,     R_TYPE, RS1, RS2, Y, Y, Y, N, N, Y, Y, Y),
        RVISA.FCLASS_S  -> List(FCLASS_S,  R_TYPE, RS1, RS2, Y, Y, N, N, N, Y, Y, N),
        RVISA.FCVT_S_W  -> List(FCVT_S_W,  R_TYPE, RS1, RS2, Y, N, N, N, Y, Y, Y, N),
        RVISA.FCVT_S_WU -> List(FCVT_S_WU, R_TYPE, RS1, RS2, Y, N, N, N, Y, Y, Y, N),
        RVISA.FMV_W_X   -> List(FMV_W_X,   R_TYPE, RS1, RS2, Y, N, N, N, Y, Y, Y, N),
    )
}

object MemDecodeMap {
    val default = List(LB, I_TYPE, RS1, IMM, N, N, N, N, N, N, N, N)
    val map = Array(
        RVISA.LB  -> List(LB,  I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.LH  -> List(LH,  I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.LW  -> List(LW,  I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.LBU -> List(LBU, I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.LHU -> List(LHU, I_TYPE, RS1, IMM, Y, N, N, N, N, Y, Y, N),
        RVISA.SB  -> List(SB,  S_TYPE, RS1, IMM, N, N, N, N, N, Y, Y, Y),
        RVISA.SH  -> List(SH,  S_TYPE, RS1, IMM, N, N, N, N, N, Y, Y, Y),
        RVISA.SW  -> List(SW,  S_TYPE, RS1, IMM, N, N, N, N, N, Y, Y, Y),
        RVISA.FLW -> List(FLW, I_TYPE, RS1, IMM, Y, N, N, N, Y, Y, Y, N),
        RVISA.FSW -> List(FSW, S_TYPE, RS1, IMM, N, N, Y, N, N, Y, Y, Y),
    )
}

object IMulDivDecodeMap {
    val default = List(MUL, R_TYPE, RS1, RS2, N, N, N, N, N, N, N, N)
    val map = Array(
        RVISA.MUL    -> List(MUL,    R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.MULH   -> List(MULH,   R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.MULHSU -> List(MULHSU, R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.MULHU  -> List(MULHU,  R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.DIV    -> List(DIV,    R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.DIVU   -> List(DIVU,   R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.REM    -> List(REM,    R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
        RVISA.REMU   -> List(REMU,   R_TYPE, RS1, RS2, Y, N, N, N, N, Y, Y, Y),
    )
}

object FDivDecodeMap {
    val default = List(FDIV_S, R_TYPE, RS1, RS2, N, N, N, N, N, N, N, N)
    val map = Array(
        RVISA.FDIV_S  -> List(FDIV_S,  R_TYPE, RS1, RS2, Y, Y, Y, N, Y, Y, Y, Y),
        RVISA.FSQRT_S -> List(FSQRT_S, R_TYPE, RS1, RS2, Y, Y, N, N, Y, Y, Y, N),
    )
}

class DecoderIO extends Bundle {
    val instPkgIn = Input(new InstructionPackage())
    val instPkgOut = Output(new InstructionPackage())
}

class Decoder(ALU: Boolean, FPU: Boolean, Branch: Boolean, Mem: Boolean, IMulDiv: Boolean, FDiv: Boolean) extends Module {
    val io = IO(new DecoderIO)

    val aluSignals = if (ALU) ListLookup(io.instPkgIn.inst, ALUDecodeMap.default, ALUDecodeMap.map) else ALUDecodeMap.default
    val fpuSignals = if (FPU) ListLookup(io.instPkgIn.inst, FPUDecodeMap.default, FPUDecodeMap.map) else FPUDecodeMap.default
    val branchSignals = if (Branch) ListLookup(io.instPkgIn.inst, BranchDecodeMap.default, BranchDecodeMap.map) else BranchDecodeMap.default
    val memSignals = if (Mem) ListLookup(io.instPkgIn.inst, MemDecodeMap.default, MemDecodeMap.map) else MemDecodeMap.default
    val iMulDivSignals = if (IMulDiv) ListLookup(io.instPkgIn.inst, IMulDivDecodeMap.default, IMulDivDecodeMap.map) else IMulDivDecodeMap.default
    val fDivSignals = if (FDiv) ListLookup(io.instPkgIn.inst, FDivDecodeMap.default, FDivDecodeMap.map) else FDivDecodeMap.default

    assert(
        PopCount(VecInit(aluSignals(9), fpuSignals(9), branchSignals(9), memSignals(9), iMulDivSignals(9), fDivSignals(9)).asUInt) <= 1.U,
        "Multiple signals are active"
    )

    def selectSignal(idx: Int) = MuxCase(
        aluSignals(idx),
        Array(
            aluSignals(9).asBool -> aluSignals(idx),
            fpuSignals(9).asBool -> fpuSignals(idx),
            branchSignals(9).asBool -> branchSignals(idx),
            memSignals(9).asBool -> memSignals(idx),
            iMulDivSignals(9).asBool -> iMulDivSignals(idx),
            fDivSignals(9).asBool -> fDivSignals(idx)
        ).toIndexedSeq
    )

    val op = selectSignal(0)
    val immType = selectSignal(1)
    val src1Sel = selectSignal(2)
    val src2Sel = selectSignal(3)
    val rdValidRaw = selectSignal(4)
    val rs1 = selectSignal(5) ## io.instPkgIn.inst(19, 15)
    val rs2 = selectSignal(6) ## io.instPkgIn.inst(24, 20)
    val rs3 = selectSignal(7) ## io.instPkgIn.inst(31, 27)
    val rd = selectSignal(8) ## io.instPkgIn.inst(11, 7)
    val shallowDepOK = aluSignals(9).asBool || branchSignals(9).asBool
    val rs1ReadValid = selectSignal(10).asBool
    val rs2ReadValid = selectSignal(11).asBool

    val isGPR = !selectSignal(8).asBool
    val rdIsZero = io.instPkgIn.inst(11, 7) === 0.U
    val rdValid = rdValidRaw.asBool && !(isGPR && rdIsZero)

    def immGen(immType: UInt, inst: UInt) = MuxLookup(immType, 0.U(32.W))(Seq(
        U_TYPE -> (inst(31, 12) ## 0.U(12.W)),
        J_TYPE -> SE(inst(31) ## inst(19, 12) ## inst(20) ## inst(30, 21) ## 0.U(1.W), 32),
        I_TYPE -> SE(inst(31, 20), 32),
        S_TYPE -> SE(inst(31, 25) ## inst(11, 7), 32),
        B_TYPE -> SE(inst(31) ## inst(7) ## inst(30, 25) ## inst(11, 8) ## 0.U(1.W), 32),
    ))

    val rm = io.instPkgIn.inst(14, 12)

    io.instPkgOut := io.instPkgIn.IDUpdate(
        rs1, rs2, rs3, rd, rdValid, op, rm,
        immGen(immType, io.instPkgIn.inst), src1Sel, src2Sel,
        shallowDepOK, rs1ReadValid, rs2ReadValid
    )
}
