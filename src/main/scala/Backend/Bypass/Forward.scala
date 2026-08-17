import chisel3._
import chisel3.util._

// Forward模块的IO
class ForwardIO extends Bundle {
    // 8条流水线的EX2阶段InstPkg（用于判断前递）
    val ex2Pkgs = Input(Vec(8, new InstructionPackage))
    // 8条流水线的EX3阶段InstPkg（用于判断前递）
    val ex3Pkgs = Input(Vec(8, new InstructionPackage))
    // EX3的实际GPR前递数据与流水包分离，避免组合执行结果污染元数据路径
    val ex3GprData = Input(Vec(8, UInt(32.W)))
    // 8条流水线的WB阶段InstPkg（用于判断前递）
    val wbPkgs = Input(Vec(8, new InstructionPackage))

    // WB forwarding is split by register file so FPR data is not physically
    // connected to fixed-GPR consumer lanes, and vice versa.
    val wbGprValid = Input(Vec(8, Bool()))
    val wbGprAddr = Input(Vec(8, UInt(5.W)))
    val wbGprData = Input(Vec(8, UInt(32.W)))
    val wbFprValid = Input(Vec(8, Bool()))
    val wbFprAddr = Input(Vec(8, UInt(5.W)))
    val wbFprData = Input(Vec(8, UInt(32.W)))
    
    // 8条流水线的EX1阶段InstPkg（需要前递的目标）
    val ex1Pkgs = Input(Vec(8, new InstructionPackage))
    
    // 前递后的数据输出（rs1Data, rs2Data, rs3Data）
    val fwdRs1Data = Output(Vec(8, UInt(32.W)))
    val fwdRs2Data = Output(Vec(8, UInt(32.W)))
    val fwdRs3Data = Output(Vec(8, UInt(32.W)))
    val fwdGprRs1Data = Output(Vec(8, UInt(32.W)))
    val fwdGprRs2Data = Output(Vec(8, UInt(32.W)))
    val fwdFprRs1Data = Output(Vec(8, UInt(32.W)))
    val fwdFprRs2Data = Output(Vec(8, UInt(32.W)))
    // Slot 7 Branch专用路径不连接EX3整数乘除法结果
    val fwdBranchRs1Data = Output(UInt(32.W))
    val fwdBranchRs2Data = Output(UInt(32.W))
}

class Forward extends Module {
    val io = IO(new ForwardIO)
    
    // 各流水线在EX2、EX3和WB阶段是否可以前递（根据文档表格）
    // EX2前递能力：0:No, 1-7:ALU
    val ex2CanForward = VecInit(Seq(false.B, true.B, true.B, true.B, true.B, true.B, true.B, true.B))
    
    // EX3前递能力：0:No, 1-7:ALU
    val ex3CanForward = VecInit(Seq(false.B, true.B, true.B, true.B, true.B, true.B, true.B, true.B))
    
    def gprFloatResultAtEx2(op: UInt, lane: Int): Bool = {
        if (lane <= 2) {
            op === ZirconConfig.EXEOp.FEQ_S || op === ZirconConfig.EXEOp.FLT_S ||
            op === ZirconConfig.EXEOp.FLE_S || op === ZirconConfig.EXEOp.FCLASS_S ||
            op === ZirconConfig.EXEOp.FMV_X_W
        } else {
            false.B
        }
    }

    def gprFloatResultAtEx3(op: UInt, lane: Int): Bool = {
        if (lane <= 2) {
            gprFloatResultAtEx2(op, lane) || op === ZirconConfig.EXEOp.FCVT_W_S ||
            op === ZirconConfig.EXEOp.FCVT_WU_S
        } else {
            false.B
        }
    }

    def fprFloatResultAtEx2(op: UInt, lane: Int): Bool = {
        if (lane <= 2) {
            op === ZirconConfig.EXEOp.FSGNJ_S || op === ZirconConfig.EXEOp.FSGNJN_S ||
            op === ZirconConfig.EXEOp.FSGNJX_S || op === ZirconConfig.EXEOp.FMIN_S ||
            op === ZirconConfig.EXEOp.FMAX_S || op === ZirconConfig.EXEOp.FMV_W_X
        } else {
            false.B
        }
    }

    def fprFloatResultAtEx3(op: UInt, lane: Int): Bool = {
        if (lane <= 2) {
            fprFloatResultAtEx2(op, lane) || op === ZirconConfig.EXEOp.FDIV_S ||
            op === ZirconConfig.EXEOp.FSQRT_S ||
            op === ZirconConfig.EXEOp.FCVT_S_W || op === ZirconConfig.EXEOp.FCVT_S_WU
        } else {
            false.B
        }
    }

    def isCSR(op: UInt): Bool = {
        op === ZirconConfig.EXEOp.CSRRW ||
        op === ZirconConfig.EXEOp.CSRRS ||
        op === ZirconConfig.EXEOp.CSRRC
    }

    // Decode producer availability once. Address matching remains local to
    // each consumer, but operation decoding no longer sits in every source path.
    val ex2ProducerValid = Wire(Vec(8, Bool()))
    val ex3ProducerValid = Wire(Vec(8, Bool()))
    val ex2ProducerData = Wire(Vec(8, UInt(32.W)))
    val ex3ProducerData = Wire(Vec(8, UInt(32.W)))
    val ex2FprProducerValid = Wire(Vec(8, Bool()))
    val ex3FprProducerValid = Wire(Vec(8, Bool()))
    val ex2FprProducerData = Wire(Vec(8, UInt(32.W)))
    val ex3FprProducerData = Wire(Vec(8, UInt(32.W)))
    val branchEx2ProducerValid = Wire(Vec(8, Bool()))
    val branchEx3ProducerValid = Wire(Vec(8, Bool()))
    val branchEx3ProducerData = Wire(Vec(8, UInt(32.W)))

    for (j <- 0 until 8) {
        val ex2GprFloatReady = gprFloatResultAtEx2(io.ex2Pkgs(j).op, j)
        val ex3GprFloatReady = gprFloatResultAtEx3(io.ex3Pkgs(j).op, j)
        val ex2FprFloatReady = fprFloatResultAtEx2(io.ex2Pkgs(j).op, j)
        val ex3FprFloatReady = fprFloatResultAtEx3(io.ex3Pkgs(j).op, j)
        val ex2IsFloat = if (j <= 2) io.ex2Pkgs(j).op(6) else false.B
        val ex3IsFloat = if (j <= 2) io.ex3Pkgs(j).op(6) else false.B
        val ex2IsIntegerMulDiv = if (j == 3 || j == 4) io.ex2Pkgs(j).op(4) else false.B
        val ex3IsIntegerMulDiv = if (j == 3 || j == 4) io.ex3Pkgs(j).op(4) else false.B
        val ex2IsCSR = isCSR(io.ex2Pkgs(j).op)
        val ex3IsCSR = isCSR(io.ex3Pkgs(j).op)

        ex2ProducerValid(j) := io.ex2Pkgs(j).rdValid && !io.ex2Pkgs(j).rd(5) && !ex2IsCSR &&
                               ((ex2CanForward(j) && !ex2IsFloat) || ex2GprFloatReady)
        ex3ProducerValid(j) := io.ex3Pkgs(j).rdValid && !io.ex3Pkgs(j).rd(5) && !ex3IsCSR &&
                               ((ex3CanForward(j) && !ex3IsFloat) || ex3GprFloatReady)
        ex2ProducerData(j) := Mux(ex2GprFloatReady, io.ex2Pkgs(j).fpuResult, io.ex2Pkgs(j).aluResult)
        ex3ProducerData(j) := Mux(ex3GprFloatReady, io.ex3Pkgs(j).fpuResult, io.ex3GprData(j))

        // Branch keeps ordinary ALU/FPU-to-GPR forwarding but never sees the
        // combinational integer multiply/divide data bus.
        branchEx2ProducerValid(j) := ex2ProducerValid(j) && !ex2IsIntegerMulDiv
        branchEx3ProducerValid(j) := ex3ProducerValid(j) && !ex3IsIntegerMulDiv
        branchEx3ProducerData(j) := Mux(
            ex3GprFloatReady,
            io.ex3Pkgs(j).fpuResult,
            io.ex3Pkgs(j).aluResult
        )

        ex2FprProducerValid(j) := io.ex2Pkgs(j).rdValid && io.ex2Pkgs(j).rd(5) && ex2FprFloatReady
        ex3FprProducerValid(j) := io.ex3Pkgs(j).rdValid && io.ex3Pkgs(j).rd(5) && ex3FprFloatReady
        ex2FprProducerData(j) := io.ex2Pkgs(j).fpuResult
        ex3FprProducerData(j) := io.ex3Pkgs(j).fpuResult
    }

    // Select the highest-numbered matching lane with a balanced tree. Each
    // merge gives the higher lane priority, reducing an eight-lane selection
    // to three mux levels without one-hot decode overhead.
    def selectStage(matches: Seq[Bool], data: Seq[UInt]): (Bool, UInt) = {
        require(matches.nonEmpty && matches.length == data.length)

        def merge(low: (Bool, UInt), high: (Bool, UInt)): (Bool, UInt) = {
            (low._1 || high._1, Mux(high._1, high._2, low._2))
        }

        var candidates = matches.zip(data)
        while (candidates.length > 1) {
            candidates = candidates.grouped(2).map {
                case Seq(low, high) => merge(low, high)
                case Seq(single) => single
            }.toSeq
        }
        candidates.head
    }

    def forwardBranchGpr(rsAddr: UInt, rsData: UInt): UInt = {
        val ex2Matches = (0 until 8).map { j =>
            branchEx2ProducerValid(j) && io.ex2Pkgs(j).rd(4, 0) === rsAddr(4, 0)
        }
        val ex3Matches = (0 until 8).map { j =>
            branchEx3ProducerValid(j) && io.ex3Pkgs(j).rd(4, 0) === rsAddr(4, 0)
        }
        val wbMatches = (0 until 8).map { j =>
            io.wbGprValid(j) && io.wbGprAddr(j) === rsAddr(4, 0)
        }

        val (ex2Hit, ex2Data) = selectStage(ex2Matches, ex2ProducerData.toSeq)
        val (ex3Hit, ex3Data) = selectStage(ex3Matches, branchEx3ProducerData.toSeq)
        val (wbHit, wbData) = selectStage(wbMatches, io.wbGprData.toSeq)
        Mux(ex2Hit, ex2Data, Mux(ex3Hit, ex3Data, Mux(wbHit, wbData, rsData)))
    }

    io.fwdBranchRs1Data := forwardBranchGpr(io.ex1Pkgs(7).rs1, io.ex1Pkgs(7).rs1Data)
    io.fwdBranchRs2Data := forwardBranchGpr(io.ex1Pkgs(7).rs2, io.ex1Pkgs(7).rs2Data)
    
    // 为每条流水线的每个源寄存器查找前递数据
    for (i <- 0 until 8) {
        val ex1Pkg = io.ex1Pkgs(i)
        
        // 前3条流水线有3个源寄存器，后面只有2个
        val hasRs3 = i < 3
        
        def forwardGpr(rsAddr: UInt, rsData: UInt): UInt = {
            val ex2Matches = (0 until 8).map { j =>
                ex2ProducerValid(j) && io.ex2Pkgs(j).rd(4, 0) === rsAddr(4, 0)
            }
            val ex3Matches = (0 until 8).map { j =>
                ex3ProducerValid(j) && io.ex3Pkgs(j).rd(4, 0) === rsAddr(4, 0)
            }
            val wbMatches = (0 until 8).map { j =>
                io.wbGprValid(j) && io.wbGprAddr(j) === rsAddr(4, 0)
            }

            val (ex2Hit, ex2Data) = selectStage(ex2Matches, ex2ProducerData.toSeq)
            val (ex3Hit, ex3Data) = selectStage(ex3Matches, ex3ProducerData.toSeq)
            val (wbHit, wbData) = selectStage(wbMatches, io.wbGprData.toSeq)

            Mux(ex2Hit, ex2Data, Mux(ex3Hit, ex3Data, Mux(wbHit, wbData, rsData)))
        }

        def forwardFpr(rsAddr: UInt, rsData: UInt): UInt = {
            val earlyFprProducerLanes = Seq(0, 1, 2)
            val fprProducerLanes = Seq(0, 1, 2, 5, 6)
            val ex2Matches = earlyFprProducerLanes.map { j =>
                ex2FprProducerValid(j) && io.ex2Pkgs(j).rd(4, 0) === rsAddr(4, 0)
            }
            val ex3Matches = earlyFprProducerLanes.map { j =>
                ex3FprProducerValid(j) && io.ex3Pkgs(j).rd(4, 0) === rsAddr(4, 0)
            }
            val wbMatches = fprProducerLanes.map { j =>
                io.wbFprValid(j) && io.wbFprAddr(j) === rsAddr(4, 0)
            }
            val (ex2Hit, ex2Data) = selectStage(ex2Matches, earlyFprProducerLanes.map(ex2FprProducerData(_)))
            val (ex3Hit, ex3Data) = selectStage(ex3Matches, earlyFprProducerLanes.map(ex3FprProducerData(_)))
            val wbDataCandidates = fprProducerLanes.map(io.wbFprData(_))
            val (wbHit, wbData) = selectStage(wbMatches, wbDataCandidates)
            Mux(ex2Hit, ex2Data, Mux(ex3Hit, ex3Data, Mux(wbHit, wbData, rsData)))
        }

        val gprRs1Data = forwardGpr(ex1Pkg.rs1, ex1Pkg.rs1Data)
        val gprRs2Data = forwardGpr(ex1Pkg.rs2, ex1Pkg.rs2Data)
        val fprRs1Data = forwardFpr(ex1Pkg.rs1, ex1Pkg.rs1Data)
        val fprRs2Data = forwardFpr(ex1Pkg.rs2, ex1Pkg.rs2Data)

        io.fwdGprRs1Data(i) := gprRs1Data
        io.fwdGprRs2Data(i) := gprRs2Data
        io.fwdFprRs1Data(i) := fprRs1Data
        io.fwdFprRs2Data(i) := fprRs2Data
        
        // Slot 0 is FPR-only. Slots 3, 4 and 7 are GPR-only. LSU address
        // operands are GPRs, while FSW data can come from the FPR network.
        io.fwdRs1Data(i) := (i match {
            case 0 => fprRs1Data
            case 1 | 2 => Mux(ex1Pkg.rs1(5), fprRs1Data, gprRs1Data)
            case _ => gprRs1Data
        })
        io.fwdRs2Data(i) := (i match {
            case 0 => fprRs2Data
            case 1 | 2 | 5 | 6 => Mux(ex1Pkg.rs2(5), fprRs2Data, gprRs2Data)
            case _ => gprRs2Data
        })
        io.fwdRs3Data(i) := Mux(hasRs3.B, forwardFpr(ex1Pkg.rs3, ex1Pkg.rs3Data), 0.U)
    }
}
