import chisel3._
import chisel3.util._

class ForwardIO extends Bundle {
    val ex1Pkgs = Input(Vec(8, new InstructionPackage))
    val ex2Pkgs = Input(Vec(8, new InstructionPackage))
    val ex3Pkgs = Input(Vec(8, new InstructionPackage))
    val wbPkgs = Input(Vec(8, new InstructionPackage))

    // Ordinary EX2/EX3/WB forwarding into EX1.
    val fwdRs1Data = Output(Vec(8, UInt(32.W)))
    val fwdRs2Data = Output(Vec(8, UInt(32.W)))
    val fwdRs3Data = Output(Vec(8, UInt(32.W)))

    // Dependency decisions made in EX1 and saved with the instruction package.
    val shallowRs1Sel = Output(Vec(8, UInt(8.W)))
    val shallowRs2Sel = Output(Vec(8, UInt(8.W)))
    val lateRs1Sel = Output(Vec(8, UInt(8.W)))
    val lateRs2Sel = Output(Vec(8, UInt(8.W)))
    val needsEX2Replay = Output(Vec(8, Bool()))

    // EX2 operands selected only from the saved selectors. No register compare
    // or priority encoder is present on this path.
    val replayRs1Data = Output(Vec(8, UInt(32.W)))
    val replayRs2Data = Output(Vec(8, UInt(32.W)))
}

class Forward extends Module {
    val io = IO(new ForwardIO)

    private def highestMatchOH(matches: Vec[Bool]): UInt = {
        MuxCase(
            0.U(8.W),
            (7 to 0 by -1).map { lane =>
                matches(lane) -> (BigInt(1) << lane).U(8.W)
            }
        )
    }

    private def selectedAluData(sel: UInt, pkgs: Vec[InstructionPackage]): UInt =
        Mux1H(sel, pkgs.map(_.aluResult))

    for (i <- 0 until 8) {
        val ex1Pkg = io.ex1Pkgs(i)

        def forwardRs(rsAddr: UInt, rsData: UInt): UInt = {
            val ex2Matches = Wire(Vec(8, Bool()))
            val ex3Matches = Wire(Vec(8, Bool()))
            val wbMatches = Wire(Vec(8, Bool()))

            for (j <- 0 until 8) {
                ex2Matches(j) := io.ex2Pkgs(j).rdValid &&
                    !io.ex2Pkgs(j).needsEX2Replay &&
                    io.ex2Pkgs(j).rd === rsAddr
                ex3Matches(j) := io.ex3Pkgs(j).rdValid &&
                    io.ex3Pkgs(j).rd === rsAddr
                wbMatches(j) := io.wbPkgs(j).rdValid &&
                    io.wbPkgs(j).rd === rsAddr
            }

            val ex2Sel = highestMatchOH(ex2Matches)
            val ex3Sel = highestMatchOH(ex3Matches)
            val wbSel = highestMatchOH(wbMatches)

            Mux(ex2Sel.orR, selectedAluData(ex2Sel, io.ex2Pkgs),
                Mux(ex3Sel.orR, selectedAluData(ex3Sel, io.ex3Pkgs),
                    Mux(wbSel.orR, Mux1H(wbSel, io.wbPkgs.map(_.rfWdata)), rsData)))
        }

        io.fwdRs1Data(i) := Mux(
            ex1Pkg.rs1ReadValid,
            forwardRs(ex1Pkg.rs1, ex1Pkg.rs1Data),
            ex1Pkg.rs1Data
        )
        io.fwdRs2Data(i) := Mux(
            ex1Pkg.rs2ReadValid,
            forwardRs(ex1Pkg.rs2, ex1Pkg.rs2Data),
            ex1Pkg.rs2Data
        )
        io.fwdRs3Data(i) := Mux(
            (i < 3).B,
            forwardRs(ex1Pkg.rs3, ex1Pkg.rs3Data),
            0.U
        )

        // Same-packet dependency. Only lower lanes may produce for this lane.
        val shallowRs1Matches = WireInit(VecInit(Seq.fill(8)(false.B)))
        val shallowRs2Matches = WireInit(VecInit(Seq.fill(8)(false.B)))
        for (j <- 0 until i) {
            val producerOK = io.ex1Pkgs(j).rdValid && io.ex1Pkgs(j).shallowDepOK
            val consumerOK = ex1Pkg.shallowDepOK
            shallowRs1Matches(j) := producerOK && consumerOK &&
                ex1Pkg.rs1ReadValid && io.ex1Pkgs(j).rd === ex1Pkg.rs1
            shallowRs2Matches(j) := producerOK && consumerOK &&
                ex1Pkg.rs2ReadValid && io.ex1Pkgs(j).rd === ex1Pkg.rs2
        }
        io.shallowRs1Sel(i) := highestMatchOH(shallowRs1Matches)
        io.shallowRs2Sel(i) := highestMatchOH(shallowRs2Matches)

        // Cross-packet late dependency. Select the architecturally youngest
        // writer first, then classify that selected writer as replay/non-replay.
        val ex2Rs1Matches = Wire(Vec(8, Bool()))
        val ex2Rs2Matches = Wire(Vec(8, Bool()))
        for (j <- 0 until 8) {
            ex2Rs1Matches(j) := io.ex2Pkgs(j).rdValid &&
                ex1Pkg.rs1ReadValid && io.ex2Pkgs(j).rd === ex1Pkg.rs1
            ex2Rs2Matches(j) := io.ex2Pkgs(j).rdValid &&
                ex1Pkg.rs2ReadValid && io.ex2Pkgs(j).rd === ex1Pkg.rs2
        }
        val ex2Rs1Writer = highestMatchOH(ex2Rs1Matches)
        val ex2Rs2Writer = highestMatchOH(ex2Rs2Matches)
        val ex2ReplayMask = VecInit(io.ex2Pkgs.map(_.needsEX2Replay)).asUInt
        io.lateRs1Sel(i) := Mux(
            ex1Pkg.shallowDepOK,
            ex2Rs1Writer & ex2ReplayMask,
            0.U
        )
        io.lateRs2Sel(i) := Mux(
            ex1Pkg.shallowDepOK,
            ex2Rs2Writer & ex2ReplayMask,
            0.U
        )

        io.needsEX2Replay(i) :=
            io.shallowRs1Sel(i).orR ||
            io.shallowRs2Sel(i).orR ||
            io.lateRs1Sel(i).orR ||
            io.lateRs2Sel(i).orR

        val shallowRs1Data =
            selectedAluData(io.ex2Pkgs(i).shallowRs1Sel, io.ex2Pkgs)
        val shallowRs2Data =
            selectedAluData(io.ex2Pkgs(i).shallowRs2Sel, io.ex2Pkgs)
        val lateRs1Data =
            selectedAluData(io.ex2Pkgs(i).lateRs1Sel, io.ex3Pkgs)
        val lateRs2Data =
            selectedAluData(io.ex2Pkgs(i).lateRs2Sel, io.ex3Pkgs)

        val rs1AfterLate = Mux(
            io.ex2Pkgs(i).lateRs1Sel.orR,
            lateRs1Data,
            io.ex2Pkgs(i).rs1Data
        )
        val rs2AfterLate = Mux(
            io.ex2Pkgs(i).lateRs2Sel.orR,
            lateRs2Data,
            io.ex2Pkgs(i).rs2Data
        )
        io.replayRs1Data(i) := Mux(
            io.ex2Pkgs(i).shallowRs1Sel.orR,
            shallowRs1Data,
            rs1AfterLate
        )
        io.replayRs2Data(i) := Mux(
            io.ex2Pkgs(i).shallowRs2Sel.orR,
            shallowRs2Data,
            rs2AfterLate
        )
    }
}
