import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp._

class MulGlobalStallTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "MUL under global divider stall"

    private def initialize(c: ALUiMDPipeline): Unit = {
        ShallowDependencyTestUtil.clear(c.io.backend.instPkgIn)
        c.io.forward.fwdRs1Data.poke(0.U)
        c.io.forward.fwdRs2Data.poke(0.U)
        c.io.forward.fwdRs3Data.poke(0.U)
        c.io.forward.shallowRs1Sel.poke(0.U)
        c.io.forward.shallowRs2Sel.poke(0.U)
        c.io.forward.lateRs1Sel.poke(0.U)
        c.io.forward.lateRs2Sel.poke(0.U)
        c.io.forward.needsEX2Replay.poke(false.B)
        c.io.forward.replayRs1Data.poke(0.U)
        c.io.forward.replayRs2Data.poke(0.U)
        c.io.hazard.ex1Flush.poke(false.B)
        c.io.hazard.ex2Flush.poke(false.B)
        c.io.hazard.ex3Flush.poke(false.B)
        c.io.hazard.ex1Stall.poke(false.B)
        c.io.hazard.ex2Stall.poke(false.B)
        c.io.hazard.ex3Stall.poke(false.B)
        c.io.hazard.wbStall.poke(false.B)
        c.io.hazard.wbFlush.poke(false.B)
    }

    for (holdCycles <- Seq(0, 1, 20, 45)) {
        it should s"preserve 32 times 1 across $holdCycles global stall cycles" in {
            test(new ALUiMDPipeline).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
                initialize(c)
                c.io.forward.fwdRs1Data.poke(32.U)
                c.io.forward.fwdRs2Data.poke(1.U)
                c.io.backend.instPkgIn.op.poke(MUL)
                c.io.backend.instPkgIn.rdValid.poke(true.B)
                c.io.backend.instPkgIn.rd.poke(21.U)
                c.clock.step() // MUL enters EX2 on the FDIV startup edge.
                ShallowDependencyTestUtil.clear(c.io.backend.instPkgIn)
                c.io.forward.fwdRs1Data.poke(0.U)
                c.io.forward.fwdRs2Data.poke(0.U)
                if (holdCycles > 0) {
                    c.io.hazard.ex1Stall.poke(true.B)
                    c.io.hazard.ex2Stall.poke(true.B)
                    c.io.hazard.ex3Stall.poke(true.B)
                    c.io.hazard.wbFlush.poke(true.B)
                    // Model FDIV/another lane holding the pipeline, while this
                    // lane's integer divider remains idle.
                    c.io.hazard.divBusy.expect(false.B)
                    c.clock.step(holdCycles)
                    c.io.hazard.ex1Stall.poke(false.B)
                    c.io.hazard.ex2Stall.poke(false.B)
                    c.io.hazard.ex3Stall.poke(false.B)
                    c.io.hazard.wbFlush.poke(false.B)
                }
                c.clock.step(2)
                c.io.frontend.gprWen.expect(true.B)
                c.io.frontend.gprWaddr.expect(21.U)
                c.io.frontend.gprWdata.expect(32.U)
                c.clock.step()
                c.io.frontend.gprWen.expect(false.B)
            }
        }
    }

    it should "advance an EX2 multiply while a nested replay drains EX2" in {
        test(new ALUiMDPipeline).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            initialize(c)
            c.io.forward.fwdRs1Data.poke(7.U)
            c.io.forward.fwdRs2Data.poke(9.U)
            c.io.backend.instPkgIn.op.poke(MUL)
            c.io.backend.instPkgIn.rdValid.poke(true.B)
            c.io.backend.instPkgIn.rd.poke(21.U)
            c.clock.step() // MUL enters EX2 and multiplier stage 1.

            ShallowDependencyTestUtil.clear(c.io.backend.instPkgIn)
            c.io.forward.fwdRs1Data.poke(0.U)
            c.io.forward.fwdRs2Data.poke(0.U)
            c.io.hazard.ex1Stall.poke(true.B)
            c.io.hazard.ex2Stall.poke(true.B)
            c.io.hazard.ex3Stall.poke(false.B)
            c.clock.step() // MUL drains into EX3; multiplier must advance.

            c.io.hazard.ex1Stall.poke(false.B)
            c.io.hazard.ex2Stall.poke(false.B)
            c.clock.step()
            c.io.frontend.gprWen.expect(true.B)
            c.io.frontend.gprWaddr.expect(21.U)
            c.io.frontend.gprWdata.expect(63.U)
        }
    }
}
