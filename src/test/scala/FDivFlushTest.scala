import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp._

// Exercise the real Hazard and FDIV pipeline together, including the clocked
// ID-EX1 boundary. Operand data is supplied directly by the test frontend.
class FDivFlushHarness extends Module {
    val io = IO(new Bundle {
        val idPkg = Input(new InstructionPackage)
        val redirectEX2 = Input(Bool())
        val redirectEX3 = Input(Bool())
        val intBusy = Input(Bool())
        val rawFlush = Output(Bool())
        val branchFlush = Output(Bool())
        val ex1Flush = Output(Bool())
        val ex2Stall = Output(Bool())
        val busy = Output(Bool())
        val ex1 = Output(new InstructionPackage)
        val ex2 = Output(new InstructionPackage)
        val ex3 = Output(new InstructionPackage)
        val wb = Output(new InstructionPackage)
    })
    val hazard = Module(new Hazard)
    val pipe = Module(new FDivFPUPipeline)
    val ex1 = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(hazard.io.backend.ex1Flush(0)) {
        ex1 := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!hazard.io.backend.ex1Stall(0)) {
        ex1 := io.idPkg
    }
    pipe.io.backend.instPkgIn := ex1
    pipe.io.forward.fwdRs1Data := ex1.rs1Data
    pipe.io.forward.fwdRs2Data := ex1.rs2Data
    pipe.io.forward.fwdRs3Data := ex1.rs3Data
    pipe.io.forward.shallowRs1Sel := 0.U
    pipe.io.forward.shallowRs2Sel := 0.U
    pipe.io.forward.lateRs1Sel := 0.U
    pipe.io.forward.lateRs2Sel := 0.U
    pipe.io.forward.needsEX2Replay := false.B
    pipe.io.forward.replayRs1Data := 0.U
    pipe.io.forward.replayRs2Data := 0.U
    pipe.io.hazard.ex1Flush := hazard.io.backend.ex1Flush(0)
    pipe.io.hazard.branchFlush := hazard.io.backend.ex1BranchFlush(0)
    pipe.io.hazard.ex1Stall := hazard.io.backend.ex1Stall(0)
    pipe.io.hazard.ex2Flush := hazard.io.backend.ex2Flush(0)
    pipe.io.hazard.ex2Stall := hazard.io.backend.ex2Stall(0)
    pipe.io.hazard.ex3Flush := hazard.io.backend.ex3Flush(0)
    pipe.io.hazard.ex3Stall := hazard.io.backend.ex3Stall(0)
    pipe.io.hazard.wbFlush := hazard.io.backend.wbFlush(0)
    pipe.io.hazard.wbStall := hazard.io.backend.wbStall(0)
    for (i <- 0 until 8) {
        hazard.io.frontend.idPkgs(i) := 0.U.asTypeOf(new InstructionPackage)
        hazard.io.backend.ex1Pkgs(i) := 0.U.asTypeOf(new InstructionPackage)
        hazard.io.backend.ex2Pkgs(i) := 0.U.asTypeOf(new InstructionPackage)
        hazard.io.backend.ex1ShallowRs1Sel(i) := 0.U
        hazard.io.backend.ex1ShallowRs2Sel(i) := 0.U
        hazard.io.backend.ex1LateRs1Sel(i) := 0.U
        hazard.io.backend.ex1LateRs2Sel(i) := 0.U
        hazard.io.backend.ex1NeedsEX2Replay(i) := false.B
    }
    hazard.io.frontend.idPkgs(0) := io.idPkg
    hazard.io.backend.ex1Pkgs(0) := pipe.io.hazard.ex1Pkg
    hazard.io.backend.ex2Pkgs(0) := pipe.io.hazard.ex2Pkg
    hazard.io.backend.divBusy(0) := io.intBusy
    hazard.io.backend.divBusy(1) := false.B
    hazard.io.backend.fdivBusy := pipe.io.hazard.fdivBusy
    hazard.io.backend.predFailEX2 := io.redirectEX2
    hazard.io.backend.predFailEX3 := io.redirectEX3
    hazard.io.backend.branchTgtEX2 := 0.U
    hazard.io.backend.branchTgtEX3 := 0.U
    io.rawFlush := hazard.io.backend.ex1RawFlush(0)
    io.branchFlush := hazard.io.backend.ex1BranchFlush(0)
    io.ex1Flush := hazard.io.backend.ex1Flush(0)
    io.ex2Stall := hazard.io.backend.ex2Stall(0)
    io.busy := pipe.io.hazard.fdivBusy
    io.ex1 := ex1
    io.ex2 := pipe.io.forward.ex2Pkg
    io.ex3 := pipe.io.forward.ex3Pkg
    io.wb := pipe.io.forward.wbPkg
}

class FDivFlushTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "FDIV flush causes"

    it should "preserve RAW producers and cancel only accepted branch paths" in {
        test(new FDivFlushHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            def clearID(): Unit = ShallowDependencyTestUtil.clear(c.io.idPkg)
            def resetCase(): Unit = {
                clearID()
                c.io.redirectEX2.poke(false.B)
                c.io.redirectEX3.poke(false.B)
                c.io.intBusy.poke(false.B)
                c.reset.poke(true.B)
                c.clock.step(2)
                c.reset.poke(false.B)
            }
            def issue(sqrt: Boolean = false): Unit = {
                clearID()
                c.io.idPkg.pc.poke(0x80000100L.U)
                c.io.idPkg.inst.poke((if (sqrt) 0x580100d3L else 0x183100d3L).U)
                c.io.idPkg.op.poke(if (sqrt) FSQRT_S else FDIV_S)
                c.io.idPkg.rdValid.poke(true.B)
                c.io.idPkg.rd.poke(33.U)
                c.io.idPkg.rs1.poke(34.U)
                c.io.idPkg.rs2.poke(35.U)
                c.io.idPkg.rs1ReadValid.poke(true.B)
                c.io.idPkg.rs2ReadValid.poke((!sqrt).B)
                c.io.idPkg.rs1Data.poke((if (sqrt) 0x41100000L else 0x40c00000L).U)
                c.io.idPkg.rs2Data.poke(0x40000000L.U)
                c.clock.step() // ID -> EX1; request has not yet been accepted.
                clearID()
            }
            def waitDone(): Unit = {
                var cycles = 0
                while (c.io.busy.peek().litToBoolean && cycles < 100) {
                    c.io.branchFlush.expect(false.B)
                    c.io.ex2Stall.expect(true.B)
                    c.clock.step()
                    cycles += 1
                }
                assert(cycles > 0 && cycles < 100, "FDIV must start and finish")
            }
            def drain(expectedWrites: Int): Unit = {
                var writes = 0
                for (_ <- 0 until 8) {
                    if (c.io.wb.rdValid.peek().litToBoolean) {
                        writes += 1
                        c.io.wb.pc.expect(0x80000100L.U)
                        c.io.wb.rfWdata.expect(0x40400000L.U) // 6 / 2 or sqrt(9)
                        c.io.wb.fflags.expect(0.U)
                    }
                    c.clock.step()
                }
                assert(writes == expectedWrites, s"unexpected FDIV write count: $writes")
            }

            // RAW is a bubble at the ID-EX1 boundary, not an execution cancel.
            for (sqrt <- Seq(false, true); consumer <- Seq(FADD_S, FSW)) {
                resetCase()
                issue(sqrt)
                c.io.idPkg.op.poke(consumer)
                if (consumer.litValue == FSW.litValue) {
                    c.io.idPkg.rs2ReadValid.poke(true.B)
                    c.io.idPkg.rs2.poke(33.U)
                } else {
                    c.io.idPkg.rs1ReadValid.poke(true.B)
                    c.io.idPkg.rs1.poke(33.U)
                }
                c.io.rawFlush.expect(true.B)
                c.io.ex1Flush.expect(true.B)
                c.io.branchFlush.expect(false.B)
                c.io.ex2Stall.expect(false.B)
                c.clock.step()
                c.io.ex1.rdValid.expect(false.B)
                c.io.ex2.rdValid.expect(true.B)
                c.io.busy.expect(true.B)
                clearID()
                waitDone()
                drain(1)
            }

            // Either redirect takes precedence over a simultaneous RAW and
            // prevents the younger EX1 request from starting or committing.
            for (ex3 <- Seq(false, true)) {
                resetCase()
                issue()
                c.io.idPkg.rs1ReadValid.poke(true.B)
                c.io.idPkg.rs1.poke(33.U)
                c.io.redirectEX2.poke((!ex3).B)
                c.io.redirectEX3.poke(ex3.B)
                c.io.branchFlush.expect(true.B)
                c.io.rawFlush.expect(false.B)
                c.clock.step()
                c.io.busy.expect(false.B)
                clearID()
                c.io.redirectEX2.poke(false.B)
                c.io.redirectEX3.poke(false.B)
                drain(0)
            }

            // A redirect is deferred throughout busy. EX2 redirect preserves
            // its same-package FDIV; EX3 redirect discards the younger EX2 FDIV.
            for (ex3 <- Seq(false, true)) {
                resetCase()
                issue()
                c.clock.step()
                c.io.redirectEX2.poke((!ex3).B)
                c.io.redirectEX3.poke(ex3.B)
                waitDone()
                c.io.branchFlush.expect(true.B)
                c.clock.step()
                c.io.redirectEX2.poke(false.B)
                c.io.redirectEX3.poke(false.B)
                drain(if (ex3) 0 else 1)
            }

            // A completed older FDIV already in EX3 survives an EX2 redirect.
            resetCase()
            issue()
            c.clock.step()
            waitDone()
            c.clock.step()
            c.io.redirectEX2.poke(true.B)
            c.clock.step()
            c.io.redirectEX2.poke(false.B)
            drain(1)
        }
    }

    it should "preserve the control truth table while exposing mutually exclusive causes" in {
        test(new Hazard) { c =>
            for (i <- 0 until 8) {
                ShallowDependencyTestUtil.clear(c.io.frontend.idPkgs(i))
                ShallowDependencyTestUtil.clear(c.io.backend.ex1Pkgs(i))
                ShallowDependencyTestUtil.clear(c.io.backend.ex2Pkgs(i))
                c.io.backend.ex1ShallowRs1Sel(i).poke(0.U)
                c.io.backend.ex1ShallowRs2Sel(i).poke(0.U)
                c.io.backend.ex1LateRs1Sel(i).poke(0.U)
                c.io.backend.ex1LateRs2Sel(i).poke(0.U)
                c.io.backend.ex1NeedsEX2Replay(i).poke(false.B)
            }
            c.io.backend.branchTgtEX2.poke(0.U)
            c.io.backend.branchTgtEX3.poke(0.U)
            c.io.backend.ex1Pkgs(0).op.poke(FDIV_S)
            c.io.backend.ex1Pkgs(0).rd.poke(33.U)
            c.io.backend.ex1Pkgs(0).rdValid.poke(true.B)
            c.io.frontend.idPkgs(5).rs2.poke(33.U)
            for (busyMask <- 0 until 8; raw <- Seq(false, true);
                 ex2 <- Seq(false, true); ex3 <- Seq(false, true)) {
                c.io.backend.divBusy(0).poke(((busyMask & 1) != 0).B)
                c.io.backend.divBusy(1).poke(((busyMask & 2) != 0).B)
                c.io.backend.fdivBusy.poke(((busyMask & 4) != 0).B)
                c.io.frontend.idPkgs(5).rs2ReadValid.poke(raw.B)
                c.io.backend.predFailEX2.poke(ex2.B)
                c.io.backend.predFailEX3.poke(ex3.B)
                val busy = busyMask != 0
                val branch = !busy && (ex2 || ex3)
                val bubble = !busy && !ex2 && !ex3 && raw
                c.io.frontend.flush.expect(branch.B)
                c.io.frontend.stall.expect((busy || bubble).B)
                for (i <- 0 until 8) {
                    c.io.backend.ex1RawFlush(i).expect(bubble.B)
                    c.io.backend.ex1BranchFlush(i).expect(branch.B)
                    c.io.backend.ex1Flush(i).expect((bubble || branch).B)
                    c.io.backend.ex1Stall(i).expect(busy.B)
                    c.io.backend.ex2Stall(i).expect(busy.B)
                    c.io.backend.ex3Stall(i).expect(busy.B)
                    c.io.backend.ex2Flush(i).expect(branch.B)
                    c.io.backend.ex3Flush(i).expect((!busy && ex3).B)
                    c.io.backend.wbFlush(i).expect(busy.B)
                    c.io.backend.wbStall(i).expect(false.B)
                }
            }
        }
    }
}
