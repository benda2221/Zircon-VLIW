import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp._

object ShallowDependencyTestUtil {
    def clear(pkg: InstructionPackage): Unit = {
        pkg.pc.poke(0.U)
        pkg.inst.poke(0.U)
        pkg.rs1.poke(0.U)
        pkg.rs2.poke(0.U)
        pkg.rs3.poke(0.U)
        pkg.rd.poke(0.U)
        pkg.rdValid.poke(false.B)
        pkg.rs1Data.poke(0.U)
        pkg.rs2Data.poke(0.U)
        pkg.rs3Data.poke(0.U)
        pkg.op.poke(0.U)
        pkg.rm.poke(0.U)
        pkg.imm.poke(0.U)
        pkg.src1Sel.poke(0.U)
        pkg.src2Sel.poke(0.U)
        pkg.shallowDepOK.poke(false.B)
        pkg.rs1ReadValid.poke(false.B)
        pkg.rs2ReadValid.poke(false.B)
        pkg.aluResult.poke(0.U)
        pkg.shallowRs1Sel.poke(0.U)
        pkg.shallowRs2Sel.poke(0.U)
        pkg.lateRs1Sel.poke(0.U)
        pkg.lateRs2Sel.poke(0.U)
        pkg.needsEX2Replay.poke(false.B)
        pkg.fpuResult.poke(0.U)
        pkg.fflags.poke(0.U)
        pkg.branchTgt.poke(0.U)
        pkg.predFail.poke(false.B)
        pkg.memResult.poke(0.U)
        pkg.rfWdata.poke(0.U)
    }
}

class ShallowDependencyForwardTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Forward shallow dependency path"

    it should "select the highest lower producer and forward its EX2 result" in {
        test(new Forward) { c =>
            for (i <- 0 until 8) {
                ShallowDependencyTestUtil.clear(c.io.ex1Pkgs(i))
                ShallowDependencyTestUtil.clear(c.io.ex2Pkgs(i))
                ShallowDependencyTestUtil.clear(c.io.ex3Pkgs(i))
                ShallowDependencyTestUtil.clear(c.io.wbPkgs(i))
            }

            // Lane 0 and lane 1 both write x5. Lane 3 reads x5; lane 1 wins.
            for (producer <- Seq(0, 1)) {
                c.io.ex1Pkgs(producer).shallowDepOK.poke(true.B)
                c.io.ex1Pkgs(producer).rdValid.poke(true.B)
                c.io.ex1Pkgs(producer).rd.poke(5.U)
            }
            c.io.ex1Pkgs(3).shallowDepOK.poke(true.B)
            c.io.ex1Pkgs(3).rs1ReadValid.poke(true.B)
            c.io.ex1Pkgs(3).rs1.poke(5.U)
            c.io.shallowRs1Sel(3).expect("b00000010".U)
            c.io.shallowRs2Sel(3).expect(0.U)
            c.io.needsEX2Replay(3).expect(true.B)

            // Lane 1 now has an EX2 replay ALU and may consume lane 0.
            c.io.ex1Pkgs(1).rs1ReadValid.poke(true.B)
            c.io.ex1Pkgs(1).rs1.poke(5.U)
            c.io.shallowRs1Sel(1).expect(1.U)
            c.io.needsEX2Replay(1).expect(true.B)

            // The selector is saved in the EX2 package and selects lane 1.
            c.io.ex2Pkgs(3).shallowRs1Sel.poke("b00000010".U)
            c.io.ex2Pkgs(1).aluResult.poke("h12345678".U)
            c.io.ex2Pkgs(3).rs2Data.poke("h89abcdef".U)
            c.io.replayRs1Data(3).expect("h12345678".U)
            c.io.replayRs2Data(3).expect("h89abcdef".U)

            // A replaying EX2 ALU result must not enter ordinary forwarding.
            c.io.ex1Pkgs(4).rs1ReadValid.poke(true.B)
            c.io.ex1Pkgs(4).rs1.poke(5.U)
            c.io.ex1Pkgs(4).rs1Data.poke("hdeadbeef".U)
            c.io.ex1Pkgs(4).shallowDepOK.poke(true.B)
            c.io.ex2Pkgs(1).rd.poke(5.U)
            c.io.ex2Pkgs(1).rdValid.poke(true.B)
            c.io.ex2Pkgs(1).needsEX2Replay.poke(true.B)
            c.io.fwdRs1Data(4).expect("hdeadbeef".U)

            // The same selected writer is classified as a cross-packet late source.
            c.io.lateRs1Sel(4).expect("b00000010".U)
            c.io.ex2Pkgs(4).lateRs1Sel.poke("b00000010".U)
            c.io.ex3Pkgs(1).aluResult.poke("hcafef00d".U)
            c.io.replayRs1Data(4).expect("hcafef00d".U)

            // A non-replay-capable consumer must wait for ordinary EX3 forwarding.
            c.io.ex1Pkgs(4).shallowDepOK.poke(false.B)
            c.io.lateRs1Sel(4).expect(0.U)
        }
    }
}

class ShallowDependencyDecoderTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Decoder shallow dependency metadata"

    it should "decode ALU and branch register-read validity" in {
        test(new Decoder(
            ALU = true, FPU = false, Branch = true,
            Mem = false, IMulDiv = false, FDiv = false
        )) { c =>
            ShallowDependencyTestUtil.clear(c.io.instPkgIn)

            // add x3, x1, x2
            c.io.instPkgIn.inst.poke("h002081b3".U)
            c.io.instPkgOut.shallowDepOK.expect(true.B)
            c.io.instPkgOut.rs1ReadValid.expect(true.B)
            c.io.instPkgOut.rs2ReadValid.expect(true.B)

            // addi x3, x1, 5
            c.io.instPkgIn.inst.poke("h00508193".U)
            c.io.instPkgOut.rs1ReadValid.expect(true.B)
            c.io.instPkgOut.rs2ReadValid.expect(false.B)

            // lui x3, 1
            c.io.instPkgIn.inst.poke("h000011b7".U)
            c.io.instPkgOut.rs1ReadValid.expect(false.B)
            c.io.instPkgOut.rs2ReadValid.expect(false.B)

            // beq x1, x2, 0
            c.io.instPkgIn.inst.poke("h00208063".U)
            c.io.instPkgOut.shallowDepOK.expect(true.B)
            c.io.instPkgOut.rs1ReadValid.expect(true.B)
            c.io.instPkgOut.rs2ReadValid.expect(true.B)

            // jalr x3, 0(x1)
            c.io.instPkgIn.inst.poke("h000081e7".U)
            c.io.instPkgOut.rs1ReadValid.expect(true.B)
            c.io.instPkgOut.rs2ReadValid.expect(false.B)

            // jal x3, 0
            c.io.instPkgIn.inst.poke("h000001ef".U)
            c.io.instPkgOut.rs1ReadValid.expect(false.B)
            c.io.instPkgOut.rs2ReadValid.expect(false.B)
        }
    }
}

class ShallowDependencyBranchTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "ALUBranchPipeline shallow dependency redirect"

    it should "suppress EX2 redirect and redirect with corrected operands in EX3" in {
        test(new ALUBranchPipeline) { c =>
            ShallowDependencyTestUtil.clear(c.io.backend.instPkgIn)
            c.io.forward.fwdRs1Data.poke(0.U)
            c.io.forward.fwdRs2Data.poke(7.U)
            c.io.forward.fwdRs3Data.poke(0.U)
            c.io.forward.shallowRs1Sel.poke(1.U)
            c.io.forward.shallowRs2Sel.poke(0.U)
            c.io.forward.lateRs1Sel.poke(0.U)
            c.io.forward.lateRs2Sel.poke(0.U)
            c.io.forward.needsEX2Replay.poke(true.B)
            c.io.forward.replayRs1Data.poke(7.U)
            c.io.forward.replayRs2Data.poke(7.U)
            c.io.hazard.ex1Flush.poke(false.B)
            c.io.hazard.ex1Stall.poke(false.B)
            c.io.hazard.ex2Flush.poke(false.B)
            c.io.hazard.ex2Stall.poke(false.B)
            c.io.hazard.ex3Flush.poke(false.B)
            c.io.hazard.ex3Stall.poke(false.B)
            c.io.hazard.wbFlush.poke(false.B)
            c.io.hazard.wbStall.poke(false.B)

            c.io.backend.instPkgIn.op.poke(BEQ)
            c.io.backend.instPkgIn.pc.poke("h100".U)
            c.io.backend.instPkgIn.imm.poke(8.U)
            c.io.backend.instPkgIn.rs1Data.poke(0.U)
            c.io.backend.instPkgIn.rs2Data.poke(7.U)

            c.clock.step()
            c.io.hazard.predFailEX2.expect(false.B)

            c.clock.step()
            c.io.hazard.predFailEX3.expect(true.B)
            c.io.hazard.branchTgtEX3.expect("h108".U)
        }
    }
}

class ShallowDependencyEX2ALUTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Pipeline EX2 shallow ALU"

    it should "replace the invalid EX1 result before entering EX3" in {
        test(new ALUiMDPipeline) { c =>
            ShallowDependencyTestUtil.clear(c.io.backend.instPkgIn)
            c.io.forward.fwdRs1Data.poke(1.U)
            c.io.forward.fwdRs2Data.poke(2.U)
            c.io.forward.fwdRs3Data.poke(0.U)
            c.io.forward.shallowRs1Sel.poke(1.U)
            c.io.forward.shallowRs2Sel.poke(0.U)
            c.io.forward.lateRs1Sel.poke(0.U)
            c.io.forward.lateRs2Sel.poke(0.U)
            c.io.forward.needsEX2Replay.poke(true.B)
            c.io.forward.replayRs1Data.poke(10.U)
            c.io.forward.replayRs2Data.poke(2.U)
            c.io.hazard.ex1Flush.poke(false.B)
            c.io.hazard.ex1Stall.poke(false.B)
            c.io.hazard.ex2Flush.poke(false.B)
            c.io.hazard.ex2Stall.poke(false.B)
            c.io.hazard.ex3Flush.poke(false.B)
            c.io.hazard.ex3Stall.poke(false.B)
            c.io.hazard.wbFlush.poke(false.B)
            c.io.hazard.wbStall.poke(false.B)

            c.io.backend.instPkgIn.op.poke(ADD)
            c.io.backend.instPkgIn.src1Sel.poke(0.U)
            c.io.backend.instPkgIn.src2Sel.poke(0.U)

            c.clock.step(2)
            c.io.forward.ex3Pkg.aluResult.expect(12.U)
            c.io.forward.ex3Pkg.needsEX2Replay.expect(true.B)
        }
    }
}

class LateForwardingBackendTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Backend late forwarding"

    it should "use the highest replaying writer from the previous packet" in {
        test(new Backend) { c =>
            def clearInputs(): Unit = {
                for (i <- 0 until 8) {
                    ShallowDependencyTestUtil.clear(c.io.frontend.instPkgs(i))
                    c.io.hazard.ex1Flush(i).poke(false.B)
                    c.io.hazard.ex1RawFlush(i).poke(false.B)
                    c.io.hazard.ex1BranchFlush(i).poke(false.B)
                    c.io.hazard.ex1Stall(i).poke(false.B)
                    c.io.hazard.ex2Flush(i).poke(false.B)
                    c.io.hazard.ex2Stall(i).poke(false.B)
                    c.io.hazard.ex3Flush(i).poke(false.B)
                    c.io.hazard.ex3Stall(i).poke(false.B)
                    c.io.hazard.wbFlush(i).poke(false.B)
                    c.io.hazard.wbStall(i).poke(false.B)
                }
                c.io.mem.lsu0.load.rdata.poke(0.U)
                c.io.mem.lsu1.load.rdata.poke(0.U)
            }

            clearInputs()

            // Packet P: lane 1 writes x5=10; lane 2 shallow-replays x5=15.
            val p1 = c.io.frontend.instPkgs(1)
            p1.inst.poke(1.U)
            p1.op.poke(ADD)
            p1.rd.poke(5.U)
            p1.rdValid.poke(true.B)
            p1.shallowDepOK.poke(true.B)
            p1.rs1.poke(6.U)
            p1.rs1ReadValid.poke(true.B)
            p1.rs1Data.poke(10.U)
            p1.rs2.poke(0.U)
            p1.rs2ReadValid.poke(true.B)
            p1.rs2Data.poke(0.U)

            val p2 = c.io.frontend.instPkgs(2)
            p2.inst.poke(1.U)
            p2.op.poke(ADD)
            p2.rd.poke(5.U)
            p2.rdValid.poke(true.B)
            p2.shallowDepOK.poke(true.B)
            p2.rs1.poke(5.U)
            p2.rs1ReadValid.poke(true.B)
            p2.rs1Data.poke(0.U)
            p2.rs2.poke(7.U)
            p2.rs2ReadValid.poke(true.B)
            p2.rs2Data.poke(5.U)
            c.clock.step()

            clearInputs()
            // Packet Q: lane 1 consumes the architecturally younger x5 writer.
            val q1 = c.io.frontend.instPkgs(1)
            q1.inst.poke(1.U)
            q1.op.poke(SUB)
            q1.rd.poke(8.U)
            q1.rdValid.poke(true.B)
            q1.shallowDepOK.poke(true.B)
            q1.rs1.poke(9.U)
            q1.rs1ReadValid.poke(true.B)
            q1.rs1Data.poke(100.U)
            q1.rs2.poke(5.U)
            q1.rs2ReadValid.poke(true.B)
            q1.rs2Data.poke(0.U)
            c.clock.step()

            clearInputs()
            c.io.hazard.ex1LateRs2Sel(1).expect("b00000100".U)
            c.io.hazard.ex1NeedsEX2Replay(1).expect(true.B)
            c.clock.step()

            c.clock.step(2)
            c.io.frontend.gprWen(1).expect(true.B)
            c.io.frontend.gprWdata(1).expect(85.U)
        }
    }
}

class ShallowDependencyHazardTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Hazard shallow dependency controls"

    it should "stall one following RAW and apply the wider EX3 redirect flush" in {
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
            c.io.backend.divBusy.foreach(_.poke(false.B))
            c.io.backend.fdivBusy.poke(false.B)
            c.io.backend.predFailEX2.poke(false.B)
            c.io.backend.branchTgtEX2.poke(0.U)
            c.io.backend.predFailEX3.poke(false.B)
            c.io.backend.branchTgtEX3.poke(0.U)

            c.io.backend.ex1Pkgs(2).rdValid.poke(true.B)
            c.io.backend.ex1Pkgs(2).rd.poke(5.U)
            c.io.backend.ex1Pkgs(2).op.poke(ADD)
            c.io.backend.ex1NeedsEX2Replay(2).poke(true.B)
            c.io.frontend.idPkgs(3).rs1ReadValid.poke(true.B)
            c.io.frontend.idPkgs(3).rs1.poke(5.U)
            c.io.frontend.idPkgs(3).shallowDepOK.poke(false.B)
            c.io.frontend.stall.expect(true.B)
            c.io.backend.ex1Flush.foreach(_.expect(true.B))

            // A replay-capable ALU consumer may enter EX1 and use late forwarding.
            c.io.frontend.idPkgs(3).shallowDepOK.poke(true.B)
            c.io.frontend.stall.expect(false.B)

            // The same rule must hold while the replay producer is in EX2;
            // MUL/DIV/FPU/LSU consumers cannot use the EX2 ALU replay path.
            c.io.backend.ex1NeedsEX2Replay(2).poke(false.B)
            c.io.backend.ex1Pkgs(2).rdValid.poke(false.B)
            c.io.backend.ex2Pkgs(2).rdValid.poke(true.B)
            c.io.backend.ex2Pkgs(2).rd.poke(5.U)
            c.io.backend.ex2Pkgs(2).op.poke(ADD)
            c.io.backend.ex2Pkgs(2).needsEX2Replay.poke(true.B)
            c.io.frontend.idPkgs(3).shallowDepOK.poke(false.B)
            c.io.frontend.stall.expect(true.B)
            c.io.frontend.idPkgs(3).shallowDepOK.poke(true.B)
            c.io.frontend.stall.expect(false.B)
            c.io.backend.ex2Pkgs(2).rdValid.poke(false.B)

            // Lane 2 needs late data and is the shallow producer selected by lane 3.
            c.io.backend.ex1Pkgs(2).rdValid.poke(true.B)
            c.io.backend.ex1NeedsEX2Replay(2).poke(true.B)
            c.io.backend.ex1LateRs1Sel(2).poke(1.U)
            c.io.backend.ex1ShallowRs1Sel(3).poke("b00000100".U)
            c.io.frontend.stall.expect(true.B)
            for (i <- 0 until 8) {
                c.io.backend.ex1Stall(i).expect(true.B)
                c.io.backend.ex2Stall(i).expect(true.B)
                c.io.backend.ex3Stall(i).expect(false.B)
                c.io.backend.ex1Flush(i).expect(false.B)
            }

            c.io.backend.ex1LateRs1Sel(2).poke(0.U)
            c.io.backend.ex1ShallowRs1Sel(3).poke(0.U)
            c.io.backend.ex1NeedsEX2Replay(2).poke(false.B)
            c.io.backend.ex1Pkgs(2).rdValid.poke(false.B)

            c.io.backend.predFailEX2.poke(true.B)
            c.io.backend.predFailEX3.poke(true.B)
            c.io.frontend.flush.expect(true.B)
            for (i <- 0 until 8) {
                c.io.backend.ex1Flush(i).expect(true.B)
                c.io.backend.ex2Flush(i).expect(true.B)
                c.io.backend.ex3Flush(i).expect(true.B)
                c.io.backend.wbFlush(i).expect(false.B)
            }
        }
    }
}

class ShallowDependencyCPUTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "CPU shallow dependency execution"

    it should "execute a LUI to ADDI dependency in one instruction packet" in {
        test(new CPU)
            .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            val nop = BigInt("00000013", 16)
            val firstPacket = Seq(
                nop,
                BigInt("000012b7", 16), // slot 1: lui  x5, 0x1
                BigInt("00728313", 16), // slot 2: addi x6, x5, 7
                nop, nop, nop, nop, nop
            )

            c.io.dmem.lsu0.load.rdata.poke(0.U)
            c.io.dmem.lsu1.load.rdata.poke(0.U)
            c.reset.poke(true.B)
            c.io.imem.insts.foreach(_.poke(nop.U))
            c.clock.step(2)
            c.reset.poke(false.B)

            for (_ <- 0 until 12) {
                val pc = c.io.imem.pc.peek().litValue
                val insts = if (pc == BigInt("80000000", 16)) firstPacket
                            else Seq.fill(8)(nop)
                for (slot <- 0 until 8)
                    c.io.imem.insts(slot).poke(insts(slot).U)
                c.clock.step()
            }

            c.io.debug.gpr(5).expect("h00001000".U)
            c.io.debug.gpr(6).expect("h00001007".U)
        }
    }

    it should "stall a late-to-shallow nested dependency for one cycle" in {
        test(new CPU)
            .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            val nop = BigInt("00000013", 16)
            val packetP = Seq(
                nop,
                BigInt("00a00293", 16), // addi x5, x0, 10
                BigInt("00528293", 16), // addi x5, x5, 5 (shallow replay)
                nop, nop, nop, nop, nop
            )
            val packetQ = Seq(
                nop,
                BigInt("00128313", 16), // addi x6, x5, 1 (late replay)
                BigInt("00230393", 16), // addi x7, x6, 2 (nested shallow)
                nop, nop, nop, nop, nop
            )

            c.io.dmem.lsu0.load.rdata.poke(0.U)
            c.io.dmem.lsu1.load.rdata.poke(0.U)
            c.reset.poke(true.B)
            c.io.imem.insts.foreach(_.poke(nop.U))
            c.clock.step(2)
            c.reset.poke(false.B)

            var sawNestedStall = false
            for (_ <- 0 until 16) {
                val pc = c.io.imem.pc.peek().litValue
                val insts = if (pc == BigInt("80000000", 16)) packetP
                            else if (pc == BigInt("80000020", 16)) packetQ
                            else Seq.fill(8)(nop)
                for (slot <- 0 until 8) {
                    c.io.imem.insts(slot).poke(insts(slot).U)
                }
                if (c.io.debug.hazardStall.peek().litToBoolean) {
                    sawNestedStall = true
                }
                c.clock.step()
            }

            assert(sawNestedStall, "nested late-to-shallow dependency did not stall")
            c.io.debug.gpr(5).expect(15.U)
            c.io.debug.gpr(6).expect(16.U)
            c.io.debug.gpr(7).expect(18.U)
        }
    }
}
