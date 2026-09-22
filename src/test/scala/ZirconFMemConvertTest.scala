import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

case class ConvertVector(op: Int, input: Long, rm: Int, result: Long, flags: Int)

class ZirconFMemConvertTest extends AnyFlatSpec with ChiselScalatestTester {
    private val FCVT_W_S = 0x58
    private val FCVT_WU_S = 0x59
    private val FCVT_S_W = 0x5a
    private val FCVT_S_WU = 0x5b
    private val FMV_X_W = 0x5c
    private val FMV_W_X = 0x5e

    private val FLW = 0x54
    private val FSW = 0x62

    private def u32(value: Int): Long = value.toLong & 0xffffffffL

    private def clearPackage(pkg: InstructionPackage): Unit = {
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

    private def initializeFPU(dut: FPU): Unit = {
        dut.io.rs1Data.poke(0.U)
        dut.io.rs2Data.poke(0.U)
        dut.io.rs3Data.poke(0.U)
        dut.io.op.poke(0.U)
        dut.io.rm.poke(0.U)
        dut.io.control.s1Enable.poke(true.B)
        dut.io.control.s2Enable.poke(true.B)
        dut.io.control.s1Flush.poke(true.B)
        dut.io.control.s2Flush.poke(true.B)
        dut.clock.step()
        dut.io.control.s1Flush.poke(false.B)
        dut.io.control.s2Flush.poke(false.B)
    }

    private def runConversions(dut: FPU, vectors: Seq[ConvertVector]): Unit = {
        for (cycle <- 0 to vectors.length) {
            if (cycle < vectors.length) {
                val vector = vectors(cycle)
                dut.io.rs1Data.poke(vector.input.U)
                dut.io.op.poke(vector.op.U)
                dut.io.rm.poke(vector.rm.U)
            } else {
                dut.io.rs1Data.poke(0.U)
                dut.io.op.poke(0.U)
                dut.io.rm.poke(0.U)
            }
            dut.clock.step()

            if (cycle >= 1) {
                val expected = vectors(cycle - 1)
                val context = f"op=0x${expected.op}%02x input=0x${expected.input}%08x rm=${expected.rm}"
                dut.io.res.expect(expected.result.U, context)
                dut.io.fflags.expect(expected.flags.U, context)
            }
        }
    }

    private def opFp(funct7: Int, rs2: Int, rs1: Int, funct3: Int, rd: Int): Long =
        u32((funct7 << 25) | (rs2 << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | 0x53)

    private def addi(rd: Int, rs1: Int, imm: Int): Long =
        u32(((imm & 0xfff) << 20) | (rs1 << 15) | (rd << 7) | 0x13)

    private def flw(rd: Int, rs1: Int, imm: Int): Long =
        u32(((imm & 0xfff) << 20) | (rs1 << 15) | (2 << 12) | (rd << 7) | 0x07)

    private def fsw(rs2: Int, rs1: Int, imm: Int): Long = {
        val encodedImm = imm & 0xfff
        u32(((encodedImm >> 5) << 25) | (rs2 << 20) | (rs1 << 15) |
            (2 << 12) | ((encodedImm & 0x1f) << 7) | 0x27)
    }

    private def fcvtWS(rd: Int, rs1: Int, rm: Int): Long =
        opFp(0x60, 0, rs1, rm, rd)

    private def fcvtSW(rd: Int, rs1: Int, rm: Int): Long =
        opFp(0x68, 0, rs1, rm, rd)

    behavior of "RV32F load/store and integer conversions"

    it should "decode mixed GPR/FPR operands independently" in {
        test(new Decoder(ALU = false, FPU = false, Branch = false, Mem = true, IMulDiv = false, FDiv = false)) { dut =>
            clearPackage(dut.io.instPkgIn)

            dut.io.instPkgIn.inst.poke(fsw(rs2 = 7, rs1 = 3, imm = 20).U)
            dut.clock.step()
            dut.io.instPkgOut.op.expect(FSW.U)
            dut.io.instPkgOut.rs1.expect(3.U)       // x3
            dut.io.instPkgOut.rs2.expect((32 + 7).U) // f7

            dut.io.instPkgIn.inst.poke(flw(rd = 5, rs1 = 4, imm = 12).U)
            dut.clock.step()
            dut.io.instPkgOut.op.expect(FLW.U)
            dut.io.instPkgOut.rs1.expect(4.U)        // x4
            dut.io.instPkgOut.rd.expect((32 + 5).U)  // f5
        }

        test(new Decoder(ALU = false, FPU = true, Branch = false, Mem = false, IMulDiv = false, FDiv = false)) { dut =>
            clearPackage(dut.io.instPkgIn)

            dut.io.instPkgIn.inst.poke(fcvtWS(rd = 9, rs1 = 6, rm = 1).U)
            dut.clock.step()
            dut.io.instPkgOut.rs1.expect((32 + 6).U) // f6
            dut.io.instPkgOut.rd.expect(9.U)         // x9

            dut.io.instPkgIn.inst.poke(fcvtSW(rd = 10, rs1 = 8, rm = 0).U)
            dut.clock.step()
            dut.io.instPkgOut.rs1.expect(8.U)         // x8
            dut.io.instPkgOut.rd.expect((32 + 10).U)  // f10
        }
    }

    it should "pass the official rv32uf conversion cases" in {
        // From riscv-tests rv32uf/fcvt.S and rv32uf/fcvt_w.S.
        val vectors = Seq(
            ConvertVector(FCVT_W_S, 0xbf8ccccdL, 1, 0xffffffffL, 0x01), // -1.1 -> -1
            ConvertVector(FCVT_W_S, 0xbf800000L, 1, 0xffffffffL, 0x00),
            ConvertVector(FCVT_W_S, 0xbf666666L, 1, 0x00000000L, 0x01),
            ConvertVector(FCVT_W_S, 0x3f666666L, 1, 0x00000000L, 0x01),
            ConvertVector(FCVT_W_S, 0x3f800000L, 1, 0x00000001L, 0x00),
            ConvertVector(FCVT_W_S, 0x3f8ccccdL, 1, 0x00000001L, 0x01),
            ConvertVector(FCVT_W_S, 0xcf32d05eL, 1, 0x80000000L, 0x10),
            ConvertVector(FCVT_W_S, 0x4f32d05eL, 1, 0x7fffffffL, 0x10),
            ConvertVector(FCVT_WU_S, 0xc0400000L, 1, 0x00000000L, 0x10),
            ConvertVector(FCVT_WU_S, 0xbf800000L, 1, 0x00000000L, 0x10),
            ConvertVector(FCVT_WU_S, 0xbf666666L, 1, 0x00000000L, 0x01),
            ConvertVector(FCVT_WU_S, 0x3f666666L, 1, 0x00000000L, 0x01),
            ConvertVector(FCVT_WU_S, 0x3f800000L, 1, 0x00000001L, 0x00),
            ConvertVector(FCVT_WU_S, 0x3f8ccccdL, 1, 0x00000001L, 0x01),
            ConvertVector(FCVT_WU_S, 0xcf32d05eL, 1, 0x00000000L, 0x10),
            ConvertVector(FCVT_WU_S, 0x4f32d05eL, 1, 0xb2d05e00L, 0x00),
            ConvertVector(FCVT_W_S, 0xffffffffL, 0, 0x7fffffffL, 0x10),
            ConvertVector(FCVT_W_S, 0xff800000L, 0, 0x80000000L, 0x10),
            ConvertVector(FCVT_W_S, 0x7fffffffL, 0, 0x7fffffffL, 0x10),
            ConvertVector(FCVT_W_S, 0x7f800000L, 0, 0x7fffffffL, 0x10),
            ConvertVector(FCVT_WU_S, 0xffffffffL, 0, 0xffffffffL, 0x10),
            ConvertVector(FCVT_WU_S, 0xff800000L, 0, 0x00000000L, 0x10),
            ConvertVector(FCVT_WU_S, 0x7fffffffL, 0, 0xffffffffL, 0x10),
            ConvertVector(FCVT_WU_S, 0x7f800000L, 0, 0xffffffffL, 0x10),
            ConvertVector(FCVT_S_W, 0x00000002L, 0, 0x40000000L, 0x00),
            ConvertVector(FCVT_S_W, 0xfffffffeL, 0, 0xc0000000L, 0x00),
            ConvertVector(FCVT_S_WU, 0x00000002L, 0, 0x40000000L, 0x00),
            ConvertVector(FCVT_S_WU, 0xfffffffeL, 0, 0x4f800000L, 0x01)
        )

        test(new FPU) { dut =>
            initializeFPU(dut)
            runConversions(dut, vectors)
        }
    }

    it should "match 2000 Berkeley SoftFloat conversion vectors in every rounding mode" in {
        val stream = Option(getClass.getResourceAsStream("/softfloat-convert-vectors.csv"))
            .getOrElse(fail("missing SoftFloat conversion vector resource"))
        val source = Source.fromInputStream(stream)
        val vectors = try {
            source.getLines().drop(1).filter(_.nonEmpty).map { line =>
                val fields = line.split(',')
                val op = fields(0) match {
                    case "f32_to_i32" => FCVT_W_S
                    case "f32_to_ui32" => FCVT_WU_S
                    case "i32_to_f32" => FCVT_S_W
                    case "ui32_to_f32" => FCVT_S_WU
                }
                ConvertVector(
                    op,
                    java.lang.Long.parseUnsignedLong(fields(1), 16),
                    fields(2).toInt,
                    java.lang.Long.parseUnsignedLong(fields(3), 16),
                    Integer.parseUnsignedInt(fields(4), 16)
                )
            }.toVector
        } finally {
            source.close()
        }

        assert(vectors.length == 2000)
        test(new FPU) { dut =>
            initializeFPU(dut)
            runConversions(dut, vectors)
        }
    }

    it should "move all 32 payload bits between integer and floating-point registers" in {
        val payloads = Seq(0x00000000L, 0x80000000L, 0x7fc12345L, 0xffffffffL, 0x12345678L)
        val vectors = payloads.flatMap { payload =>
            Seq(
                ConvertVector(FMV_X_W, payload, 0, payload, 0),
                ConvertVector(FMV_W_X, payload, 0, payload, 0)
            )
        }
        test(new FPU) { dut =>
            initializeFPU(dut)
            runConversions(dut, vectors)
        }
    }

    it should "execute both conversion directions in FPU slot zero" in {
        test(new FDivFPUPipeline) { dut =>
            clearPackage(dut.io.backend.instPkgIn)
            dut.io.forward.fwdRs1Data.poke(0.U)
            dut.io.forward.fwdRs2Data.poke(0.U)
            dut.io.forward.fwdRs3Data.poke(0.U)
            dut.io.forward.shallowRs1Sel.poke(0.U)
            dut.io.forward.shallowRs2Sel.poke(0.U)
            dut.io.forward.lateRs1Sel.poke(0.U)
            dut.io.forward.lateRs2Sel.poke(0.U)
            dut.io.forward.needsEX2Replay.poke(false.B)
            dut.io.forward.replayRs1Data.poke(0.U)
            dut.io.forward.replayRs2Data.poke(0.U)
            dut.io.hazard.ex1Flush.poke(false.B)
            dut.io.hazard.branchFlush.poke(false.B)
            dut.io.hazard.ex1Stall.poke(false.B)
            dut.io.hazard.ex2Flush.poke(false.B)
            dut.io.hazard.ex2Stall.poke(false.B)
            dut.io.hazard.ex3Flush.poke(false.B)
            dut.io.hazard.ex3Stall.poke(false.B)
            dut.io.hazard.wbFlush.poke(false.B)
            dut.io.hazard.wbStall.poke(false.B)

            // Slot 0: fcvt.s.w f1, x3
            dut.io.backend.instPkgIn.op.poke(FCVT_S_W.U)
            dut.io.backend.instPkgIn.rm.poke(0.U)
            dut.io.backend.instPkgIn.rdValid.poke(true.B)
            dut.io.backend.instPkgIn.rd.poke((32 + 1).U)
            dut.io.forward.fwdRs1Data.poke(3.U)
            dut.clock.step()

            // Slot 0: fcvt.w.s x2, f4
            dut.io.backend.instPkgIn.op.poke(FCVT_W_S.U)
            dut.io.backend.instPkgIn.rm.poke(1.U)
            dut.io.backend.instPkgIn.rd.poke(2.U)
            dut.io.forward.fwdRs1Data.poke(0x40400000L.U)
            dut.clock.step()

            clearPackage(dut.io.backend.instPkgIn)
            dut.io.forward.fwdRs1Data.poke(0.U)
            dut.clock.step()
            dut.io.frontend.fprWen.expect(true.B)
            dut.io.frontend.fprWaddr.expect(1.U)
            dut.io.frontend.fprWdata.expect(0x40400000L.U)

            dut.clock.step()
            dut.io.frontend.gprWen.expect(true.B)
            dut.io.frontend.gprWaddr.expect(2.U)
            dut.io.frontend.gprWdata.expect(3.U)
        }
    }

    it should "read FSW data from the FPR and accept both LSU FLW write ports" in {
        val nop = addi(0, 0, 0)
        test(new Frontend) { dut =>
            for (i <- 0 until 8) {
                dut.io.mem.insts(i).poke(nop.U)
                dut.io.backend.gprWen(i).poke(false.B)
                dut.io.backend.gprWaddr(i).poke(0.U)
                dut.io.backend.gprWdata(i).poke(0.U)
            }
            for (i <- 0 until 5) {
                dut.io.backend.fprWen(i).poke(false.B)
                dut.io.backend.fprWaddr(i).poke(0.U)
                dut.io.backend.fprWdata(i).poke(0.U)
            }
            dut.io.backend.branchTgt.poke(0.U)
            dut.io.backend.predFail.poke(false.B)
            dut.io.hazard.flush.poke(false.B)
            dut.io.hazard.stall.poke(false.B)

            // Seed x3 through a GPR write port and f7 through the first LSU/FLW
            // FPR write port (Backend maps pipeline 5 to port 3).
            dut.io.backend.gprWen(0).poke(true.B)
            dut.io.backend.gprWaddr(0).poke(3.U)
            dut.io.backend.gprWdata(0).poke(0x100.U)
            dut.io.backend.fprWen(3).poke(true.B)
            dut.io.backend.fprWaddr(3).poke(7.U)
            dut.io.backend.fprWdata(3).poke(0x40400000L.U)
            dut.clock.step()
            dut.io.backend.gprWen(0).poke(false.B)
            dut.io.backend.fprWen(3).poke(false.B)

            dut.io.mem.insts(5).poke(fsw(rs2 = 7, rs1 = 3, imm = 4).U)
            dut.clock.step()
            dut.io.backend.instPkg(5).rs1.expect(3.U)
            dut.io.backend.instPkg(5).rs2.expect((32 + 7).U)
            dut.io.backend.instPkg(5).rs1Data.expect(0x100.U)
            dut.io.backend.instPkg(5).rs2Data.expect(0x40400000L.U)

            // The second LSU/FLW FPR write port (pipeline 6 -> port 4) is also live.
            dut.io.backend.fprWen(4).poke(true.B)
            dut.io.backend.fprWaddr(4).poke(9.U)
            dut.io.backend.fprWdata(4).poke(0x7fc12345L.U)
            dut.clock.step()
            dut.io.debug.fpr(9).expect(0x7fc12345L.U)
        }
    }

    it should "carry FLW writeback and FSW payloads through the LSU pipeline" in {
        test(new ALULSUPipeline) { dut =>
            clearPackage(dut.io.backend.instPkgIn)
            dut.io.forward.fwdRs1Data.poke(0.U)
            dut.io.forward.fwdRs2Data.poke(0.U)
            dut.io.forward.fwdRs3Data.poke(0.U)
            dut.io.forward.shallowRs1Sel.poke(0.U)
            dut.io.forward.shallowRs2Sel.poke(0.U)
            dut.io.forward.lateRs1Sel.poke(0.U)
            dut.io.forward.lateRs2Sel.poke(0.U)
            dut.io.forward.needsEX2Replay.poke(false.B)
            dut.io.forward.replayRs1Data.poke(0.U)
            dut.io.forward.replayRs2Data.poke(0.U)
            dut.io.hazard.ex1Flush.poke(false.B)
            dut.io.hazard.ex1Stall.poke(false.B)
            dut.io.hazard.ex2Flush.poke(false.B)
            dut.io.hazard.ex2Stall.poke(false.B)
            dut.io.hazard.ex3Flush.poke(false.B)
            dut.io.hazard.ex3Stall.poke(false.B)
            dut.io.hazard.wbFlush.poke(false.B)
            dut.io.hazard.wbStall.poke(false.B)
            dut.io.lsu.loadData.poke(0x3fc00000L.U)

            // FLW f1, 0(x3)
            dut.io.backend.instPkgIn.op.poke(FLW.U)
            dut.io.backend.instPkgIn.rdValid.poke(true.B)
            dut.io.backend.instPkgIn.rd.poke((32 + 1).U)
            dut.io.backend.instPkgIn.src1Sel.poke(0.U)
            dut.io.backend.instPkgIn.src2Sel.poke(1.U)
            dut.io.forward.fwdRs1Data.poke(0x100.U)
            dut.clock.step()
            clearPackage(dut.io.backend.instPkgIn)
            dut.io.forward.fwdRs1Data.poke(0.U)
            dut.clock.step(2)
            dut.io.frontend.fprWen.expect(true.B)
            dut.io.frontend.fprWaddr.expect(1.U)
            dut.io.frontend.fprWdata.expect(0x3fc00000L.U)

            // FSW f2, 4(x3); the forwarded FPR payload must be retained into EX2.
            dut.io.backend.instPkgIn.inst.poke(fsw(rs2 = 2, rs1 = 3, imm = 4).U)
            dut.io.backend.instPkgIn.op.poke(FSW.U)
            dut.io.backend.instPkgIn.src1Sel.poke(0.U)
            dut.io.backend.instPkgIn.src2Sel.poke(1.U)
            dut.io.backend.instPkgIn.imm.poke(4.U)
            dut.io.forward.fwdRs1Data.poke(0x100.U)
            dut.io.forward.fwdRs2Data.poke(0x40400000L.U)
            dut.io.forward.replayRs2Data.poke(0x40400000L.U)
            dut.clock.step()
            dut.io.lsu.storeReq.valid.expect(true.B)
            dut.io.lsu.storeReq.op.expect(FSW.U)
            dut.io.lsu.storeReq.addr.expect(0x104.U)
            dut.io.lsu.storeReq.data.expect(0x40400000L.U)
        }
    }

    it should "detect and forward dependencies across FPU and LSU register classes" in {
        test(new Hazard) { dut =>
            for (i <- 0 until 8) {
                clearPackage(dut.io.frontend.idPkgs(i))
                clearPackage(dut.io.backend.ex1Pkgs(i))
                clearPackage(dut.io.backend.ex2Pkgs(i))
                dut.io.backend.ex1ShallowRs1Sel(i).poke(0.U)
                dut.io.backend.ex1ShallowRs2Sel(i).poke(0.U)
                dut.io.backend.ex1LateRs1Sel(i).poke(0.U)
                dut.io.backend.ex1LateRs2Sel(i).poke(0.U)
                dut.io.backend.ex1NeedsEX2Replay(i).poke(false.B)
            }
            dut.io.backend.divBusy(0).poke(false.B)
            dut.io.backend.divBusy(1).poke(false.B)
            dut.io.backend.fdivBusy.poke(false.B)
            dut.io.backend.predFailEX2.poke(false.B)
            dut.io.backend.branchTgtEX2.poke(0.U)
            dut.io.backend.predFailEX3.poke(false.B)
            dut.io.backend.branchTgtEX3.poke(0.U)

            // FSW in slot 5 consumes f2 while an older FADD is producing f2.
            dut.io.frontend.idPkgs(5).rs2.poke((32 + 2).U)
            dut.io.frontend.idPkgs(5).rs2ReadValid.poke(true.B)
            dut.io.backend.ex1Pkgs(0).rd.poke((32 + 2).U)
            dut.io.backend.ex1Pkgs(0).rdValid.poke(true.B)
            dut.io.backend.ex1Pkgs(0).op.poke(0x40.U)
            dut.clock.step()
            dut.io.frontend.stall.expect(true.B)
            for (i <- 0 until 8) dut.io.backend.ex1Flush(i).expect(true.B)
        }

        test(new Forward) { dut =>
            for (i <- 0 until 8) {
                clearPackage(dut.io.ex1Pkgs(i))
                clearPackage(dut.io.ex2Pkgs(i))
                clearPackage(dut.io.ex3Pkgs(i))
                clearPackage(dut.io.wbPkgs(i))
            }

            // A just-completed FLW in LSU slot 5 feeds a floating-point source.
            dut.io.ex1Pkgs(0).rs1.poke((32 + 1).U)
            dut.io.ex1Pkgs(0).rs1ReadValid.poke(true.B)
            dut.io.ex1Pkgs(0).rs1Data.poke(0.U)
            dut.io.wbPkgs(5).rd.poke((32 + 1).U)
            dut.io.wbPkgs(5).rdValid.poke(true.B)
            dut.io.wbPkgs(5).rfWdata.poke(0x3fc00000L.U)

            // A just-completed FPU result feeds FSW's floating-point rs2.
            dut.io.ex1Pkgs(5).rs2.poke((32 + 2).U)
            dut.io.ex1Pkgs(5).rs2ReadValid.poke(true.B)
            dut.io.ex1Pkgs(5).rs2Data.poke(0.U)
            dut.io.wbPkgs(0).rd.poke((32 + 2).U)
            dut.io.wbPkgs(0).rdValid.poke(true.B)
            dut.io.wbPkgs(0).rfWdata.poke(0x40400000L.U)
            dut.clock.step()

            dut.io.fwdRs1Data(0).expect(0x3fc00000L.U)
            dut.io.fwdRs2Data(5).expect(0x40400000L.U)
        }
    }
}
