import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source
import scala.util.Random

case class FPVector(
    op: Int,
    a: Long,
    b: Long,
    rm: Int,
    result: Long,
    flags: Option[Int]
)

class ZirconFPAddMulTest extends AnyFlatSpec with ChiselScalatestTester {
    private val FADD = 0x40
    private val FSUB = 0x41
    private val FMUL = 0x42

    private def unsigned(bits: Int): Long = bits.toLong & 0xffffffffL

    private def javaResult(op: Int, a: Long, b: Long): Long = {
        val fa = java.lang.Float.intBitsToFloat(a.toInt)
        val fb = java.lang.Float.intBitsToFloat(b.toInt)
        val value = op match {
            case FADD => fa + fb
            case FSUB => fa - fb
            case FMUL => fa * fb
        }
        unsigned(java.lang.Float.floatToIntBits(value))
    }

    private def initialize(dut: FPU): Unit = {
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
        pkg.aluResult.poke(0.U)
        pkg.fpuResult.poke(0.U)
        pkg.fflags.poke(0.U)
        pkg.branchTgt.poke(0.U)
        pkg.predFail.poke(false.B)
        pkg.memResult.poke(0.U)
        pkg.rfWdata.poke(0.U)
    }

    /** Issue one operation per cycle and verify the fixed two-boundary latency. */
    private def runPipelined(dut: FPU, vectors: Seq[FPVector]): Unit = {
        for (cycle <- 0 to vectors.length) {
            if (cycle < vectors.length) {
                val vector = vectors(cycle)
                dut.io.rs1Data.poke(vector.a.U)
                dut.io.rs2Data.poke(vector.b.U)
                dut.io.op.poke(vector.op.U)
                dut.io.rm.poke(vector.rm.U)
            } else {
                dut.io.rs1Data.poke(0.U)
                dut.io.rs2Data.poke(0.U)
                dut.io.op.poke(0.U)
                dut.io.rm.poke(0.U)
            }

            dut.clock.step()

            if (cycle >= 1) {
                val expected = vectors(cycle - 1)
                val context = f"op=0x${expected.op}%02x a=0x${expected.a}%08x b=0x${expected.b}%08x rm=${expected.rm}"
                dut.io.res.expect(expected.result.U, context)
                expected.flags.foreach(flag => dut.io.fflags.expect(flag.U, context))
            }
        }
    }

    behavior of "Zircon pipelined FADD/FSUB/FMUL"

    it should "pass the rv32uf fadd instruction vectors with dynamic RNE" in {
        // From riscv-tests isa/rv64uf/fadd.S, which rv32uf/fadd.S includes.
        val rv32uf = Seq(
            FPVector(FADD, 0x40200000L, 0x3f800000L, 7, 0x40600000L, Some(0x00)),
            FPVector(FADD, 0xc49a6333L, 0x3f8ccccdL, 7, 0xc49a4000L, Some(0x01)),
            FPVector(FADD, 0x40490fdbL, 0x322bcc77L, 7, 0x40490fdbL, Some(0x01)),
            FPVector(FSUB, 0x40200000L, 0x3f800000L, 7, 0x3fc00000L, Some(0x00)),
            FPVector(FSUB, 0xc49a6333L, 0xbf8ccccdL, 7, 0xc49a4000L, Some(0x01)),
            FPVector(FSUB, 0x40490fdbL, 0x322bcc77L, 7, 0x40490fdbL, Some(0x01)),
            FPVector(FMUL, 0x40200000L, 0x3f800000L, 7, 0x40200000L, Some(0x00)),
            FPVector(FMUL, 0xc49a6333L, 0xbf8ccccdL, 7, 0x44a9d385L, Some(0x01)),
            FPVector(FMUL, 0x40490fdbL, 0x322bcc77L, 7, 0x3306ee2dL, Some(0x01)),
            FPVector(FSUB, 0x7f800000L, 0x7f800000L, 7, 0x7fc00000L, Some(0x10))
        )

        test(new FPU) { dut =>
            initialize(dut)
            runPipelined(dut, rv32uf)
        }
    }

    it should "match SoftFloat for every RISC-V rounding mode and exception class" in {
        // Generated with Berkeley SoftFloat, tininess detected after rounding.
        val softFloatVectors = Seq(
            // Positive and negative halfway additions.
            FPVector(FADD, 0x3f800000L, 0x33800000L, 0, 0x3f800000L, Some(0x01)),
            FPVector(FADD, 0x3f800000L, 0x33800000L, 1, 0x3f800000L, Some(0x01)),
            FPVector(FADD, 0x3f800000L, 0x33800000L, 2, 0x3f800000L, Some(0x01)),
            FPVector(FADD, 0x3f800000L, 0x33800000L, 3, 0x3f800001L, Some(0x01)),
            FPVector(FADD, 0x3f800000L, 0x33800000L, 4, 0x3f800001L, Some(0x01)),
            FPVector(FADD, 0xbf800000L, 0xb3800000L, 0, 0xbf800000L, Some(0x01)),
            FPVector(FADD, 0xbf800000L, 0xb3800000L, 1, 0xbf800000L, Some(0x01)),
            FPVector(FADD, 0xbf800000L, 0xb3800000L, 2, 0xbf800001L, Some(0x01)),
            FPVector(FADD, 0xbf800000L, 0xb3800000L, 3, 0xbf800000L, Some(0x01)),
            FPVector(FADD, 0xbf800000L, 0xb3800000L, 4, 0xbf800001L, Some(0x01)),

            // Overflow selection and exact cancellation sign.
            FPVector(FADD, 0x7f7fffffL, 0x7f7fffffL, 0, 0x7f800000L, Some(0x05)),
            FPVector(FADD, 0x7f7fffffL, 0x7f7fffffL, 1, 0x7f7fffffL, Some(0x05)),
            FPVector(FADD, 0x7f7fffffL, 0x7f7fffffL, 2, 0x7f7fffffL, Some(0x05)),
            FPVector(FADD, 0x7f7fffffL, 0x7f7fffffL, 3, 0x7f800000L, Some(0x05)),
            FPVector(FADD, 0x7f7fffffL, 0x7f7fffffL, 4, 0x7f800000L, Some(0x05)),
            FPVector(FSUB, 0x3f800000L, 0x3f800000L, 0, 0x00000000L, Some(0x00)),
            FPVector(FSUB, 0x3f800000L, 0x3f800000L, 1, 0x00000000L, Some(0x00)),
            FPVector(FSUB, 0x3f800000L, 0x3f800000L, 2, 0x80000000L, Some(0x00)),
            FPVector(FSUB, 0x3f800000L, 0x3f800000L, 3, 0x00000000L, Some(0x00)),
            FPVector(FSUB, 0x3f800000L, 0x3f800000L, 4, 0x00000000L, Some(0x00)),

            // Exact subnormal arithmetic and NaN/Infinity behavior.
            FPVector(FADD, 0x00800000L, 0x807fffffL, 0, 0x00000001L, Some(0x00)),
            FPVector(FSUB, 0x00800000L, 0x007fffffL, 4, 0x00000001L, Some(0x00)),
            FPVector(FADD, 0x00000001L, 0x00000001L, 3, 0x00000002L, Some(0x00)),
            FPVector(FADD, 0x7f800000L, 0xff800000L, 0, 0x7fc00000L, Some(0x10)),
            FPVector(FADD, 0x7fc12345L, 0x3f800000L, 0, 0x7fc00000L, Some(0x00)),
            FPVector(FADD, 0x7f800001L, 0x3f800000L, 0, 0x7fc00000L, Some(0x10)),
            FPVector(FADD, 0x80000000L, 0x80000000L, 0, 0x80000000L, Some(0x00)),

            // Multiplication underflow/overflow in all five modes.
            FPVector(FMUL, 0x00000001L, 0x3f000000L, 0, 0x00000000L, Some(0x03)),
            FPVector(FMUL, 0x00000001L, 0x3f000000L, 1, 0x00000000L, Some(0x03)),
            FPVector(FMUL, 0x00000001L, 0x3f000000L, 2, 0x00000000L, Some(0x03)),
            FPVector(FMUL, 0x00000001L, 0x3f000000L, 3, 0x00000001L, Some(0x03)),
            FPVector(FMUL, 0x00000001L, 0x3f000000L, 4, 0x00000001L, Some(0x03)),
            FPVector(FMUL, 0x7f7fffffL, 0x40000000L, 0, 0x7f800000L, Some(0x05)),
            FPVector(FMUL, 0x7f7fffffL, 0x40000000L, 1, 0x7f7fffffL, Some(0x05)),
            FPVector(FMUL, 0x7f7fffffL, 0x40000000L, 2, 0x7f7fffffL, Some(0x05)),
            FPVector(FMUL, 0x7f7fffffL, 0x40000000L, 3, 0x7f800000L, Some(0x05)),
            FPVector(FMUL, 0x7f7fffffL, 0x40000000L, 4, 0x7f800000L, Some(0x05)),

            // Boundary between subnormal and normal plus invalid operations.
            FPVector(FMUL, 0x00800000L, 0x3f000000L, 0, 0x00400000L, Some(0x00)),
            FPVector(FMUL, 0x007fffffL, 0x3f800001L, 0, 0x00800000L, Some(0x01)),
            FPVector(FMUL, 0x007fffffL, 0x3f800001L, 1, 0x007fffffL, Some(0x03)),
            FPVector(FMUL, 0x007fffffL, 0x3f800001L, 2, 0x007fffffL, Some(0x03)),
            FPVector(FMUL, 0x007fffffL, 0x3f800001L, 3, 0x00800000L, Some(0x01)),
            FPVector(FMUL, 0x007fffffL, 0x3f800001L, 4, 0x00800000L, Some(0x01)),
            FPVector(FMUL, 0x7f800000L, 0x00000000L, 0, 0x7fc00000L, Some(0x10)),
            FPVector(FMUL, 0x7f800001L, 0x3f800000L, 4, 0x7fc00000L, Some(0x10)),
            FPVector(FMUL, 0xff800000L, 0xc0000000L, 2, 0x7f800000L, Some(0x00)),
            FPVector(FMUL, 0x80000000L, 0x40400000L, 3, 0x80000000L, Some(0x00))
        )

        test(new FPU) { dut =>
            initialize(dut)
            runPipelined(dut, softFloatVectors)
        }
    }

    it should "match host IEEE-754 RNE on randomized add, subtract and multiply" in {
        val random = new Random(0x5eedL)
        val vectors = Seq.fill(10000) {
            val op = Seq(FADD, FSUB, FMUL)(random.nextInt(3))
            val a = unsigned(random.nextInt())
            val b = unsigned(random.nextInt())
            FPVector(op, a, b, 0, javaResult(op, a, b), None)
        }

        test(new FPU) { dut =>
            initialize(dut)
            runPipelined(dut, vectors)
        }
    }

    it should "match 1500 deterministic SoftFloat random vectors across all rounding modes" in {
        val stream = Option(getClass.getResourceAsStream("/softfloat-add-mul-vectors.csv"))
            .getOrElse(fail("missing SoftFloat vector resource"))
        val source = Source.fromInputStream(stream)
        val vectors = try {
            source.getLines().drop(1).filter(_.nonEmpty).map { line =>
                val fields = line.split(',')
                val op = fields(0) match {
                    case "add" => FADD
                    case "sub" => FSUB
                    case "mul" => FMUL
                }
                FPVector(
                    op,
                    java.lang.Long.parseUnsignedLong(fields(1), 16),
                    java.lang.Long.parseUnsignedLong(fields(2), 16),
                    fields(3).toInt,
                    java.lang.Long.parseUnsignedLong(fields(4), 16),
                    Some(Integer.parseUnsignedInt(fields(5), 16))
                )
            }.toVector
        } finally {
            source.close()
        }

        assert(vectors.length == 1500)
        test(new FPU) { dut =>
            initialize(dut)
            runPipelined(dut, vectors)
        }
    }

    it should "hold and flush arithmetic state with the processor pipeline" in {
        test(new FPU) { dut =>
            initialize(dut)

            // Capture EX1, then hold both internal boundaries for three cycles.
            dut.io.rs1Data.poke(0x3fc00000L.U) // 1.5
            dut.io.rs2Data.poke(0x40200000L.U) // 2.5
            dut.io.op.poke(FMUL.U)
            dut.io.rm.poke(0.U)
            dut.clock.step()
            dut.io.control.s1Enable.poke(false.B)
            dut.io.control.s2Enable.poke(false.B)
            dut.clock.step(3)

            dut.io.control.s1Enable.poke(true.B)
            dut.io.control.s2Enable.poke(true.B)
            dut.io.op.poke(0.U)
            dut.clock.step()
            dut.io.res.expect(0x40700000L.U) // 3.75

            // A younger value entering EX2 must be discarded by an EX3 flush.
            dut.io.rs1Data.poke(0x3f800000L.U)
            dut.io.rs2Data.poke(0x40000000L.U)
            dut.io.op.poke(FADD.U)
            dut.clock.step()
            dut.io.control.s2Flush.poke(true.B)
            dut.io.op.poke(0.U)
            dut.clock.step()
            dut.io.res.expect(0.U)
        }
    }

    it should "write back back-to-back results in the ALU/FPU VLIW lane" in {
        test(new ALUFPUPipeline) { dut =>
            clearPackage(dut.io.backend.instPkgIn)
            dut.io.forward.fwdRs1Data.poke(0.U)
            dut.io.forward.fwdRs2Data.poke(0.U)
            dut.io.forward.fwdRs3Data.poke(0.U)
            dut.io.hazard.ex1Flush.poke(false.B)
            dut.io.hazard.ex1Stall.poke(false.B)
            dut.io.hazard.ex2Flush.poke(false.B)
            dut.io.hazard.ex2Stall.poke(false.B)
            dut.io.hazard.ex3Flush.poke(false.B)
            dut.io.hazard.ex3Stall.poke(false.B)
            dut.io.hazard.wbFlush.poke(false.B)
            dut.io.hazard.wbStall.poke(false.B)

            // Cycle 0: f1 = 1.5 + 2.25 = 3.75.
            dut.io.backend.instPkgIn.rdValid.poke(true.B)
            dut.io.backend.instPkgIn.rd.poke(33.U)
            dut.io.backend.instPkgIn.op.poke(FADD.U)
            dut.io.backend.instPkgIn.rm.poke(7.U)
            dut.io.forward.fwdRs1Data.poke(0x3fc00000L.U)
            dut.io.forward.fwdRs2Data.poke(0x40100000L.U)
            dut.clock.step()

            // Cycle 1: f2 = -2.0 * 0.5 = -1.0.
            dut.io.backend.instPkgIn.rd.poke(34.U)
            dut.io.backend.instPkgIn.op.poke(FMUL.U)
            dut.io.forward.fwdRs1Data.poke(0xc0000000L.U)
            dut.io.forward.fwdRs2Data.poke(0x3f000000L.U)
            dut.clock.step()

            // Drain the EX3/WB boundary.
            clearPackage(dut.io.backend.instPkgIn)
            dut.io.forward.fwdRs1Data.poke(0.U)
            dut.io.forward.fwdRs2Data.poke(0.U)
            dut.clock.step()
            dut.io.frontend.fprWen.expect(true.B)
            dut.io.frontend.fprWaddr.expect(1.U)
            dut.io.frontend.fprWdata.expect(0x40700000L.U)

            dut.clock.step()
            dut.io.frontend.fprWen.expect(true.B)
            dut.io.frontend.fprWaddr.expect(2.U)
            dut.io.frontend.fprWdata.expect(0xbf800000L.U)
        }
    }
}
