import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp._

class FDivResultCaptureTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "FDIV result ownership across EX3 stalls"

    it should "carry each consecutive division or square root result and flags with its packet" in {
        test(new FDivFlushHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            case class Operation(sqrt: Boolean, a: Long, b: Long, result: Long, flags: Int)
            val three = Operation(false, 0x40c00000L, 0x40000000L, 0x40400000L, 0)
            val five = Operation(false, 0x41200000L, 0x40000000L, 0x40a00000L, 0)
            val infinity = Operation(false, 0x3f800000L, 0, 0x7f800000L, 8)
            val sqrtNine = Operation(true, 0x41100000L, 0, 0x40400000L, 0)
            val sqrtTwo = Operation(true, 0x40000000L, 0, 0x3fb504f3L, 1)
            val scenarios = Seq((three, five), (infinity, five), (three, infinity),
                                (sqrtNine, sqrtTwo), (sqrtTwo, five), (three, sqrtTwo))

            def clear(): Unit = ShallowDependencyTestUtil.clear(c.io.idPkg)
            def request(index: Int, op: Operation): Unit = {
                clear()
                c.io.idPkg.pc.poke((0x80000100L + index * 32).U)
                c.io.idPkg.inst.poke((if (op.sqrt) 0x580100d3L else 0x183100d3L).U)
                c.io.idPkg.op.poke(if (op.sqrt) FSQRT_S else FDIV_S)
                c.io.idPkg.rdValid.poke(true.B)
                c.io.idPkg.rd.poke((33 + index).U)
                c.io.idPkg.rs1Data.poke(op.a.U)
                c.io.idPkg.rs2Data.poke(op.b.U)
                c.io.idPkg.rm.poke(0.U) // RNE
            }

            for ((first, second) <- scenarios) {
                clear()
                c.io.redirectEX2.poke(false.B)
                c.io.redirectEX3.poke(false.B)
                c.io.intBusy.poke(false.B)
                c.reset.poke(true.B)
                c.clock.step(2)
                c.reset.poke(false.B)
                request(0, first)
                c.clock.step() // A -> EX1
                request(1, second)
                c.clock.step() // A starts and enters EX2; B -> EX1
                clear()
                c.io.busy.expect(true.B)

                val expected = Vector(first, second)
                var writes = 0
                var starts = 1
                var previousBusy = true
                var overlapCycles = 0
                for (_ <- 0 until 180) {
                    val busy = c.io.busy.peek().litToBoolean
                    if (busy && !previousBusy) starts += 1
                    previousBusy = busy
                    if (c.io.ex3.rdValid.peek().litToBoolean) {
                        val index = ((c.io.ex3.pc.peek().litValue - 0x80000100L) / 32).toInt
                        assert(index >= 0 && index < 2)
                        c.io.ex3.fpuResult.expect(expected(index).result.U)
                        c.io.ex3.fflags.expect(expected(index).flags.U)
                        if (index == 0 && busy) overlapCycles += 1
                    }
                    if (c.io.wb.rdValid.peek().litToBoolean) {
                        assert(writes < 2, "duplicate FDIV writeback")
                        c.io.wb.pc.expect((0x80000100L + writes * 32).U)
                        c.io.wb.rd.expect((33 + writes).U)
                        c.io.wb.rfWdata.expect(expected(writes).result.U)
                        c.io.wb.fflags.expect(expected(writes).flags.U)
                        writes += 1
                    }
                    c.clock.step()
                }
                assert(writes == 2, "both results must commit exactly once")
                assert(starts == 2, "each operation must start exactly once")
                assert(overlapCycles > 0, "older result must survive a younger FDIV stall")
            }
        }
    }
}
