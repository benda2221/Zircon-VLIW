import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp.{FDIV_S, FSQRT_S}

class ZirconFPWrapperTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDivWrapper with independent divide and square-root units"

  private def runOperation(
      c: FDivWrapper,
      op: UInt,
      src1: BigInt,
      src2: BigInt,
      expected: BigInt
  ): Unit = {
    c.io.ready.expect(true.B)
    c.io.op.poke(op)
    c.io.rs1Data.poke(src1.U)
    c.io.rs2Data.poke(src2.U)
    c.io.valid.poke(true.B)
    c.clock.step()
    c.io.valid.poke(false.B)

    var cycles = 0
    while (c.io.busy.peek().litToBoolean && cycles < 64) {
      c.io.ready.expect(false.B)
      c.clock.step()
      cycles += 1
    }

    assert(cycles < 64, "floating-point iterative operation timed out")
    c.io.res.expect(expected.U)
    c.io.fflags.expect(0.U)
  }

  it should "select FDIV.S and FSQRT.S and retain the matching result" in {
    test(new FDivWrapper).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      c.io.rm.poke(0.U)
      c.io.valid.poke(false.B)
      c.io.kill.poke(false.B)
      c.io.op.poke(FDIV_S)
      c.io.rs1Data.poke(0.U)
      c.io.rs2Data.poke(0.U)

      runOperation(
        c,
        FDIV_S,
        BigInt("40c00000", 16), // 6.0f
        BigInt("40000000", 16), // 2.0f
        BigInt("40400000", 16)  // 3.0f
      )
      runOperation(
        c,
        FSQRT_S,
        BigInt("41100000", 16), // 9.0f
        0,
        BigInt("40400000", 16)  // 3.0f
      )
    }
  }
}
