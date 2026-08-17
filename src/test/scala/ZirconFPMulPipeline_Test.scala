import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class FMulPrefixAdderHarness extends Module {
  val io = IO(new Bundle {
    val src1 = Input(UInt(48.W))
    val src2 = Input(UInt(48.W))
    val cin = Input(UInt(1.W))
    val sum = Output(UInt(48.W))
    val cout = Output(UInt(1.W))
  })

  val (sum, cout) = zirconfp.FMulUtils.prefixAdder(io.src1, io.src2, io.cin, 48)
  io.sum := sum
  io.cout := cout
}

class ZirconFPMulPrefixAdderTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "zirconfp.FMulUtils.prefixAdder"

  it should "match 48-bit binary addition across randomized carry patterns" in {
    test(new FMulPrefixAdderHarness).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      val random = new Random(0x5eedL)
      val mask = (BigInt(1) << 48) - 1

      for (index <- 0 until 10000) {
        val src1 = BigInt(48, random)
        val src2 = BigInt(48, random)
        val cin = random.nextInt(2)
        val fullResult = src1 + src2 + cin

        c.io.src1.poke(src1.U)
        c.io.src2.poke(src2.U)
        c.io.cin.poke(cin.U)
        c.io.sum.expect((fullResult & mask).U, s"case $index produced the wrong sum")
        c.io.cout.expect(((fullResult >> 48) & 1).U, s"case $index produced the wrong carry")
      }
    }
  }
}
