import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class SRT2BoundaryTest extends AnyFlatSpec with ChiselScalatestTester {
    private val Mask32 = 0xffffffffL
    private val MinInt = 0x80000000L
    private val MaxInt = 0x7fffffffL

    private def signed32(value: Long): Long = value.toInt.toLong

    private def expected(aBits: Long, bBits: Long, op: Int): Long = {
        val unsignedA = aBits & Mask32
        val unsignedB = bBits & Mask32
        val signedA = signed32(unsignedA)
        val signedB = signed32(unsignedB)

        (op & 0x7) match {
            case 4 =>
                if (unsignedB == 0) Mask32
                else if (signedA == Int.MinValue && signedB == -1) MinInt
                else (signedA / signedB) & Mask32
            case 5 =>
                if (unsignedB == 0) Mask32 else (unsignedA / unsignedB) & Mask32
            case 6 =>
                if (unsignedB == 0) unsignedA
                else if (signedA == Int.MinValue && signedB == -1) 0L
                else (signedA % signedB) & Mask32
            case 7 =>
                if (unsignedB == 0) unsignedA else (unsignedA % unsignedB) & Mask32
            case _ => throw new IllegalArgumentException(s"unsupported op: $op")
        }
    }

    behavior of "SRT2"

    it should "preserve RISC-V divide and remainder edge cases" in {
        val fixed = Seq(
            (0L, 1L), (1L, 1L), (MaxInt, 3L), (Mask32, 2L),
            (0xffffffffL, 0xffffffffL), (MinInt, 1L), (MinInt, 0xffffffffL),
            (MaxInt, 0xffffffffL), (1L, 0xffffffffL),
            (0L, 0L), (1L, 0L), (MinInt, 0L), (Mask32, 0L)
        )
        val random = new Random(0x5a72)
        val randomCases = (0 until 256).map { _ =>
            val a = random.nextLong() & Mask32
            val b = random.nextLong() & Mask32
            (a, if (b == 0) 1L else b)
        }
        val operands = fixed ++ randomCases
        val cases = operands.flatMap { case (a, b) =>
            Seq(0x14, 0x15, 0x16, 0x17).map(op => (a, b, op))
        }

        test(new SRT2).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            var next = 0
            var completed = 0
            var pending: Option[(Long, Long, Int)] = None

            c.io.valid.poke(false.B)
            while (completed < cases.length) {
                c.io.valid.poke(false.B)

                if (c.io.ready.peek().litToBoolean) {
                    assert(pending.nonEmpty, "SRT2 asserted ready without an in-flight request")
                    val (a, b, op) = pending.get
                    c.io.res.expect(expected(a, b, op).U,
                        s"a=0x${a.toHexString}, b=0x${b.toHexString}, op=0x${op.toHexString}")
                    pending = None
                    completed += 1
                }

                if (pending.isEmpty && !c.io.busy.peek().litToBoolean && next < cases.length) {
                    val item = cases(next)
                    c.io.src1.poke(item._1.U)
                    c.io.src2.poke(item._2.U)
                    c.io.op.poke(item._3.U)
                    c.io.valid.poke(true.B)
                    pending = Some(item)
                    next += 1
                }

                c.clock.step(1)
            }
        }
    }
}
