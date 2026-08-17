import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp.{FADD_S, FMUL_S}

class FPUStallTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FPU VLIW pipeline controls"

  it should "hold fixed-latency results while the outer pipeline is stalled" in {
    test(new FPU).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      c.io.rs3Data.poke(0.U)
      c.io.rm.poke(0.U)
      c.io.inValid.poke(false.B)
      c.io.ex2Advance.poke(true.B)
      c.io.ex2Flush.poke(false.B)
      c.io.ex3Advance.poke(true.B)
      c.io.ex3Flush.poke(false.B)
      c.io.wbAdvance.poke(true.B)
      c.io.wbFlush.poke(false.B)

      c.io.op.poke(FADD_S)
      c.io.rs1Data.poke("h3f800000".U) // 1.0f
      c.io.rs2Data.poke("h40000000".U) // 2.0f
      c.io.inValid.poke(true.B)
      c.clock.step()
      c.io.inValid.poke(false.B)
      c.clock.step(2)
      c.io.faddResult.expect("h40400000".U) // 3.0f

      c.io.rs1Data.poke("h41200000".U) // 10.0f
      c.io.rs2Data.poke("h41a00000".U) // 20.0f
      c.io.inValid.poke(true.B)
      c.clock.step()
      c.io.inValid.poke(false.B)
      c.io.ex2Advance.poke(false.B)
      c.io.ex3Advance.poke(false.B)
      c.io.wbAdvance.poke(false.B)
      c.clock.step(3)
      c.io.faddResult.expect("h40400000".U)

      c.io.ex2Advance.poke(true.B)
      c.io.ex3Advance.poke(true.B)
      c.io.wbAdvance.poke(true.B)
      c.clock.step(2)
      c.io.faddResult.expect("h41f00000".U) // 30.0f

      c.io.op.poke(FMUL_S)
      c.io.rs1Data.poke("h40000000".U) // 2.0f
      c.io.rs2Data.poke("h40400000".U) // 3.0f
      c.io.inValid.poke(true.B)
      c.clock.step()
      c.io.inValid.poke(false.B)
      c.clock.step(2)
      c.io.fmulResult.expect("h40c00000".U) // 6.0f

      c.io.rs1Data.poke("h40800000".U) // 4.0f
      c.io.rs2Data.poke("h40a00000".U) // 5.0f
      c.io.inValid.poke(true.B)
      c.clock.step()
      c.io.inValid.poke(false.B)
      c.io.ex2Advance.poke(false.B)
      c.io.ex3Advance.poke(false.B)
      c.io.wbAdvance.poke(false.B)
      c.clock.step(3)
      c.io.fmulResult.expect("h40c00000".U)

      c.io.ex2Advance.poke(true.B)
      c.io.ex3Advance.poke(true.B)
      c.io.wbAdvance.poke(true.B)
      c.clock.step(2)
      c.io.fmulResult.expect("h41a00000".U) // 20.0f
    }
  }
}
