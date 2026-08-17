import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp.{FADD_S, FMUL_S}

class ZirconFPPipelineIntegrationTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ALUFPUPipeline floating-point result alignment"

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

  private def clearHazard(c: ALUFPUPipeline): Unit = {
    c.io.hazard.ex1Flush.poke(false.B)
    c.io.hazard.ex1Stall.poke(false.B)
    c.io.hazard.ex2Flush.poke(false.B)
    c.io.hazard.ex2Stall.poke(false.B)
    c.io.hazard.ex3Flush.poke(false.B)
    c.io.hazard.ex3Stall.poke(false.B)
    c.io.hazard.wbFlush.poke(false.B)
    c.io.hazard.wbStall.poke(false.B)
  }

  private def issue(
      c: ALUFPUPipeline,
      op: UInt,
      src1: BigInt,
      src2: BigInt,
      expected: BigInt
  ): Unit = {
    c.io.backend.instPkgIn.op.poke(op)
    c.io.backend.instPkgIn.rd.poke(33.U)
    c.io.backend.instPkgIn.rdValid.poke(true.B)
    c.io.forward.fwdRs1Data.poke(src1.U)
    c.io.forward.fwdRs2Data.poke(src2.U)
    c.io.forward.fwdGprRs1Data.poke(src1.U)
    c.io.forward.fwdGprRs2Data.poke(src2.U)
    c.io.forward.fwdFprRs1Data.poke(src1.U)
    c.io.forward.fwdFprRs2Data.poke(src2.U)
    c.clock.step()

    c.io.backend.instPkgIn.op.poke(0.U)
    c.io.backend.instPkgIn.rd.poke(0.U)
    c.io.backend.instPkgIn.rdValid.poke(false.B)
    c.io.forward.fwdRs1Data.poke(0.U)
    c.io.forward.fwdRs2Data.poke(0.U)
    c.io.forward.fwdGprRs1Data.poke(0.U)
    c.io.forward.fwdGprRs2Data.poke(0.U)
    c.io.forward.fwdFprRs1Data.poke(0.U)
    c.io.forward.fwdFprRs2Data.poke(0.U)
    c.clock.step(2)

    c.io.frontend.fprWen.expect(true.B)
    c.io.frontend.fprWaddr.expect(1.U)
    c.io.frontend.fprWdata.expect(expected.U)
    c.clock.step()
    c.io.frontend.fprWen.expect(false.B)
  }

  it should "write FADD and FMUL results with their WB instruction package" in {
    test(new ALUFPUPipeline).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearPackage(c.io.backend.instPkgIn)
      clearHazard(c)
      c.io.forward.fwdRs1Data.poke(0.U)
      c.io.forward.fwdRs2Data.poke(0.U)
      c.io.forward.fwdRs3Data.poke(0.U)
      c.io.forward.fwdGprRs1Data.poke(0.U)
      c.io.forward.fwdGprRs2Data.poke(0.U)
      c.io.forward.fwdFprRs1Data.poke(0.U)
      c.io.forward.fwdFprRs2Data.poke(0.U)

      issue(
        c,
        FADD_S,
        BigInt("3f800000", 16), // 1.0f
        BigInt("40000000", 16), // 2.0f
        BigInt("40400000", 16)  // 3.0f
      )
      issue(
        c,
        FMUL_S,
        BigInt("40000000", 16), // 2.0f
        BigInt("40400000", 16), // 3.0f
        BigInt("40c00000", 16)  // 6.0f
      )
    }
  }
}
