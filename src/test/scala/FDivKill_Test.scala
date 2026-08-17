import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp.FDIV_S

class FDivKillTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDiv pipeline kill control"

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

  private def clearHazard(c: FDivFPUPipeline): Unit = {
    c.io.hazard.ex1Flush.poke(false.B)
    c.io.hazard.ex1Stall.poke(false.B)
    c.io.hazard.ex2Flush.poke(false.B)
    c.io.hazard.ex2Stall.poke(false.B)
    c.io.hazard.ex3Flush.poke(false.B)
    c.io.hazard.ex3Stall.poke(false.B)
    c.io.hazard.wbFlush.poke(false.B)
    c.io.hazard.wbStall.poke(false.B)
  }

  it should "ignore a RAW ex1 flush but honor an execution flush" in {
    test(new FDivFPUPipeline).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearPackage(c.io.backend.instPkgIn)
      clearHazard(c)
      c.io.forward.fwdRs1Data.poke("h3f800000".U)
      c.io.forward.fwdRs2Data.poke("h40000000".U)
      c.io.forward.fwdRs3Data.poke(0.U)
      c.io.forward.fwdGprRs1Data.poke("h3f800000".U)
      c.io.forward.fwdGprRs2Data.poke("h40000000".U)
      c.io.forward.fwdFprRs1Data.poke("h3f800000".U)
      c.io.forward.fwdFprRs2Data.poke("h40000000".U)

      c.io.backend.instPkgIn.op.poke(FDIV_S)
      c.io.backend.instPkgIn.rd.poke(33.U)
      c.io.backend.instPkgIn.rdValid.poke(true.B)

      c.io.hazard.ex1Flush.poke(true.B)
      c.clock.step()
      c.io.hazard.fdivBusy.expect(true.B)

      c.io.hazard.ex1Flush.poke(false.B)
      c.io.hazard.ex2Flush.poke(true.B)
      c.clock.step()
      c.io.hazard.fdivBusy.expect(false.B)
    }
  }
}
