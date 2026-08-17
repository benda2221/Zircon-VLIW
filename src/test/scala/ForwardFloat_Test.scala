import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp.{FADD_S, FCVT_W_S, FMUL_S, FSGNJ_S}

class ForwardFloatTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Floating-point forwarding"

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

  private def clearForward(c: Forward): Unit = {
    for (i <- 0 until 8) {
      clearPackage(c.io.ex1Pkgs(i))
      clearPackage(c.io.ex2Pkgs(i))
      clearPackage(c.io.ex3Pkgs(i))
      clearPackage(c.io.wbPkgs(i))
      c.io.ex3GprData(i).poke(0.U)
      c.io.wbGprValid(i).poke(false.B)
      c.io.wbGprAddr(i).poke(0.U)
      c.io.wbGprData(i).poke(0.U)
      c.io.wbFprValid(i).poke(false.B)
      c.io.wbFprAddr(i).poke(0.U)
      c.io.wbFprData(i).poke(0.U)
    }
  }

  private def clearHazard(c: Hazard): Unit = {
    for (i <- 0 until 8) {
      clearPackage(c.io.frontend.idPkgs(i))
      clearPackage(c.io.backend.ex1Pkgs(i))
      clearPackage(c.io.backend.ex2Pkgs(i))
      clearPackage(c.io.backend.ex3Pkgs(i))
    }
    for (i <- 0 until 8) {
      c.io.backend.pipelineBusy(i).poke(false.B)
      c.io.backend.pipelineStart(i).poke(false.B)
    }
    c.io.backend.frmWritePending.poke(false.B)
    c.io.backend.memBusy.poke(false.B)
    c.io.backend.predFail.poke(false.B)
    c.io.backend.branchTgt.poke(0.U)
  }

  it should "forward floating-point results from their first valid stage" in {
    test(new Forward).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearForward(c)

      c.io.ex1Pkgs(0).rs1.poke(33.U)
      c.io.ex1Pkgs(0).rs1Data.poke("h12345678".U)

      c.io.ex2Pkgs(1).rd.poke(33.U)
      c.io.ex2Pkgs(1).rdValid.poke(true.B)
      c.io.ex2Pkgs(1).op.poke(FSGNJ_S)
      c.io.ex2Pkgs(1).fpuResult.poke("h3f800000".U)
      c.io.fwdRs1Data(0).expect("h3f800000".U)

      c.io.ex3Pkgs(2).rd.poke(33.U)
      c.io.ex3Pkgs(2).rdValid.poke(true.B)
      c.io.ex3Pkgs(2).op.poke(FADD_S)
      c.io.ex3Pkgs(2).fpuResult.poke("h40400000".U)
      c.io.fwdRs1Data(0).expect("h3f800000".U)

      c.io.ex2Pkgs(1).rdValid.poke(false.B)
      c.io.fwdRs1Data(0).expect("h12345678".U)

      c.io.wbPkgs(1).rd.poke(33.U)
      c.io.wbPkgs(1).rdValid.poke(true.B)
      c.io.wbPkgs(1).op.poke(FADD_S)
      c.io.wbPkgs(1).rfWdata.poke("h40400000".U)
      c.io.wbFprValid(1).poke(true.B)
      c.io.wbFprAddr(1).poke(1.U)
      c.io.wbFprData(1).poke("h40800000".U)
      c.io.fwdRs1Data(0).expect("h40800000".U)

      c.io.ex3Pkgs(2).rdValid.poke(false.B)
      c.io.fwdRs1Data(0).expect("h40800000".U)

      // An EX2-ready result remains forwardable while it advances through EX3.
      c.io.wbFprValid(1).poke(false.B)
      c.io.ex3Pkgs(2).rd.poke(33.U)
      c.io.ex3Pkgs(2).rdValid.poke(true.B)
      c.io.ex3Pkgs(2).op.poke(FSGNJ_S)
      c.io.ex3Pkgs(2).fpuResult.poke("h40a00000".U)
      c.io.fwdRs1Data(0).expect("h40a00000".U)
    }
  }

  it should "keep FMUL at WB and forward EX3 float-to-integer conversion results" in {
    test(new Forward).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearForward(c)

      c.io.ex1Pkgs(0).rs1.poke(33.U)
      c.io.ex1Pkgs(0).rs1Data.poke("h11111111".U)
      c.io.ex3Pkgs(1).rd.poke(33.U)
      c.io.ex3Pkgs(1).rdValid.poke(true.B)
      c.io.ex3Pkgs(1).op.poke(FMUL_S)
      c.io.ex3Pkgs(1).fpuResult.poke("h40000000".U)
      c.io.fwdRs1Data(0).expect("h11111111".U)

      c.io.wbFprValid(1).poke(true.B)
      c.io.wbFprAddr(1).poke(1.U)
      c.io.wbFprData(1).poke("h40000000".U)
      c.io.fwdRs1Data(0).expect("h40000000".U)

      c.io.ex1Pkgs(4).rs1.poke(5.U)
      c.io.ex1Pkgs(4).rs1Data.poke("h22222222".U)
      c.io.ex3Pkgs(1).rd.poke(5.U)
      c.io.ex3Pkgs(1).op.poke(FCVT_W_S)
      c.io.ex3Pkgs(1).fpuResult.poke(7.U)
      c.io.fwdRs1Data(4).expect(7.U)
    }
  }

  it should "preserve stage and lane priority after hierarchical selection" in {
    test(new Forward).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearForward(c)

      c.io.ex1Pkgs(4).rs1.poke(5.U)
      c.io.ex1Pkgs(4).rs1Data.poke("h11111111".U)

      c.io.ex2Pkgs(2).rd.poke(5.U)
      c.io.ex2Pkgs(2).rdValid.poke(true.B)
      c.io.ex2Pkgs(2).aluResult.poke("h22222222".U)
      c.io.ex2Pkgs(6).rd.poke(5.U)
      c.io.ex2Pkgs(6).rdValid.poke(true.B)
      c.io.ex2Pkgs(6).aluResult.poke("h66666666".U)
      c.io.ex3Pkgs(7).rd.poke(5.U)
      c.io.ex3Pkgs(7).rdValid.poke(true.B)
      c.io.ex3Pkgs(7).aluResult.poke("h77777777".U)
      c.io.ex3GprData(7).poke("h77777777".U)
      c.io.wbPkgs(7).rd.poke(5.U)
      c.io.wbPkgs(7).rdValid.poke(true.B)
      c.io.wbPkgs(7).rfWdata.poke("h88888888".U)
      c.io.wbGprValid(7).poke(true.B)
      c.io.wbGprAddr(7).poke(5.U)
      c.io.wbGprData(7).poke("h88888888".U)

      c.io.fwdRs1Data(4).expect("h66666666".U)

      c.io.ex2Pkgs(2).rdValid.poke(false.B)
      c.io.ex2Pkgs(6).rdValid.poke(false.B)
      c.io.fwdRs1Data(4).expect("h77777777".U)

      c.io.ex3Pkgs(7).rdValid.poke(false.B)
      c.io.fwdRs1Data(4).expect("h88888888".U)

      c.io.wbPkgs(7).rdValid.poke(false.B)
      c.io.wbGprValid(7).poke(false.B)
      c.io.fwdRs1Data(4).expect("h11111111".U)
    }
  }

  it should "route same-numbered GPR and FPR results through separate networks" in {
    test(new Forward).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearForward(c)

      c.io.wbGprValid(3).poke(true.B)
      c.io.wbGprAddr(3).poke(1.U)
      c.io.wbGprData(3).poke("h11111111".U)
      c.io.wbFprValid(0).poke(true.B)
      c.io.wbFprAddr(0).poke(1.U)
      c.io.wbFprData(0).poke("h3f800000".U)

      // Slot 0 is FPR-only.
      c.io.ex1Pkgs(0).rs1.poke(33.U)
      c.io.fwdRs1Data(0).expect("h3f800000".U)

      // Slot 1 supports mixed-domain conversion and move operations.
      c.io.ex1Pkgs(1).rs1.poke(1.U)
      c.io.ex1Pkgs(1).rs2.poke(33.U)
      c.io.fwdRs1Data(1).expect("h11111111".U)
      c.io.fwdRs2Data(1).expect("h3f800000".U)
      c.io.fwdGprRs1Data(1).expect("h11111111".U)
      c.io.fwdFprRs1Data(1).expect("h3f800000".U)
      c.io.fwdGprRs2Data(1).expect("h11111111".U)
      c.io.fwdFprRs2Data(1).expect("h3f800000".U)

      // Integer MulDiv lanes never connect to the FPR data network.
      c.io.ex1Pkgs(4).rs1.poke(1.U)
      c.io.fwdRs1Data(4).expect("h11111111".U)

      // FPR-producing FADD must never drive the same-numbered GPR network.
      c.io.ex3Pkgs(1).rd.poke(1.U)
      c.io.ex3Pkgs(1).rdValid.poke(true.B)
      c.io.ex3Pkgs(1).op.poke(FADD_S)
      c.io.ex3Pkgs(1).fpuResult.poke("h40400000".U)
      c.io.fwdRs1Data(4).expect("h11111111".U)

      // LSU address is a GPR, while FSW data can be an FPR.
      c.io.ex1Pkgs(5).rs1.poke(1.U)
      c.io.ex1Pkgs(5).rs2.poke(33.U)
      c.io.fwdRs1Data(5).expect("h11111111".U)
      c.io.fwdRs2Data(5).expect("h3f800000".U)
    }
  }

  it should "stall only until the producer reaches a forwardable stage" in {
    test(new Hazard).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearHazard(c)
      // Use a real floating-point opcode so the false-RAW filter recognizes rs1.
      c.io.frontend.idPkgs(0).inst.poke("h00000053".U)
      c.io.frontend.idPkgs(0).rs1.poke(33.U)

      c.io.backend.ex1Pkgs(1).rd.poke(33.U)
      c.io.backend.ex1Pkgs(1).rdValid.poke(true.B)
      c.io.backend.ex1Pkgs(1).op.poke(FSGNJ_S)
      c.io.frontend.stall.expect(false.B)

      c.io.backend.ex1Pkgs(1).op.poke(FADD_S)
      c.io.frontend.stall.expect(true.B)
      clearPackage(c.io.backend.ex1Pkgs(1))
      c.io.backend.ex2Pkgs(1).rd.poke(33.U)
      c.io.backend.ex2Pkgs(1).rdValid.poke(true.B)
      c.io.backend.ex2Pkgs(1).op.poke(FADD_S)
      c.io.frontend.stall.expect(true.B)
      clearPackage(c.io.backend.ex2Pkgs(1))
      c.io.backend.ex3Pkgs(1).rd.poke(33.U)
      c.io.backend.ex3Pkgs(1).rdValid.poke(true.B)
      c.io.backend.ex3Pkgs(1).op.poke(FADD_S)
      c.io.frontend.stall.expect(false.B)

      clearPackage(c.io.backend.ex3Pkgs(1))
      c.io.backend.ex1Pkgs(1).rd.poke(33.U)
      c.io.backend.ex1Pkgs(1).rdValid.poke(true.B)
      c.io.backend.ex1Pkgs(1).op.poke(FMUL_S)
      c.io.frontend.stall.expect(true.B)
      clearPackage(c.io.backend.ex1Pkgs(1))
      c.io.backend.ex2Pkgs(1).rd.poke(33.U)
      c.io.backend.ex2Pkgs(1).rdValid.poke(true.B)
      c.io.backend.ex2Pkgs(1).op.poke(FMUL_S)
      c.io.frontend.stall.expect(true.B)
      clearPackage(c.io.backend.ex2Pkgs(1))
      c.io.backend.ex3Pkgs(1).rd.poke(33.U)
      c.io.backend.ex3Pkgs(1).rdValid.poke(true.B)
      c.io.backend.ex3Pkgs(1).op.poke(FMUL_S)
      c.io.frontend.stall.expect(false.B)

      clearPackage(c.io.backend.ex3Pkgs(1))
      c.io.backend.ex1Pkgs(1).rd.poke(5.U)
      c.io.backend.ex1Pkgs(1).rdValid.poke(true.B)
      c.io.backend.ex1Pkgs(1).op.poke(FCVT_W_S)
      c.io.frontend.idPkgs(0).rs1.poke(5.U)
      c.io.frontend.stall.expect(true.B)
      clearPackage(c.io.backend.ex1Pkgs(1))
      c.io.backend.ex2Pkgs(1).rd.poke(5.U)
      c.io.backend.ex2Pkgs(1).rdValid.poke(true.B)
      c.io.backend.ex2Pkgs(1).op.poke(FCVT_W_S)
      c.io.frontend.stall.expect(false.B)
    }
  }
}
