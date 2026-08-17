import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp._

class MulBranchForwardTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Forward branch operand isolation"

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
    for (lane <- 0 until 8) {
      clearPackage(c.io.ex1Pkgs(lane))
      clearPackage(c.io.ex2Pkgs(lane))
      clearPackage(c.io.ex3Pkgs(lane))
      clearPackage(c.io.wbPkgs(lane))
      c.io.ex3GprData(lane).poke(0.U)
      c.io.wbGprValid(lane).poke(false.B)
      c.io.wbGprAddr(lane).poke(0.U)
      c.io.wbGprData(lane).poke(0.U)
      c.io.wbFprValid(lane).poke(false.B)
      c.io.wbFprAddr(lane).poke(0.U)
      c.io.wbFprData(lane).poke(0.U)
    }
  }

  it should "keep EX3 multiply data for ALUs but physically exclude it from Branch" in {
    test(new Forward).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearForward(c)
      c.io.ex1Pkgs(7).rs1.poke(5.U)
      c.io.ex1Pkgs(7).rs1Data.poke("h11111111".U)
      c.io.ex3Pkgs(3).rd.poke(5.U)
      c.io.ex3Pkgs(3).rdValid.poke(true.B)
      c.io.ex3Pkgs(3).op.poke(MUL)
      c.io.ex3Pkgs(3).aluResult.poke("h22222222".U)
      c.io.ex3GprData(3).poke("hdeadbeef".U)

      c.io.fwdGprRs1Data(7).expect("hdeadbeef".U)
      c.io.fwdBranchRs1Data.expect("h11111111".U)

      c.io.wbGprValid(3).poke(true.B)
      c.io.wbGprAddr(3).poke(5.U)
      c.io.wbGprData(3).poke("hdeadbeef".U)
      c.io.fwdBranchRs1Data.expect("hdeadbeef".U)
    }
  }

  it should "preserve EX3 forwarding from ordinary ALU producers into Branch" in {
    test(new Forward).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearForward(c)
      c.io.ex1Pkgs(7).rs1.poke(5.U)
      c.io.ex1Pkgs(7).rs2.poke(6.U)
      c.io.ex1Pkgs(7).rs1Data.poke("h11111111".U)
      c.io.ex1Pkgs(7).rs2Data.poke("h22222222".U)

      c.io.ex3Pkgs(3).rd.poke(5.U)
      c.io.ex3Pkgs(3).rdValid.poke(true.B)
      c.io.ex3Pkgs(3).op.poke(ADD)
      c.io.ex3Pkgs(3).aluResult.poke("h33333333".U)
      c.io.ex3GprData(3).poke("h33333333".U)

      c.io.ex3Pkgs(4).rd.poke(6.U)
      c.io.ex3Pkgs(4).rdValid.poke(true.B)
      c.io.ex3Pkgs(4).op.poke(SUB)
      c.io.ex3Pkgs(4).aluResult.poke("h44444444".U)
      c.io.ex3GprData(4).poke("h44444444".U)

      c.io.fwdBranchRs1Data.expect("h33333333".U)
      c.io.fwdBranchRs2Data.expect("h44444444".U)
    }
  }

}
