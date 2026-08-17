import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp._

class IntegerMulForwardTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ALUiMDPipeline integer multiply forwarding"

  private val mask32 = (BigInt(1) << 32) - 1

  private def signed32(value: BigInt): BigInt = {
    val bits = value & mask32
    if (bits.testBit(31)) bits - (BigInt(1) << 32) else bits
  }

  private def expected(op: BigInt, src1: BigInt, src2: BigInt): BigInt = {
    val unsigned1 = src1 & mask32
    val unsigned2 = src2 & mask32
    val product = op match {
      case value if value == MUL.litValue || value == MULH.litValue =>
        signed32(unsigned1) * signed32(unsigned2)
      case value if value == MULHSU.litValue =>
        signed32(unsigned1) * unsigned2
      case value if value == MULHU.litValue =>
        unsigned1 * unsigned2
    }

    if (op == MUL.litValue) product & mask32 else (product >> 32) & mask32
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

  private def clearControl(c: ALUiMDPipeline): Unit = {
    c.io.forward.fwdRs1Data.poke(0.U)
    c.io.forward.fwdRs2Data.poke(0.U)
    c.io.forward.fwdRs3Data.poke(0.U)
    c.io.forward.fwdGprRs1Data.poke(0.U)
    c.io.forward.fwdGprRs2Data.poke(0.U)
    c.io.forward.fwdFprRs1Data.poke(0.U)
    c.io.forward.fwdFprRs2Data.poke(0.U)
    c.io.hazard.ex1Flush.poke(false.B)
    c.io.hazard.ex1Stall.poke(false.B)
    c.io.hazard.ex2Flush.poke(false.B)
    c.io.hazard.ex2Stall.poke(false.B)
    c.io.hazard.ex3Flush.poke(false.B)
    c.io.hazard.ex3Stall.poke(false.B)
    c.io.hazard.wbFlush.poke(false.B)
    c.io.hazard.wbStall.poke(false.B)
  }

  it should "align all four multiply results with the EX3 GPR data path" in {
    test(new ALUiMDPipeline).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearControl(c)
      clearPackage(c.io.backend.instPkgIn)

      val vectors = Seq(
        (MUL.litValue, BigInt("fedcba98", 16), BigInt("89abcdef", 16)),
        (MULH.litValue, BigInt("80000001", 16), BigInt("fffffffd", 16)),
        (MULHSU.litValue, BigInt("80000001", 16), BigInt("ffffffff", 16)),
        (MULHU.litValue, BigInt("fedcba98", 16), BigInt("89abcdef", 16))
      )

      vectors.zipWithIndex.foreach { case ((op, src1, src2), index) =>
        c.io.backend.instPkgIn.inst.poke((index + 1).U)
        c.io.backend.instPkgIn.rd.poke(5.U)
        c.io.backend.instPkgIn.rdValid.poke(true.B)
        c.io.backend.instPkgIn.op.poke(op.U)
        c.io.forward.fwdRs1Data.poke(src1.U)
        c.io.forward.fwdRs2Data.poke(src2.U)
        c.clock.step()

        clearPackage(c.io.backend.instPkgIn)
        c.io.forward.fwdRs1Data.poke(0.U)
        c.io.forward.fwdRs2Data.poke(0.U)
        c.clock.step()

        c.io.forward.ex3Pkg.inst.expect((index + 1).U)
        c.io.forward.ex3Pkg.rdValid.expect(true.B)
        c.io.forward.ex3Pkg.op.expect(op.U)
        c.io.forward.ex3GprData.expect(expected(op, src1, src2).U)

        c.clock.step(2)
      }
    }
  }

  it should "keep multiply data aligned across global stalls and suppress flushed packages" in {
    test(new ALUiMDPipeline).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearControl(c)
      clearPackage(c.io.backend.instPkgIn)

      c.io.backend.instPkgIn.inst.poke(1.U)
      c.io.backend.instPkgIn.rd.poke(5.U)
      c.io.backend.instPkgIn.rdValid.poke(true.B)
      c.io.backend.instPkgIn.op.poke(MUL)
      c.io.forward.fwdRs1Data.poke(7.U)
      c.io.forward.fwdRs2Data.poke(9.U)
      c.clock.step()

      clearPackage(c.io.backend.instPkgIn)
      c.io.forward.fwdRs1Data.poke(0.U)
      c.io.forward.fwdRs2Data.poke(0.U)
      c.io.hazard.ex1Stall.poke(true.B)
      c.io.hazard.ex2Stall.poke(true.B)
      c.io.hazard.ex3Stall.poke(true.B)
      c.clock.step(3)
      c.io.forward.ex2Pkg.inst.expect(1.U)
      c.io.forward.ex3Pkg.rdValid.expect(false.B)

      c.io.hazard.ex1Stall.poke(false.B)
      c.io.hazard.ex2Stall.poke(false.B)
      c.io.hazard.ex3Stall.poke(false.B)
      c.clock.step()
      c.io.forward.ex3Pkg.inst.expect(1.U)
      c.io.forward.ex3Pkg.rdValid.expect(true.B)
      c.io.forward.ex3GprData.expect(63.U)

      c.clock.step(2)
      c.io.backend.instPkgIn.inst.poke(2.U)
      c.io.backend.instPkgIn.rd.poke(6.U)
      c.io.backend.instPkgIn.rdValid.poke(true.B)
      c.io.backend.instPkgIn.op.poke(MUL)
      c.io.forward.fwdRs1Data.poke(11.U)
      c.io.forward.fwdRs2Data.poke(13.U)
      c.io.hazard.ex2Flush.poke(true.B)
      c.clock.step()

      clearPackage(c.io.backend.instPkgIn)
      c.io.forward.fwdRs1Data.poke(0.U)
      c.io.forward.fwdRs2Data.poke(0.U)
      c.io.hazard.ex2Flush.poke(false.B)
      c.clock.step()
      c.io.forward.ex3Pkg.rdValid.expect(false.B)
    }
  }
}
