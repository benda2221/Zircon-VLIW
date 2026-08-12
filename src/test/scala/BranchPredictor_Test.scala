import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BranchPredictorTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "BranchPredictor"

  private def encodeBranch(offset: Int, funct3: Int = 1, rs1: Int = 1, rs2: Int = 2): BigInt = {
    val imm = offset & 0x1fff
    BigInt(
      (((imm >> 12) & 0x1) << 31) |
      (((imm >> 5) & 0x3f) << 25) |
      ((rs2 & 0x1f) << 20) |
      ((rs1 & 0x1f) << 15) |
      ((funct3 & 0x7) << 12) |
      (((imm >> 1) & 0xf) << 8) |
      (((imm >> 11) & 0x1) << 7) |
      0x63
    ) & 0xffffffffL
  }

  private def encodeJal(offset: Int, rd: Int = 1): BigInt = {
    val imm = offset & 0x1fffff
    BigInt(
      (((imm >> 20) & 0x1) << 31) |
      (((imm >> 1) & 0x3ff) << 21) |
      (((imm >> 11) & 0x1) << 20) |
      (((imm >> 12) & 0xff) << 12) |
      ((rd & 0x1f) << 7) |
      0x6f
    ) & 0xffffffffL
  }

  private def encodeJalr(rd: Int, rs1: Int, offset: Int = 0): BigInt = {
    BigInt(
      ((offset & 0xfff) << 20) |
      ((rs1 & 0x1f) << 15) |
      ((rd & 0x1f) << 7) |
      0x67
    ) & 0xffffffffL
  }

  private def clearUpdate(c: BranchPredictor): Unit = {
    c.io.update.valid.poke(false.B)
    c.io.update.pc.poke(0.U)
    c.io.update.inst.poke(0.U)
    c.io.update.taken.poke(false.B)
    c.io.update.target.poke(0.U)
  }

  it should "predict cold backward branches and train conditional direction" in {
    test(new BranchPredictor).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearUpdate(c)
      c.io.lookup.packetPC.poke("h1000".U)
      c.io.lookup.slot7Inst.poke(encodeBranch(-32).U)
      c.io.lookup.taken.expect(true.B)
      c.io.lookup.target.expect("h0ffc".U)

      val forwardBranch = encodeBranch(32)
      c.io.lookup.slot7Inst.poke(forwardBranch.U)
      c.io.lookup.taken.expect(false.B)
      c.io.lookup.target.expect("h103c".U)

      c.io.update.valid.poke(true.B)
      c.io.update.pc.poke("h101c".U)
      c.io.update.inst.poke(forwardBranch.U)
      c.io.update.taken.poke(true.B)
      c.io.update.target.poke("h103c".U)
      c.clock.step()
      clearUpdate(c)
      c.io.lookup.taken.expect(true.B)

      c.io.update.valid.poke(true.B)
      c.io.update.pc.poke("h101c".U)
      c.io.update.inst.poke(forwardBranch.U)
      c.io.update.taken.poke(false.B)
      c.clock.step()
      clearUpdate(c)
      c.io.lookup.taken.expect(false.B)
    }
  }

  it should "keep branches that collided in the smaller table independent" in {
    test(new BranchPredictor(directionEntries = 64)).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearUpdate(c)
      val backwardBranch = encodeBranch(-32)
      val forwardBranch = encodeBranch(32)
      val backwardPC = 0x101c
      val forwardPC = 0x121c

      for (_ <- 0 until 3) {
        c.io.update.valid.poke(true.B)
        c.io.update.pc.poke(backwardPC.U)
        c.io.update.inst.poke(backwardBranch.U)
        c.io.update.taken.poke(true.B)
        c.clock.step()

        c.io.update.pc.poke(forwardPC.U)
        c.io.update.inst.poke(forwardBranch.U)
        c.io.update.taken.poke(false.B)
        c.clock.step()
      }
      clearUpdate(c)

      c.io.lookup.packetPC.poke((backwardPC - 28).U)
      c.io.lookup.slot7Inst.poke(backwardBranch.U)
      c.io.lookup.taken.expect(true.B)

      c.io.lookup.packetPC.poke((forwardPC - 28).U)
      c.io.lookup.slot7Inst.poke(forwardBranch.U)
      c.io.lookup.taken.expect(false.B)
    }
  }

  it should "bypass same-cycle direction and indirect-target training" in {
    test(new BranchPredictor).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearUpdate(c)
      val branchPC = 0x101c
      val forwardBranch = encodeBranch(32)
      c.io.lookup.packetPC.poke((branchPC - 28).U)
      c.io.lookup.slot7Inst.poke(forwardBranch.U)
      c.io.lookup.taken.expect(false.B)

      c.io.update.valid.poke(true.B)
      c.io.update.pc.poke(branchPC.U)
      c.io.update.inst.poke(forwardBranch.U)
      c.io.update.taken.poke(true.B)
      c.io.update.target.poke("h103c".U)
      c.io.lookup.taken.expect(true.B)

      val indirect = encodeJalr(rd = 0, rs1 = 6)
      c.io.update.inst.poke(indirect.U)
      c.io.update.target.poke("h2220".U)
      c.io.lookup.slot7Inst.poke(indirect.U)
      c.io.lookup.taken.expect(true.B)
      c.io.lookup.target.expect("h2220".U)
    }
  }

  it should "learn a stable backward-loop trip count" in {
    test(new BranchPredictor).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearUpdate(c)
      val branchPC = 0x101c
      val loopBranch = encodeBranch(-32)

      def update(taken: Boolean): Unit = {
        c.io.update.valid.poke(true.B)
        c.io.update.pc.poke(branchPC.U)
        c.io.update.inst.poke(loopBranch.U)
        c.io.update.taken.poke(taken.B)
        c.clock.step()
      }

      for (_ <- 0 until 4) {
        update(true)
        update(true)
        update(true)
        update(false)
      }
      clearUpdate(c)
      c.io.lookup.packetPC.poke((branchPC - 28).U)
      c.io.lookup.slot7Inst.poke(loopBranch.U)

      c.io.lookup.taken.expect(true.B)
      update(true)
      c.io.lookup.taken.expect(true.B)
      update(true)
      c.io.lookup.taken.expect(true.B)
      update(true)
      c.io.lookup.taken.expect(false.B)
    }
  }

  it should "fall back to the BHT when a learned loop runs longer" in {
    test(new BranchPredictor).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearUpdate(c)
      val branchPC = 0x101c
      val loopBranch = encodeBranch(-32)

      def update(taken: Boolean): Unit = {
        c.io.update.valid.poke(true.B)
        c.io.update.pc.poke(branchPC.U)
        c.io.update.inst.poke(loopBranch.U)
        c.io.update.taken.poke(taken.B)
        c.clock.step()
      }

      for (_ <- 0 until 4) {
        update(true)
        update(true)
        update(true)
        update(false)
      }

      c.io.lookup.packetPC.poke((branchPC - 28).U)
      c.io.lookup.slot7Inst.poke(loopBranch.U)
      update(true)
      update(true)
      update(true)
      c.io.lookup.taken.expect(false.B)

      update(true)
      clearUpdate(c)
      c.io.lookup.taken.expect(true.B)
    }
  }

  it should "predict direct and learned indirect jump targets" in {
    test(new BranchPredictor).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearUpdate(c)
      c.io.lookup.packetPC.poke("h1000".U)
      c.io.lookup.slot7Inst.poke(encodeJal(64).U)
      c.io.lookup.taken.expect(true.B)
      c.io.lookup.target.expect("h105c".U)

      val indirect = encodeJalr(rd = 0, rs1 = 6)
      c.io.lookup.slot7Inst.poke(indirect.U)
      c.io.lookup.taken.expect(false.B)

      c.io.update.valid.poke(true.B)
      c.io.update.pc.poke("h101c".U)
      c.io.update.inst.poke(indirect.U)
      c.io.update.taken.poke(true.B)
      c.io.update.target.poke("h2220".U)
      c.clock.step()
      clearUpdate(c)
      c.io.lookup.taken.expect(true.B)
      c.io.lookup.target.expect("h2220".U)
    }
  }

  it should "use resolved calls and returns to maintain the return stack" in {
    test(new BranchPredictor).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearUpdate(c)
      val call = encodeJal(64, rd = 1)
      val ret = encodeJalr(rd = 0, rs1 = 1)

      c.io.update.valid.poke(true.B)
      c.io.update.pc.poke("h101c".U)
      c.io.update.inst.poke(call.U)
      c.io.update.taken.poke(true.B)
      c.io.update.target.poke("h105c".U)
      c.clock.step()
      clearUpdate(c)

      c.io.lookup.packetPC.poke("h2000".U)
      c.io.lookup.slot7Inst.poke(ret.U)
      c.io.lookup.taken.expect(true.B)
      c.io.lookup.target.expect("h1020".U)

      c.io.update.valid.poke(true.B)
      c.io.update.pc.poke("h201c".U)
      c.io.update.inst.poke(ret.U)
      c.io.update.taken.poke(true.B)
      c.io.update.target.poke("h1020".U)
      c.clock.step()
      clearUpdate(c)
      c.io.lookup.taken.expect(false.B)
    }
  }
}

class BranchPredictionResolutionTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Branch prediction resolution"

  private def initialize(c: Branch): Unit = {
    c.io.src1.poke(7.U)
    c.io.src2.poke(7.U)
    c.io.op.poke(ZirconConfig.EXEOp.BEQ)
    c.io.pc.poke("h101c".U)
    c.io.imm.poke(32.U)
    c.io.predTaken.poke(false.B)
    c.io.predTarget.poke(0.U)
  }

  it should "detect direction and target misses and select the recovery PC" in {
    test(new Branch).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      initialize(c)
      c.io.predFail.expect(true.B)
      c.io.branchTgt.expect("h103c".U)

      c.io.predTaken.poke(true.B)
      c.io.predTarget.poke("h103c".U)
      c.io.predFail.expect(false.B)

      c.io.predTarget.poke("h203c".U)
      c.io.predFail.expect(true.B)
      c.io.branchTgt.expect("h103c".U)

      c.io.src2.poke(8.U)
      c.io.predTaken.poke(true.B)
      c.io.predFail.expect(true.B)
      c.io.branchTgt.expect("h1020".U)
    }
  }
}
