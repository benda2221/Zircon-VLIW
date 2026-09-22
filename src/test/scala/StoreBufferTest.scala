import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp._

object StoreBufferTestUtil {
    def clearStore(req: StoreRequest): Unit = {
        req.valid.poke(false.B)
        req.op.poke(0.U)
        req.addr.poke(0.U)
        req.data.poke(0.U)
        req.pc.poke(0.U)
        req.inst.poke(0.U)
    }

    def setStore(req: StoreRequest, op: UInt, addr: BigInt,
                 data: BigInt, pc: BigInt = 0x80000000L,
                 inst: BigInt = 0x23L): Unit = {
        req.valid.poke(true.B)
        req.op.poke(op)
        req.addr.poke(addr.U)
        req.data.poke(data.U)
        req.pc.poke(pc.U)
        req.inst.poke(inst.U)
    }

    def clearLoad(req: LoadRequest): Unit = {
        req.valid.poke(false.B)
        req.op.poke(0.U)
        req.addr.poke(0.U)
    }
}

class StoreBufferTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "EX3-aligned StoreBuffer"

    it should "capture every store, hold on stall, and kill only the incoming entry" in {
        test(new StoreBuffer) { c =>
            StoreBufferTestUtil.clearStore(c.io.enqueue)
            c.io.killIncoming.poke(false.B)
            c.io.stageAdvance.poke(true.B)
            c.reset.poke(true.B)
            c.clock.step()
            c.reset.poke(false.B)
            c.io.entry.valid.expect(false.B)
            c.io.commit.valid.expect(false.B)

            StoreBufferTestUtil.setStore(
                c.io.enqueue, SW, 0x1000, 0x11223344, inst = 0x00102023)
            c.io.commit.valid.expect(false.B)
            c.clock.step()
            c.io.entry.valid.expect(true.B)
            c.io.entry.addr.expect(0x1000.U)
            c.io.entry.data.expect("h11223344".U)
            c.io.commit.valid.expect(true.B)

            // A stalled EX3 stage neither commits nor overwrites the entry.
            c.io.stageAdvance.poke(false.B)
            StoreBufferTestUtil.setStore(c.io.enqueue, SB, 0x2000, 0x55)
            c.io.commit.valid.expect(false.B)
            c.clock.step(2)
            c.io.entry.addr.expect(0x1000.U)
            c.io.entry.data.expect("h11223344".U)

            // ex3Flush kills the younger enqueue, while the current EX3 entry
            // remains eligible to commit in this cycle.
            c.io.stageAdvance.poke(true.B)
            c.io.killIncoming.poke(true.B)
            c.io.commit.valid.expect(true.B)
            c.io.commit.addr.expect(0x1000.U)
            c.clock.step()
            c.io.entry.valid.expect(false.B)
            c.io.commit.valid.expect(false.B)
        }
    }

    it should "commit one entry while capturing the next consecutive store" in {
        test(new StoreBuffer) { c =>
            StoreBufferTestUtil.clearStore(c.io.enqueue)
            c.io.killIncoming.poke(false.B)
            c.io.stageAdvance.poke(true.B)
            c.reset.poke(true.B)
            c.clock.step()
            c.reset.poke(false.B)

            StoreBufferTestUtil.setStore(c.io.enqueue, SW, 0x1000, 0x11111111)
            c.clock.step()
            c.io.commit.valid.expect(true.B)
            c.io.commit.data.expect("h11111111".U)

            StoreBufferTestUtil.setStore(c.io.enqueue, SW, 0x1004, 0x22222222)
            c.io.commit.data.expect("h11111111".U)
            c.clock.step()
            c.io.commit.valid.expect(true.B)
            c.io.commit.addr.expect(0x1004.U)
            c.io.commit.data.expect("h22222222".U)
        }
    }
}

class LSUMemoryOrderTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "dual-lane LSU memory ordering"

    private def initialize(c: LSUMemoryOrder): Unit = {
        for (lane <- 0 until 2) {
            StoreBufferTestUtil.clearLoad(c.io.pipeline(lane).loadReq)
            StoreBufferTestUtil.clearStore(c.io.pipeline(lane).storeReq)
            c.io.ex3Flush(lane).poke(false.B)
            c.io.ex3Stall(lane).poke(false.B)
        }
        c.io.mem.lsu0.load.rdata.poke(0.U)
        c.io.mem.lsu1.load.rdata.poke(0.U)
    }

    it should "drop a flushed EX2 store without suppressing an older commit" in {
        test(new LSUMemoryOrder) { c =>
            initialize(c)
            c.reset.poke(true.B)
            c.clock.step()
            c.reset.poke(false.B)

            StoreBufferTestUtil.setStore(
                c.io.pipeline(0).storeReq, SW, 0x1000, 0x11223344)
            c.clock.step()
            c.io.mem.lsu0.store.valid.expect(true.B)
            c.io.mem.lsu0.store.addr.expect(0x1000.U)

            StoreBufferTestUtil.setStore(
                c.io.pipeline(0).storeReq, SB, 0x2000, 0x55)
            c.io.ex3Flush(0).poke(true.B)
            // This is the previous packet's commit, not the killed enqueue.
            c.io.mem.lsu0.store.valid.expect(true.B)
            c.io.mem.lsu0.store.addr.expect(0x1000.U)
            c.clock.step()
            c.io.mem.lsu0.store.valid.expect(false.B)
        }
    }

    it should "forward both older buffers in slot order with byte precision" in {
        test(new LSUMemoryOrder) { c =>
            initialize(c)
            c.reset.poke(true.B)
            c.clock.step()
            c.reset.poke(false.B)

            StoreBufferTestUtil.setStore(
                c.io.pipeline(0).storeReq, SW, 0x100, 0xaabbccddL)
            StoreBufferTestUtil.setStore(
                c.io.pipeline(1).storeReq, SB, 0x101, 0xee)
            c.clock.step()

            StoreBufferTestUtil.clearStore(c.io.pipeline(0).storeReq)
            StoreBufferTestUtil.clearStore(c.io.pipeline(1).storeReq)
            c.io.pipeline(0).loadReq.valid.poke(true.B)
            c.io.pipeline(0).loadReq.op.poke(LW)
            c.io.pipeline(0).loadReq.addr.poke(0x100.U)
            c.io.mem.lsu0.load.rdata.poke("h11223344".U)

            // slot5 SW gives 0xaabbccdd, then the later slot6 SB changes
            // address 0x101 (byte 1) to 0xee.
            c.io.pipeline(0).loadData.expect("haabbeedd".U)
        }
    }

    it should "forward a current slot-5 store only to a same-packet slot-6 load" in {
        test(new LSUMemoryOrder) { c =>
            initialize(c)
            c.reset.poke(true.B)
            c.clock.step()
            c.reset.poke(false.B)

            StoreBufferTestUtil.setStore(
                c.io.pipeline(0).storeReq, SW, 0x200, 0x44332211)
            c.io.pipeline(1).loadReq.valid.poke(true.B)
            c.io.pipeline(1).loadReq.op.poke(LBU)
            c.io.pipeline(1).loadReq.addr.poke(0x202.U)
            c.io.mem.lsu1.load.rdata.poke("haabbccdd".U)
            c.io.pipeline(1).loadData.expect("haabbcc33".U)

            // Reverse ordering is forbidden: current slot6 is later than a
            // current slot5 load and must not affect it.
            StoreBufferTestUtil.clearStore(c.io.pipeline(0).storeReq)
            StoreBufferTestUtil.clearLoad(c.io.pipeline(1).loadReq)
            c.io.pipeline(0).loadReq.valid.poke(true.B)
            c.io.pipeline(0).loadReq.op.poke(LW)
            c.io.pipeline(0).loadReq.addr.poke(0x300.U)
            StoreBufferTestUtil.setStore(
                c.io.pipeline(1).storeReq, SW, 0x300, 0xdeadbeefL)
            c.io.mem.lsu0.load.rdata.poke("h12345678".U)
            c.io.pipeline(0).loadData.expect("h12345678".U)
        }
    }
}

class ShallowBranchStoreBufferCPUTest extends AnyFlatSpec
    with ChiselScalatestTester {
    behavior of "CPU StoreBuffer around an EX3 shallow branch redirect"

    private def encodeB(imm: Int, rs2: Int, rs1: Int,
                        funct3: Int): BigInt = {
        val encodedImm = BigInt(imm) & 0x1fff
        (((encodedImm >> 12) & 0x1) << 31) |
        (((encodedImm >> 5) & 0x3f) << 25) |
        (BigInt(rs2) << 20) |
        (BigInt(rs1) << 15) |
        (BigInt(funct3) << 12) |
        (((encodedImm >> 1) & 0xf) << 8) |
        (((encodedImm >> 11) & 0x1) << 7) |
        0x63
    }

    it should "commit the branch packet store and discard the younger store" in {
        test(new CPU).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            val nop = BigInt("00000013", 16)
            val base = BigInt("80000000", 16)
            val storeBase = BigInt("80001000", 16)

            val setupPacket = Seq(
                nop,
                BigInt("800012b7", 16), // slot1: lui  x5,0x80001
                BigInt("05500313", 16), // slot2: addi x6,x0,0x55
                nop, nop, nop, nop, nop
            )
            val shallowBranchPacket = Seq(
                nop,
                BigInt("00100393", 16), // slot1: addi x7,x0,1
                nop, nop, nop,
                BigInt("0062a023", 16), // slot5: sw x6,0(x5), legal
                nop,
                encodeB(0x24, 0, 7, 1) // slot7: bne x7,x0,packet3
            )
            val wrongPathPacket = Seq(
                nop, nop, nop, nop, nop,
                BigInt("0062a223", 16), // slot5: sw x6,4(x5), must die
                nop, nop
            )

            c.io.dmem.lsu0.load.rdata.poke(0.U)
            c.io.dmem.lsu1.load.rdata.poke(0.U)
            c.io.imem.insts.foreach(_.poke(nop.U))
            c.reset.poke(true.B)
            c.clock.step(2)
            c.reset.poke(false.B)

            var legalStores = 0
            var wrongPathStores = 0
            for (_ <- 0 until 24) {
                val pc = c.io.imem.pc.peek().litValue
                val packet = pc match {
                    case value if value == base        => setupPacket
                    case value if value == base + 0x20 => shallowBranchPacket
                    case value if value == base + 0x40 => wrongPathPacket
                    case _ => Seq.fill(8)(nop)
                }
                for (slot <- 0 until 8) {
                    c.io.imem.insts(slot).poke(packet(slot).U)
                }

                if (c.io.dmem.lsu0.store.valid.peek().litValue != 0) {
                    val addr = c.io.dmem.lsu0.store.addr.peek().litValue
                    if (addr == storeBase) legalStores += 1
                    if (addr == storeBase + 4) wrongPathStores += 1
                }
                c.clock.step()
            }

            assert(legalStores == 1,
                s"expected one same-packet store commit, saw $legalStores")
            assert(wrongPathStores == 0,
                s"wrong-path store committed $wrongPathStores time(s)")
        }
    }
}
