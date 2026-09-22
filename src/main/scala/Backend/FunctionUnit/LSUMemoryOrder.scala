import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._

class LSUMemoryOrderIO extends Bundle {
    val pipeline = Vec(2, Flipped(new PipelineLSUIO))
    val ex3Flush = Input(Vec(2, Bool()))
    val ex3Stall = Input(Vec(2, Bool()))
    val mem = new BackendMemIO
}

// Owns the two EX3-aligned store buffers and preserves memory order between
// slots 5 and 6.  The external memory model is always ready in this experiment.
class LSUMemoryOrder extends Module {
    val io = IO(new LSUMemoryOrderIO)

    private def loadBytes(op: UInt): UInt = {
        MuxLookup(op, 0.U(3.W))(Seq(
            LB  -> 1.U(3.W), LBU -> 1.U(3.W),
            LH  -> 2.U(3.W), LHU -> 2.U(3.W),
            LW  -> 4.U(3.W), FLW -> 4.U(3.W)
        ))
    }

    private def storeBytes(op: UInt): UInt = {
        MuxLookup(op, 0.U(3.W))(Seq(
            SB  -> 1.U(3.W),
            SH  -> 2.U(3.W),
            SW  -> 4.U(3.W), FSW -> 4.U(3.W)
        ))
    }

    // The memory response is right-justified at the requested byte address.
    // Overlay a store byte-by-byte so partial SB/SH forwarding is also exact.
    private def overlayStore(baseData: UInt, load: LoadRequest,
                             store: StoreRequest): UInt = {
        val resultBytes = Wire(Vec(4, UInt(8.W)))
        val loadSize = loadBytes(load.op)
        val storeSize = storeBytes(store.op)

        for (loadByte <- 0 until 4) {
            resultBytes(loadByte) := baseData(8 * loadByte + 7, 8 * loadByte)
            for (storeByte <- 0 until 4) {
                when(load.valid && store.valid &&
                     loadByte.U < loadSize && storeByte.U < storeSize &&
                     (load.addr + loadByte.U) ===
                         (store.addr + storeByte.U)) {
                    resultBytes(loadByte) :=
                        store.data(8 * storeByte + 7, 8 * storeByte)
                }
            }
        }
        Cat(resultBytes.reverse)
    }

    val buffers = Seq.fill(2)(Module(new StoreBuffer))
    val memPorts = Seq(io.mem.lsu0, io.mem.lsu1)

    for (lane <- 0 until 2) {
        buffers(lane).io.enqueue := io.pipeline(lane).storeReq
        buffers(lane).io.killIncoming := io.ex3Flush(lane)
        buffers(lane).io.stageAdvance := !io.ex3Stall(lane)

        // A killed/stalled EX2 load must not cause an external side effect.
        memPorts(lane).load.valid := io.pipeline(lane).loadReq.valid &&
            !io.ex3Flush(lane) && !io.ex3Stall(lane)
        memPorts(lane).load.op := io.pipeline(lane).loadReq.op
        memPorts(lane).load.addr := io.pipeline(lane).loadReq.addr

        // Store commit belongs to the current EX3 package.  ex3Flush only
        // kills the younger enqueue and therefore is intentionally absent.
        memPorts(lane).store.valid := buffers(lane).io.commit.valid
        memPorts(lane).store.op := buffers(lane).io.commit.op
        memPorts(lane).store.addr := buffers(lane).io.commit.addr
        memPorts(lane).store.data := buffers(lane).io.commit.data
    }

    // Both pending buffer entries are older than either current EX2 load.
    // Slot 6 is later than slot 5 and therefore overlays it on byte conflicts.
    val load0AfterSlot5 = overlayStore(
        io.mem.lsu0.load.rdata,
        io.pipeline(0).loadReq,
        buffers(0).io.entry
    )
    val load0AfterSlot6 = overlayStore(
        load0AfterSlot5,
        io.pipeline(0).loadReq,
        buffers(1).io.entry
    )

    val load1AfterSlot5 = overlayStore(
        io.mem.lsu1.load.rdata,
        io.pipeline(1).loadReq,
        buffers(0).io.entry
    )
    val load1AfterSlot6 = overlayStore(
        load1AfterSlot5,
        io.pipeline(1).loadReq,
        buffers(1).io.entry
    )

    // In the current packet slot 5 precedes slot 6.  Its incoming store must
    // be visible to a same-packet slot-6 load even though it has not reached
    // the registered store buffer yet.  Never forward in the reverse direction.
    val currentSlot5Store = WireDefault(io.pipeline(0).storeReq)
    currentSlot5Store.valid := io.pipeline(0).storeReq.valid &&
        !io.ex3Flush(0) && !io.ex3Stall(0)
    val load1AfterCurrentSlot5 = overlayStore(
        load1AfterSlot6,
        io.pipeline(1).loadReq,
        currentSlot5Store
    )

    io.pipeline(0).loadData := load0AfterSlot6
    io.pipeline(1).loadData := load1AfterCurrentSlot5
}
