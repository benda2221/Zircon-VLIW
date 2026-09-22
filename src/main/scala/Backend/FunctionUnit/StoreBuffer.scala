import chisel3._

class StoreBufferIO extends Bundle {
    val enqueue = Input(new StoreRequest)
    // ex3Flush kills the younger EX2 package entering this buffer.  It must
    // not kill the entry that is already aligned with the current EX3 package.
    val killIncoming = Input(Bool())
    val stageAdvance = Input(Bool())

    val entry  = Output(new StoreRequest)
    val commit = Output(new StoreRequest)
}

// One entry is sufficient while the external store channel is always ready:
// the current entry commits while the next EX2 store is captured.
class StoreBuffer extends Module {
    val io = IO(new StoreBufferIO)

    val entryReg = RegInit(0.U.asTypeOf(new StoreRequest))

    io.entry := entryReg
    io.commit := entryReg
    io.commit.valid := entryReg.valid && io.stageAdvance

    when(io.stageAdvance) {
        entryReg := io.enqueue
        entryReg.valid := io.enqueue.valid && !io.killIncoming
    }
}
