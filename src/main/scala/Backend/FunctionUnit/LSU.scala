import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._
import ZirconUtil._

class LoadRequest extends Bundle {
    val valid = Bool()
    val op    = UInt(7.W)
    val addr  = UInt(32.W)
}

class StoreRequest extends Bundle {
    val valid = Bool()
    val op    = UInt(7.W)
    val addr  = UInt(32.W)
    val data  = UInt(32.W)
    // These fields do not affect memory, but make buffer failures traceable.
    val pc    = UInt(32.W)
    val inst  = UInt(32.W)
}

// Interface between an EX2 LSU pipeline and the shared memory-order unit.
class PipelineLSUIO extends Bundle {
    val loadReq  = Output(new LoadRequest)
    val storeReq = Output(new StoreRequest)
    // Right-justified load bytes after store-buffer forwarding.
    val loadData = Input(UInt(32.W))
}

// External memory interface.  A buffered EX3 store and an EX2 load can be
// active in the same cycle, so load and store use independent channels.
class LoadMemIO extends Bundle {
    val valid = Output(Bool())
    val op    = Output(UInt(7.W))
    val addr  = Output(UInt(32.W))
    val rdata = Input(UInt(32.W))
}

class StoreMemIO extends Bundle {
    val valid = Output(Bool())
    val op    = Output(UInt(7.W))
    val addr  = Output(UInt(32.W))
    val data  = Output(UInt(32.W))
}

class LSUMemIO extends Bundle {
    val load  = new LoadMemIO
    val store = new StoreMemIO
}

// Backend memory interface for the two memory lanes (slots 5 and 6).
class BackendMemIO extends Bundle {
    val lsu0 = new LSUMemIO
    val lsu1 = new LSUMemIO
}

class LSUIO extends Bundle {
    val valid = Input(Bool())
    val op    = Input(UInt(7.W))
    val addr  = Input(UInt(32.W))
    val wdata = Input(UInt(32.W))
    val pc    = Input(UInt(32.W))
    val inst  = Input(UInt(32.W))
    val pipeline = new PipelineLSUIO
    val res   = Output(UInt(32.W))
}

class LSU extends Module {
    val io = IO(new LSUIO)

    val isLoad = io.op === LB || io.op === LH || io.op === LW ||
                 io.op === LBU || io.op === LHU || io.op === FLW
    val isStore = io.op === SB || io.op === SH ||
                  io.op === SW || io.op === FSW

    io.pipeline.loadReq.valid := io.valid && isLoad
    io.pipeline.loadReq.op := io.op
    io.pipeline.loadReq.addr := io.addr

    io.pipeline.storeReq.valid := io.valid && isStore
    io.pipeline.storeReq.op := io.op
    io.pipeline.storeReq.addr := io.addr
    io.pipeline.storeReq.data := io.wdata
    io.pipeline.storeReq.pc := io.pc
    io.pipeline.storeReq.inst := io.inst

    val memData = io.pipeline.loadData
    
    // 根据load指令类型进行扩展
    val res = WireDefault(memData)
    switch(io.op) {
        is(LB) {
            // 符号扩展8位
            res := SE(memData(7, 0), 32)
        }
        is(LH) {
            // 符号扩展16位
            res := SE(memData(15, 0), 32)
        }
        is(LW) {
            // 32位直接返回
            res := memData
        }
        is(LBU) {
            // 零扩展8位
            res := ZE(memData(7, 0), 32)
        }
        is(LHU) {
            // 零扩展16位
            res := ZE(memData(15, 0), 32)
        }
        is(FLW) {
            // 浮点加载32位
            res := memData
        }
    }
    
    io.res := res
}
