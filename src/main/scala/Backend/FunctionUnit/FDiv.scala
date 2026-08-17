import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._
import zirconfp.{FDiv, FSqrt}

/**
 * FDivWrapper包装类 - 封装独立的FDiv和FSqrt模块
 * 支持: FDIV.S, FSQRT.S
 * 特点: 使用握手协议，计算周期不固定
 */
class FDivWrapperIO extends Bundle {
    val rs1Data = Input(UInt(32.W))
    val rs2Data = Input(UInt(32.W))
    val op = Input(UInt(7.W))
    val rm = Input(UInt(3.W))
    val valid = Input(Bool())       // 输入有效信号
    val kill = Input(Bool())        // 终止当前运算
    val res = Output(UInt(32.W))
    val fflags = Output(UInt(5.W))
    val busy = Output(Bool())       // 除法器忙信号
    val ready = Output(Bool())      // 可以接受新输入
}

class FDivWrapper extends Module {
    val io = IO(new FDivWrapperIO)

    val fdiv = Module(new FDiv)
    val fsqrt = Module(new FSqrt)

    val requestIsSqrt = io.op === FSQRT_S
    val activeIsSqrt = RegInit(false.B)
    val unitBusy = fdiv.io.busy || fsqrt.io.busy
    val selectedReady = Mux(requestIsSqrt, fsqrt.io.inReady, fdiv.io.inReady)
    val requestFire = io.valid && !unitBusy && selectedReady

    when(requestFire) {
        activeIsSqrt := requestIsSqrt
    }

    fdiv.io.src1 := io.rs1Data
    fdiv.io.src2 := io.rs2Data
    fdiv.io.rm := io.rm
    fdiv.io.inValid := requestFire && !requestIsSqrt
    fdiv.io.outReady := true.B
    fdiv.io.kill := io.kill

    fsqrt.io.src := io.rs1Data
    fsqrt.io.rm := io.rm
    fsqrt.io.inValid := requestFire && requestIsSqrt
    fsqrt.io.outReady := true.B
    fsqrt.io.kill := io.kill

    io.res := Mux(activeIsSqrt, fsqrt.io.result, fdiv.io.result)
    io.fflags := Mux(activeIsSqrt, fsqrt.io.fflags, fdiv.io.fflags)
    io.busy := unitBusy
    io.ready := !unitBusy && selectedReady
}

