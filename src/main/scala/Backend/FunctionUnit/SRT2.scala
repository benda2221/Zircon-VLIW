import chisel3._
import chisel3.util._
import ZirconUtil._
import ZirconConfig.EXEOp._

class SRT2IO extends Bundle {
    val src1  = Input(UInt(32.W))
    val src2  = Input(UInt(32.W))
    val op    = Input(UInt(5.W))
    val valid = Input(Bool())
    val res   = Output(UInt(32.W))
    val busy  = Output(Bool())
    val ready = Output(Bool())
    val dbg   = Output(new SRT2DBG)
}

class SRT2DBG extends Bundle {
    val busyCycle = UInt(64.W)
}

class SRT2 extends Module {
    val io = IO(new SRT2IO)

    /* Stage 1: Initialization */

    // val signSrc1 = io.src1(31)
    // val signSrc2 = io.src2(31)
    val resSign  = Mux(io.op === DIV, io.src1(31) ^ io.src2(31), io.src1(31))
    val src1Neg  = BLevelPAdder32(~io.src1, 0.U, 1.U).io.res
    val src2Neg  = BLevelPAdder32(~io.src2, 0.U, 1.U).io.res
    // val src1Abs  = Mux(!io.op(0) && signSrc1, src1Neg, io.src1)
    // val src2Abs  = Mux(!io.op(0) && signSrc2, src2Neg, io.src2)

    val src1S2    = ShiftRegister(io.src1, 1, 0.U, io.valid)
    val src2S2    = ShiftRegister(io.src2, 1, 0.U, io.valid)
    val resSignS2 = ShiftRegister(resSign, 1, false.B, io.valid)
    val src1NegS2 = ShiftRegister(src1Neg, 1, 0.U, io.valid)
    val src2NegS2 = ShiftRegister(src2Neg, 1, 0.U, io.valid)
    val opS2      = ShiftRegister(io.op, 1, 0.U, io.valid)
    val validS2   = ShiftRegister(io.valid, 1, false.B, !io.busy)
    
    val src1AbsS2 = Mux(!opS2(0) && src1S2(31), src1NegS2, src1S2)(31, 0)
    val src2AbsS2 = Mux(!opS2(0) && src2S2(31), src2NegS2, src2S2)(31, 0)


    val en       = validS2 && opS2(2) && opS2(4)
    val iter     = RegInit(63.U(6.W))
    val rmdReg   = RegInit(0.U(65.W))
    val rmdMReg  = RegInit(0.U(33.W))
    val divReg   = RegInit(0.U(33.W))
    val adder    = BLevelPAdder33(rmdReg(63, 31), divReg, 0.U)
    io.busy      := !iter(5)


    def countLeadingZeros(x: UInt): UInt = {
        Log2Rev(Reverse(x))(4, 0)
    }

    val src1LeadingZeros = countLeadingZeros(src1AbsS2)
    val src2LeadingZeros = countLeadingZeros(src2AbsS2)
    val zeroOfSrc1GreaterThanSrc2 = src1LeadingZeros > src2LeadingZeros
    
    when(io.busy){
        iter := iter - 1.U
    }.elsewhen(en){
        when(zeroOfSrc1GreaterThanSrc2){
            iter := 63.U
        }.otherwise{
            iter := src2LeadingZeros - src1LeadingZeros
        }
    }
    /* stage 2: calculate the quotient */

    when(en && iter(5)){
        divReg     := src2AbsS2 << src2LeadingZeros
    }
    when(io.busy){
        // 3 cases, scan the top 2 bits of the remainder
        val top2 = rmdReg(63, 62)
        // if the top 2 bits are the same, q = 0, and shift the remainder left
        when(top2 === 0.U || top2 === 3.U){
            adder.io.src2 := 0.U
            rmdReg  := (adder.io.res ## rmdReg(30, 0) ## 0.U)
            rmdMReg := rmdMReg << 1
        }.otherwise{
            when(top2(1) === 1.U){
                // if the top 2 bits are not the same and is negative, q = -1, shift and add the divisor
                adder.io.src2 := divReg
                rmdReg        := (adder.io.res ## rmdReg(30, 0) ## 0.U)
                rmdMReg       := (rmdMReg << 1) | 1.U
            }.otherwise{
                // if the top 2 bits are not the same and is positive, q = 1, shift and sub the divisor
                adder.io.src2 := ~divReg
                adder.io.cin  := 1.U
                rmdReg        := (adder.io.res ## rmdReg(30, 0) ## 1.U) 
                rmdMReg       := rmdMReg << 1
            }
        }

    }.elsewhen(en){
        adder.io.src1 := rmdReg
        adder.io.src2 := ~rmdMReg
        adder.io.cin  := 1.U
        rmdReg        := Mux(zeroOfSrc1GreaterThanSrc2, (src1AbsS2 ## 0.U(32.W)) << src2LeadingZeros, (src1AbsS2 ## 0.U(32.W)) << src1LeadingZeros >> 1)
        rmdMReg       := 0.U
    }.otherwise{
        adder.io.src1 := rmdReg
        adder.io.src2 := ~rmdMReg
        adder.io.cin  := 1.U
    }


    
    /* stage 3: calculate the result */
    val resSignS3   = ShiftRegister(resSignS2, 1, false.B, iter(5))
    val src1S3      = ShiftRegister(src1S2, 1, 0.U, iter(5))
    val opS3        = ShiftRegister(opS2, 1, 0.U, iter(5))
    val validS3     = ShiftRegister(validS2, 1, false.B, iter(5))
    val divS3IsZero = divReg === 0.U
    val readyS3     = iter(5) && validS3 && opS3(2)
    val readyReg    = RegNext(readyS3, false.B)
    val src2LeadingZerosS3 = ShiftRegister(src2LeadingZeros, 1, 0.U, iter(5))
    val quotientS3  = BLevelPAdder32(adder.io.res, Mux(rmdReg(64), 0xFFFFFFFFL.U(32.W), 0.U), 0.U).io.res
    val remainderS3 = BLevelPAdder32(rmdReg(63, 32), Mux(rmdReg(64), divReg, 0.U), 0.U).io.res >> src2LeadingZerosS3

    // The output-valid pulse was already delayed by one cycle. Use that cycle
    // to register remainder recovery before the final sign correction.
    val quotientRecovered = RegEnable(quotientS3, 0.U(32.W), readyS3)
    val remainderRecovered = RegEnable(remainderS3, 0.U(32.W), readyS3)
    val resultSignRecovered = RegEnable(resSignS3, false.B, readyS3)
    val dividendRecovered = RegEnable(src1S3, 0.U(32.W), readyS3)
    val opRecovered = RegEnable(opS3, 0.U(5.W), readyS3)
    val divisorIsZeroRecovered = RegEnable(divS3IsZero, false.B, readyS3)

    val resultAdder = Module(new BLevelPAdder32)
    // for div, if the divisor is 0, the result is 0xffffffff according to the RISC-V spec
    // for rem, if the divisor is 0, the result is the divident
    resultAdder.io.src1  := Mux1H(Seq(
        (opRecovered === DIV,  Mux(divisorIsZeroRecovered, 0xffffffffL.U, Mux(resultSignRecovered, ~quotientRecovered, quotientRecovered))),
        (opRecovered === DIVU, Mux(divisorIsZeroRecovered, 0xffffffffL.U, quotientRecovered)),
        (opRecovered === REM,  Mux(divisorIsZeroRecovered, dividendRecovered, Mux(resultSignRecovered, ~remainderRecovered, remainderRecovered))),
        (opRecovered === REMU, Mux(divisorIsZeroRecovered, dividendRecovered, remainderRecovered))
    ))
    resultAdder.io.src2 := 0.U
    resultAdder.io.cin  := !opRecovered(0) && resultSignRecovered && !divisorIsZeroRecovered
    io.res              := resultAdder.io.res
    io.ready            := readyReg

    val busyCycleReg = RegInit(0.U(64.W))
    busyCycleReg     := busyCycleReg + io.busy
    io.dbg.busyCycle := busyCycleReg
}
