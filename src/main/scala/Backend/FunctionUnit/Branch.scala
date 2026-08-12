import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._

class BranchIO extends Bundle{
    val src1       = Input(UInt(32.W))
    val src2       = Input(UInt(32.W))
    val op         = Input(UInt(7.W))
    val pc         = Input(UInt(32.W))
    val imm        = Input(UInt(32.W))
    val predTaken  = Input(Bool())
    val predTarget = Input(UInt(32.W))
    val predFail   = Output(Bool())
    val branchTgt  = Output(UInt(32.W))
    val actualTaken = Output(Bool())
    val actualTarget = Output(UInt(32.W))
}

class Branch extends Module{
    val io = IO(new BranchIO)

    val realJp         = WireDefault(false.B)
    val tgtAdderSrc1   = WireDefault(io.pc)
    val tgtAdderSrc2   = WireDefault(io.imm)
    val calculatedTarget = BLevelPAdder32(tgtAdderSrc1, tgtAdderSrc2, 0.U).io.res
    val cmpSrc1        = WireDefault(io.src1)
    val cmpSrc2        = WireDefault(io.src2)  
    val cmpAdder       = BLevelPAdder32(cmpSrc1, ~cmpSrc2, 1.U)
    
    switch(io.op){
        is(BEQ) { realJp := io.src1 === io.src2 }
        is(BNE) { realJp := io.src1 =/= io.src2 }
        is(BLT) { realJp := cmpSrc1(31) && !cmpSrc2(31) || !(cmpSrc1(31) ^ cmpSrc2(31)) && cmpAdder.io.res(31) }
        is(BGE) { realJp := (!cmpSrc1(31) || cmpSrc2(31)) && ((cmpSrc1(31) ^ cmpSrc2(31)) || !cmpAdder.io.res(31)) }
        is(BLTU){ realJp := !cmpAdder.io.cout }
        is(BGEU){ realJp := cmpAdder.io.cout }
        is(JAL) { realJp := true.B }
        is(JALR){ realJp := true.B; tgtAdderSrc1 := io.src1 }
    }
    
    val actualTarget = Mux(io.op === JALR, calculatedTarget & "hfffffffe".U, calculatedTarget)
    val directionMiss = io.predTaken =/= realJp
    val targetMiss = io.predTaken && realJp && io.predTarget =/= actualTarget

    io.actualTaken := realJp
    io.actualTarget := actualTarget
    io.predFail := directionMiss || targetMiss
    io.branchTgt := Mux(realJp, actualTarget, io.pc + 4.U)
}
