import chisel3._
import chisel3.util._

class BranchPredictorLookupIO extends Bundle {
    val packetPC = Input(UInt(32.W))
    val slot7Inst = Input(UInt(32.W))
    val taken = Output(Bool())
    val target = Output(UInt(32.W))
}

class BranchPredictorUpdateIO extends Bundle {
    val valid = Input(Bool())
    val pc = Input(UInt(32.W))
    val inst = Input(UInt(32.W))
    val taken = Input(Bool())
    val target = Input(UInt(32.W))
}

class BranchPredictorIO extends Bundle {
    val lookup = new BranchPredictorLookupIO
    val update = new BranchPredictorUpdateIO
}

class BranchPredictor(
    directionEntries: Int = 16,
    btbEntries: Int = 16,
    loopEntries: Int = 8,
    rasDepth: Int = 8
) extends Module {
    require(isPow2(directionEntries))
    require(isPow2(btbEntries))
    require(isPow2(loopEntries))
    require(isPow2(rasDepth))

    val io = IO(new BranchPredictorIO)

    private val directionIndexWidth = log2Ceil(directionEntries)
    private val btbIndexWidth = log2Ceil(btbEntries)
    private val btbTagLowBit = btbIndexWidth + 5
    private val btbTagWidth = 32 - btbTagLowBit
    private val loopIndexWidth = log2Ceil(loopEntries)
    private val loopTagLowBit = loopIndexWidth + 5
    private val loopTagWidth = 32 - loopTagLowBit
    private val rasCountWidth = log2Ceil(rasDepth + 1)
    private val rasIndexWidth = log2Ceil(rasDepth)

    private def directionIndex(pc: UInt): UInt = pc(directionIndexWidth + 4, 5)
    private def btbIndex(pc: UInt): UInt = pc(btbIndexWidth + 4, 5)
    private def btbTag(pc: UInt): UInt = pc(31, btbTagLowBit)
    private def loopIndex(pc: UInt): UInt = pc(loopIndexWidth + 4, 5)
    private def loopTag(pc: UInt): UInt = pc(31, loopTagLowBit)
    private def opcode(inst: UInt): UInt = inst(6, 0)
    private def isConditional(inst: UInt): Bool = opcode(inst) === "b1100011".U
    private def isJal(inst: UInt): Bool = opcode(inst) === "b1101111".U
    private def isJalr(inst: UInt): Bool = opcode(inst) === "b1100111".U
    private def isLinkRegister(reg: UInt): Bool = reg === 1.U || reg === 5.U
    private def isCall(inst: UInt): Bool = (isJal(inst) || isJalr(inst)) && isLinkRegister(inst(11, 7))
    private def isReturn(inst: UInt): Bool = isJalr(inst) && inst(11, 7) === 0.U &&
        isLinkRegister(inst(19, 15)) && inst(31, 20) === 0.U

    private def branchImmediate(inst: UInt): UInt = Cat(
        Fill(19, inst(31)), inst(31), inst(7), inst(30, 25),
        inst(11, 8), 0.U(1.W)
    )

    private def jalImmediate(inst: UInt): UInt = Cat(
        Fill(11, inst(31)), inst(31), inst(19, 12), inst(20),
        inst(30, 21), 0.U(1.W)
    )

    val directionValid = RegInit(VecInit(Seq.fill(directionEntries)(false.B)))
    val directionCounters = RegInit(VecInit(Seq.fill(directionEntries)(1.U(2.W))))
    val btbValid = RegInit(VecInit(Seq.fill(btbEntries)(false.B)))
    val btbTags = Reg(Vec(btbEntries, UInt(btbTagWidth.W)))
    val btbTargets = Reg(Vec(btbEntries, UInt(32.W)))

    // Override the BHT only after three matching loop trip counts. A changed
    // trip count immediately removes confidence and falls back to the BHT.
    val loopValid = RegInit(VecInit(Seq.fill(loopEntries)(false.B)))
    val loopTags = Reg(Vec(loopEntries, UInt(loopTagWidth.W)))
    val loopTripCounts = RegInit(VecInit(Seq.fill(loopEntries)(0.U(8.W))))
    val loopCurrentCounts = RegInit(VecInit(Seq.fill(loopEntries)(0.U(8.W))))
    val loopConfidence = RegInit(VecInit(Seq.fill(loopEntries)(0.U(2.W))))

    val ras = Reg(Vec(rasDepth, UInt(32.W)))
    val rasCount = RegInit(0.U(rasCountWidth.W))
    val rasTopIndex = Mux(
        rasCount === 0.U,
        0.U(rasIndexWidth.W),
        (rasCount - 1.U)(rasIndexWidth - 1, 0)
    )
    val rasTop = ras(rasTopIndex)

    val lookupInst = io.lookup.slot7Inst
    val lookupBranchPC = io.lookup.packetPC + 28.U
    val lookupDirectionIndex = directionIndex(lookupBranchPC)
    val lookupBtbIndex = btbIndex(lookupBranchPC)
    val lookupLoopIndex = loopIndex(lookupBranchPC)
    val lookupIsConditional = isConditional(lookupInst)
    val lookupIsJal = isJal(lookupInst)
    val lookupIsJalr = isJalr(lookupInst)
    val lookupIsReturn = isReturn(lookupInst)

    val updateInst = io.update.inst
    val updateDirectionIndex = directionIndex(io.update.pc)
    val updateBtbIndex = btbIndex(io.update.pc)
    val updateLoopIndex = loopIndex(io.update.pc)
    val updateIsConditional = isConditional(updateInst)
    val updateIsJalr = isJalr(updateInst)
    val updateIsReturn = isReturn(updateInst)
    val updateIsCall = isCall(updateInst)

    val currentUpdateCounter = directionCounters(updateDirectionIndex)
    val updatedDirectionCounter = WireDefault(currentUpdateCounter)
    when(io.update.taken) {
        when(currentUpdateCounter =/= 3.U) {
            updatedDirectionCounter := currentUpdateCounter + 1.U
        }
    }.otherwise {
        when(currentUpdateCounter =/= 0.U) {
            updatedDirectionCounter := currentUpdateCounter - 1.U
        }
    }

    // A tight loop can resolve and fetch the same branch in one cycle. Bypass
    // the just-trained value instead of observing the stale register value.
    val directionUpdateBypass = io.update.valid && updateIsConditional &&
        io.update.pc === lookupBranchPC
    val lookupDirectionCounter = Mux(
        directionUpdateBypass,
        updatedDirectionCounter,
        directionCounters(lookupDirectionIndex)
    )
    val conditionalBiasTaken = branchImmediate(lookupInst)(31)
    val bhtConditionalTaken = Mux(
        directionValid(lookupDirectionIndex) || directionUpdateBypass,
        lookupDirectionCounter(1),
        conditionalBiasTaken
    )
    val loopHit = lookupIsConditional && conditionalBiasTaken &&
        loopValid(lookupLoopIndex) &&
        loopTags(lookupLoopIndex) === loopTag(lookupBranchPC)
    val loopPredictionValid = loopHit && loopConfidence(lookupLoopIndex) === 3.U
    val loopTaken = loopCurrentCounts(lookupLoopIndex) =/=
        loopTripCounts(lookupLoopIndex)
    val conditionalTaken = Mux(loopPredictionValid, loopTaken, bhtConditionalTaken)

    val btbUpdateBypass = io.update.valid && io.update.taken && updateIsJalr &&
        !updateIsReturn && io.update.pc === lookupBranchPC
    val registeredBtbHit = btbValid(lookupBtbIndex) &&
        btbTags(lookupBtbIndex) === btbTag(lookupBranchPC)
    val btbHit = btbUpdateBypass || registeredBtbHit
    val btbTarget = Mux(btbUpdateBypass, io.update.target, btbTargets(lookupBtbIndex))
    val returnHit = lookupIsReturn && rasCount =/= 0.U
    val indirectTaken = lookupIsJalr && (returnHit || btbHit)

    io.lookup.taken := lookupIsJal || (lookupIsConditional && conditionalTaken) || indirectTaken
    io.lookup.target := MuxCase(io.lookup.packetPC + 32.U, Seq(
        lookupIsJal -> (lookupBranchPC + jalImmediate(lookupInst)),
        lookupIsConditional -> (lookupBranchPC + branchImmediate(lookupInst)),
        returnHit -> rasTop,
        (lookupIsJalr && btbHit) -> btbTarget
    ))

    when(io.update.valid && updateIsConditional) {
        directionValid(updateDirectionIndex) := true.B
        directionCounters(updateDirectionIndex) := updatedDirectionCounter
    }

    val updateIsBackwardLoop = updateIsConditional && branchImmediate(updateInst)(31)
    val updateLoopHit = loopValid(updateLoopIndex) &&
        loopTags(updateLoopIndex) === loopTag(io.update.pc)
    when(io.update.valid && updateIsBackwardLoop) {
        when(!updateLoopHit) {
            loopValid(updateLoopIndex) := true.B
            loopTags(updateLoopIndex) := loopTag(io.update.pc)
            loopTripCounts(updateLoopIndex) := 0.U
            loopCurrentCounts(updateLoopIndex) := Mux(io.update.taken, 1.U, 0.U)
            loopConfidence(updateLoopIndex) := 0.U
        }.elsewhen(io.update.taken) {
            when(loopConfidence(updateLoopIndex) === 3.U &&
                loopCurrentCounts(updateLoopIndex) === loopTripCounts(updateLoopIndex)) {
                loopConfidence(updateLoopIndex) := 0.U
            }
            when(loopCurrentCounts(updateLoopIndex) =/= 255.U) {
                loopCurrentCounts(updateLoopIndex) := loopCurrentCounts(updateLoopIndex) + 1.U
            }
        }.otherwise {
            val observedTripCount = loopCurrentCounts(updateLoopIndex)
            when(observedTripCount === loopTripCounts(updateLoopIndex)) {
                when(loopConfidence(updateLoopIndex) =/= 3.U) {
                    loopConfidence(updateLoopIndex) := loopConfidence(updateLoopIndex) + 1.U
                }
            }.otherwise {
                loopTripCounts(updateLoopIndex) := observedTripCount
                loopConfidence(updateLoopIndex) := 0.U
            }
            loopCurrentCounts(updateLoopIndex) := 0.U
        }
    }

    // Direct jumps and branches calculate their target from the instruction.
    // The BTB is reserved for non-return JALR operations.
    when(io.update.valid && io.update.taken && updateIsJalr && !updateIsReturn) {
        btbValid(updateBtbIndex) := true.B
        btbTags(updateBtbIndex) := btbTag(io.update.pc)
        btbTargets(updateBtbIndex) := io.update.target
    }

    // Update the RAS only when a branch resolves, so wrong-path fetches cannot
    // corrupt it and no speculative rollback state is required.
    when(io.update.valid && io.update.taken) {
        when(updateIsCall) {
            when(rasCount === rasDepth.U) {
                for (i <- 0 until rasDepth - 1) {
                    ras(i) := ras(i + 1)
                }
                ras(rasDepth - 1) := io.update.pc + 4.U
            }.otherwise {
                ras(rasCount(rasIndexWidth - 1, 0)) := io.update.pc + 4.U
                rasCount := rasCount + 1.U
            }
        }.elsewhen(updateIsReturn && rasCount =/= 0.U) {
            rasCount := rasCount - 1.U
        }
    }
}
