package zirconfp

import chisel3._
import chisel3.util._

object FDivRoundingMode {
  val RNE = 0.U(3.W)
  val RTZ = 1.U(3.W)
  val RDN = 2.U(3.W)
  val RUP = 3.U(3.W)
  val RMM = 4.U(3.W)
}

object FDivConstants {
  val CanonicalNaN = "h7fc00000".U(32.W)
  val PositiveInfinity = "h7f800000".U(32.W)
  val MaxFinite = "h7f7fffff".U(32.W)
}

class FDivIO extends Bundle {
  val src1 = Input(UInt(32.W))
  val src2 = Input(UInt(32.W))
  val rm = Input(UInt(3.W))

  val inValid = Input(Bool())
  val inReady = Output(Bool())
  val outValid = Output(Bool())
  val outReady = Input(Bool())
  val kill = Input(Bool())

  val result = Output(UInt(32.W))
  val fflags = Output(UInt(5.W))
  val busy = Output(Bool())
}

/**
  * Iterative IEEE 754 single-precision divider using radix-4 SRT division.
  *
  * Quotient digits are selected from {-2, -1, 0, +1, +2}. The implementation
  * uses a truncated quotient-digit selector and on-the-fly conversion. Two
  * radix-4 digits are produced per iteration cycle. Normal operands bypass the
  * leading-zero normalization stage, while subnormal operands take one extra
  * cycle.
  */
class FDiv extends Module {
  import FDivConstants._
  import FDivRoundingMode._

  val io = IO(new FDivIO)

  val idle :: normalize :: initialize :: centerInitial :: iterate :: postprocess :: rounding :: response :: Nil = Enum(8)
  val state = RegInit(idle)

  val operandAReg = Reg(UInt(32.W))
  val operandBReg = Reg(UInt(32.W))
  val significandAReg = Reg(UInt(24.W))
  val significandBReg = Reg(UInt(24.W))
  val unbiasedExponentAReg = Reg(SInt(12.W))
  val unbiasedExponentBReg = Reg(SInt(12.W))
  val rawInitialRemainderReg = Reg(UInt(25.W))
  val numeratorAtLeastDenominatorReg = Reg(Bool())
  val divisorReg = Reg(UInt(24.W))
  val negativeDivisorReg = Reg(UInt(25.W))
  val remainderSumReg = Reg(UInt(25.W))
  val remainderCarryReg = Reg(UInt(25.W))
  val quotientReg = Reg(UInt(26.W))
  val quotientMinusOneReg = Reg(UInt(26.W))
  val iterationReg = Reg(UInt(3.W))
  val resultExponentReg = Reg(SInt(12.W))
  val resultSignReg = Reg(Bool())
  val roundingModeReg = Reg(UInt(3.W))
  val quotientWithStickyReg = Reg(UInt(27.W))
  val biasedExponentReg = Reg(SInt(13.W))
  val resultSignPostReg = Reg(Bool())
  val roundingModePostReg = Reg(UInt(3.W))
  val resultReg = RegInit(0.U(32.W))
  val flagsReg = RegInit(0.U(5.W))

  io.inReady := state === idle
  io.outValid := state === response
  io.result := resultReg
  io.fflags := flagsReg
  io.busy := state =/= idle

  val signA = io.src1(31)
  val signB = io.src2(31)
  val exponentA = io.src1(30, 23)
  val exponentB = io.src2(30, 23)
  val fractionA = io.src1(22, 0)
  val fractionB = io.src2(22, 0)

  val isZeroA = !exponentA.orR && !fractionA.orR
  val isZeroB = !exponentB.orR && !fractionB.orR
  val isInfinityA = exponentA.andR && !fractionA.orR
  val isInfinityB = exponentB.andR && !fractionB.orR
  val isNaNA = exponentA.andR && fractionA.orR
  val isNaNB = exponentB.andR && fractionB.orR
  val isSignalingNaNA = isNaNA && !fractionA(22)
  val isSignalingNaNB = isNaNB && !fractionB(22)
  val resultSign = signA ^ signB

  val invalidOperation = (isZeroA && isZeroB) ||
    (isInfinityA && isInfinityB)
  val hasNaN = isNaNA || isNaNB
  val finiteDivideByZero = isZeroB && !isZeroA && !isInfinityA && !hasNaN
  val specialCase = hasNaN || invalidOperation || isZeroA || isZeroB ||
    isInfinityA || isInfinityB

  val operandSignA = operandAReg(31)
  val operandSignB = operandBReg(31)
  val operandExponentA = operandAReg(30, 23)
  val operandExponentB = operandBReg(30, 23)
  val operandFractionA = operandAReg(22, 0)
  val operandFractionB = operandBReg(22, 0)

  val specialResult = WireDefault(0.U(32.W))
  val specialFlags = WireDefault(0.U(5.W))
  when(hasNaN || invalidOperation) {
    specialResult := CanonicalNaN
    specialFlags := Cat(
      isSignalingNaNA || isSignalingNaNB || invalidOperation,
      0.U(4.W)
    )
  }.elsewhen(isInfinityA || isZeroB) {
    specialResult := Cat(resultSign, PositiveInfinity(30, 0))
    specialFlags := Cat(0.U(1.W), finiteDivideByZero, 0.U(3.W))
  }.elsewhen(isZeroA || isInfinityB) {
    specialResult := Cat(resultSign, 0.U(31.W))
  }

  val leadingZerosA = PriorityEncoder(Reverse(operandFractionA))
  val leadingZerosB = PriorityEncoder(Reverse(operandFractionB))
  val subnormalShiftA = leadingZerosA + 1.U
  val subnormalShiftB = leadingZerosB + 1.U
  val normalizedSubnormalA = (Cat(0.U(1.W), operandFractionA) << subnormalShiftA)(23, 0)
  val normalizedSubnormalB = (Cat(0.U(1.W), operandFractionB) << subnormalShiftB)(23, 0)
  val significandA = Mux(
    operandExponentA.orR,
    Cat(1.U(1.W), operandFractionA),
    normalizedSubnormalA
  )
  val significandB = Mux(
    operandExponentB.orR,
    Cat(1.U(1.W), operandFractionB),
    normalizedSubnormalB
  )

  val unbiasedExponentA = Wire(SInt(12.W))
  val unbiasedExponentB = Wire(SInt(12.W))
  unbiasedExponentA := Mux(
    operandExponentA.orR,
    operandExponentA.zext - 127.S(12.W),
    (-127).S(12.W) - leadingZerosA.zext
  )
  unbiasedExponentB := Mux(
    operandExponentB.orR,
    operandExponentB.zext - 127.S(12.W),
    (-127).S(12.W) - leadingZerosB.zext
  )

  val numeratorAtLeastDenominator = significandAReg >= significandBReg
  val rawInitialRemainder = Mux(
    numeratorAtLeastDenominator,
    Cat(0.U(1.W), significandAReg) - Cat(0.U(1.W), significandBReg),
    Cat(significandAReg, 0.U(1.W)) - Cat(0.U(1.W), significandBReg)
  )
  // Recenter the normalized quotient around either 1 or 2 so the initial
  // remainder lies in [-D/2, D/2], inside the radix-4 SRT convergence range.
  val extendedRawInitialRemainder = Cat(0.U(1.W), rawInitialRemainderReg)
  val extendedInitialDivisor = Cat(0.U(2.W), significandBReg)
  val initialRemainderMinusDivisor =
    extendedRawInitialRemainder - extendedInitialDivisor
  val doubledInitialRemainderMinusDivisor =
    Cat(rawInitialRemainderReg, 0.U(1.W)) - extendedInitialDivisor
  val initialRemainderAboveHalf =
    !doubledInitialRemainderMinusDivisor(25) &&
      doubledInitialRemainderMinusDivisor.orR
  val centeredInitialRemainder = Wire(SInt(26.W))
  centeredInitialRemainder := Mux(
    initialRemainderAboveHalf,
    initialRemainderMinusDivisor.asSInt,
    extendedRawInitialRemainder.asSInt
  )
  val initialQuotient = Mux(initialRemainderAboveHalf, 2.U(26.W), 1.U(26.W))
  val initialQuotientMinusOne = Mux(initialRemainderAboveHalf, 1.U(26.W), 0.U(26.W))

  val extendedDivisor = Wire(SInt(26.W))
  extendedDivisor := divisorReg.zext
  val positiveDivisor = Cat(0.U(1.W), divisorReg)
  val positiveDoubleDivisor = Cat(divisorReg, 0.U(1.W))
  val negativeDoubleDivisor = Cat(negativeDivisorReg(23, 0), 0.U(1.W))
  val initialNegativeDivisor =
    (~Cat(0.U(1.W), significandBReg)).asUInt + 1.U

  // D is in [1, 2). Six divisor bits and eight signed remainder bits are
  // sufficient to place both QDS transitions inside the radix-4 overlap
  // intervals for the relaxed |R| <= 2D/3 convergence bound.
  val qdsDivisorTop = divisorReg(23, 18)
  val qdsThreshold1Numerator = Cat(0.U(1.W), qdsDivisorTop) + 2.U
  val qdsThreshold2Numerator =
    Cat(0.U(2.W), qdsDivisorTop) + Cat(0.U(1.W), qdsDivisorTop, 0.U(1.W)) + 2.U
  val qdsThreshold1 = Wire(SInt(8.W))
  val qdsThreshold2 = Wire(SInt(8.W))
  qdsThreshold1 := (qdsThreshold1Numerator >> 2).zext
  qdsThreshold2 := (qdsThreshold2Numerator >> 2).zext

  def carrySave3(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    require(a.getWidth == b.getWidth && b.getWidth == c.getWidth)
    val width = a.getWidth
    val sum = a ^ b ^ c
    val carryGenerate = (a & b) | (a & c) | (b & c)
    val carry = Cat(carryGenerate(width - 2, 0), 0.U(1.W))
    (sum, carry)
  }

  def selectSrtDigit(
      remainderSum: UInt,
      remainderCarry: UInt
  ): SInt = {
    // The omitted carry from bits 16:0 changes this estimate by at most one.
    // Both QDS boundaries have enough overlap to tolerate that uncertainty.
    val selectionRemainder =
      (remainderSum(24, 17) + remainderCarry(24, 17)).asSInt

    val digit = WireDefault(0.S(3.W))
    when(selectionRemainder >= qdsThreshold2) {
      digit := 2.S
    }.elsewhen(selectionRemainder >= qdsThreshold1) {
      digit := 1.S
    }.elsewhen(selectionRemainder <= -qdsThreshold2) {
      digit := (-2).S
    }.elsewhen(selectionRemainder <= -qdsThreshold1) {
      digit := (-1).S
    }
    digit
  }

  def srtStep(
      remainderSum: UInt,
      remainderCarry: UInt,
      quotient: UInt,
      quotientMinusOne: UInt,
      digit: SInt
  ): (UInt, UInt, UInt, UInt) = {
    val shiftedSum = Cat(remainderSum(22, 0), 0.U(2.W))
    val shiftedCarry = Cat(remainderCarry(22, 0), 0.U(2.W))
    val recurrenceAddend = WireDefault(0.U(25.W))
    when(digit === 2.S) {
      recurrenceAddend := negativeDoubleDivisor
    }.elsewhen(digit === 1.S) {
      recurrenceAddend := negativeDivisorReg
    }.elsewhen(digit === (-1).S) {
      recurrenceAddend := positiveDivisor
    }.elsewhen(digit === (-2).S) {
      recurrenceAddend := positiveDoubleDivisor
    }

    val (nextRemainderSum, nextRemainderCarry) =
      carrySave3(shiftedSum, shiftedCarry, recurrenceAddend)

    // On-the-fly conversion keeps Q and Q-1, replacing the signed quotient
    // accumulator's carry-propagating addition with shifts and multiplexers.
    val shiftedQuotient = Cat(quotient(23, 0), 0.U(2.W))
    val shiftedQuotientMinusOne = Cat(quotientMinusOne(23, 0), 0.U(2.W))
    val nextQuotient = WireDefault(shiftedQuotient)
    val nextQuotientMinusOne = WireDefault(shiftedQuotientMinusOne | 3.U)
    when(digit === 2.S) {
      nextQuotient := shiftedQuotient | 2.U
      nextQuotientMinusOne := shiftedQuotient | 1.U
    }.elsewhen(digit === 1.S) {
      nextQuotient := shiftedQuotient | 1.U
      nextQuotientMinusOne := shiftedQuotient
    }.elsewhen(digit === (-1).S) {
      nextQuotient := shiftedQuotientMinusOne | 3.U
      nextQuotientMinusOne := shiftedQuotientMinusOne | 2.U
    }.elsewhen(digit === (-2).S) {
      nextQuotient := shiftedQuotientMinusOne | 2.U
      nextQuotientMinusOne := shiftedQuotientMinusOne | 1.U
    }

    (nextRemainderSum, nextRemainderCarry, nextQuotient, nextQuotientMinusOne)
  }

  // Two radix-4 substeps produce four quotient bits per cycle. This is the
  // middle point between the original three-substep path and the fully split
  // one-substep implementation.
  val firstDigit = selectSrtDigit(remainderSumReg, remainderCarryReg)
  val (firstSum, firstCarry, firstQuotient, firstQuotientMinusOne) =
    srtStep(
      remainderSumReg,
      remainderCarryReg,
      quotientReg,
      quotientMinusOneReg,
      firstDigit
    )
  val secondDigit = selectSrtDigit(firstSum, firstCarry)
  val (iterationSum, iterationCarry, iterationQuotient, iterationQuotientMinusOne) =
    srtStep(
      firstSum,
      firstCarry,
      firstQuotient,
      firstQuotientMinusOne,
      secondDigit
    )

  // Twelve radix-4 digits provide 24 fractional bits. Select Q-1 when the
  // final residual is negative before deriving the round and sticky bits.
  val remainderValue = (remainderSumReg + remainderCarryReg).asSInt
  val finalRemainderIsNegative = remainderValue < 0.S
  val correctedScaledQuotientBits = Mux(
    finalRemainderIsNegative,
    quotientMinusOneReg,
    quotientReg
  )

  // Form all post-processing remainder candidates in parallel. Each three-term
  // expression first passes through a CSA, so only one carry-propagating add is
  // present on a candidate path. Selection happens after the parallel adds.
  val doubledRemainderSum = Cat(remainderSumReg(23, 0), 0.U(1.W))
  val doubledRemainderCarry = Cat(remainderCarryReg(23, 0), 0.U(1.W))

  val (positiveRoundSum, positiveRoundCarry) = carrySave3(
    doubledRemainderSum,
    doubledRemainderCarry,
    negativeDivisorReg
  )
  val positiveRoundRemainder =
    (positiveRoundSum + positiveRoundCarry).asSInt

  val (negativeRoundSum, negativeRoundCarry) = carrySave3(
    doubledRemainderSum,
    doubledRemainderCarry,
    positiveDivisor
  )
  val negativeRoundRemainder =
    (negativeRoundSum + negativeRoundCarry).asSInt

  val (negativeNoRoundSum, negativeNoRoundCarry) = carrySave3(
    doubledRemainderSum,
    doubledRemainderCarry,
    positiveDoubleDivisor
  )
  val negativeNoRoundRemainder =
    (negativeNoRoundSum + negativeNoRoundCarry).asSInt

  val positiveNoRoundRemainder =
    (doubledRemainderSum + doubledRemainderCarry).asSInt
  val positiveRoundBit = positiveRoundRemainder >= 0.S
  val negativeRoundBit = negativeRoundRemainder >= 0.S
  val finalRoundBit = Mux(
    finalRemainderIsNegative,
    negativeRoundBit,
    positiveRoundBit
  )
  val finalRemainder = Mux(
    finalRemainderIsNegative,
    Mux(negativeRoundBit, negativeRoundRemainder, negativeNoRoundRemainder),
    Mux(positiveRoundBit, positiveRoundRemainder, positiveNoRoundRemainder)
  )
  val quotientWithSticky = Cat(
    correctedScaledQuotientBits(24, 0),
    finalRoundBit,
    finalRemainder.asUInt.orR
  )

  // If the registered partial remainder is exactly zero, all remaining SRT
  // digits are zero. Complete the fixed-width quotient with wiring shifts and
  // bypass the unused recurrence cycles.
  def completeExactQuotient(quotient: UInt): UInt = {
    MuxLookup(iterationReg, quotient)(Seq(
      0.U -> (quotient << 24)(25, 0),
      1.U -> (quotient << 20)(25, 0),
      2.U -> (quotient << 16)(25, 0),
      3.U -> (quotient << 12)(25, 0),
      4.U -> (quotient << 8)(25, 0),
      5.U -> (quotient << 4)(25, 0)
    ))
  }

  def shiftRightJam(value: UInt, distance: UInt): UInt = {
    val width = value.getWidth
    val shifted = value >> distance
    val discarded = VecInit((0 until width).map { bit =>
      value(bit) && distance > bit.U
    }).asUInt.orR
    Cat(shifted(width - 1, 1), shifted(0) || discarded)
  }

  val biasedExponent = Wire(SInt(13.W))
  biasedExponent := resultExponentReg + 127.S
  val isSubnormalResult = biasedExponentReg <= 0.S
  val subnormalShiftSigned = 1.S(14.W) - biasedExponentReg
  val subnormalShiftUnsigned = subnormalShiftSigned.asUInt
  val subnormalShift = Mux(
    subnormalShiftSigned >= 27.S,
    27.U(5.W),
    subnormalShiftUnsigned(4, 0)
  )
  val roundedInput = Mux(
    isSubnormalResult,
    shiftRightJam(quotientWithStickyReg, subnormalShift),
    quotientWithStickyReg
  )

  val unroundedSignificand = roundedInput(26, 3)
  val guard = roundedInput(2)
  val round = roundedInput(1)
  val sticky = roundedInput(0)
  val inexact = guard || round || sticky
  val useRNE = roundingModePostReg === RNE || roundingModePostReg > RMM
  val roundIncrement = MuxCase(false.B, Seq(
    useRNE -> (guard && (round || sticky || unroundedSignificand(0))),
    (roundingModePostReg === RTZ) -> false.B,
    (roundingModePostReg === RDN) -> (resultSignPostReg && inexact),
    (roundingModePostReg === RUP) -> (!resultSignPostReg && inexact),
    (roundingModePostReg === RMM) -> guard
  ))
  val roundedSignificand = Cat(0.U(1.W), unroundedSignificand) + roundIncrement

  val normalRoundingCarry = roundedSignificand(24)
  val normalSignificand = Mux(
    normalRoundingCarry,
    roundedSignificand(24, 1),
    roundedSignificand(23, 0)
  )
  val normalExponentAfterRound = biasedExponentReg + normalRoundingCarry.asUInt.zext
  val normalOverflow = biasedExponentReg >= 255.S || normalExponentAfterRound >= 255.S
  val normalExponentBits = normalExponentAfterRound.asUInt
  val normalResult = Cat(
    resultSignPostReg,
    normalExponentBits(7, 0),
    normalSignificand(22, 0)
  )

  val overflowToInfinity = useRNE || roundingModePostReg === RMM ||
    (roundingModePostReg === RDN && resultSignPostReg) ||
    (roundingModePostReg === RUP && !resultSignPostReg)
  val overflowMagnitude = Mux(overflowToInfinity, PositiveInfinity, MaxFinite)
  val overflowResult = Cat(resultSignPostReg, overflowMagnitude(30, 0))

  val subnormalRoundsToNormal = roundedSignificand(23)
  val subnormalResult = Cat(
    resultSignPostReg,
    Mux(subnormalRoundsToNormal, 1.U(8.W), 0.U(8.W)),
    roundedSignificand(22, 0)
  )
  val underflow = inexact && !subnormalRoundsToNormal

  val finiteResult = MuxCase(normalResult, Seq(
    normalOverflow -> overflowResult,
    isSubnormalResult -> subnormalResult
  ))
  val finiteFlags = MuxCase(
    Cat(0.U(4.W), inexact),
    Seq(
      normalOverflow -> "b00101".U(5.W),
      isSubnormalResult -> Cat(0.U(3.W), underflow, inexact)
    )
  )

  when(io.kill) {
    state := idle
  }.elsewhen(state === idle) {
    when(io.inValid) {
      when(specialCase) {
        resultReg := specialResult
        flagsReg := specialFlags
        state := response
      }.otherwise {
        roundingModeReg := io.rm
        when(exponentA.orR && exponentB.orR) {
          significandAReg := Cat(1.U(1.W), fractionA)
          significandBReg := Cat(1.U(1.W), fractionB)
          unbiasedExponentAReg := exponentA.zext - 127.S(12.W)
          unbiasedExponentBReg := exponentB.zext - 127.S(12.W)
          resultSignReg := resultSign
          state := initialize
        }.otherwise {
          operandAReg := io.src1
          operandBReg := io.src2
          state := normalize
        }
      }
    }
  }.elsewhen(state === normalize) {
    significandAReg := significandA
    significandBReg := significandB
    unbiasedExponentAReg := unbiasedExponentA
    unbiasedExponentBReg := unbiasedExponentB
    resultSignReg := operandSignA ^ operandSignB
    state := initialize
  }.elsewhen(state === initialize) {
    rawInitialRemainderReg := rawInitialRemainder.asUInt
    numeratorAtLeastDenominatorReg := numeratorAtLeastDenominator
    divisorReg := significandBReg
    negativeDivisorReg := initialNegativeDivisor
    state := centerInitial
  }.elsewhen(state === centerInitial) {
    remainderSumReg := centeredInitialRemainder.asUInt(24, 0)
    remainderCarryReg := 0.U
    quotientReg := initialQuotient
    quotientMinusOneReg := initialQuotientMinusOne
    iterationReg := 0.U
    resultExponentReg := unbiasedExponentAReg - unbiasedExponentBReg -
      Mux(numeratorAtLeastDenominatorReg, 0.S, 1.S)
    state := iterate
  }.elsewhen(state === iterate) {
    when(remainderValue === 0.S) {
      quotientReg := completeExactQuotient(quotientReg)
      quotientMinusOneReg := completeExactQuotient(quotientMinusOneReg)
      state := postprocess
    }.otherwise {
      remainderSumReg := iterationSum
      remainderCarryReg := iterationCarry
      quotientReg := iterationQuotient
      quotientMinusOneReg := iterationQuotientMinusOne
      when(iterationReg === 5.U) {
        state := postprocess
      }.otherwise {
        iterationReg := iterationReg + 1.U
      }
    }
  }.elsewhen(state === postprocess) {
    quotientWithStickyReg := quotientWithSticky
    biasedExponentReg := biasedExponent
    resultSignPostReg := resultSignReg
    roundingModePostReg := roundingModeReg
    state := rounding
  }.elsewhen(state === rounding) {
    resultReg := finiteResult
    flagsReg := finiteFlags
    state := response
  }.elsewhen(state === response && io.outReady) {
    state := idle
  }

  when(state === iterate || state === postprocess) {
    assert(divisorReg.orR, "FDiv divisor must be non-zero while iterating")
    val remainderTimes3 = remainderValue + (remainderValue << 1)
    val convergenceLimit = extendedDivisor << 1
    assert(
      remainderTimes3 <= convergenceLimit &&
        remainderTimes3 >= -convergenceLimit,
      "FDiv partial remainder left the SRT convergence interval"
    )
  }
}
