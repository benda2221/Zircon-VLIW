package zirconfp

import chisel3._
import chisel3.util._

// IEEE 754 single-precision format constants
object IEEE754Constants {
    val SIGN_BIT = 31
    val EXP_HIGH = 30
    val EXP_LOW = 23
    val MANTISSA_HIGH = 22
    val MANTISSA_LOW = 0
    val MANTISSA_WIDTH = 23
    val EXP_WIDTH = 8
    val GRS_WIDTH = 3
    val FULL_MANTISSA_WIDTH = 27  // 24-bit mantissa + 3 GRS bits
    val EXTENDED_SHIFT_WIDTH = 32
    val EXTENDED_MANTISSA_WIDTH = 56
    val SHIFT_THRESHOLD_HIGH = 7
    val SHIFT_THRESHOLD_LOW = 5
}

object FPUtils {
    import IEEE754Constants._

    def floatBreakdown(x: UInt): (Bool, UInt, UInt) = {
        val sign = x(SIGN_BIT)
        val exponent = x(EXP_HIGH, EXP_LOW)
        val mantissa = x(MANTISSA_HIGH, MANTISSA_LOW)
        (sign, exponent, mantissa)
    }

    def Adder(src1: UInt, src2: UInt, cin: UInt, width: Int): (UInt, UInt) = {
        val result = src1 +& src2 +& cin(0)
        (result(width - 1, 0), result(width))
    }
}


class FAddIO extends Bundle {
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val op   = Input(Bool()) // 0: add, 1: sub
    val result = Output(UInt(32.W))
}


class FAddPathIO extends Bundle {
    val biggerSrc = Input(UInt(32.W))
    val smallerSrc = Input(UInt(32.W))
    val op = Input(Bool())
    val expDiff = Input(UInt(8.W))
    val result = Output(UInt(32.W))
}

class FAddFarPath extends Module {
    import FPUtils._
    import IEEE754Constants._

    val io = IO(new FAddPathIO())

    // Decompose floating-point operands into IEEE 754 fields
    val (biggerSign, biggerExponent, biggerMantissa) = floatBreakdown(io.biggerSrc)
    val (smallerSign, smallerExponent, smallerMantissa) = floatBreakdown(io.smallerSrc)

    // Determine effective operation (add or subtract based on signs)
    val farop = biggerSign ^ smallerSign ^ io.op

    // Add implicit leading 1 to mantissas
    val (biggerMantissaSB, smallerMantissaSB) = (Cat(1.U, biggerMantissa), Cat(1.U, smallerMantissa))
    val biggerMantissaFull = Cat(biggerMantissaSB, 0.U(GRS_WIDTH.W))

    // Align smaller mantissa by shifting right according to exponent difference
    val smallerMantissaExtended = Cat(smallerMantissaSB, 0.U(EXTENDED_SHIFT_WIDTH.W))
    val smallerMantissaExtendedShifted = Wire(UInt(EXTENDED_MANTISSA_WIDTH.W))
    smallerMantissaExtendedShifted := smallerMantissaExtended >> io.expDiff

    // Extract Guard, Round, and Sticky (GRS) bits for rounding
    val smallerMantissaExtendedShiftedLSB = smallerMantissaExtendedShifted(EXTENDED_MANTISSA_WIDTH - 1, EXTENDED_SHIFT_WIDTH)
    val smallerMantissaExtendedShiftedGR = smallerMantissaExtendedShifted(SIGN_BIT, EXP_HIGH)
    val isLargeShift = io.expDiff(SHIFT_THRESHOLD_HIGH, SHIFT_THRESHOLD_LOW).orR
    val smallerMantissaExtendedShiftedS = Mux(isLargeShift, smallerMantissa.orR, smallerMantissaExtendedShifted(EXP_HIGH - 1, 0).orR)
    val smallerMantissaFull = Cat(smallerMantissaExtendedShiftedLSB, smallerMantissaExtendedShiftedGR, smallerMantissaExtendedShiftedS)

    // Perform mantissa addition or subtraction
    val (sum, carry) = Adder(biggerMantissaFull, Mux(farop === 1.U, ~smallerMantissaFull, smallerMantissaFull), farop, FULL_MANTISSA_WIDTH)

    // First normalization stage: detect overflow or underflow
    val regularPlus1 = carry === !farop.asBool
    val regularMinus1 = sum(FULL_MANTISSA_WIDTH - 1) === 0.U && !regularPlus1
    val resultMantissaRegularized = Mux(
        regularMinus1, sum << 1,
        Mux(regularPlus1, Cat((farop.asBool && carry.asBool), sum)(FULL_MANTISSA_WIDTH, 2) ## sum(1, 0).orR, sum))

    // Extract mantissa and GRS bits after normalization
    val (resultMantissaSB, resultMantissaG, resultMantissaR, resultMantissaS) =
        (resultMantissaRegularized(FULL_MANTISSA_WIDTH - 1, GRS_WIDTH),
         resultMantissaRegularized(2),
         resultMantissaRegularized(1),
         resultMantissaRegularized(0))

    // Apply IEEE 754 round-to-nearest-even
    val roundPlus1 = resultMantissaG && (resultMantissaR || resultMantissaS || resultMantissaSB(0))
    val (resultMantissa, carry2) = Adder(resultMantissaSB, roundPlus1, 0.U, MANTISSA_WIDTH + 1)

    // Second normalization stage: handle rounding overflow
    // Overflow prediction using AND-reduction O(log n) instead of waiting for adder carry O(n)
    val overflow = roundPlus1 && resultMantissaSB.andR
    val resultMantissaFinal = Cat(carry2, resultMantissa) >> overflow

    // Calculate result exponent with normalization adjustments
    val (resultExponent, _) = Adder(biggerExponent,
        Cat(Fill(EXP_WIDTH - 1, regularMinus1 && !overflow), regularPlus1 && overflow),
        regularPlus1 || (regularMinus1 ^ overflow),
        EXP_WIDTH
    )

    // Result sign matches the bigger operand
    val resultSign = biggerSign
    io.result := Cat(resultSign, resultExponent, resultMantissaFinal(MANTISSA_HIGH, MANTISSA_LOW))
}


class FAddClosePath extends Module {
    import FPUtils._
    import IEEE754Constants._

    val io = IO(new FAddPathIO())

    // Decompose floating-point operands into IEEE 754 fields
    val (biggerSign, biggerExponent, biggerMantissa) = floatBreakdown(io.biggerSrc)
    val (smallerSign, smallerExponent, smallerMantissa) = floatBreakdown(io.smallerSrc)

    // Determine effective operation (add or subtract based on signs)
    val closeop = biggerSign ^ smallerSign ^ io.op

    // Add implicit leading 1 to mantissas
    val (biggerMantissaSB, smallerMantissaSB) = (Cat(1.U, biggerMantissa), Cat(1.U, smallerMantissa))
    val biggerMantissaFull = Cat(biggerMantissaSB, 0.U(1.W))

    // Align smaller mantissa by shifting right according to exponent difference
    val smallerMantissaExtended = Cat(smallerMantissaSB, 0.U(1.W))
    val smallerMantissaFull = Mux(io.expDiff(0), smallerMantissaExtended >> 1, smallerMantissaExtended)
    // leading zero predictor
    def leadingZeroPredictor(src1: UInt, src2: UInt): (UInt, UInt, UInt) = {
        val n = src1.getWidth
        assert(n == src2.getWidth, "src1 and src2 must have the same width")
        val (src1Pad, src2Pad) = (Cat(src1, 0.U(1.W)), Cat(src2, 0.U(1.W)))
        val xor = src1Pad ^ src2Pad
        val z = src1Pad | ~src2Pad
        val f = VecInit.tabulate(n){i =>
            xor(i+1) && z(i)
        }.asUInt
        val lz = PriorityEncoder(Reverse(f))
        val (lzPlus1, _) = Adder(lz, 1.U, 0.U, lz.getWidth)
        val (lzMinus1, _) = Adder(lz, ~(1.U), 1.U, lz.getWidth)
        (lz, lzPlus1, lzMinus1)
    }
    val (lzPredictBigger, lzPlus1Bigger, lzMinus1Bigger) = leadingZeroPredictor(biggerMantissaFull, smallerMantissaFull)
    val (lzPredictSmaller, lzPlus1Smaller, lzMinus1Smaller) = leadingZeroPredictor(smallerMantissaFull, biggerMantissaFull)
    // perform addition or subtraction for mantissa
    val (sum, carry) = Adder(biggerMantissaFull, Mux(closeop === 1.U, ~smallerMantissaFull, smallerMantissaFull), closeop, 25)
    // select appropriate LZP based on carry (indicates which operand was actually larger)
    val lzPredict = Mux(carry === 1.U, lzPredictBigger, lzPredictSmaller)
    val lzPlus1 = Mux(carry === 1.U, lzPlus1Bigger, lzPlus1Smaller)
    val lzMinus1 = Mux(carry === 1.U, lzMinus1Bigger, lzMinus1Smaller)
    // if the expDiff == 0, and closeop is minus and the carry is zero, then we need to get the absolute value of the sum
    // whatever the expDiff is, if the closeop is add, we need to regularize the result mantissa
    val subFix = !io.expDiff(0) && closeop === 1.U
    val resultMantissaRegularized = Mux(closeop === 0.U && carry === 1.U, Cat(carry, sum), sum ## 0.U(1.W))
    val roundPlus1 = (closeop === 0.U || sum(24) === 1.U) && resultMantissaRegularized(1) && (resultMantissaRegularized(2) || resultMantissaRegularized(0))

    // round off or get the absolute value of the sum
    // For subFix (subtraction with expDiff=0): compute absolute value
    // For normal case: perform rounding on regularized mantissa
    val adderSrc1 = Mux(subFix, 0.U, resultMantissaRegularized(25, 2))
    val adderSrc2 = Mux(subFix, Mux(carry === 0.U, ~(sum(24, 1)), sum(24, 1)), 0.U)
    val adderCarryIn = Mux(subFix, Mux(carry === 0.U, 1.U, 0.U), roundPlus1)
    val (resultMantissaRoundOffTemp, resultMantissaRoundOffCarry) = Adder(adderSrc1, adderSrc2, adderCarryIn, 24)
    val resultMantissaRoundOff = resultMantissaRoundOffTemp ## (closeop & resultMantissaRegularized(1))
    // Overflow prediction using AND-reduction O(log n) instead of waiting for adder carry O(n)
    val overflow = roundPlus1 && resultMantissaRegularized(25, 2).andR && !subFix

    // fix prediction enable
    def fixPredictionEn(src: UInt, prediction: UInt): Bool = {
        val srcShifted = src << prediction
        !srcShifted(24)
    }
    val fixPredictionEnable = fixPredictionEn(resultMantissaRoundOff, lzPredict)
    val lzOffset = Mux(fixPredictionEnable, lzPlus1, lzPredict)
    // compute final mantissa based on operation type
    val mantissaWithCarry = Cat(resultMantissaRoundOffCarry, resultMantissaRoundOff)
    val resultMantissa = Mux(closeop === 1.U,
        // subtraction: normalize with LZP, then adjust for overflow
        {
            val carryBit = Mux(io.expDiff(0), resultMantissaRoundOffCarry, 0.U(1.W))
            val mantissaExtended = Cat(carryBit, resultMantissaRoundOff)
            (mantissaExtended << lzOffset) >> overflow
        },
        // addition: just adjust for overflow
        mantissaWithCarry >> overflow
    )
    // compute result exponent with adjustments for normalization and overflow
    val exponentAdjustment = Mux(closeop === 0.U,
        // addition: increment exponent if overflow occurred
        carry & overflow,
        // subtraction: decrement exponent by LZP offset (using two's complement)
        {
            val lzpOffset = Mux(io.expDiff(0),
                lzOffset,
                Mux(~fixPredictionEnable && overflow, lzMinus1, lzPredict)
            )
            ~(0.U(3.W) ## lzpOffset)
        }
    )
    val exponentCarryIn = Mux(closeop === 0.U,
        // addition: carry in if overflow occurred
        carry | overflow,
        // subtraction: carry in for two's complement (except specific cases)
        Mux(io.expDiff(0), 1.U, Mux(fixPredictionEnable && !overflow, 0.U, 1.U))
    )
    val (resultExponent, _) = Adder(biggerExponent, exponentAdjustment, exponentCarryIn, EXP_WIDTH)
    val resultSign = Mux(closeop === 1.U && carry === 0.U, !biggerSign, biggerSign)
    io.result := Cat(resultSign, resultExponent, resultMantissa(MANTISSA_HIGH+1, MANTISSA_LOW+1))

}

class FAdd extends Module {
    import FPUtils._
    import IEEE754Constants._

    val io = IO(new FAddIO())

    // Step 1: Decompose operands and compute exponent difference using parallel subtraction
    val (sign1, exp1, mant1) = floatBreakdown(io.src1)
    val (sign2, exp2, mant2) = floatBreakdown(io.src2)

    // Parallel exponent subtraction for absolute difference
    val (expDiff1, carry1) = Adder(exp1, ~exp2, 1.U, 8)
    val (expDiff2, carry2) = Adder(exp2, ~exp1, 1.U, 8)

    // Select based on carry (overflow indicates which was larger)
    val expDiff = Mux(carry1.asBool, expDiff1, expDiff2)
    val biggerSrc = Mux(carry1.asBool, io.src1, io.src2)
    val smallerSrc = Mux(carry1.asBool, io.src2, io.src1)

    // Path operation is always the same as io.op
    // For addition: commutative, so order doesn't matter
    // For subtraction: we'll fix the sign later if operands were swapped
    val pathOp = io.op

    // Step 2: Path selection - Far if expDiff > 1, Close if expDiff <= 1
    // expDiff > 1 means expDiff >= 2, so check if any bit [7:1] is set
    val isFarPath = expDiff(7, 1).orR  // Check if bits [7:1] contain any 1

    // Step 3: Instantiate both path modules
    val farPath = Module(new FAddFarPath)
    val closePath = Module(new FAddClosePath)

    // Connect Far path
    farPath.io.biggerSrc := biggerSrc
    farPath.io.smallerSrc := smallerSrc
    farPath.io.op := pathOp
    farPath.io.expDiff := expDiff

    // Connect Close path
    closePath.io.biggerSrc := biggerSrc
    closePath.io.smallerSrc := smallerSrc
    closePath.io.op := pathOp
    closePath.io.expDiff := expDiff

    // Step 4: Select result based on path selection
    val pathResult = Mux(isFarPath, farPath.io.result, closePath.io.result)

    // If operands were swapped (carry1=false) and operation is subtraction (io.op=true),
    // we need to flip the result sign because: src1 - src2 = -(src2 - src1)
    val needSignFlip = !carry1.asBool && io.op
    val resultWithCorrectSign = Mux(needSignFlip,
        Cat(~pathResult(31), pathResult(30, 0)),  // Flip sign bit
        pathResult
    )

    io.result := resultWithCorrectSign
}

class FAddPipelineIO extends Bundle {
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val op = Input(Bool())
    val inValid = Input(Bool())
    val ex2Advance = Input(Bool())
    val ex2Flush = Input(Bool())
    val ex3Advance = Input(Bool())
    val ex3Flush = Input(Bool())
    val wbAdvance = Input(Bool())
    val wbFlush = Input(Bool())
    val outValid = Output(Bool())
    val result = Output(UInt(32.W))
}

/**
  * Three-stage VLIW pipeline for normalized binary32 addition/subtraction.
  *
  * EX1 orders the operands and selects the close/far path. EX2 performs alignment,
  * mantissa arithmetic and leading-zero prediction. EX3 performs normalization,
  * round-to-nearest-even and result packing. The result is registered at WB.
  * Each internal register uses the same advance/flush control as the matching
  * InstructionPackage register in Zircon-VLIW.
  */
class FAddPipeline extends Module {
    import FPUtils._
    import IEEE754Constants._

    val io = IO(new FAddPipelineIO())

    val s1Valid = RegInit(false.B)
    val s2Valid = RegInit(false.B)
    val s3Valid = RegInit(false.B)

    io.outValid := s3Valid

    // Stage 1: exponent comparison, operand ordering and path selection.
    val (_, inputExp1, _) = floatBreakdown(io.src1)
    val (_, inputExp2, _) = floatBreakdown(io.src2)
    val (inputExpDiff1, inputCarry1) = Adder(inputExp1, ~inputExp2, 1.U, EXP_WIDTH)
    val (inputExpDiff2, _) = Adder(inputExp2, ~inputExp1, 1.U, EXP_WIDTH)
    val inputFirstIsBigger = inputCarry1.asBool

    val s1BiggerSrc = Reg(UInt(32.W))
    val s1SmallerSrc = Reg(UInt(32.W))
    val s1ExpDiff = Reg(UInt(EXP_WIDTH.W))
    val s1Op = Reg(Bool())
    val s1IsFar = Reg(Bool())
    val s1NeedSignFlip = Reg(Bool())

    // Stage 2 far-path state.
    val s2IsFar = Reg(Bool())
    val s2NeedSignFlip = Reg(Bool())
    val s2FarSign = Reg(Bool())
    val s2FarExponent = Reg(UInt(EXP_WIDTH.W))
    val s2FarOp = Reg(Bool())
    val s2FarSum = Reg(UInt(FULL_MANTISSA_WIDTH.W))
    val s2FarCarry = Reg(Bool())

    // Stage 2 close-path state.
    val s2CloseSign = Reg(Bool())
    val s2CloseExponent = Reg(UInt(EXP_WIDTH.W))
    val s2CloseOp = Reg(Bool())
    val s2CloseExpDiffBit = Reg(Bool())
    val s2CloseSum = Reg(UInt(25.W))
    val s2CloseCarry = Reg(Bool())
    val s2CloseLz = Reg(UInt(5.W))
    val s2CloseLzPlus1 = Reg(UInt(5.W))
    val s2CloseLzMinus1 = Reg(UInt(5.W))

    val (farBiggerSign, farBiggerExponent, farBiggerMantissa) = floatBreakdown(s1BiggerSrc)
    val (farSmallerSign, _, farSmallerMantissa) = floatBreakdown(s1SmallerSrc)
    val farOp = farBiggerSign ^ farSmallerSign ^ s1Op
    val farBiggerMantissaFull = Cat(1.U(1.W), farBiggerMantissa, 0.U(GRS_WIDTH.W))
    val farSmallerExtended = Cat(1.U(1.W), farSmallerMantissa, 0.U(EXTENDED_SHIFT_WIDTH.W))
    val farSmallerShifted = farSmallerExtended >> s1ExpDiff
    val farShiftedSignificand = farSmallerShifted(EXTENDED_MANTISSA_WIDTH - 1, EXTENDED_SHIFT_WIDTH)
    val farGuardRound = farSmallerShifted(SIGN_BIT, EXP_HIGH)
    val farLargeShift = s1ExpDiff(SHIFT_THRESHOLD_HIGH, SHIFT_THRESHOLD_LOW).orR
    val farSticky = Mux(farLargeShift, farSmallerMantissa.orR, farSmallerShifted(EXP_HIGH - 1, 0).orR)
    val farSmallerMantissaFull = Cat(farShiftedSignificand, farGuardRound, farSticky)
    val (farSum, farCarry) = Adder(
        farBiggerMantissaFull,
        Mux(farOp, ~farSmallerMantissaFull, farSmallerMantissaFull),
        farOp,
        FULL_MANTISSA_WIDTH
    )

    def leadingZeroPredictor(src1: UInt, src2: UInt): (UInt, UInt, UInt) = {
        val width = src1.getWidth
        val src1Pad = Cat(src1, 0.U(1.W))
        val src2Pad = Cat(src2, 0.U(1.W))
        val xor = src1Pad ^ src2Pad
        val z = src1Pad | ~src2Pad
        val predictionBits = VecInit.tabulate(width) { i => xor(i + 1) && z(i) }.asUInt
        val prediction = PriorityEncoder(Reverse(predictionBits))
        val (plus1, _) = Adder(prediction, 1.U, 0.U, prediction.getWidth)
        val (minus1, _) = Adder(prediction, ~1.U(prediction.getWidth.W), 1.U, prediction.getWidth)
        (prediction, plus1, minus1)
    }

    val (closeBiggerSign, closeBiggerExponent, closeBiggerMantissa) = floatBreakdown(s1BiggerSrc)
    val (closeSmallerSign, _, closeSmallerMantissa) = floatBreakdown(s1SmallerSrc)
    val closeOp = closeBiggerSign ^ closeSmallerSign ^ s1Op
    val closeBiggerMantissaFull = Cat(1.U(1.W), closeBiggerMantissa, 0.U(1.W))
    val closeSmallerExtended = Cat(1.U(1.W), closeSmallerMantissa, 0.U(1.W))
    val closeSmallerMantissaFull = Mux(s1ExpDiff(0), closeSmallerExtended >> 1, closeSmallerExtended)
    val (closeLzBigger, closeLzPlus1Bigger, closeLzMinus1Bigger) =
        leadingZeroPredictor(closeBiggerMantissaFull, closeSmallerMantissaFull)
    val (closeLzSmaller, closeLzPlus1Smaller, closeLzMinus1Smaller) =
        leadingZeroPredictor(closeSmallerMantissaFull, closeBiggerMantissaFull)
    val (closeSum, closeCarry) = Adder(
        closeBiggerMantissaFull,
        Mux(closeOp, ~closeSmallerMantissaFull, closeSmallerMantissaFull),
        closeOp,
        25
    )
    val closeLz = Mux(closeCarry.asBool, closeLzBigger, closeLzSmaller)
    val closeLzPlus1 = Mux(closeCarry.asBool, closeLzPlus1Bigger, closeLzPlus1Smaller)
    val closeLzMinus1 = Mux(closeCarry.asBool, closeLzMinus1Bigger, closeLzMinus1Smaller)

    // Stage 3 far-path normalization and rounding.
    val farRegularPlus1 = s2FarCarry === !s2FarOp
    val farRegularMinus1 = !s2FarSum(FULL_MANTISSA_WIDTH - 1) && !farRegularPlus1
    val farRegularized = Mux(
        farRegularMinus1,
        s2FarSum << 1,
        Mux(
            farRegularPlus1,
            Cat(s2FarOp && s2FarCarry, s2FarSum)(FULL_MANTISSA_WIDTH, 2) ## s2FarSum(1, 0).orR,
            s2FarSum
        )
    )
    val farMantissaBeforeRound = farRegularized(FULL_MANTISSA_WIDTH - 1, GRS_WIDTH)
    val farRoundUp = farRegularized(2) &&
        (farRegularized(1) || farRegularized(0) || farMantissaBeforeRound(0))
    val (farRoundedMantissa, farRoundCarry) =
        Adder(farMantissaBeforeRound, farRoundUp, 0.U, MANTISSA_WIDTH + 1)
    val farRoundOverflow = farRoundUp && farMantissaBeforeRound.andR
    val farFinalMantissa = Cat(farRoundCarry, farRoundedMantissa) >> farRoundOverflow
    val (farResultExponent, _) = Adder(
        s2FarExponent,
        Cat(
            Fill(EXP_WIDTH - 1, farRegularMinus1 && !farRoundOverflow),
            farRegularPlus1 && farRoundOverflow
        ),
        farRegularPlus1 || (farRegularMinus1 ^ farRoundOverflow),
        EXP_WIDTH
    )
    val farResult = Cat(s2FarSign, farResultExponent, farFinalMantissa(MANTISSA_HIGH, MANTISSA_LOW))

    // Stage 3 close-path normalization and rounding.
    val closeSubFix = !s2CloseExpDiffBit && s2CloseOp
    val closeRegularized = Mux(
        !s2CloseOp && s2CloseCarry,
        Cat(s2CloseCarry, s2CloseSum),
        s2CloseSum ## 0.U(1.W)
    )
    val closeRoundUp = (!s2CloseOp || s2CloseSum(24)) && closeRegularized(1) &&
        (closeRegularized(2) || closeRegularized(0))
    val closeAdderSrc1 = Mux(closeSubFix, 0.U, closeRegularized(25, 2))
    val closeAdderSrc2 = Mux(
        closeSubFix,
        Mux(!s2CloseCarry, ~s2CloseSum(24, 1), s2CloseSum(24, 1)),
        0.U
    )
    val closeAdderCarryIn = Mux(closeSubFix, Mux(!s2CloseCarry, 1.U, 0.U), closeRoundUp)
    val (closeRoundedTemp, closeRoundedCarry) =
        Adder(closeAdderSrc1, closeAdderSrc2, closeAdderCarryIn, 24)
    val closeRounded = closeRoundedTemp ## (s2CloseOp && closeRegularized(1))
    val closeRoundOverflow = closeRoundUp && closeRegularized(25, 2).andR && !closeSubFix
    val closePredictionNeedsFix = !(closeRounded << s2CloseLz)(24)
    val closeLzOffset = Mux(closePredictionNeedsFix, s2CloseLzPlus1, s2CloseLz)
    val closeMantissaWithCarry = Cat(closeRoundedCarry, closeRounded)
    val closeResultMantissa = Mux(
        s2CloseOp,
        {
            val carryBit = Mux(s2CloseExpDiffBit, closeRoundedCarry, 0.U(1.W))
            (Cat(carryBit, closeRounded) << closeLzOffset) >> closeRoundOverflow
        },
        closeMantissaWithCarry >> closeRoundOverflow
    )
    val closeExponentAdjustment = Mux(
        !s2CloseOp,
        s2CloseCarry && closeRoundOverflow,
        {
            val lzpOffset = Mux(
                s2CloseExpDiffBit,
                closeLzOffset,
                Mux(!closePredictionNeedsFix && closeRoundOverflow, s2CloseLzMinus1, s2CloseLz)
            )
            ~(0.U(3.W) ## lzpOffset)
        }
    )
    val closeExponentCarryIn = Mux(
        !s2CloseOp,
        s2CloseCarry || closeRoundOverflow,
        Mux(s2CloseExpDiffBit, 1.U, Mux(closePredictionNeedsFix && !closeRoundOverflow, 0.U, 1.U))
    )
    val (closeResultExponent, _) =
        Adder(s2CloseExponent, closeExponentAdjustment, closeExponentCarryIn, EXP_WIDTH)
    val closeResultSign = Mux(s2CloseOp && !s2CloseCarry, !s2CloseSign, s2CloseSign)
    val closeResult = Cat(
        closeResultSign,
        closeResultExponent,
        closeResultMantissa(MANTISSA_HIGH + 1, MANTISSA_LOW + 1)
    )

    val selectedResult = Mux(s2IsFar, farResult, closeResult)
    val stage3Result = Mux(
        s2NeedSignFlip,
        Cat(~selectedResult(SIGN_BIT), selectedResult(EXP_HIGH, MANTISSA_LOW)),
        selectedResult
    )
    val s3Result = Reg(UInt(32.W))
    io.result := s3Result

    when(io.wbFlush) {
        s3Valid := false.B
    }.elsewhen(io.wbAdvance) {
        s3Valid := s2Valid
        when(s2Valid) {
            s3Result := stage3Result
        }
    }

    when(io.ex3Flush) {
        s2Valid := false.B
    }.elsewhen(io.ex3Advance) {
        s2Valid := s1Valid
        when(s1Valid) {
            s2IsFar := s1IsFar
            s2NeedSignFlip := s1NeedSignFlip
            s2FarSign := farBiggerSign
            s2FarExponent := farBiggerExponent
            s2FarOp := farOp
            s2FarSum := farSum
            s2FarCarry := farCarry.asBool
            s2CloseSign := closeBiggerSign
            s2CloseExponent := closeBiggerExponent
            s2CloseOp := closeOp
            s2CloseExpDiffBit := s1ExpDiff(0)
            s2CloseSum := closeSum
            s2CloseCarry := closeCarry.asBool
            s2CloseLz := closeLz
            s2CloseLzPlus1 := closeLzPlus1
            s2CloseLzMinus1 := closeLzMinus1
        }
    }

    when(io.ex2Flush) {
        s1Valid := false.B
    }.elsewhen(io.ex2Advance) {
        s1Valid := io.inValid
        when(io.inValid) {
            s1BiggerSrc := Mux(inputFirstIsBigger, io.src1, io.src2)
            s1SmallerSrc := Mux(inputFirstIsBigger, io.src2, io.src1)
            s1ExpDiff := Mux(inputFirstIsBigger, inputExpDiff1, inputExpDiff2)
            s1Op := io.op
            s1IsFar := Mux(inputFirstIsBigger, inputExpDiff1, inputExpDiff2)(7, 1).orR
            s1NeedSignFlip := !inputFirstIsBigger && io.op
        }
    }
}
