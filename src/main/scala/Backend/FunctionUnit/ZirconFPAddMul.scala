import chisel3._
import chisel3.util._

/** Pipeline controls shared with the processor EX1/EX2/EX3 boundaries. */
class ZirconFPPipelineControl extends Bundle {
    val s1Enable = Input(Bool())
    val s1Flush  = Input(Bool())
    val s2Enable = Input(Bool())
    val s2Flush  = Input(Bool())
}

private object ZirconFPUtils {
    val RNE = 0.U(3.W)
    val RTZ = 1.U(3.W)
    val RDN = 2.U(3.W)
    val RUP = 3.U(3.W)
    val RMM = 4.U(3.W)

    /** Fixed-width right shift with all discarded bits folded into bit zero. */
    def shiftRightJam(value: UInt, distance: UInt): UInt = {
        val width = value.getWidth
        val result = Wire(UInt(width.W))
        val allOnes = ((BigInt(1) << width) - 1).U(width.W)

        when(distance === 0.U) {
            result := value
        }.elsewhen(distance >= width.U) {
            result := Cat(0.U((width - 1).W), value.orR)
        }.otherwise {
            val shifted = value >> distance
            val lostMask = allOnes >> (width.U - distance)
            result := Cat(shifted(width - 1, 1), shifted(0) | (value & lostMask).orR)
        }
        result
    }

    /** Right shift with jam, narrowing a value known to fit after shifting. */
    def shiftRightJamTo(value: UInt, distance: UInt, outWidth: Int): UInt = {
        val inWidth = value.getWidth
        require(outWidth <= inWidth)
        val result = Wire(UInt(outWidth.W))
        val allOnes = ((BigInt(1) << inWidth) - 1).U(inWidth.W)

        when(distance >= inWidth.U) {
            result := Cat(0.U((outWidth - 1).W), value.orR)
        }.otherwise {
            val shifted = value >> distance
            val lostMask = Mux(
                distance === 0.U,
                0.U(inWidth.W),
                allOnes >> (inWidth.U - distance)
            )
            val narrowed = shifted(outWidth - 1, 0)
            result := Cat(
                narrowed(outWidth - 1, 1),
                narrowed(0) | (value & lostMask).orR
            )
        }
        result
    }

    /** IEEE-754/RISC-V rounding and packing shared by add and multiply. */
    def roundPack(
        sign: Bool,
        exponent: UInt,
        significand: UInt,
        rm: UInt,
        exactZero: Bool,
        zeroSign: Bool,
        specialNaN: Bool,
        specialInf: Bool,
        specialSign: Bool,
        invalid: Bool
    ): (UInt, UInt) = {
        require(significand.getWidth == 27)

        val retained = significand(26, 3)
        val guard = significand(2)
        val round = significand(1)
        val sticky = significand(0)
        val inexact = guard || round || sticky

        val increment = MuxLookup(rm, false.B)(Seq(
            RNE -> (guard && (round || sticky || retained(0))),
            RTZ -> false.B,
            RDN -> (sign && inexact),
            RUP -> (!sign && inexact),
            RMM -> guard
        ))

        val roundedWide = Cat(0.U(1.W), retained) + increment
        val roundedCarry = roundedWide(24)
        val roundedSignificand = Mux(
            roundedCarry,
            roundedWide(24, 1),
            roundedWide(23, 0)
        )
        val roundedExponent = exponent + roundedCarry

        // A rounded subnormal becomes normal exactly when its hidden bit appears.
        val isNormal = roundedSignificand(23)
        val overflow = isNormal && roundedExponent >= 255.U
        val toInfinity = rm === RNE || rm === RMM ||
            (rm === RUP && !sign) || (rm === RDN && sign)

        val finiteExponent = Mux(isNormal, roundedExponent(7, 0), 0.U(8.W))
        val finiteResult = Cat(sign, finiteExponent, roundedSignificand(22, 0))
        val overflowResult = Mux(
            toInfinity,
            Cat(sign, "hff".U(8.W), 0.U(23.W)),
            Cat(sign, "hfe".U(8.W), Fill(23, 1.U(1.W)))
        )
        val commonResult = Mux(overflow, overflowResult, finiteResult)

        // RISC-V uses tininess after rounding: UF is raised only for tiny, inexact results.
        val tinyAfterRounding = !isNormal
        val commonFlags = Cat(
            false.B,
            false.B,
            overflow,
            tinyAfterRounding && inexact,
            inexact || overflow
        )

        val canonicalNaN = "h7fc00000".U(32.W)
        val infinity = Cat(specialSign, "hff".U(8.W), 0.U(23.W))
        val zero = Cat(zeroSign, 0.U(31.W))

        val result = MuxCase(commonResult, Seq(
            specialNaN -> canonicalNaN,
            specialInf -> infinity,
            exactZero   -> zero
        ))
        val flags = Mux(
            specialNaN,
            Cat(invalid, 0.U(4.W)),
            Mux(specialInf || exactZero, 0.U(5.W), commonFlags)
        )
        (result, flags)
    }
}

class ZirconFPAddS1 extends Bundle {
    val signBig = Bool()
    val signsEqual = Bool()
    val exponent = UInt(9.W)
    val bigSignificand = UInt(27.W)
    val smallSignificand = UInt(27.W)
    val rm = UInt(3.W)
    val zeroSign = Bool()
    val specialNaN = Bool()
    val specialInf = Bool()
    val specialSign = Bool()
    val invalid = Bool()
}

class ZirconFPAddS2 extends Bundle {
    val sign = Bool()
    val exponent = UInt(9.W)
    val significand = UInt(27.W)
    val rm = UInt(3.W)
    val exactZero = Bool()
    val zeroSign = Bool()
    val specialNaN = Bool()
    val specialInf = Bool()
    val specialSign = Bool()
    val invalid = Bool()
}

class ZirconFPAddPipelineIO extends Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val sub = Input(Bool())
    val rm = Input(UInt(3.W))
    val control = new ZirconFPPipelineControl
    val result = Output(UInt(32.W))
    val fflags = Output(UInt(5.W))
}

/**
 * Three-stage single-precision adder derived from the Zircon-FloatPoint
 * far/close-path decomposition:
 *   EX1: classify, order and align; EX2: add/subtract and normalize;
 *   EX3: round, pack and generate exception flags.
 */
class ZirconFPAddPipeline extends Module {
    import ZirconFPUtils._

    val io = IO(new ZirconFPAddPipelineIO)

    val signA = io.a(31)
    val signB = io.b(31) ^ io.sub
    val expA = io.a(30, 23)
    val expB = io.b(30, 23)
    val fracA = io.a(22, 0)
    val fracB = io.b(22, 0)

    val aIsNaN = expA.andR && fracA.orR
    val bIsNaN = expB.andR && fracB.orR
    val aIsSNaN = aIsNaN && !fracA(22)
    val bIsSNaN = bIsNaN && !fracB(22)
    val aIsInf = expA.andR && !fracA.orR
    val bIsInf = expB.andR && !fracB.orR

    val effectiveExpA = Mux(expA === 0.U, 1.U(8.W), expA)
    val effectiveExpB = Mux(expB === 0.U, 1.U(8.W), expB)
    val sigA = Cat(expA =/= 0.U, fracA)
    val sigB = Cat(expB =/= 0.U, fracB)

    val aMagnitudeGreaterOrEqual = effectiveExpA > effectiveExpB ||
        (effectiveExpA === effectiveExpB && sigA >= sigB)
    val bigExp = Mux(aMagnitudeGreaterOrEqual, effectiveExpA, effectiveExpB)
    val smallExp = Mux(aMagnitudeGreaterOrEqual, effectiveExpB, effectiveExpA)
    val bigSig = Mux(aMagnitudeGreaterOrEqual, sigA, sigB)
    val smallSig = Mux(aMagnitudeGreaterOrEqual, sigB, sigA)
    val signBig = Mux(aMagnitudeGreaterOrEqual, signA, signB)
    val expDifference = bigExp - smallExp

    val bigExtended = Cat(bigSig, 0.U(3.W))
    val smallExtended = Cat(smallSig, 0.U(3.W))
    val smallAligned = shiftRightJam(smallExtended, expDifference)

    val infinityInvalid = aIsInf && bIsInf && (signA =/= signB)
    val specialNaN = aIsNaN || bIsNaN || infinityInvalid
    val specialInf = !specialNaN && (aIsInf || bIsInf)
    val specialSign = Mux(aIsInf, signA, signB)
    val invalid = aIsSNaN || bIsSNaN || infinityInvalid
    val zeroSign = Mux(signA === signB, signA, io.rm === RDN)

    val s1Next = Wire(new ZirconFPAddS1)
    s1Next.signBig := signBig
    s1Next.signsEqual := signA === signB
    s1Next.exponent := bigExp
    s1Next.bigSignificand := bigExtended
    s1Next.smallSignificand := smallAligned
    s1Next.rm := io.rm
    s1Next.zeroSign := zeroSign
    s1Next.specialNaN := specialNaN
    s1Next.specialInf := specialInf
    s1Next.specialSign := specialSign
    s1Next.invalid := invalid

    val s1 = RegInit(0.U.asTypeOf(new ZirconFPAddS1))
    when(io.control.s1Flush) {
        s1 := 0.U.asTypeOf(new ZirconFPAddS1)
    }.elsewhen(io.control.s1Enable) {
        s1 := s1Next
    }

    val sum = Cat(0.U(1.W), s1.bigSignificand) +& s1.smallSignificand
    val difference = s1.bigSignificand - s1.smallSignificand
    val additionCarry = sum(27)
    val additionSignificand = Mux(
        additionCarry,
        Cat(sum(27, 2), sum(1) | sum(0)),
        sum(26, 0)
    )
    val additionExponent = s1.exponent + additionCarry

    val exactZero = !s1.signsEqual && difference === 0.U
    val leadingZeros = PriorityEncoder(Reverse(difference))
    // The 27-bit significand never needs more than 26 left shifts. Capping
    // and narrowing this amount avoids inferring an unnecessarily huge
    // barrel shifter from the 9-bit exponent field.
    val maximumLeftShift = Mux(
        s1.exponent > 27.U,
        26.U(5.W),
        (s1.exponent - 1.U)(4, 0)
    )
    val leftShift = Mux(leadingZeros > maximumLeftShift, maximumLeftShift, leadingZeros)
    val subtractionSignificand = (difference << leftShift)(26, 0)
    val subtractionExponent = s1.exponent - leftShift

    val s2Next = Wire(new ZirconFPAddS2)
    s2Next.sign := s1.signBig
    s2Next.exponent := Mux(s1.signsEqual, additionExponent, subtractionExponent)
    s2Next.significand := Mux(s1.signsEqual, additionSignificand, subtractionSignificand)
    s2Next.rm := s1.rm
    s2Next.exactZero := exactZero
    s2Next.zeroSign := s1.zeroSign
    s2Next.specialNaN := s1.specialNaN
    s2Next.specialInf := s1.specialInf
    s2Next.specialSign := s1.specialSign
    s2Next.invalid := s1.invalid

    val s2 = RegInit(0.U.asTypeOf(new ZirconFPAddS2))
    when(io.control.s2Flush) {
        s2 := 0.U.asTypeOf(new ZirconFPAddS2)
    }.elsewhen(io.control.s2Enable) {
        s2 := s2Next
    }

    val (result, flags) = roundPack(
        s2.sign,
        s2.exponent,
        s2.significand,
        s2.rm,
        s2.exactZero,
        s2.zeroSign,
        s2.specialNaN,
        s2.specialInf,
        s2.specialSign,
        s2.invalid
    )
    io.result := result
    io.fflags := flags
}

class ZirconFPMulS1 extends Bundle {
    val sign = Bool()
    val exponentA = UInt(8.W)
    val exponentB = UInt(8.W)
    val product = UInt(48.W)
    val rm = UInt(3.W)
    val specialNaN = Bool()
    val specialInf = Bool()
    val specialZero = Bool()
    val invalid = Bool()
}

class ZirconFPMulS2 extends Bundle {
    val sign = Bool()
    val exponent = UInt(10.W)
    val significand = UInt(27.W)
    val rm = UInt(3.W)
    val specialNaN = Bool()
    val specialInf = Bool()
    val specialZero = Bool()
    val invalid = Bool()
}

class ZirconFPMulPipelineIO extends Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val rm = Input(UInt(3.W))
    val control = new ZirconFPPipelineControl
    val result = Output(UInt(32.W))
    val fflags = Output(UInt(5.W))
}

/**
 * Three-stage single-precision multiplier derived from Zircon-FloatPoint:
 *   EX1: classify, exponent sum and 24x24 product;
 *   EX2: normalize/subnormalize with sticky-bit preservation;
 *   EX3: round, pack and generate exception flags.
 */
class ZirconFPMulPipeline extends Module {
    import ZirconFPUtils._

    val io = IO(new ZirconFPMulPipelineIO)

    val expA = io.a(30, 23)
    val expB = io.b(30, 23)
    val fracA = io.a(22, 0)
    val fracB = io.b(22, 0)
    val sign = io.a(31) ^ io.b(31)

    val aIsNaN = expA.andR && fracA.orR
    val bIsNaN = expB.andR && fracB.orR
    val aIsSNaN = aIsNaN && !fracA(22)
    val bIsSNaN = bIsNaN && !fracB(22)
    val aIsInf = expA.andR && !fracA.orR
    val bIsInf = expB.andR && !fracB.orR
    val aIsZero = expA === 0.U && !fracA.orR
    val bIsZero = expB === 0.U && !fracB.orR

    val zeroTimesInfinity = (aIsZero && bIsInf) || (bIsZero && aIsInf)
    val specialNaN = aIsNaN || bIsNaN || zeroTimesInfinity
    val specialInf = !specialNaN && (aIsInf || bIsInf)
    val specialZero = !specialNaN && !specialInf && (aIsZero || bIsZero)
    val invalid = aIsSNaN || bIsSNaN || zeroTimesInfinity

    val sigA = Cat(expA =/= 0.U, fracA)
    val sigB = Cat(expB =/= 0.U, fracB)
    val effectiveExpA = Mux(expA === 0.U, 1.U(8.W), expA)
    val effectiveExpB = Mux(expB === 0.U, 1.U(8.W), expB)

    val s1Next = Wire(new ZirconFPMulS1)
    s1Next.sign := sign
    s1Next.exponentA := effectiveExpA
    s1Next.exponentB := effectiveExpB
    s1Next.product := sigA * sigB
    s1Next.rm := io.rm
    s1Next.specialNaN := specialNaN
    s1Next.specialInf := specialInf
    s1Next.specialZero := specialZero
    s1Next.invalid := invalid

    val s1 = RegInit(0.U.asTypeOf(new ZirconFPMulS1))
    when(io.control.s1Flush) {
        s1 := 0.U.asTypeOf(new ZirconFPMulS1)
    }.elsewhen(io.control.s1Enable) {
        s1 := s1Next
    }

    val leadingZeros = PriorityEncoder(Reverse(s1.product))
    val leadingPosition = 47.U - leadingZeros
    // Keep explicit signed headroom: large finite operands can have an
    // intermediate biased exponent well above 255 before overflow packing.
    val exponentSum = s1.exponentA +& s1.exponentB
    val baseExponent = Wire(SInt(12.W))
    baseExponent := exponentSum.zext - 127.S
    val normalExponent = Wire(SInt(12.W))
    normalExponent := baseExponent + leadingPosition.zext - 46.S
    val targetExponent = Wire(SInt(12.W))
    targetExponent := Mux(normalExponent < 1.S, 1.S, normalExponent)
    // sig27 = product * 2^(baseExponent - targetExponent - 20)
    val signedShift = baseExponent - targetExponent - 20.S

    val normalizedSignificand = Wire(UInt(27.W))
    when(s1.product === 0.U) {
        normalizedSignificand := 0.U
    }.elsewhen(signedShift >= 0.S) {
        val leftShift = signedShift.asUInt
        normalizedSignificand := (s1.product << leftShift(4, 0))(26, 0)
    }.otherwise {
        val rightShift = (0.S - signedShift).asUInt
        normalizedSignificand := shiftRightJamTo(s1.product, rightShift, 27)
    }

    val s2Next = Wire(new ZirconFPMulS2)
    s2Next.sign := s1.sign
    s2Next.exponent := targetExponent.asUInt
    s2Next.significand := normalizedSignificand
    s2Next.rm := s1.rm
    s2Next.specialNaN := s1.specialNaN
    s2Next.specialInf := s1.specialInf
    s2Next.specialZero := s1.specialZero
    s2Next.invalid := s1.invalid

    val s2 = RegInit(0.U.asTypeOf(new ZirconFPMulS2))
    when(io.control.s2Flush) {
        s2 := 0.U.asTypeOf(new ZirconFPMulS2)
    }.elsewhen(io.control.s2Enable) {
        s2 := s2Next
    }

    val (result, flags) = roundPack(
        s2.sign,
        s2.exponent,
        s2.significand,
        s2.rm,
        s2.specialZero,
        s2.sign,
        s2.specialNaN,
        s2.specialInf,
        s2.sign,
        s2.invalid
    )
    io.result := result
    io.fflags := flags
}
