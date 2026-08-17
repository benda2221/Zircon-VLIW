package zirconfp

import chisel3._
import chisel3.util._

object FSqrtRoundingMode {
  val RNE = 0.U(3.W)
  val RTZ = 1.U(3.W)
  val RDN = 2.U(3.W)
  val RUP = 3.U(3.W)
  val RMM = 4.U(3.W)
}

object FSqrtConstants {
  val CanonicalNaN = "h7fc00000".U(32.W)
  val PositiveInfinity = "h7f800000".U(32.W)
}

class FSqrtIO extends Bundle {
  val src = Input(UInt(32.W))
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
  * Iterative IEEE 754 single-precision square-root unit.
  *
  * The core accepts one operation at a time. The resolved RISC-V rounding mode
  * must be supplied on rm; dynamic mode (rm=111) is resolved by the caller.
  * fflags are ordered as {NV, DZ, OF, UF, NX}.
  */
class FSqrt extends Module {
  import FSqrtConstants._
  import FSqrtRoundingMode._

  val io = IO(new FSqrtIO)
  private val RemainderWidth = 28
  private val ExpandedWidth = 31
  private val RootWidth = 26

  val idle :: normalizeSubnormal :: iterate :: response :: Nil = Enum(4)
  val state = RegInit(idle)

  // The first two root bits are generated while accepting the request. Every
  // later cycle produces one radix-8 digit and keeps a carry-save remainder.
  val radicandReg = Reg(UInt(24.W))
  val remainderSumReg = Reg(UInt(RemainderWidth.W))
  val remainderCarryReg = Reg(UInt(RemainderWidth.W))
  val rootReg = Reg(UInt(RootWidth.W))
  val iterationReg = Reg(UInt(3.W))
  val subnormalFractionReg = Reg(UInt(23.W))
  val subnormalShiftReg = Reg(UInt(5.W))
  val resultExponentReg = Reg(SInt(12.W))
  val roundingModeReg = Reg(UInt(3.W))
  val resultReg = RegInit(0.U(32.W))
  val flagsReg = RegInit(0.U(5.W))
  val finiteResponseReg = RegInit(false.B)

  io.inReady := state === idle
  io.outValid := state === response
  io.busy := state =/= idle

  val sign = io.src(31)
  val exponent = io.src(30, 23)
  val fraction = io.src(22, 0)

  val isZero = !exponent.orR && !fraction.orR
  val isInfinity = exponent.andR && !fraction.orR
  val isNaN = exponent.andR && fraction.orR
  val isSignalingNaN = isNaN && !fraction(22)
  val negativeInvalid = sign && !isZero && !isNaN
  val specialCase = isNaN || negativeInvalid || isZero || isInfinity

  val specialResult = WireDefault(0.U(32.W))
  val specialFlags = WireDefault(0.U(5.W))
  when(isNaN || negativeInvalid) {
    specialResult := CanonicalNaN
    specialFlags := Cat(isSignalingNaN || negativeInvalid, 0.U(4.W))
  }.elsewhen(isZero) {
    specialResult := io.src
  }.elsewhen(isInfinity) {
    specialResult := PositiveInfinity
  }

  def formInitialRadicand(significand: UInt, unbiasedExponent: SInt): UInt = Mux(
    unbiasedExponent.asUInt(0),
    Cat(significand, 0.U(28.W)),
    Cat(0.U(1.W), significand, 0.U(27.W))
  )

  val normalUnbiasedExponent = exponent.zext - 127.S(12.W)
  val normalInitialRadicand = formInitialRadicand(
    Cat(1.U(1.W), fraction),
    normalUnbiasedExponent
  )

  val normalizedSubnormal = (
    Cat(0.U(1.W), subnormalFractionReg) << subnormalShiftReg
  )(23, 0)
  val subnormalUnbiasedExponent =
    (-126).S(12.W) - subnormalShiftReg.zext
  val subnormalInitialRadicand = formInitialRadicand(
    normalizedSubnormal,
    subnormalUnbiasedExponent
  )

  def parallelPrefixGenerate(a: UInt, b: UInt, carryIn: Bool): UInt = {
    val width = a.getWidth
    val bitPropagate = a ^ b
    var groupPropagate = bitPropagate
    var groupGenerate = (a & b) | (bitPropagate(0) && carryIn).asUInt

    for (shift <- Seq(1, 2, 4, 8, 16).filter(_ < width)) {
      val upperGenerate = groupGenerate(width - 1, shift) |
        (groupPropagate(width - 1, shift) & groupGenerate(width - shift - 1, 0))
      val upperPropagate = groupPropagate(width - 1, shift) &
        groupPropagate(width - shift - 1, 0)
      groupGenerate = Cat(upperGenerate, groupGenerate(shift - 1, 0))
      groupPropagate = Cat(upperPropagate, groupPropagate(shift - 1, 0))
    }

    groupGenerate
  }

  def parallelPrefixAdd(a: UInt, b: UInt, carryIn: Bool): UInt = {
    val width = a.getWidth
    val bitPropagate = a ^ b
    val groupGenerate = parallelPrefixGenerate(a, b, carryIn)

    val carries = Cat(groupGenerate(width - 2, 0), carryIn)
    bitPropagate ^ carries
  }

  def carrySave(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    val sum = a ^ b ^ c
    val carryGenerate = (a & b) | (a & c) | (b & c)
    val carry = Cat(carryGenerate(a.getWidth - 2, 0), 0.U(1.W))
    (sum, carry)
  }

  def initialRootDigitFor(nibble: UInt): UInt = Mux(
    nibble >= 9.U,
    3.U(2.W),
    Mux(nibble >= 4.U, 2.U(2.W), Mux(nibble.orR, 1.U(2.W), 0.U(2.W)))
  )

  def initialRemainderFor(nibble: UInt, rootDigit: UInt): UInt = {
    val trial = MuxLookup(rootDigit, 0.U(4.W))(Seq(
      1.U -> 1.U(4.W),
      2.U -> 4.U(4.W),
      3.U -> 9.U(4.W)
    ))
    nibble - trial
  }

  val normalInitialNibble = normalInitialRadicand(51, 48)
  val normalInitialRootDigit = initialRootDigitFor(normalInitialNibble)
  val normalInitialRemainder = initialRemainderFor(
    normalInitialNibble,
    normalInitialRootDigit
  )
  val normalInitialRadicandTail = normalInitialRadicand(47, 24)
  val normalInitialResultExponent = normalUnbiasedExponent >> 1

  val subnormalInitialNibble = subnormalInitialRadicand(51, 48)
  val subnormalInitialRootDigit = initialRootDigitFor(subnormalInitialNibble)
  val subnormalInitialRemainder = initialRemainderFor(
    subnormalInitialNibble,
    subnormalInitialRootDigit
  )
  val subnormalInitialRadicandTail = subnormalInitialRadicand(47, 24)
  val subnormalInitialResultExponent = subnormalUnbiasedExponent >> 1

  def radix8Candidate(
      remainderSum: UInt,
      remainderCarry: UInt,
      root: UInt,
      sextet: UInt,
      digit: Int
  ): (UInt, UInt) = {
    val expandedSum = Cat(remainderSum, sextet)(ExpandedWidth - 1, 0)
    val expandedCarry = Cat(remainderCarry, 0.U(6.W))(ExpandedWidth - 1, 0)
    if (digit == 0) {
      return (expandedSum, expandedCarry)
    }

    val rootWide = Cat(0.U((ExpandedWidth - RootWidth).W), root)
    val rootTerms = (0 until 3).map { bit =>
      if ((digit & (1 << bit)) != 0) {
        (rootWide << (4 + bit))(ExpandedWidth - 1, 0)
      } else {
        0.U(ExpandedWidth.W)
      }
    }
    val trialGroup = carrySave(rootTerms(0), rootTerms(1), rootTerms(2))
    val trial = carrySave(
      trialGroup._1,
      trialGroup._2,
      (digit * digit).U(ExpandedWidth.W)
    )

    // expanded - (trialSum + trialCarry)
    val differenceGroup0 = carrySave(expandedSum, expandedCarry, ~trial._1)
    val differenceGroup1 = carrySave(
      ~trial._2,
      2.U(ExpandedWidth.W),
      0.U(ExpandedWidth.W)
    )
    val differenceMerged = carrySave(
      differenceGroup0._1,
      differenceGroup0._2,
      differenceGroup1._1
    )
    carrySave(differenceMerged._1, differenceMerged._2, differenceGroup1._2)
  }

  def selectByIndex(values: Seq[UInt], index: UInt): UInt = {
    val level0 = (0 until 4).map { pair =>
      Mux(index(0), values(2 * pair + 1), values(2 * pair))
    }
    val level1 = (0 until 2).map { pair =>
      Mux(index(1), level0(2 * pair + 1), level0(2 * pair))
    }
    Mux(index(2), level1(1), level1(0))
  }

  val radix8Sextet = radicandReg(23, 18)
  val radix8Candidates = (0 until 8).map { digit =>
    radix8Candidate(
      remainderSumReg,
      remainderCarryReg,
      rootReg,
      radix8Sextet,
      digit
    )
  }
  val radix8Valid = Wire(Vec(8, Bool()))
  radix8Valid(0) := true.B
  for (digit <- 1 until 8) {
    radix8Valid(digit) := !parallelPrefixAdd(
      radix8Candidates(digit)._1,
      radix8Candidates(digit)._2,
      false.B
    )(ExpandedWidth - 1)
  }
  val radix8Digit = Cat(
    radix8Valid(4),
    radix8Valid(2) ^ radix8Valid(4) ^ radix8Valid(6),
    (1 until 8).map(radix8Valid(_)).reduce(_ ^ _)
  )

  val iterationRemainderSum = selectByIndex(
    radix8Candidates.map(_._1(RemainderWidth - 1, 0)),
    radix8Digit
  )
  val iterationRemainderCarry = selectByIndex(
    radix8Candidates.map(_._2(RemainderWidth - 1, 0)),
    radix8Digit
  )
  val iterationRoot = ((rootReg << 3) | radix8Digit)(
    RootWidth - 1,
    0
  )
  val iterationRadicand = Cat(radicandReg(17, 0), 0.U(6.W))

  // rootReg contains the hidden bit, 23 fraction bits, guard, and round.
  // A non-zero final remainder represents every bit below the round bit.
  val remainderZero = !remainderSumReg(0) && (
    (remainderSumReg(RemainderWidth - 2, 0) |
      remainderCarryReg(RemainderWidth - 2, 0)) ===
      (remainderSumReg(RemainderWidth - 1, 1) ^
        remainderCarryReg(RemainderWidth - 1, 1))
  )
  val rootWithSticky = Cat(rootReg, !remainderZero)
  val unroundedSignificand = rootWithSticky(26, 3)
  val guard = rootWithSticky(2)
  val round = rootWithSticky(1)
  val sticky = rootWithSticky(0)
  val inexact = guard || round || sticky

  val useRNE = roundingModeReg === RNE || roundingModeReg > RMM
  val roundIncrement = MuxCase(false.B, Seq(
    useRNE -> (guard && (round || sticky || unroundedSignificand(0))),
    (roundingModeReg === RTZ) -> false.B,
    (roundingModeReg === RDN) -> false.B,
    (roundingModeReg === RUP) -> inexact,
    (roundingModeReg === RMM) -> guard
  ))
  val roundedSignificand = Cat(0.U(1.W), unroundedSignificand) + roundIncrement
  val roundingCarry = roundedSignificand(24)
  val resultSignificand = Mux(
    roundingCarry,
    roundedSignificand(24, 1),
    roundedSignificand(23, 0)
  )

  val biasedExponent = Wire(SInt(13.W))
  biasedExponent := resultExponentReg + 127.S
  val resultExponent = biasedExponent + roundingCarry.asUInt.zext
  val finiteResult = Cat(0.U(1.W), resultExponent.asUInt(7, 0), resultSignificand(22, 0))
  val finiteFlags = Cat(0.U(4.W), inexact)
  io.result := Mux(finiteResponseReg, finiteResult, resultReg)
  io.fflags := Mux(finiteResponseReg, finiteFlags, flagsReg)

  when(io.kill) {
    state := idle
  }.elsewhen(state === idle) {
    when(io.inValid) {
      when(specialCase) {
        resultReg := specialResult
        flagsReg := specialFlags
        finiteResponseReg := false.B
        state := response
      }.elsewhen(!exponent.orR) {
        subnormalFractionReg := fraction
        subnormalShiftReg := PriorityEncoder(Reverse(fraction)) + 1.U
        roundingModeReg := io.rm
        finiteResponseReg := true.B
        state := normalizeSubnormal
      }.otherwise {
        radicandReg := normalInitialRadicandTail
        remainderSumReg := normalInitialRemainder
        remainderCarryReg := 0.U
        rootReg := normalInitialRootDigit
        iterationReg := 0.U
        resultExponentReg := normalInitialResultExponent
        roundingModeReg := io.rm
        finiteResponseReg := true.B
        state := iterate
      }
    }
  }.elsewhen(state === normalizeSubnormal) {
    radicandReg := subnormalInitialRadicandTail
    remainderSumReg := subnormalInitialRemainder
    remainderCarryReg := 0.U
    rootReg := subnormalInitialRootDigit
    iterationReg := 0.U
    resultExponentReg := subnormalInitialResultExponent
    state := iterate
  }.elsewhen(state === iterate) {
    radicandReg := iterationRadicand
    remainderSumReg := iterationRemainderSum
    remainderCarryReg := iterationRemainderCarry
    rootReg := iterationRoot
    when(iterationReg === 7.U) {
      state := response
    }.otherwise {
      iterationReg := iterationReg + 1.U
    }
  }.elsewhen(state === response && io.outReady) {
    state := idle
  }
}
