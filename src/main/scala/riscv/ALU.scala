package riscv

import chisel3._
import chisel3.core.Data
import chisel3.util._

class ALU extends Module {
  val io = IO(new Bundle {
    val data_in = Input(UInt(32.W))
    val op = Input(UInt(4.W))

    val read_a = Input(Bool())
    val read_b = Input(Bool())

    val data_out = Output(UInt(32.W))
  })

  val reg_a = Reg(UInt(32.W))
  val reg_b = Reg(UInt(32.W))

  when(io.read_a) {
    reg_a := io.data_in
  }
  when(io.read_b) {
    reg_b := io.data_in
  }

  val op_add :: op_signed_eq :: op_signed_ne :: op_signed_lt :: op_signed_gte :: op_and :: op_or :: op_xor :: op_lsl :: op_lsr :: op_asr :: op_unsigned_gte :: op_unsigned_eq :: op_unsigned_ne :: op_unsigned_lt :: op_sub :: Nil = Enum(16)

  val True = 1.U(32.W)
  val False = 0.U(32.W)

  private val signed_a = reg_a.asSInt
  private val signed_b = reg_b.asSInt()

  private val shift_amt = reg_b(4, 0)

  private val eq = signed_a === signed_b
  private val signed_lt = signed_a < signed_b
  private val unsigned_lt = reg_a < reg_b

  io.data_out :=
    muxTree(io.op, Array(
      // 00000 - op_add
      (signed_a + signed_b).asUInt,
      // 00001 - op_signed_eq
      toBoolValue(eq),
      // 00010 - op_signed_ne
      toBoolValue(!eq),
      // 00011 - op_signed_lt
      toBoolValue(signed_lt),
      // 00100 - op_signed_gte
      toBoolValue(!signed_lt),
      // 00101 - op_and
      reg_a & reg_b,
      // 00110 - op_or
      reg_a | reg_b,
      // 00111 - op_xor
      reg_a ^ reg_b,
      // 1000 - op_lsl
      reg_a << shift_amt,
      // 1001 - op_lsr
      reg_a >> shift_amt,
      // 1010 - op_asr
      (signed_a >> shift_amt).asUInt,
      // 1011 - op_unsigned_gte
      toBoolValue(!unsigned_lt),
      // 1100 - op_unsigned_eq
      toBoolValue(eq),
      // 1101 - op_unsigned_ne
      toBoolValue(!eq),
      // 1110 - op_unsigned_lt
      toBoolValue(unsigned_lt),
      // 1111 - op_sub
      (signed_a - signed_b).asUInt
    ))

  private def toBoolValue(c: Bool) = Mux(c, True, False)

  private def muxTree(c: UInt, v: Array[Data]): Data = {
    val len = v.length

    if (len == 1) {
      v(0)
    } else {
      val idx = 30 - Integer.numberOfLeadingZeros(len)

      val cond = c(idx)

      val v1 = v.slice(0, len / 2)
      val v2 = v.slice(len / 2, len)

      Mux(cond, muxTree(c, v2), muxTree(c, v1))
    }
  }
}
