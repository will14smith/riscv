package riscv.test

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import riscv.ALU

class ALUTest extends ChiselFlatSpec {
  private val add_tests = Array(
    (0, 0, 0),
    (0, 1, 1),
    (1, 0, 1),
    (1, 1, 2),
    (-1, 0, -1),
    (0, -1, -1),
    (-1, 1, 0),
    (1, -1, 0),
    (-1, -1, -2),
    // overflow
    (0x80000001, 0x80000001, 2)
  )

  private val eq_tests = Array(
    (0, 0, 1),
    (0, 1, 0),
    (1, 0, 0),
    (-1, -1, 1),
    (-1, 1, 0),
    (1, -1, 0)
  )
  private val ne_tests = eq_tests map { case (a, b, r) => (a, b, if (r == 1) 0 else 1) }

  private val s_lt_tests = Array(
    (0, 0, 0),
    (0, 1, 1),
    (1, 0, 0),
    (-1, -1, 0),
    (-1, 1, 1),
    (1, -1, 0)
  )
  private val s_gte_tests = s_lt_tests map { case (a, b, r) => (a, b, if (r == 1) 0 else 1) }

  private val and_tests = Array(
    (0, 0, 0),
    (0, 1, 0),
    (1, 0, 0),
    (1, 1, 1),
    (-1, 0, 0),
    (0, -1, 0),
    (-1, -1, -1),
    (1, -1, 1),
    (-1, 1, 1)
  )
  private val or_tests = Array(
    (0, 0, 0),
    (0, 1, 1),
    (1, 0, 1),
    (1, 1, 1),
    (-1, 0, -1),
    (0, -1, -1),
    (-1, -1, -1),
    (1, -1, -1),
    (-1, 1, -1)
  )
  private val xor_tests = Array(
    (0, 0, 0),
    (0, 1, 1),
    (1, 0, 1),
    (1, 1, 0),
    (-1, 0, -1),
    (0, -1, -1),
    (-1, -1, 0),
    (1, -1, -2),
    (-1, 1, -2)
  )

  private val lsl_tests = Array(
    (0, 0, 0),
    (0, 1, 0),
    (1, 1, 2),
    (2, 1, 4),
    (1, 2, 4),
    (1, 3, 8),
    (3, 1, 6),
    (-1, 1, -2),
    (-1, 2, -4),
    // only low 5 bits are used
    (-1, 32, -1)
  )

  private val lsr_tests = Array(
    (0, 0, 0),
    (0, 1, 0),    //    it should s"return usigned gte correct in $backend" in {
    //      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_unsigned_gte, u_gte_tests)) should be(true)
    //    }
    (1, 1, 0),
    (2, 1, 1),
    (6, 1, 3),
    (6, 2, 1),
    (7, 1, 3),
    (7, 2, 1),
    (-1, 1, 0x7fffffff),
    (-1, 30, 3),
    // ony low 5 bits are used
    (-1, 32, -1)
  )

  private val asr_tests = Array(
    (0, 0, 0),
    (0, 1, 0),
    (1, 1, 0),
    (2, 1, 1),
    (6, 1, 3),
    (6, 2, 1),
    (7, 1, 3),
    (7, 2, 1),
    (-1, 1, 0xffffffff),
    (-1, 30, -1),
    (-7, 1, -4),
    (-7, 2, -2),
    // ony low 5 bits are used
    (-1, 32, -1)
  )

  private val u_lt_tests = Array(
    (0, 0, 0),
    (0, 1, 1),
    (1, 0, 0),
    (-1, -1, 0),
    (-1, 1, 0),
    (1, -1, 1)
  )
  private val u_gte_tests = u_lt_tests map { case (a, b, r) => (a, b, if (r == 1) 0 else 1) }

  private val sub_tests = Array(
    (0, 0, 0),
    (1, 0, 1),
    (0, 1, -1),
    (2, 1, 1),
    (1, 2, -1),
    (8, 3, 5),
    (-8, 3, -11),
    (8, -3, 11),
    (-8, -3, -5),
    (3, 8, -5),
    (-3, 8, -11),
    (3, -8, 11),
    (-3, -8, 5)
  )

  backends foreach { backend =>
    it should s"return add correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_add, add_tests)) should be(true)
    }
    it should s"return signed eq correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_signed_eq, eq_tests)) should be(true)
    }
    it should s"return signed ne correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_signed_ne, ne_tests)) should be(true)
    }
    it should s"return signed lt correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_signed_lt, s_lt_tests)) should be(true)
    }
    it should s"return signed gte correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_signed_gte, s_gte_tests)) should be(true)
    }
    it should s"return and correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_and, and_tests)) should be(true)
    }
    it should s"return or correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_or, or_tests)) should be(true)
    }
    it should s"return xor correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_xor, xor_tests)) should be(true)
    }
    it should s"return lsl correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_lsl, lsl_tests)) should be(true)
    }
    it should s"return lsr correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_lsr, lsr_tests)) should be(true)
    }
    it should s"return asr correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_asr, asr_tests)) should be(true)
    }
    it should s"return usigned gte correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_unsigned_gte, u_gte_tests)) should be(true)
    }
    it should s"return unsigned eq correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_unsigned_eq, eq_tests)) should be(true)
    }
    it should s"return unsigned ne correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_unsigned_ne, ne_tests)) should be(true)
    }
    it should s"return usigned lt correct in $backend" in {
      Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_unsigned_lt, u_lt_tests)) should be(true)
    }
        it should s"return sub correct in $backend" in {
          Driver(() => new ALU, backend)(c => new ALUTester(c, c.op_sub, sub_tests)) should be(true)
        }
  }

  class ALUTester(c: ALU, op: UInt, tests: Array[(Int, Int, Int)]) extends PeekPokeTester(c) {
    poke(c.io.data_in, 0)
    poke(c.io.op, op)

    poke(c.io.read_a, 0)
    poke(c.io.read_b, 0)

    for ((a, b, expected) <- tests) {
      poke(c.io.data_in, toUInt(a))
      poke(c.io.read_a, 1)
      step(1)
      poke(c.io.read_a, 0)


      poke(c.io.data_in, toUInt(b))
      poke(c.io.read_b, 1)
      step(1)
      poke(c.io.read_b, 0)

      expect(c.io.data_out, toUInt(expected), "op = " + op.intValue() + " a = " + a + " b = " + b)
    }

    def toUInt(a: Int): UInt = {
      // This is to allow negative values to work
      ("h" + a.toHexString).U(32.W)
    }
  }

}
