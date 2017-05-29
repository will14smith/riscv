package riscv.test

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import riscv.Memory

class MemoryTest extends ChiselFlatSpec {
  backends foreach { backend =>
    it should s"unsigned 8-bit in $backend" in {
      Driver(() => new Memory, backend)(c => new UnsignedTest(c, c.size_8, 0xf8, Array((1, 1), (1, 1), (1, 1), (1, 1)))) should be(true)
    }
    it should s"signed 8-bit in $backend" in {
      Driver(() => new Memory, backend)(c => new SignedTest(c, c.size_8, Array(
        (4, 0x78, "h78", 1, 1),
        (4, 0xf8, "hfffffff8", 1, 1)
      ))) should be(true)
    }
    it should s"unsigned 16-bit in $backend" in {
      Driver(() => new Memory, backend)(c => new UnsignedTest(c, c.size_16, 0x76f8, Array((1, 1), (1, 1), (1, 1), (2, 2)))) should be(true)
    }
    it should s"signed 16-bit in $backend" in {
      Driver(() => new Memory, backend)(c => new SignedTest(c, c.size_16, Array(
        (4, 0x78f8, "h78f8", 1, 1),
        (4, 0xf8f8, "hfffff8f8", 1, 1),
        (7, 0x78f8, "h78f8", 2, 2),
        (7, 0xf8f8, "hfffff8f8", 2, 2)
      ))) should be(true)
    }
    it should s"32-bit in $backend" in {
      Driver(() => new Memory, backend)(c => new UnsignedTest(c, c.size_32, 0x23a076f8, Array((1, 1), (2, 2), (2, 2), (2, 2)))) should be(true)
    }
  }

  // cycles format: (Write Cycles, Read Cycles)
  class UnsignedTest(c: Memory, size: UInt, base: Int, cycles: Array[(Int, Int)]) extends PeekPokeTester(c) {
    poke(c.io.size, size)
    poke(c.io.signed, 0)

    for (addr <- 4 until 8) {
      val (expected_write_cycles, expected_read_cycles) = cycles(addr - 4)

      val value = base + addr

      // write
      poke(c.io.data_in, addr)
      poke(c.io.valid_addr, 1)
      poke(c.io.write, 1)

      step(1)

      poke(c.io.valid_addr, 0)
      poke(c.io.data_in, value)

      step(expected_write_cycles)

      expect(c.io.done, 1)

      // read
      poke(c.io.data_in, addr)
      poke(c.io.valid_addr, 1)
      poke(c.io.write, 0)

      step(1)
      poke(c.io.valid_addr, 0)

      step(expected_read_cycles)

      expect(c.io.done, 1)
      expect(c.io.data_out, value)
    }
  }

  // test format: (Address, Value, Expected Value, Write Cycles, Read Cycles
  class SignedTest(c: Memory, size: UInt, tests: Array[(Int, Int, String, Int, Int)]) extends PeekPokeTester(c) {
    poke(c.io.size, size)
    poke(c.io.signed, 1)

    for ((addr, value, expected, expected_write_cycles, expected_read_cycles) <- tests) {
      // write
      poke(c.io.data_in, addr)
      poke(c.io.valid_addr, 1)
      poke(c.io.write, 1)

      step(1)

      poke(c.io.valid_addr, 0)
      poke(c.io.data_in, value)

      step(expected_write_cycles)

      expect(c.io.done, 1)

      // read
      poke(c.io.data_in, addr)
      poke(c.io.valid_addr, 1)
      poke(c.io.write, 0)

      step(1)
      poke(c.io.valid_addr, 0)

      step(expected_read_cycles)

      expect(c.io.done, 1)
      expect(c.io.data_out, expected.U(32.W))
    }
  }

}
