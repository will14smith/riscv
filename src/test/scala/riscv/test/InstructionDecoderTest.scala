package riscv.test

import chisel3.core.UInt
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import riscv.InstructionDecoder

class InstructionDecoderTest extends ChiselFlatSpec {
  backends foreach { backend =>
    it should s"return op in $backend" in {
      Driver(() => new InstructionDecoder, backend)(c => new RangeTest(c, 6, 0, c.io.op)) should be(true)
    }
    it should s"return rd in $backend" in {
      Driver(() => new InstructionDecoder, backend)(c => new RangeTest(c, 11, 7, c.io.rd)) should be(true)
    }
    it should s"return rs1 in $backend" in {
      Driver(() => new InstructionDecoder, backend)(c => new RangeTest(c, 19, 15, c.io.rs1)) should be(true)
    }
    it should s"return rs2 in $backend" in {
      Driver(() => new InstructionDecoder, backend)(c => new RangeTest(c, 24, 20, c.io.rs2)) should be(true)
    }
    it should s"return func3 in $backend" in {
      Driver(() => new InstructionDecoder, backend)(c => new RangeTest(c, 14, 12, c.io.func3)) should be(true)
    }
    it should s"return func7 in $backend" in {
      Driver(() => new InstructionDecoder, backend)(c => new RangeTest(c, 31, 25, c.io.func7)) should be(true)
    }

    it should s"return I-type imm in $backend" in {
      //Driver(() => new InstructionDecoder, backend)(c => new NameTest(c)) should be(true)
    }
    it should s"return S-type imm in $backend" in {
      //Driver(() => new InstructionDecoder, backend)(c => new NameTest(c)) should be(true)
    }
    it should s"return B-type imm in $backend" in {
      //Driver(() => new InstructionDecoder, backend)(c => new NameTest(c)) should be(true)
    }
    it should s"return U-type imm in $backend" in {
      //Driver(() => new InstructionDecoder, backend)(c => new NameTest(c)) should be(true)
    }
    it should s"return J-type imm in $backend" in {
      //Driver(() => new InstructionDecoder, backend)(c => new NameTest(c)) should be(true)
    }
  }

  class RangeTest(c: InstructionDecoder, upper: Int, lower: Int, port: UInt) extends PeekPokeTester(c)
  {
    poke(c.io.data_in, 0)
    poke(c.io.read_instruction, 1)
    step(1)

    expect(port, 0)

    for (i <- 0 until 31) {
      val data = 1 << i
      val expected = if (i >= lower && i <= upper) (1 << (i - lower)) else 0

      poke(c.io.data_in, data)
      poke(c.io.read_instruction, 1)
      step(1)

      poke(c.io.read_instruction, 0)
      expect(port, expected, s"at bit ${i}")
      step(1)

      expect(port, expected, s"at bit ${i} (persist)")
    }
  }
}
