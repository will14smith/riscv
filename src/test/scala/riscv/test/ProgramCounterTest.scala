package riscv.test

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import riscv.ProgramCounter

class ProgramCounterTest extends ChiselFlatSpec {
  backends foreach { backend =>
    it should s"correctly write values in $backend" in {
      Driver(() => new ProgramCounter, backend)(c => new WriteTest(c)) should be(true)
    }
    it should s"correctly increment in $backend" in {
      Driver(() => new ProgramCounter, backend)(c => new IncTest(c)) should be(true)
    }
  }

  class WriteTest(c: ProgramCounter) extends PeekPokeTester(c) {
    poke(c.io.inc, 0)

    for (_ <- 0 until 10) {
      val addr = rnd.nextInt(0x7fffffff)

      poke(c.io.data_in, addr)
      poke(c.io.write, 1)

      step(1)
      expect(c.io.data_out, addr)
      poke(c.io.write, 0)

      step(1)
      expect(c.io.data_out, addr)
    }
  }

  class IncTest(c: ProgramCounter) extends PeekPokeTester(c) {
    var addr = rnd.nextInt(0x7fffffff)

    poke(c.io.data_in, addr)
    poke(c.io.write, 1)
    poke(c.io.inc, 0)
    step(1)
    poke(c.io.write, 0)

    for (_ <- 0 until 10) {
      addr = addr + 4

      poke(c.io.inc, 1)

      step(1)
      expect(c.io.data_out, addr)
      poke(c.io.inc, 0)

      step(1)
      expect(c.io.data_out, addr)
    }
  }
}
