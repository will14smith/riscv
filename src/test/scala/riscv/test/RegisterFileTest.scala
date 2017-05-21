package riscv.test

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import riscv.RegisterFile

class RegisterFileTest extends ChiselFlatSpec {
  backends foreach { backend =>
     it should s"correctly read 0/ignore writes to x0 in $backend" in {
      Driver(() => new RegisterFile, backend)(c => new ReadWrite0Test(c)) should be(true)
    }
    it should s"correctly read/write from x[1-31] in $backend" in {
      Driver(() => new RegisterFile, backend)(c => new ReadWriteTest(c)) should be(true)
    }

    class ReadWrite0Test(c: RegisterFile) extends PeekPokeTester(c) {
      poke(c.io.data_in, "hf0f0f0f0".U)
      poke(c.io.address, 0)
      poke(c.io.write, 0)

      step(1)

      // test initial value = 0
      expect(c.io.data_out, 0)
      poke(c.io.write, 1)

      step(1)

      // test write had no effect
      expect(c.io.data_out, 0)
    }

    class ReadWriteTest(c: RegisterFile) extends PeekPokeTester(c) {
      val pattern1 = "hf0f0f0f0".U
      val pattern2 = "h0f0f0f0f".U

      poke(c.io.data_in, pattern1)

      for (i <- 1 until 31) {
        poke(c.io.address, i)

        poke(c.io.data_in, 0)
        poke(c.io.write, 1)

        step(1)

        // check clearing works
        expect(c.io.data_out, 0)

        poke(c.io.data_in, pattern1)
        poke(c.io.write, 1)

        step(1)

        // test writing 1/2 of the bits
        expect(c.io.data_out, pattern1)

        poke(c.io.data_in, pattern2)
        poke(c.io.write, 1)

        step(1)

        // test writing other 1/2 of the bits
        expect(c.io.data_out, pattern2)

        poke(c.io.write, 0)

        step(1)

        // test previous value holds
        expect(c.io.data_out, pattern2)
      }
    }
  }
}
