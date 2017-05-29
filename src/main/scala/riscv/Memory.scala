package riscv

import chisel3._
import chisel3.core.Data
import chisel3.util._

class Memory extends Module {
  val io = IO(new Bundle {
    val data_in = Input(UInt(32.W))

    val valid_addr = Input(Bool())
    val write = Input(Bool())
    val size = Input(UInt(2.W))
    val signed = Input(Bool())

    val data_out = Output(UInt(32.W))
    val done = Output(Bool())
  })

  val size_8 :: size_16 :: size_24_unused :: size_32 :: Nil = Enum(4)

  val state_initial :: state_read_1 :: state_read_2 :: state_write_1 :: state_write_2 :: Nil = Enum(5)
  val state = RegInit(state_initial)

  val addr = RegInit(0.U(32.W))
  val tmp = RegInit(0.U(32.W))
  val out = RegInit(0.U(32.W))

  io.data_out := out

  val step_addr = Mux(state === state_read_2 || state === state_write_2, addr + 4.U, addr)

  val mem = Mem(16 * 1024, UInt(32.W))

  private val r = mem(step_addr(31, 2))

  private val r0 = r(7, 0)
  private val r1 = r(15, 8)
  private val r2 = r(23, 16)
  private val r3 = r(31, 24)

  private val w0 = io.data_in(7, 0)
  private val w1 = io.data_in(15, 8)
  private val w2 = io.data_in(23, 16)
  private val w3 = io.data_in(31, 24)

  val done = RegInit(0.U(1.W))
  io.done := done

  switch(state) {
    is(state_initial) {
      done := 0.U

      when(io.valid_addr) {
        addr := io.data_in
        state := Mux(io.write, state_write_1, state_read_1)
      }
    }

    is(state_read_1) {
      switch(io.size) {
        is(size_8) {
          val l_tmp = lower_addr_mux(r0, r1, r2, r3)
          out := Mux(io.signed,
            Cat(Fill(24, l_tmp(7)), l_tmp),
            Cat(Fill(24, 0.U), l_tmp))

          goto_done()
        }
        is(size_16) {
          val l_tmp = lower_addr_mux(
            Cat(r1, r0),
            Cat(r2, r1),
            Cat(r3, r2),
            r3
          )

          when(addr(1, 0) =/= 3.U) {
            out := Mux(io.signed,
              Cat(Fill(16, l_tmp(15)), l_tmp),
              Cat(Fill(16, 0.U), l_tmp))

            goto_done()
          }.otherwise {
            tmp := l_tmp
            state := state_read_2
          }
        }
        is(size_32) {
          when(addr(1, 0) === 0.U) {
            out := r
            goto_done()
          }.otherwise {
            tmp := lower_addr_mux(
              0.U,
              Cat(r3, r2, r1),
              Cat(r3, r2),
              r3
            )
            state := state_read_2
          }
        }
      }
    }


    is(state_read_2) {
      switch(io.size) {
        is(size_16) {
          out := Mux(io.signed,
            Cat(Fill(16, r0(7)), r0, tmp(7, 0)),
            Cat(Fill(16, 0.U), r0, tmp(7, 0)))

          goto_done()
        }
        is(size_32) {
          out := lower_addr_mux(
            0.U,
            Cat(r0, tmp(23, 0)),
            Cat(r1, r0, tmp(15, 0)),
            Cat(r2, r1, r0, tmp(7, 0)))

          goto_done()
        }
      }
    }

    is(state_write_1) {
      switch(io.size) {
        is(size_8) {
          val l_tmp = lower_addr_mux(
            Cat(r3, r2, r1, w0),
            Cat(r3, r2, w0, r0),
            Cat(r3, w0, r1, r0),
            Cat(w0, r2, r1, r0))

          mem(step_addr(31, 2)) := l_tmp

          goto_done()
        }
        is(size_16) {
          when(addr(1, 0) =/= 3.U) {
            val l_tmp = lower_addr_mux(
              Cat(r3, r2, w1, w0),
              Cat(r3, w1, w0, r0),
              Cat(w1, w0, r1, r0),
              0.U)

            mem(step_addr(31, 2)) := l_tmp

            goto_done()
          }.otherwise {
            val l_tmp = Cat(w0, r2, r1, r0)

            mem(step_addr(31, 2)) := l_tmp

            state := state_write_2
          }
        }
        is(size_32) {
          when(addr(1, 0) === 0.U) {
            // mem.io.data_in := io.data_in
            // mem.io.write := 1.U
            mem(step_addr(31, 2)) := io.data_in

            goto_done()
          }.otherwise {
            val w = lower_addr_mux(
              0.U,
              Cat(w2, w1, w0, r0),
              Cat(w1, w0, r1, r0),
              Cat(w0, r2, r1, r0)
            )

            mem(step_addr(31, 2)) := w

            state := state_write_2
          }
        }
      }
    }

    is(state_write_2) {
      switch(io.size) {
        is(size_16) {
          val l_tmp = Cat(r3, r2, r1, w1)

          mem(step_addr(31, 2)) := l_tmp

          goto_done()
        }
        is(size_32) {
          val w = lower_addr_mux(
            0.U,
            Cat(r3, r2, r1, w3),
            Cat(r3, r2, w3, w2),
            Cat(r3, w3, w2, w1)
          )

          mem(step_addr(31, 2)) := w

          goto_done()
        }
      }
    }
  }

  private def lower_addr_mux[A <: Data](v0: A, v1: A, v2: A, v3: A): A = {
    Mux(step_addr(1), Mux(step_addr(0), v3, v2), Mux(step_addr(0), v1, v0))
  }

  private def goto_done(): Unit = {
    state := state_initial
    done := 1.U
  }
}
