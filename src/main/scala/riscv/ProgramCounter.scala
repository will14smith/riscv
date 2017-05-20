package riscv

import chisel3._

class ProgramCounter extends Module {
  val stride = 4.U

  val io = IO(new Bundle{
    val data_in = Input(UInt(32.W))
    val inc = Input(Bool())
    val write = Input(Bool())

    val data_out = Output(UInt(32.W))
  })

  val addr = Reg(UInt(32.W))
  val next = Wire(UInt(32.W))

  next := Mux(io.inc, addr + stride, io.data_in)
  when (io.inc || io.write) { addr := next }

  io.data_out := addr
}
