package riscv

import chisel3._

class RegisterFile extends Module {
  val register_count = 32

  val io = IO(new Bundle {
    val data_in = Input(UInt(32.W))
    val address = Input(UInt(5.W))
    val write = Input(Bool())

    val data_out = Output(UInt(32.W))
  })

  val registers = Mem(register_count, Bits(32.W))

  // x0 is read-only zero
  when(io.address =/= 0.U) {
    io.data_out := registers(io.address)

    when(io.write) {
      registers(io.address) := io.data_in
    }
  } .otherwise {
    io.data_out := 0.U
  }
}
