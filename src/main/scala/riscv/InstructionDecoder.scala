package riscv

import chisel3._
import chisel3.util.{Cat, Fill}

class InstructionDecoder extends Module {
  val io = IO(new Bundle {
    val data_in = Input(UInt(32.W))
    val read_instruction = Input(Bool())

    val imm = Output(UInt(32.W))
    val op = Output(UInt(7.W))
    val rd = Output(UInt(5.W))
    val rs1 = Output(UInt(5.W))
    val rs2 = Output(UInt(5.W))
    val func3 = Output(UInt(3.W))
    val func7 = Output(UInt(7.W))
  })

  val reg = Reg(UInt(32.W))

  when (io.read_instruction) { reg := io.data_in }

  io.op := reg(6, 0)
  io.rd := reg(11, 7)
  io.rs1 := reg(19, 15)
  io.rs2 := reg(24, 20)
  io.func3 := reg(14, 12)
  io.func7 := reg(31, 25)

  val imm_type_i = Wire(Bool())
  val imm_type_s = Wire(Bool())
  val imm_type_b = Wire(Bool())
  val imm_type_u = Wire(Bool())
  val imm_type_j = Wire(Bool())

  // This assumes imm_type_r doesn't matter (no IMM)
  // It is also designed for RV32I only

  imm_type_i := (!reg(2) && !reg(5)) || (reg(2) && !reg(3) && !reg(4))
  imm_type_s := !reg(4) && reg(5) && !reg(6)
  imm_type_b := !reg(2) && reg(6)
  imm_type_u := reg(2) && !reg(6)
  imm_type_j := !reg(3)

  when(imm_type_i) {
    io.imm := Cat(Fill(21, reg(31)), reg(30, 20))
  } .elsewhen(imm_type_s) {
    io.imm := Cat(Fill(21, reg(31)), reg(30, 25), reg(11, 7))
  } .elsewhen(imm_type_b) {
    io.imm := Cat(Fill(20, reg(31)), reg(7), reg(30, 25), reg(11, 8), 0.U(1.W))
  } .elsewhen(imm_type_u) {
    io.imm := Cat(reg(31, 12), 0.U(12.W))
  } .elsewhen(imm_type_j) {
    io.imm := Cat(Fill(12, reg(31)), reg(19, 12), reg(20), reg(30, 25), reg(24, 21), 0.U(1.W))
  }
}
