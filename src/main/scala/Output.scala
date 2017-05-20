object Output extends App {
  chisel3.Driver.execute(args, () => new riscv.InstructionDecoder)
}
