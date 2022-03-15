package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

class CNNActIO extends Bundle {
  val act_valid = Input(Bool())
  val data_main = Input(UInt(64.W))
  val data_zero = Input(UInt(64.W))
  val data_vwidth = Input(UInt(4.W))

  val act_res = Output(UInt(64.W))
  val act_ok = Output(Bool())
}

class CNNActSub(group: Int, width: Int) extends Module {
  val io = IO(new Bundle {
    val in_data = Input(Vec(group, UInt(width.W)))
    val in_zero = Input(Vec(group, UInt(width.W)))
    val res = Output(Vec(group, UInt(width.W)))
  })
  
  for(i <- 0 until group) {
    io.res(i) := Mux(io.in_data(i) >= io.in_zero(i), io.in_data(i), io.in_zero(i))
  }
}

class CNNAct extends NutCoreModule {
  val io = IO(new CNNActIO)

  val act_mdu_4_16 = Module(new CNNActSub(4, 16))
  for(i <- 0 until 4) {
    act_mdu_4_16.io.in_data(i) := io.data_main(i*16+15, i*16)
    act_mdu_4_16.io.in_zero(i) := io.data_zero(i*16+15, i*16)
  }

  val act_mdu_8_8 = Module(new CNNActSub(8, 8))
  for(i <- 0 until 8) {
    act_mdu_8_8.io.in_data(i) := io.data_main(i*8+7, i*8)
    act_mdu_8_8.io.in_zero(i) := io.data_zero(i*8+7, i*8)
  }

  val act_mdu_16_4 = Module(new CNNActSub(16, 4))
  for(i <- 0 until 16) {
    act_mdu_16_4.io.in_data(i) := io.data_main(i*4+3, i*4)
    act_mdu_16_4.io.in_zero(i) := io.data_zero(i*4+3, i*4)
  }

  val act_mdu_32_2 = Module(new CNNActSub(32, 2))
  for(i <- 0 until 32) {
    act_mdu_32_2.io.in_data(i) := io.data_main(i*2+1, i*2)
    act_mdu_32_2.io.in_zero(i) := io.data_zero(i*2+1, i*2)
  }

  io.act_res := LookupTreeDefault(io.data_vwidth, 0.U(64.W), List(
    "b1000".U  -> act_mdu_4_16.io.res.reduce{ (a, b) => Cat(b, a) },
    "b0100".U  -> act_mdu_8_8.io.res.reduce{ (a, b) => Cat(b, a) },
    "b0010".U  -> act_mdu_16_4.io.res.reduce{ (a, b) => Cat(b, a) },
    "b0001".U  -> act_mdu_32_2.io.res.reduce{ (a, b) => Cat(b, a) }
  ))
  io.act_ok := true.B
}