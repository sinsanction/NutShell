package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

object MAX2 {
  def apply(a: UInt, b: UInt) = {
    Mux(a>=b, a, b)
  }
}

class CNN_VECTOR_REGFILE_IO extends Bundle {
  val vaddr = Input(UInt(5.W))
  val vtag = Input(UInt(3.W))
  val vop = Input(UInt(3.W))   //100: load-v.d  010: load-v.p  001: load-v.width
  val k = Input(UInt(4.W))
  val vwen = Input(Bool())
  val load_data = Input(UInt(16*5.W))
  val load_vwidth = Input(UInt(64.W))

  val data_main = Output(Vec(25, UInt(16.W)))
  val data_kernel = Output(Vec(25, UInt(8.W)))
  val data_main_vwidth = Output(UInt(4.W))
  val data_kernel_vwidth = Output(UInt(4.W))
  val act_vwidth = Output(UInt(4.W))
}

class CNN_VECTOR_REGFILE extends NutCoreModule {
  val io = IO(new CNN_VECTOR_REGFILE_IO)

  //vector reg
  val vrf_main = Mem(5, UInt(16*5.W))
  val vrf_kernel = Mem(5, UInt(8*5.W))
  
  //local vwidth reg
  val vwidth_main = Mem(5, UInt(4.W))
  val vwidth_kernel = Mem(5, UInt(4.W))

  //vwidth reg
  val global_vwidth = Mem(8, UInt(8.W))
  val select_vw = global_vwidth(io.vtag)
  io.act_vwidth := global_vwidth(0)(7, 4)
  when (io.vwen && io.vop === 1.U) {
      global_vwidth(0) := io.load_vwidth(7, 0)
      global_vwidth(1) := io.load_vwidth(15, 8)
      global_vwidth(2) := io.load_vwidth(23, 16)
      global_vwidth(3) := io.load_vwidth(31, 24)
      global_vwidth(4) := io.load_vwidth(39, 32)
      global_vwidth(5) := io.load_vwidth(47, 40)
      global_vwidth(6) := io.load_vwidth(55, 48)
      global_vwidth(7) := io.load_vwidth(63, 56)
  }

  // vrf write
  val vtype = io.vaddr(4)
  val vindex = io.vaddr(2, 0)

  when (io.vwen && io.vop === 4.U) {
      when (vtype === 0.U) {
          vrf_main(vindex) := io.load_data
          vwidth_main(vindex) := select_vw(7, 4)
      }.otherwise {
          vrf_kernel(vindex) := io.load_data(39, 0)
          vwidth_kernel(vindex) := select_vw(3, 0)
      }
  }.elsewhen (io.vwen && io.vop === 2.U) {
      when (io.k === 1.U) {
          vrf_main(0) := io.load_data
          vwidth_main(0) := select_vw(7, 4)
      }
      .elsewhen (io.k === 2.U) {
          vrf_main(0) := vrf_main(1)
          vrf_main(1) := io.load_data
          vwidth_main(0) := vwidth_main(1)
          vwidth_main(1) := select_vw(7, 4)
      }
      .elsewhen (io.k === 3.U) {
          vrf_main(0) := vrf_main(1)
          vrf_main(1) := vrf_main(2)
          vrf_main(2) := io.load_data
          vwidth_main(0) := vwidth_main(1)
          vwidth_main(1) := vwidth_main(2)
          vwidth_main(2) := select_vw(7, 4)
      }
      .elsewhen (io.k === 4.U) {
          vrf_main(0) := vrf_main(1)
          vrf_main(1) := vrf_main(2)
          vrf_main(2) := vrf_main(3)
          vrf_main(3) := io.load_data
          vwidth_main(0) := vwidth_main(1)
          vwidth_main(1) := vwidth_main(2)
          vwidth_main(2) := vwidth_main(3)
          vwidth_main(3) := select_vw(7, 4)
      }
      .elsewhen (io.k === 5.U) {
          vrf_main(0) := vrf_main(1)
          vrf_main(1) := vrf_main(2)
          vrf_main(2) := vrf_main(3)
          vrf_main(3) := vrf_main(4)
          vrf_main(4) := io.load_data
          vwidth_main(0) := vwidth_main(1)
          vwidth_main(1) := vwidth_main(2)
          vwidth_main(2) := vwidth_main(3)
          vwidth_main(3) := vwidth_main(4)
          vwidth_main(4) := select_vw(7, 4)
      }
  }

  // vrf read
  val vrf_main_0 = vrf_main(0)
  val vrf_main_1 = Mux(io.k >= 2.U, vrf_main(1), 0.U(16*5.W))
  val vrf_main_2 = Mux(io.k >= 3.U, vrf_main(2), 0.U(16*5.W))
  val vrf_main_3 = Mux(io.k >= 4.U, vrf_main(3), 0.U(16*5.W))
  val vrf_main_4 = Mux(io.k >= 5.U, vrf_main(4), 0.U(16*5.W))

  val vrf_kernel_0 = vrf_kernel(0)
  val vrf_kernel_1 = Mux(io.k >= 2.U, vrf_kernel(1), 0.U(8*5.W))
  val vrf_kernel_2 = Mux(io.k >= 3.U, vrf_kernel(2), 0.U(8*5.W))
  val vrf_kernel_3 = Mux(io.k >= 4.U, vrf_kernel(3), 0.U(8*5.W))
  val vrf_kernel_4 = Mux(io.k >= 5.U, vrf_kernel(4), 0.U(8*5.W))

  io.data_main(0) := vrf_main_0(15, 0)
  io.data_main(1) := vrf_main_1(15, 0)
  io.data_main(2) := vrf_main_2(15, 0)
  io.data_main(3) := vrf_main_3(15, 0)
  io.data_main(4) := vrf_main_4(15, 0)

  io.data_main(5) := vrf_main_0(31, 16)
  io.data_main(6) := vrf_main_1(31, 16)
  io.data_main(7) := vrf_main_2(31, 16)
  io.data_main(8) := vrf_main_3(31, 16)
  io.data_main(9) := vrf_main_4(31, 16)

  io.data_main(10) := vrf_main_0(47, 32)
  io.data_main(11) := vrf_main_1(47, 32)
  io.data_main(12) := vrf_main_2(47, 32)
  io.data_main(13) := vrf_main_3(47, 32)
  io.data_main(14) := vrf_main_4(47, 32)

  io.data_main(15) := vrf_main_0(63, 48)
  io.data_main(16) := vrf_main_1(63, 48)
  io.data_main(17) := vrf_main_2(63, 48)
  io.data_main(18) := vrf_main_3(63, 48)
  io.data_main(19) := vrf_main_4(63, 48)

  io.data_main(20) := vrf_main_0(79, 64)
  io.data_main(21) := vrf_main_1(79, 64)
  io.data_main(22) := vrf_main_2(79, 64)
  io.data_main(23) := vrf_main_3(79, 64)
  io.data_main(24) := vrf_main_4(79, 64)

  for(i <- 0 until 5) {
      io.data_kernel(i) := vrf_kernel_0(8*i+7, 8*i)
      io.data_kernel(i+5) := vrf_kernel_1(8*i+7, 8*i)
      io.data_kernel(i+10) := vrf_kernel_2(8*i+7, 8*i)
      io.data_kernel(i+15) := vrf_kernel_3(8*i+7, 8*i)
      io.data_kernel(i+20) := vrf_kernel_4(8*i+7, 8*i)
  }

  // vwidth read
  val main_vw_max = Wire(Vec(5, UInt(4.W)))
  for(i <- 0 until 5) {
      if(i == 0) main_vw_max(i) := vwidth_main(0)
      else       main_vw_max(i) := MAX2(vwidth_main(i), main_vw_max(i-1))
  }
  val kernel_vw_max = Wire(Vec(5, UInt(4.W)))
  for(i <- 0 until 5) {
      if(i == 0) kernel_vw_max(i) := vwidth_kernel(0)
      else       kernel_vw_max(i) := MAX2(vwidth_kernel(i), kernel_vw_max(i-1))
  }

  io.data_main_vwidth := 0.U
  io.data_kernel_vwidth := 0.U
  when (io.k === 1.U) {
      io.data_main_vwidth := main_vw_max(0)
      io.data_kernel_vwidth := kernel_vw_max(0)
  }
  .elsewhen (io.k === 2.U) {
      io.data_main_vwidth := main_vw_max(1)
      io.data_kernel_vwidth := kernel_vw_max(1)
  }
  .elsewhen (io.k === 3.U) {
      io.data_main_vwidth := main_vw_max(2)
      io.data_kernel_vwidth := kernel_vw_max(2)
  }
  .elsewhen (io.k === 4.U) {
      io.data_main_vwidth := main_vw_max(3)
      io.data_kernel_vwidth := kernel_vw_max(3)
  }
  .elsewhen (io.k === 5.U) {
      io.data_main_vwidth := main_vw_max(4)
      io.data_kernel_vwidth := kernel_vw_max(4)
  }
}
