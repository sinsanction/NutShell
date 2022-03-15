package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

object Max2 {
  def apply(a: UInt, b: UInt) = {
    Mux(a>=b, a, b)
  }
}

class CNNVectorRegFileIO extends Bundle {
  val vaddr = Input(UInt(5.W))
  val vtag = Input(UInt(3.W))
  val vop = Input(UInt(3.W))   //100: load-v.d  010: load-v.p  001: load-v.width
  val k = Input(UInt(4.W))
  val vwen = Input(Bool())
  val load_data = Input(UInt((16*5).W))
  val load_kernel = Input(UInt((8*5).W))
  val load_vwidth = Input(UInt(64.W))

  val data_main = Output(Vec(25, UInt(16.W)))
  val data_kernel = Output(Vec(25, UInt(8.W)))
  val data_main_vwidth = Output(UInt(4.W))
  val data_kernel_vwidth = Output(UInt(4.W))
  val select_vwidth = Output(UInt(8.W))
  val act_vwidth = Output(UInt(4.W))
}

class CNNVectorRegFile extends NutCoreModule {
  val io = IO(new CNNVectorRegFileIO)

  //vector reg
  val vrf_main = Mem(5, UInt((16*5).W))
  val vrf_kernel = Mem(5, UInt((8*5).W))
  
  //local vwidth reg
  val vwidth_main = Mem(5, UInt(4.W))
  val vwidth_kernel = Mem(5, UInt(4.W))

  //vwidth reg
  val global_vwidth = Mem(8, UInt(8.W))
  val select_vw = global_vwidth(io.vtag)
  io.select_vwidth := select_vw
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
          vrf_kernel(vindex) := io.load_kernel
          vwidth_kernel(vindex) := select_vw(3, 0)
      }
  }.elsewhen (io.vwen && io.vop === 2.U) {
      when (io.k >= 1.U) {
          vrf_main(0) := Mux(io.k === 1.U, io.load_data, vrf_main(1))
          vwidth_main(0) := Mux(io.k === 1.U, select_vw(7, 4), vwidth_main(1))
      }
      when (io.k >= 2.U) {
          vrf_main(1) := Mux(io.k === 2.U, io.load_data, vrf_main(2))
          vwidth_main(1) := Mux(io.k === 2.U, select_vw(7, 4), vwidth_main(2))
      }
      when (io.k >= 3.U) {
          vrf_main(2) := Mux(io.k === 3.U, io.load_data, vrf_main(3))
          vwidth_main(2) := Mux(io.k === 3.U, select_vw(7, 4), vwidth_main(3))
      }
      when (io.k >= 4.U) {
          vrf_main(3) := Mux(io.k === 4.U, io.load_data, vrf_main(4))
          vwidth_main(3) := Mux(io.k === 4.U, select_vw(7, 4), vwidth_main(4))
      }
      when (io.k >= 5.U) {
          vrf_main(4) := io.load_data
          vwidth_main(4) := select_vw(7, 4)
      }
  }

  // vrf read
  val vrf_main_data = Wire(Vec(5, UInt((16*5).W)))
  val vrf_kernel_data = Wire(Vec(5, UInt((8*5).W)))
  for (i <- 0 until 5) {
      vrf_main_data(i)   := Mux(io.k >= (i+1).U, vrf_main(i), 0.U((16*5).W))
      vrf_kernel_data(i) := Mux(io.k >= (i+1).U, vrf_kernel(i), 0.U((8*5).W))
  }

  for (i <- 0 until 5) {
      for (j <- 0 until 5) {
          io.data_main(5*i+j)   := vrf_main_data(j)(16*i+15, 16*i)
          io.data_kernel(5*i+j) := vrf_kernel_data(i)(8*j+7, 8*j)
      }
  }

  // vwidth read
  val main_vw_max = Wire(Vec(5, UInt(4.W)))
  for(i <- 0 until 5) {
      if(i == 0) main_vw_max(i) := vwidth_main(0)
      else       main_vw_max(i) := Max2(vwidth_main(i), main_vw_max(i-1))
  }
  val kernel_vw_max = Wire(Vec(5, UInt(4.W)))
  for(i <- 0 until 5) {
      if(i == 0) kernel_vw_max(i) := vwidth_kernel(0)
      else       kernel_vw_max(i) := Max2(vwidth_kernel(i), kernel_vw_max(i-1))
  }

  io.data_main_vwidth := LookupTreeDefault(io.k, 0.U(4.W), List(
    1.U  -> main_vw_max(0),
    2.U  -> main_vw_max(1),
    3.U  -> main_vw_max(2),
    4.U  -> main_vw_max(3),
    5.U  -> main_vw_max(4)
  ))
  io.data_kernel_vwidth := LookupTreeDefault(io.k, 0.U(4.W), List(
    1.U  -> kernel_vw_max(0),
    2.U  -> kernel_vw_max(1),
    3.U  -> kernel_vw_max(2),
    4.U  -> kernel_vw_max(3),
    5.U  -> kernel_vw_max(4)
  ))
}
