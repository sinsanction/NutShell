package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

class CNNPoolMax(length: Int) extends Module {
  val io = IO(new Bundle {
    val data_main = Input(Vec(length*length, UInt(16.W)))
    val data_res = Output(UInt(16.W))
  })
  
  io.data_res := io.data_main.reduce( (a, b) => Mux(a >= b, a, b) )
}

class CNNPoolAvg(length: Int) extends Module {
  val io = IO(new Bundle {
    val k = Input(UInt(4.W))
    val data_main = Input(Vec(length*length, UInt(16.W)))
    val data_res = Output(UInt(16.W))
  })

  val sum = Wire(UInt(21.W))
  sum := io.data_main.reduce(_ +& _)

  val div4 = (sum >> 2) + sum(1)

  val mul_res = Wire(UInt(42.W))
  mul_res := sum * 1864135.U(21.W)
  val div9 = (mul_res >> 24.U) + mul_res(23)

  val div16 = (sum >> 4) + sum(3)

  val mul_res1 = Wire(UInt(51.W))
  val div5 = Wire(UInt(19.W))
  val mul_res2 = Wire(UInt(49.W))
  val div25 = Wire(UInt(17.W))
  mul_res1 := sum * 858993459.U(30.W)
  div5 := (mul_res1 >> 32.U) + mul_res1(31)
  mul_res2 := div5 * 858993459.U(30.W)
  div25 := (mul_res2 >> 32.U) + mul_res2(31)

  io.data_res := LookupTreeDefault(io.k, 0.U(16.W), List(
    1.U  -> sum(15, 0),
    2.U  -> div4(15, 0),
    3.U  -> div9(15, 0),
    4.U  -> div16(15, 0),
    5.U  -> div25(15, 0)
  ))
}

class CNNPoolIO(length: Int) extends Bundle {
  val pool_valid = Input(Bool())
  val k = Input(UInt(4.W))
  val agm = Input(UInt(2.W))
  val data_main = Input(Vec(length*length, UInt(16.W)))
  val data_vwidth = Input(UInt(4.W))

  val pool_res = Output(UInt(64.W))
  val pool_ok = Output(Bool())

  override def cloneType = (new CNNPoolIO(length)).asInstanceOf[this.type]
}

class CNNPool(length: Int) extends NutCoreModule {
  val io = IO(new CNNPoolIO(length))

  val pool_max_mdu = Module(new CNNPoolMax(length))
  pool_max_mdu.io.data_main := io.data_main

  val pool_avg_mdu = Module(new CNNPoolAvg(length))
  pool_avg_mdu.io.data_main := io.data_main
  pool_avg_mdu.io.k := io.k

  val res = Wire(UInt(16.W))
  res := LookupTreeDefault(io.agm, 0.U(16.W), List(
    "b01".U  -> pool_max_mdu.io.data_res,
    "b10".U  -> pool_avg_mdu.io.data_res
  ))

  //val res2 = Wire(UInt(16.W))
  //res2 := LookupTreeDefault(io.data_vwidth, 0.U(16.W), List(
    //"b1000".U  -> res,
    //"b0100".U  -> Mux(res > 255.U(16.W), 255.U(16.W), res),
    //"b0010".U  -> Mux(res > 15.U(16.W), 15.U(16.W), res),
    //"b0001".U  -> Mux(res > 3.U(16.W), 3.U(16.W), res)
  //))
  io.pool_res := Cat(0.U(48.W), res)
  io.pool_ok := true.B
}
