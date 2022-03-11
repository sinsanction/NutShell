package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

class CNN_POOL_MAX(length: Int) extends Module {
  val io = IO(new Bundle {
    val data_main = Input(Vec(length*length, UInt(16.W)))
    val data_res = Output(UInt(16.W))
  })
  
  io.data_res := io.data_main.reduce( (a, b) => Mux(a >= b, a, b) )
}

class CNN_POOL_AVG(length: Int) extends Module {
  val io = IO(new Bundle {
    val k = Input(UInt(4.W))
    val data_main = Input(Vec(length*length, UInt(16.W)))
    val data_res = Output(UInt(16.W))
  })

  val sum = Wire(UInt(21.W))
  sum := io.data_main.reduce(_ +& _)
  
  io.data_res := 0.U
  switch (io.k) {
    is( 1.U ) {
      io.data_res := sum(15, 0)
    }
    is( 2.U ) {
      val div = sum >> 2.U
      io.data_res := div(15, 0)
    }
    is( 3.U ) {
      val mul_res = Wire(UInt(42.W))
      mul_res := sum * 1864135.U(21.W)
      val div = (mul_res >> 24.U) + mul_res(23)
      io.data_res := div(15, 0)
    }
    is( 4.U ) {
      val div = sum >> 4.U
      io.data_res := div(15, 0)
    }
    is( 5.U ) {
      val mul_res1 = Wire(UInt(51.W))
      mul_res1 := sum * 858993459.U(30.W)
      val div1 = Wire(UInt(19.W))
      div1 := (mul_res1 >> 32.U) + mul_res1(31)

      val mul_res2 = Wire(UInt(49.W))
      mul_res2 := div1 * 858993459.U(30.W)
      val div2 = Wire(UInt(17.W))
      div2 := (mul_res2 >> 32.U) + mul_res2(31)

      io.data_res := div2(15, 0)
    }
  }
}

class CNN_POOL_IO(length: Int) extends Bundle {
  val pool_valid = Input(Bool())
  val k = Input(UInt(4.W))
  val agm = Input(UInt(2.W))
  val data_main = Input(Vec(length*length, UInt(16.W)))
  val data_vwidth = Input(UInt(4.W))

  val pool_res = Output(UInt(64.W))
  val pool_ok = Output(Bool())
}

class CNN_POOL(length: Int) extends NutCoreModule {
  val io = IO(new CNN_POOL_IO(length))

  val pool_max_mdu = Module(new CNN_POOL_MAX(length))
  pool_max_mdu.io.data_main := io.data_main

  val pool_avg_mdu = Module(new CNN_POOL_AVG(length))
  pool_avg_mdu.io.data_main := io.data_main
  pool_avg_mdu.io.k := io.k

  val res = Wire(UInt(16.W))
  res := 0.U
  when(io.agm(0) === 1.U) {
    res := pool_max_mdu.io.data_res
  }.elsewhen(io.agm(1) === 1.U) {
    res := pool_avg_mdu.io.data_res
  }

  val res2 = Wire(UInt(16.W))
  io.pool_res := 0.U(64.W)
  when (io.data_vwidth(3) === 1.U) {
    io.pool_res := Cat(0.U(48.W), res)
  }.elsewhen (io.data_vwidth(2) === 1.U){
    res2 := Mux(res > 255.U(16.W), 255.U(16.W), res)
    io.pool_res := Cat(0.U(48.W), res2)
  }.elsewhen (io.data_vwidth(1) === 1.U) {
    res2 := Mux(res > 15.U(16.W), 15.U(16.W), res)
    io.pool_res := Cat(0.U(48.W), res2)
  }.elsewhen (io.data_vwidth(0) === 1.U) {
    res2 := Mux(res > 3.U(16.W), 3.U(16.W), res)
    io.pool_res := Cat(0.U(48.W), res2)
  }

  io.pool_ok := true.B
}
