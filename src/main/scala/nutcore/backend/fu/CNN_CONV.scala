package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

class BOOTH_ENCODER_P extends Module {
  val io = IO(new Bundle {
    val y = Input(UInt(3.W))
    val x = Input(UInt(18.W))
    val p = Output(UInt(18.W))
  })

  io.p := 0.U
  switch (io.y) {
    is( 0.U ) {
      io.p := 0.U
    }
    is( 1.U ) {
      io.p := io.x
    }
    is( 2.U ) {
      io.p := io.x
    }
    is( 3.U ) {
      io.p := io.x << 1
    }
    is( 4.U ) {
      io.p := ~(io.x << 1)
    }
    is( 5.U ) {
      io.p := ~io.x
    }
    is( 6.U ) {
      io.p := ~io.x
    }
    is( 7.U ) {
      io.p := 0.U
    }
  }
}
object BOOTH_ENCODER_P {
  def apply(y: UInt, x: UInt) = {
    val m = Module(new BOOTH_ENCODER_P)
    m.io.y := y
    m.io.x := x
    m.io.p
  }
}

class BOOTH_ENCODER_C extends Module {
  val io = IO(new Bundle {
    val y = Input(UInt(3.W))
    val c = Output(UInt(1.W))
  })

  io.c := 0.U
  switch (io.y) {
    is( 4.U ) {
      io.c := 1.U
    }
    is( 5.U ) {
      io.c := 1.U
    }
    is( 6.U ) {
      io.c := 1.U
    }
  }
}
object BOOTH_ENCODER_C {
  def apply(y: UInt) = {
    val m = Module(new BOOTH_ENCODER_C)
    m.io.y := y
    m.io.c
  }
}

object ADDER_FULL_S {
  def apply(a: UInt, b: UInt, cin: UInt) = {
    a ^ b ^ cin
  }
}
object ADDER_FULL_COUT {
  def apply(a: UInt, b: UInt, cin: UInt) = {
    a & b | a & cin | b & cin
  }
}
object ADDER_HALF_S {
  def apply(a: UInt, b: UInt) = {
    a ^ b
  }
}
object ADDER_HALF_COUT {
  def apply(a: UInt, b: UInt) = {
    a & b
  }
}

class Wallace_Tree_25 extends Module {
  val io = IO(new Bundle {
    val n = Input(UInt(25.W))
    val cin = Input(UInt(24.W))
    val s = Output(UInt(1.W))
    val c = Output(UInt(1.W))
    val cout = Output(UInt(24.W))
  })
  val out_cout = Wire(Vec(24, UInt(1.W)))

  //layer1: 8 full adder
  val ly1_out = Wire(Vec(8, UInt(1.W)))
  for(i <- 0 until 8) {
    ly1_out(i) := ADDER_FULL_S( io.n(3*i), io.n(3*i+1), io.n(3*i+2) )
    out_cout(i) := ADDER_FULL_COUT( io.n(3*i), io.n(3*i+1), io.n(3*i+2) )
  }

  //layer2: 5 full adder + 1 half adder
  val ly2_in = Cat( ly1_out.reduce{ (a, b) => Cat(b, a) }, Cat( io.cin(7, 0), io.n(24) ) )  //17 bit
  val ly2_out = Wire(Vec(6, UInt(1.W)))
  for(i <- 0 until 5) {
    ly2_out(i) := ADDER_FULL_S( ly2_in(3*i), ly2_in(3*i+1), ly2_in(3*i+2) )
    out_cout(i+8) := ADDER_FULL_COUT( ly2_in(3*i), ly2_in(3*i+1), ly2_in(3*i+2) )
  }
  ly2_out(5) := ADDER_HALF_S( ly2_in(15), ly2_in(16) )
  out_cout(13) := ADDER_HALF_COUT( ly2_in(15), ly2_in(16) )

  //layer3: 4 full adder
  val ly3_in = Cat( ly2_out.reduce{ (a, b) => Cat(b, a) }, io.cin(13, 8) )  //12 bit
  val ly3_out = Wire(Vec(4, UInt(1.W)))
  for(i <- 0 until 4) {
    ly3_out(i) := ADDER_FULL_S( ly3_in(3*i), ly3_in(3*i+1), ly3_in(3*i+2) )
    out_cout(i+14) := ADDER_FULL_COUT( ly3_in(3*i), ly3_in(3*i+1), ly3_in(3*i+2) )
  }

  //layer4: 2 full adder + 1 half adder
  val ly4_in = Cat( ly3_out.reduce{ (a, b) => Cat(b, a) }, io.cin(17, 14) )  //8 bit
  val ly4_out = Wire(UInt(3.W))
  for(i <- 0 until 2) {
    ly4_out(i) := ADDER_FULL_S( ly4_in(3*i), ly4_in(3*i+1), ly4_in(3*i+2) )
    out_cout(i+18) := ADDER_FULL_COUT( ly4_in(3*i), ly4_in(3*i+1), ly4_in(3*i+2) )
  }
  ly4_out(2) := ADDER_HALF_S( ly4_in(6), ly4_in(7) )
  out_cout(20) := ADDER_HALF_COUT( ly4_in(6), ly4_in(7) )

  //layer5: 2 full adder
  val ly5_in = Cat( ly4_out.reduce{ (a, b) => Cat(b, a) }, io.cin(20, 18) )  //6 bit
  val ly5_out = Wire(Vec(2, UInt(1.W)))
  for(i <- 0 until 2) {
    ly5_out(i) := ADDER_FULL_S( ly5_in(3*i), ly5_in(3*i+1), ly5_in(3*i+2) )
    out_cout(i+21) := ADDER_FULL_COUT( ly5_in(3*i), ly5_in(3*i+1), ly5_in(3*i+2) )
  }

  //layer6: 1 full adder
  val ly6_in = Cat( ly5_out.reduce{ (a, b) => Cat(b, a) }, io.cin(21) )  //3 bit
  val ly6_out = Wire(UInt(1.W))
  ly6_out := ADDER_FULL_S( ly6_in(0), ly6_in(1), ly6_in(2) )
  out_cout(23) := ADDER_FULL_COUT( ly6_in(0), ly6_in(1), ly6_in(2) )

  //layer7: 1 full adder
  val ly7_in = Cat( ly6_out, io.cin(23, 22) )  //3 bit

  io.s := ADDER_FULL_S( ly7_in(0), ly7_in(1), ly7_in(2) )
  io.c := ADDER_FULL_COUT( ly7_in(0), ly7_in(1), ly7_in(2) )
  io.cout := out_cout.reduce{ (a, b) => Cat(b, a) }
}

class Wallace_Adder extends Module {
  val io = IO(new Bundle {
    val data = Input(Vec(25, UInt(18.W)))
    val cin = Input(Vec(25, UInt(1.W)))
    val s = Output(UInt(18.W))
    val c = Output(UInt(18.W))
  })

  val wallace_in = Wire(Vec(18, UInt(25.W)))
  val wallace_cin = Wire(UInt(24.W))
  wallace_cin := io.cin.reduce{ (a, b) => Cat(b, a) }
  for(i <- 0 until 18) {
    wallace_in(i) := io.data.reduce{ (a, b) => Cat(b(i), a(i)) }
  }

  val wallace_tree = Vec(18, Module(new Wallace_Tree_25).io)
  val out_s = Wire(Vec(18, UInt(1.W)))
  val out_c = Wire(Vec(18, UInt(1.W)))
  for(i <- 0 until 18) {
    if (i == 0) {
        wallace_tree(i).n := wallace_in(i)
        wallace_tree(i).cin := wallace_cin
        out_s(i) := wallace_tree(i).s
        out_c(i) := io.cin(24)
    }
    else {
        wallace_tree(i).n := wallace_in(i)
        wallace_tree(i).cin := wallace_tree(i-1).cout
        out_s(i) := wallace_tree(i).s
        out_c(i) := wallace_tree(i-1).c
    }
  }
  io.s := out_s.reduce{ (a, b) => Cat(b, a) }
  io.c := out_c.reduce{ (a, b) => Cat(b, a) }
}

class CNN_CONV_SUB25 extends Module {
  val io = IO(new Bundle {
    val conv_valid = Input(Bool())
    val data_main = Input(Vec(25, UInt(16.W)))
    val data_kernel = Input(Vec(25, UInt(8.W)))
    val data_kernel_vwidth = Input(UInt(4.W))

    val data_res = Output(UInt(16.W))
    val data_ok = Output(Bool())
  })

  // Part1
  //Booth
  val booth_p1 = Wire(Vec(25, UInt(18.W)))
  val booth_p2 = Wire(Vec(25, UInt(18.W)))
  val booth_p3 = Wire(Vec(25, UInt(18.W)))
  val booth_p4 = Wire(Vec(25, UInt(18.W)))

  val booth_c1 = Wire(Vec(25, UInt(1.W)))
  val booth_c2 = Wire(Vec(25, UInt(1.W)))
  val booth_c3 = Wire(Vec(25, UInt(1.W)))
  val booth_c4 = Wire(Vec(25, UInt(1.W)))

  for(i <- 0 until 25) {
    booth_p1(i) := BOOTH_ENCODER_P( Cat(io.data_kernel(i)(1, 0), 0.U(1.W)), ZeroExt(io.data_main(i), 18) )
    booth_p2(i) := BOOTH_ENCODER_P( io.data_kernel(i)(3, 1), ZeroExt(io.data_main(i), 18) << 2 )
    booth_p3(i) := BOOTH_ENCODER_P( io.data_kernel(i)(5, 3), ZeroExt(io.data_main(i), 18) << 4 )
    booth_p4(i) := BOOTH_ENCODER_P( io.data_kernel(i)(7, 5), ZeroExt(io.data_main(i), 18) << 6 )

    booth_c1(i) := BOOTH_ENCODER_C( Cat(io.data_kernel(i)(1, 0), 0.U(1.W)) )
    booth_c2(i) := BOOTH_ENCODER_C( io.data_kernel(i)(3, 1) )
    booth_c3(i) := BOOTH_ENCODER_C( io.data_kernel(i)(5, 3) )
    booth_c4(i) := BOOTH_ENCODER_C( io.data_kernel(i)(7, 5) )
  }

  //Wallace
  val wallace_adder1 = Module(new Wallace_Adder)
  val wallace_adder2 = Module(new Wallace_Adder)
  val wallace_adder3 = Module(new Wallace_Adder)
  val wallace_adder4 = Module(new Wallace_Adder)
  for(i <- 0 until 25) {
    wallace_adder1.io.data(i) := booth_p1(i)
    wallace_adder2.io.data(i) := booth_p2(i)
    wallace_adder3.io.data(i) := booth_p3(i)
    wallace_adder4.io.data(i) := booth_p4(i)
    wallace_adder1.io.cin(i) := booth_c1(i)
    wallace_adder2.io.cin(i) := booth_c2(i)
    wallace_adder3.io.cin(i) := booth_c3(i)
    wallace_adder4.io.cin(i) := booth_c4(i)
  }
  val s_1 = wallace_adder1.io.s
  val s_2 = wallace_adder2.io.s
  val s_3 = wallace_adder3.io.s
  val s_4 = wallace_adder4.io.s
  val c_1 = wallace_adder1.io.c
  val c_2 = wallace_adder2.io.c
  val c_3 = wallace_adder3.io.c
  val c_4 = wallace_adder4.io.c

  val res_int21 = s_1 + c_1

  // Part2
  //reg
  val state = RegInit(false.B)
  val part2_valid = io.conv_valid && !state &&  ( (io.data_kernel_vwidth(3) === 1.U) || (io.data_kernel_vwidth(2) === 1.U) )
  when (part2_valid) { state := true.B }
  when (state) { state := false.B }
  val s_1_reg = RegEnable(s_1, part2_valid)
  val s_2_reg = RegEnable(s_2, part2_valid)
  val s_3_reg = RegEnable(s_3, part2_valid)
  val s_4_reg = RegEnable(s_4, part2_valid)
  val c_1_reg = RegEnable(c_1, part2_valid)
  val c_2_reg = RegEnable(c_2, part2_valid)
  val c_3_reg = RegEnable(c_3, part2_valid)
  val c_4_reg = RegEnable(c_4, part2_valid)

  val res_int8 = s_1_reg + s_2_reg + s_3_reg + s_4_reg + c_1_reg + c_2_reg + c_3_reg + c_4_reg
  val res_int4 = s_1_reg + s_2_reg + c_1_reg + c_2_reg

  val res_final_18 = Wire(UInt(18.W))
  when (state) {
      when (io.data_kernel_vwidth(3) === 1.U) {
          res_final_18 := res_int8
      }.otherwise {
          res_final_18 := res_int4
      }
  }.otherwise {
      res_final_18 := res_int21
  }

  when (res_final_18(17, 16) === 0.U) {
      io.data_res := res_final_18(15, 0)
  }.elsewhen (res_final_18(17, 16) === 1.U) {
      io.data_res := 65535.U(16.W)
  }.otherwise {
      io.data_res := 0.U(16.W)
  }

  when (part2_valid) { io.data_ok := false.B }
  .otherwise { io.data_ok := true.B }
}

class CNN_CONV_IO(length: Int) extends Bundle {
  val conv_valid = Input(Bool())
  val k = Input(UInt(4.W))
  val data_main = Input(Vec(length*length, UInt(16.W)))
  val data_kernel = Input(Vec(length*length, UInt(8.W)))
  val data_main_vwidth = Input(UInt(4.W))
  val data_kernel_vwidth = Input(UInt(4.W))

  val conv_res = Output(UInt(64.W))
  val conv_ok = Output(Bool())
}

class CNN_CONV(length: Int) extends NutCoreModule {
  val io = IO(new CNN_CONV_IO(length))

  val conv_mdu = Module(new CNN_CONV_SUB25)
  conv_mdu.io.conv_valid := io.conv_valid
  conv_mdu.io.data_main := io.data_main
  conv_mdu.io.data_kernel := io.data_kernel
  conv_mdu.io.data_kernel_vwidth := io.data_kernel_vwidth

  val res = Wire(UInt(16.W))
  res := conv_mdu.io.data_res

  val res2 = Wire(UInt(16.W))
  io.conv_res := 0.U(64.W)

  when (io.data_main_vwidth(3) === 1.U) {
    io.conv_res := Cat(0.U(48.W), res)
  }.elsewhen (io.data_main_vwidth(2) === 1.U){
    res2 := Mux(res > 255.U(16.W), 255.U(16.W), res)
    io.conv_res := Cat(0.U(48.W), res2)
  }.elsewhen (io.data_main_vwidth(1) === 1.U) {
    res2 := Mux(res > 15.U(16.W), 15.U(16.W), res)
    io.conv_res := Cat(0.U(48.W), res2)
  }.elsewhen (io.data_main_vwidth(0) === 1.U) {
    res2 := Mux(res > 3.U(16.W), 3.U(16.W), res)
    io.conv_res := Cat(0.U(48.W), res2)
  }

  io.conv_ok := conv_mdu.io.data_ok
}
