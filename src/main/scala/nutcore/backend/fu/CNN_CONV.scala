package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import difftest._

class BoothEncoderP extends Module {
  val io = IO(new Bundle {
    val y = Input(UInt(3.W))
    val x = Input(UInt(32.W))
    val p = Output(UInt(32.W))
  })

  io.p := LookupTree(io.y, List(
    "b000".U -> 0.U,
    "b001".U -> io.x,
    "b010".U -> io.x,
    "b011".U -> (io.x << 1),
    "b100".U -> ~(io.x << 1),
    "b101".U -> ~io.x,
    "b110".U -> ~io.x,
    "b111".U -> 0.U
  ))
}
object BoothEncoderP {
  def apply(y: UInt, x: UInt) = {
    val m = Module(new BoothEncoderP)
    m.io.y := y
    m.io.x := x
    m.io.p
  }
}

class BoothEncoderC extends Module {
  val io = IO(new Bundle {
    val y = Input(UInt(3.W))
    val c = Output(UInt(1.W))
  })

  io.c := LookupTreeDefault(io.y, 0.U, List(
    4.U -> 1.U,
    5.U -> 1.U,
    6.U -> 1.U
  ))
}
object BoothEncoderC {
  def apply(y: UInt) = {
    val m = Module(new BoothEncoderC)
    m.io.y := y
    m.io.c
  }
}

object AdderFullS {
  def apply(a: UInt, b: UInt, cin: UInt) = {
    a ^ b ^ cin
  }
}
object AdderFullCout {
  def apply(a: UInt, b: UInt, cin: UInt) = {
    a & b | a & cin | b & cin
  }
}
object AdderHalfS {
  def apply(a: UInt, b: UInt) = {
    a ^ b
  }
}
object AdderHalfCout {
  def apply(a: UInt, b: UInt) = {
    a & b
  }
}

class WallaceTree25 extends Module {
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
    ly1_out(i)  := AdderFullS( io.n(3*i), io.n(3*i+1), io.n(3*i+2) )
    out_cout(i) := AdderFullCout( io.n(3*i), io.n(3*i+1), io.n(3*i+2) )
  }

  //layer2: 5 full adder + 1 half adder
  val ly2_in = Cat( ly1_out.reduce{ (a, b) => Cat(b, a) }, Cat( io.cin(7, 0), io.n(24) ) )  //17 bit
  val ly2_out = Wire(Vec(6, UInt(1.W)))
  for(i <- 0 until 5) {
    ly2_out(i)    := AdderFullS( ly2_in(3*i), ly2_in(3*i+1), ly2_in(3*i+2) )
    out_cout(i+8) := AdderFullCout( ly2_in(3*i), ly2_in(3*i+1), ly2_in(3*i+2) )
  }
  ly2_out(5)   := AdderHalfS( ly2_in(15), ly2_in(16) )
  out_cout(13) := AdderHalfCout( ly2_in(15), ly2_in(16) )

  //layer3: 4 full adder
  val ly3_in = Cat( ly2_out.reduce{ (a, b) => Cat(b, a) }, io.cin(13, 8) )  //12 bit
  val ly3_out = Wire(Vec(4, UInt(1.W)))
  for(i <- 0 until 4) {
    ly3_out(i)     := AdderFullS( ly3_in(3*i), ly3_in(3*i+1), ly3_in(3*i+2) )
    out_cout(i+14) := AdderFullCout( ly3_in(3*i), ly3_in(3*i+1), ly3_in(3*i+2) )
  }

  //layer4: 2 full adder + 1 half adder
  val ly4_in = Cat( ly3_out.reduce{ (a, b) => Cat(b, a) }, io.cin(17, 14) )  //8 bit
  val ly4_out = Wire(Vec(3, UInt(1.W)))
  for(i <- 0 until 2) {
    ly4_out(i)     := AdderFullS( ly4_in(3*i), ly4_in(3*i+1), ly4_in(3*i+2) )
    out_cout(i+18) := AdderFullCout( ly4_in(3*i), ly4_in(3*i+1), ly4_in(3*i+2) )
  }
  ly4_out(2)   := AdderHalfS( ly4_in(6), ly4_in(7) )
  out_cout(20) := AdderHalfCout( ly4_in(6), ly4_in(7) )

  //layer5: 2 full adder
  val ly5_in = Cat( ly4_out.reduce{ (a, b) => Cat(b, a) }, io.cin(20, 18) )  //6 bit
  val ly5_out = Wire(Vec(2, UInt(1.W)))
  for(i <- 0 until 2) {
    ly5_out(i)     := AdderFullS( ly5_in(3*i), ly5_in(3*i+1), ly5_in(3*i+2) )
    out_cout(i+21) := AdderFullCout( ly5_in(3*i), ly5_in(3*i+1), ly5_in(3*i+2) )
  }

  //layer6: 1 full adder
  val ly6_in = Cat( ly5_out.reduce{ (a, b) => Cat(b, a) }, io.cin(21) )  //3 bit
  val ly6_out = Wire(UInt(1.W))
  ly6_out      := AdderFullS( ly6_in(0), ly6_in(1), ly6_in(2) )
  out_cout(23) := AdderFullCout( ly6_in(0), ly6_in(1), ly6_in(2) )

  //layer7: 1 full adder
  val ly7_in = Cat( ly6_out, io.cin(23, 22) )  //3 bit

  io.s := AdderFullS( ly7_in(0), ly7_in(1), ly7_in(2) )
  io.c := AdderFullCout( ly7_in(0), ly7_in(1), ly7_in(2) )
  io.cout := out_cout.reduce{ (a, b) => Cat(b, a) }
}

class WallaceAdder extends Module {
  val io = IO(new Bundle {
    val data = Input(Vec(25, UInt(32.W)))
    val cin = Input(Vec(25, UInt(1.W)))
    val s = Output(UInt(32.W))
    val c = Output(UInt(32.W))
  })

  val wallace_in = Wire(Vec(32, UInt(25.W)))
  val wallace_cin = Wire(UInt(24.W))
  wallace_cin := (io.cin.reduce{ (a, b) => Cat(b, a) })(23,0)
  for(i <- 0 until 32) {
    //wallace_in(i) := io.data.reduce{ (a, b) => Cat(b(i), a(i)) }
    wallace_in(i) := (io.data.map{ a => a(i).asUInt }).reduce{ (a: UInt, b: UInt) => Cat(b, a) }
  }

  val wallace_tree = VecInit(Seq.fill(32)(Module(new WallaceTree25).io))
  val out_s = Wire(Vec(32, UInt(1.W)))
  val out_c = Wire(Vec(32, UInt(1.W)))

  for(i <- 0 until 32) {
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

class CNNConvSub25 extends NutCoreModule {
  val io = IO(new Bundle {
    val conv_valid = Input(Bool())
    val data_main = Input(Vec(25, UInt(16.W)))
    val data_kernel = Input(Vec(25, UInt(8.W)))
    val data_kernel_vwidth = Input(UInt(4.W))

    val data_res = Output(UInt(32.W))
    val data_ok = Output(Bool())
  })

  // Stage 1
  //Booth
  val booth_p = WireInit(VecInit.fill(4, 25)(0.U(32.W)))
  val booth_c = WireInit(VecInit.fill(4, 25)(0.U(1.W)))
  for (i <- 0 until 4) {
    for (j <- 0 until 25) {
      if (i == 0) {
        booth_p(i)(j) := BoothEncoderP( Cat(io.data_kernel(j)(1, 0), 0.U(1.W)), ZeroExt(io.data_main(j), 32) )
        booth_c(i)(j) := BoothEncoderC( Cat(io.data_kernel(j)(1, 0), 0.U(1.W)) )
      }
      else {
        booth_p(i)(j) := BoothEncoderP( io.data_kernel(j)(2*i+1, 2*i-1), ZeroExt(io.data_main(j), 32) << (2*i) )
        booth_c(i)(j) := BoothEncoderC( io.data_kernel(j)(2*i+1, 2*i-1) )
      }
    }
  }

  //Wallace
  val wallace_adder = VecInit(Seq.fill(4)(Module(new WallaceAdder).io))
  val out_s = Wire(Vec(4, UInt(32.W)))
  val out_c = Wire(Vec(4, UInt(32.W)))
  for(i <- 0 until 4) {
    wallace_adder(i).data := booth_p(i)
    wallace_adder(i).cin  := booth_c(i)
    out_s(i) := wallace_adder(i).s
    out_c(i) := wallace_adder(i).c
  }

  val res_int21 = out_s(0) + out_c(0)

  // Stage 2
  //reg
  val s_stage1 :: s_stage2 :: Nil = Enum(2)
  val state = RegInit(s_stage1)
  val stage2_valid = io.conv_valid && (state === s_stage1) && ( io.data_kernel_vwidth(3) || io.data_kernel_vwidth(2) )
  switch (state) {
    is (s_stage1) {
      when ( stage2_valid ) { state := s_stage2 }
    }
    is (s_stage2) {
      state := s_stage1
    }
  }
  val out_s_reg = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
  val out_c_reg = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
  when (stage2_valid) { out_s_reg := out_s }
  when (stage2_valid) { out_c_reg := out_c }

  val res_int8 = out_s_reg.reduce(_ + _) + out_c_reg.reduce(_ + _)
  val res_int4 = out_s_reg(0) + out_s_reg(1) + out_c_reg(0) + out_c_reg(1)

  val res_final_32 = Wire(UInt(32.W))
  res_final_32 := Mux(state === s_stage2, Mux(io.data_kernel_vwidth(3), res_int8, res_int4), res_int21)
  io.data_res := Mux(res_final_32(31, 30) === 0.U, res_final_32(31, 0), Mux(res_final_32(31, 30) === 1.U, res_final_32(31, 0), 0.U(32.W)))
  io.data_ok := !stage2_valid

  //Debug(io.conv_valid, "conv valid: %d, k_vwidth: %d, state: %d, stage2_valid: %d, data_ok: %d, cycle: %d\n", io.conv_valid, io.data_kernel_vwidth, state, stage2_valid, io.data_ok, GTimer())
}

class CNNConvIO(length: Int) extends Bundle {
  val conv_valid = Input(Bool())
  val k = Input(UInt(4.W))
  val data_main = Input(Vec(length*length, UInt(16.W)))
  val data_kernel = Input(Vec(length*length, UInt(8.W)))
  val data_main_vwidth = Input(UInt(4.W))
  val data_kernel_vwidth = Input(UInt(4.W))

  val conv_res = Output(UInt(64.W))
  val conv_ok = Output(Bool())

  override def cloneType = (new CNNConvIO(length)).asInstanceOf[this.type]
}

class CNNConv(length: Int) extends NutCoreModule {
  val io = IO(new CNNConvIO(length))

  val conv_mdu = Module(new CNNConvSub25)
  conv_mdu.io.conv_valid := io.conv_valid
  conv_mdu.io.data_main := io.data_main
  conv_mdu.io.data_kernel := io.data_kernel
  conv_mdu.io.data_kernel_vwidth := io.data_kernel_vwidth

  val res = conv_mdu.io.data_res
  //val res2 = Wire(UInt(16.W))

  //res2 := LookupTreeDefault(io.data_main_vwidth, 0.U(16.W), List(
  //  "b1000".U  -> res,
  //  "b0100".U  -> Mux(res > 255.U(16.W), 255.U(16.W), res),
  //  "b0010".U  -> Mux(res > 15.U(16.W), 15.U(16.W), res),
  //  "b0001".U  -> Mux(res > 3.U(16.W), 3.U(16.W), res)
  //))

  io.conv_res := Cat(0.U(32.W), res)
  io.conv_ok := conv_mdu.io.data_ok
}
