package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import top.Settings
import difftest._

object CNNOpType {
  def conv  = "b0000001".U
  def pool  = "b0000010".U
  def act   = "b0000100".U

  def loadv_d = "b0001100".U
  def loadv_p = "b0001010".U
  def loadv_w = "b0001001".U

  def isLoadV(func: UInt) = func(3)
  def isAlg(func: UInt) = !func(3)
  def isConv(func: UInt) = isAlg(func) && func(0)
  def isPool(func: UInt) = isAlg(func) && func(1)
  def isAct(func: UInt)  = isAlg(func) && func(2)
  def isLoadD(func: UInt) = isLoadV(func) && func(2)
  def isLoadP(func: UInt) = isLoadV(func) && func(1)
  def isLoadW(func: UInt) = isLoadV(func) && func(0)
}

object Cat5 {
 def apply(a1: UInt, a2: UInt, a3: UInt, a4: UInt, a5: UInt) = {
   Cat(a5, a4, a3, a2, a1)
 }
}

object SignZeroExt {
  def apply(a: UInt, sign: Bool, len: Int) = {
    Mux(sign, SignExt(a, len), ZeroExt(a, len))
  }
}

class CNNtoLSUIO extends NutCoreBundle {
  val valid = Output(Bool())
  val src1 = Output(UInt(XLEN.W))
  val src2 = Output(UInt(XLEN.W))
  val func = Output(FuOpType())

  val load_valid = Input(Bool())
  val load_data = Input(UInt(XLEN.W))
  val load_exception = Input(Bool())
}

class CNNIO extends FunctionUnitIO {
  val imm = Input(UInt(XLEN.W))
  val vtag = Input(UInt(3.W))
  val length_k = Input(UInt(4.W))
  val vec_addr = Input(UInt(5.W))
  val algorithm = Input(UInt(2.W))
  val loadv = new CNNtoLSUIO
}

class CNNFU extends NutCoreModule {
  val io = IO(new CNNIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val lsu_data = io.loadv.load_data
  val lsu_valid = io.loadv.load_valid
  val lsu_ex = io.loadv.load_exception

  val isConv  = CNNOpType.isConv(func)  && valid
  val isPool  = CNNOpType.isPool(func)  && valid
  val isAct   = CNNOpType.isAct(func)   && valid
  val isLoadD = CNNOpType.isLoadD(func) && valid
  val isLoadP = CNNOpType.isLoadP(func) && valid
  val isLoadW = CNNOpType.isLoadW(func) && valid

  // VRF
  val vreg_mdu = Module(new CNNVectorRegFile)
  val select_vwidth = vreg_mdu.io.select_vwidth

  val vtype = io.vec_addr(4)
  val isInt16 = select_vwidth(7) && (vtype === 0.U)
  val isInt8 = select_vwidth(6) && (vtype === 0.U) || select_vwidth(3) && (vtype === 1.U)
  val isInt4 = select_vwidth(5) && (vtype === 0.U) || select_vwidth(2) && (vtype === 1.U)
  val isInt2 = select_vwidth(4) && (vtype === 0.U) || select_vwidth(1) && (vtype === 1.U)
  val isInt1 = select_vwidth(0) && (vtype === 1.U)
  val vwidth_valid = (vtype === 0.U) && (PopCount(select_vwidth(7,4)) === 1.U) || (vtype === 1.U) && (PopCount(select_vwidth(3,0)) === 1.U)
  val k_valid = (io.length_k =/= 0.U)
  val isSign = (vtype === 1.U)
  val intNumVec = Cat5(isInt1, isInt2, isInt4, isInt8, isInt16)

  Debug(valid, "cnn valid: %d, conv: %d, pool: %d, act: %d, loadv-w: %d, loadv-d: %d, loadv-p: %d, cycle: %d\n", valid, isConv, isPool, isAct, isLoadW, isLoadD, isLoadP, GTimer())
  //Debug(valid, "cnn bit: type: %d, int16: %d, int8: %d, int4: %d, int2: %d, int1: %d\n", vtype, isInt16, isInt8, isInt4, isInt2, isInt1)

  // state reg
  val s_stage1 :: s_stage2 :: Nil = Enum(2)
  val state = RegInit(s_stage1)
  val data_stage1_reg = RegInit(0.U(XLEN.W))

  // Int16
  val int16_src1 = Cat(src1(XLEN-1, 3), "b000".U(3.W))
  val int16_src2_stage1 = 0.U(XLEN.W)
  val int16_src2_stage2 = 8.U(XLEN.W)
  val int16_src2 = Mux(state === s_stage2, int16_src2_stage2, int16_src2_stage1)
  val int16_func = LSUOpType.ld
  val int16_stage2_valid = (src1(2,1) +& io.length_k(2,0)) >= 5.U(4.W)
  // Int8
  val int8_src1 = Cat(src1(XLEN-1, 3), "b000".U(3.W))
  val int8_src2_stage1 = Mux(src1(2)===1.U, 4.U(XLEN.W), 0.U(XLEN.W))
  val int8_src2_stage2 = 8.U(XLEN.W)
  val int8_src2 = Mux(state === s_stage2, int8_src2_stage2, int8_src2_stage1)
  val int8_func = Mux(state === s_stage2, LSUOpType.lwu, Mux(src1(2)===1.U, LSUOpType.lwu, LSUOpType.ld))
  val int8_stage2_valid = (src1(2,0) +& io.length_k(2,0)) >= 9.U(4.W)
  // Int4
  val int4_src1 = Cat("b0".U(1.W), Cat(src1(XLEN-1, 3), "b00".U(2.W)))
  val int4_src2_stage1 = Mux(src1(2)===1.U, 2.U(XLEN.W), 0.U(XLEN.W))
  val int4_src2_stage2 = 4.U(XLEN.W)
  val int4_src2 = Mux(state === s_stage2, int4_src2_stage2, int4_src2_stage1)
  val int4_func = Mux(state === s_stage2, LSUOpType.lhu, Mux(src1(2)===1.U, LSUOpType.lhu, LSUOpType.lwu))
  val int4_stage2_valid = (src1(2,0) +& io.length_k(2,0)) >= 9.U(4.W)
  // Int2
  val int2_src1 = Cat("b00".U(2.W), Cat(src1(XLEN-1, 3), "b0".U(1.W)))
  val int2_src2_stage1 = Mux(src1(2)===1.U, 1.U(XLEN.W), 0.U(XLEN.W))
  val int2_src2_stage2 = 2.U(XLEN.W)
  val int2_src2 = Mux(state === s_stage2, int2_src2_stage2, int2_src2_stage1)
  val int2_func = Mux(state === s_stage2, LSUOpType.lbu, Mux(src1(2)===1.U, LSUOpType.lbu, LSUOpType.lhu))
  val int2_stage2_valid = (src1(2,0) +& io.length_k(2,0)) >= 9.U(4.W)
  // Int1
  val int1_src1 = Cat("b000".U(3.W), src1(XLEN-1, 3))
  val int1_src2_stage1 = 0.U(XLEN.W)
  val int1_src2_stage2 = 1.U(XLEN.W)
  val int1_src2 = Mux(state === s_stage2, int1_src2_stage2, int1_src2_stage1)
  val int1_func = LSUOpType.lbu
  val int1_stage2_valid = (src1(2,0) +& io.length_k(2,0)) >= 9.U(4.W)

  // ALL
  val LoadvCtrlTable = List(
    "b10000".U -> (int16_src1, int16_src2, int16_func, int16_stage2_valid),
    "b01000".U -> (int8_src1,  int8_src2,  int8_func,  int8_stage2_valid),
    "b00100".U -> (int4_src1,  int4_src2,  int4_func,  int4_stage2_valid),
    "b00010".U -> (int2_src1,  int2_src2,  int2_func,  int2_stage2_valid),
    "b00001".U -> (int1_src1,  int1_src2,  int1_func,  int1_stage2_valid)
  )
  val loadv_src1   = LookupTreeDefault(intNumVec, 0.U(XLEN.W),   LoadvCtrlTable.map(p => (p._1, p._2._1)))
  val loadv_src2   = LookupTreeDefault(intNumVec, 0.U(XLEN.W),   LoadvCtrlTable.map(p => (p._1, p._2._2)))
  val loadv_func   = LookupTreeDefault(intNumVec, 0.U(7.W),      LoadvCtrlTable.map(p => (p._1, p._2._3)))
  val stage2_valid = LookupTreeDefault(intNumVec, false.B,       LoadvCtrlTable.map(p => (p._1, p._2._4)))

  // state reg
  switch (state) {
    is (s_stage1) {
      when ( (isLoadD || isLoadP) && stage2_valid && lsu_valid && !lsu_ex ) { state := s_stage2 }
    }
    is (s_stage2) {
      when ( lsu_valid ) { state := s_stage1 }
    }
  }
  when ( (state === s_stage1) && (isLoadD || isLoadP) && stage2_valid && lsu_valid && !lsu_ex ) { data_stage1_reg := lsu_data }

  // load data
  val data_stage1 = Mux(stage2_valid, data_stage1_reg, lsu_data)

  val int16_load_data = LookupTree(src1(2,1), List(
    "b00".U -> Cat(lsu_data(15,0), data_stage1),
    "b01".U -> Cat(lsu_data(31,0), data_stage1(63,16)),
    "b10".U -> Cat(lsu_data(47,0), data_stage1(63,32)),
    "b11".U -> Cat(lsu_data, data_stage1(63,48))
  ))
  val int8_load_data = LookupTree(src1(2,0), List(
    "b000".U -> Cat5( SignZeroExt(lsu_data( 7, 0),isSign,16), SignZeroExt(lsu_data(15, 8),isSign,16), SignZeroExt(lsu_data(23,16),isSign,16), SignZeroExt(lsu_data(31,24),isSign,16), SignZeroExt(lsu_data(39,32),isSign,16) ),
    "b001".U -> Cat5( SignZeroExt(lsu_data(15, 8),isSign,16), SignZeroExt(lsu_data(23,16),isSign,16), SignZeroExt(lsu_data(31,24),isSign,16), SignZeroExt(lsu_data(39,32),isSign,16), SignZeroExt(lsu_data(47,40),isSign,16) ),
    "b010".U -> Cat5( SignZeroExt(lsu_data(23,16),isSign,16), SignZeroExt(lsu_data(31,24),isSign,16), SignZeroExt(lsu_data(39,32),isSign,16), SignZeroExt(lsu_data(47,40),isSign,16), SignZeroExt(lsu_data(55,48),isSign,16) ),
    "b011".U -> Cat5( SignZeroExt(lsu_data(31,24),isSign,16), SignZeroExt(lsu_data(39,32),isSign,16), SignZeroExt(lsu_data(47,40),isSign,16), SignZeroExt(lsu_data(55,48),isSign,16), SignZeroExt(lsu_data(63,56),isSign,16) ),
    "b100".U -> Cat5( SignZeroExt(data_stage1( 7, 0),isSign,16), SignZeroExt(data_stage1(15, 8),isSign,16), SignZeroExt(data_stage1(23,16),isSign,16), SignZeroExt(data_stage1(31,24),isSign,16), SignZeroExt(lsu_data( 7, 0),isSign,16) ),
    "b101".U -> Cat5( SignZeroExt(data_stage1(15, 8),isSign,16), SignZeroExt(data_stage1(23,16),isSign,16), SignZeroExt(data_stage1(31,24),isSign,16), SignZeroExt(lsu_data( 7, 0),isSign,16),    SignZeroExt(lsu_data(15, 8),isSign,16) ),
    "b110".U -> Cat5( SignZeroExt(data_stage1(23,16),isSign,16), SignZeroExt(data_stage1(31,24),isSign,16), SignZeroExt(lsu_data( 7, 0),isSign,16),    SignZeroExt(lsu_data(15, 8),isSign,16),    SignZeroExt(lsu_data(23,16),isSign,16) ),
    "b111".U -> Cat5( SignZeroExt(data_stage1(31,24),isSign,16), SignZeroExt(lsu_data( 7, 0),isSign,16),    SignZeroExt(lsu_data(15, 8),isSign,16),    SignZeroExt(lsu_data(23,16),isSign,16),    SignZeroExt(lsu_data(31,24),isSign,16) )
  ))
  val int4_load_data = LookupTree(src1(2,0), List(
    "b000".U -> Cat5( SignZeroExt(lsu_data( 3, 0),isSign,16), SignZeroExt(lsu_data( 7, 4),isSign,16), SignZeroExt(lsu_data(11, 8),isSign,16), SignZeroExt(lsu_data(15,12),isSign,16), SignZeroExt(lsu_data(19,16),isSign,16) ),
    "b001".U -> Cat5( SignZeroExt(lsu_data( 7, 4),isSign,16), SignZeroExt(lsu_data(11, 8),isSign,16), SignZeroExt(lsu_data(15,12),isSign,16), SignZeroExt(lsu_data(19,16),isSign,16), SignZeroExt(lsu_data(23,20),isSign,16) ),
    "b010".U -> Cat5( SignZeroExt(lsu_data(11, 8),isSign,16), SignZeroExt(lsu_data(15,12),isSign,16), SignZeroExt(lsu_data(19,16),isSign,16), SignZeroExt(lsu_data(23,20),isSign,16), SignZeroExt(lsu_data(27,24),isSign,16) ),
    "b011".U -> Cat5( SignZeroExt(lsu_data(15,12),isSign,16), SignZeroExt(lsu_data(19,16),isSign,16), SignZeroExt(lsu_data(23,20),isSign,16), SignZeroExt(lsu_data(27,24),isSign,16), SignZeroExt(lsu_data(31,28),isSign,16) ),
    "b100".U -> Cat5( SignZeroExt(data_stage1( 3, 0),isSign,16), SignZeroExt(data_stage1( 7, 4),isSign,16), SignZeroExt(data_stage1(11, 8),isSign,16), SignZeroExt(data_stage1(15,12),isSign,16), SignZeroExt(lsu_data( 3, 0),isSign,16) ),
    "b101".U -> Cat5( SignZeroExt(data_stage1( 7, 4),isSign,16), SignZeroExt(data_stage1(11, 8),isSign,16), SignZeroExt(data_stage1(15,12),isSign,16), SignZeroExt(lsu_data( 3, 0),isSign,16),    SignZeroExt(lsu_data( 7, 4),isSign,16) ),
    "b110".U -> Cat5( SignZeroExt(data_stage1(11, 8),isSign,16), SignZeroExt(data_stage1(15,12),isSign,16), SignZeroExt(lsu_data( 3, 0),isSign,16),    SignZeroExt(lsu_data( 7, 4),isSign,16),    SignZeroExt(lsu_data(11, 8),isSign,16) ),
    "b111".U -> Cat5( SignZeroExt(data_stage1(15,12),isSign,16), SignZeroExt(lsu_data( 3, 0),isSign,16),    SignZeroExt(lsu_data( 7, 4),isSign,16),    SignZeroExt(lsu_data(11, 8),isSign,16),    SignZeroExt(lsu_data(15,12),isSign,16) )
  ))
  val int2_load_data = LookupTree(src1(2,0), List(
    "b000".U -> Cat5( SignZeroExt(lsu_data( 1, 0),isSign,16), SignZeroExt(lsu_data( 3, 2),isSign,16), SignZeroExt(lsu_data( 5, 4),isSign,16), SignZeroExt(lsu_data( 7, 6),isSign,16), SignZeroExt(lsu_data( 9, 8),isSign,16) ),
    "b001".U -> Cat5( SignZeroExt(lsu_data( 3, 2),isSign,16), SignZeroExt(lsu_data( 5, 4),isSign,16), SignZeroExt(lsu_data( 7, 6),isSign,16), SignZeroExt(lsu_data( 9, 8),isSign,16), SignZeroExt(lsu_data(11,10),isSign,16) ),
    "b010".U -> Cat5( SignZeroExt(lsu_data( 5, 4),isSign,16), SignZeroExt(lsu_data( 7, 6),isSign,16), SignZeroExt(lsu_data( 9, 8),isSign,16), SignZeroExt(lsu_data(11,10),isSign,16), SignZeroExt(lsu_data(13,12),isSign,16) ),
    "b011".U -> Cat5( SignZeroExt(lsu_data( 7, 6),isSign,16), SignZeroExt(lsu_data( 9, 8),isSign,16), SignZeroExt(lsu_data(11,10),isSign,16), SignZeroExt(lsu_data(13,12),isSign,16), SignZeroExt(lsu_data(15,14),isSign,16) ),
    "b100".U -> Cat5( SignZeroExt(data_stage1( 1, 0),isSign,16), SignZeroExt(data_stage1( 3, 2),isSign,16), SignZeroExt(data_stage1( 5, 4),isSign,16), SignZeroExt(data_stage1( 7, 6),isSign,16), SignZeroExt(lsu_data( 1, 0),isSign,16) ),
    "b101".U -> Cat5( SignZeroExt(data_stage1( 3, 2),isSign,16), SignZeroExt(data_stage1( 5, 4),isSign,16), SignZeroExt(data_stage1( 7, 6),isSign,16), SignZeroExt(lsu_data( 1, 0),isSign,16),    SignZeroExt(lsu_data( 3, 2),isSign,16) ),
    "b110".U -> Cat5( SignZeroExt(data_stage1( 5, 4),isSign,16), SignZeroExt(data_stage1( 7, 6),isSign,16), SignZeroExt(lsu_data( 1, 0),isSign,16),    SignZeroExt(lsu_data( 3, 2),isSign,16),    SignZeroExt(lsu_data( 5, 4),isSign,16) ),
    "b111".U -> Cat5( SignZeroExt(data_stage1( 7, 6),isSign,16), SignZeroExt(lsu_data( 1, 0),isSign,16),    SignZeroExt(lsu_data( 3, 2),isSign,16),    SignZeroExt(lsu_data( 5, 4),isSign,16),    SignZeroExt(lsu_data( 7, 6),isSign,16) )
  ))
  val int1_load_data = LookupTree(src1(2,0), List(
    "b000".U -> Cat5( ZeroExt(lsu_data(0),16), ZeroExt(lsu_data(1),16), ZeroExt(lsu_data(2),16), ZeroExt(lsu_data(3),16), ZeroExt(lsu_data(4),16) ),
    "b001".U -> Cat5( ZeroExt(lsu_data(1),16), ZeroExt(lsu_data(2),16), ZeroExt(lsu_data(3),16), ZeroExt(lsu_data(4),16), ZeroExt(lsu_data(5),16) ),
    "b010".U -> Cat5( ZeroExt(lsu_data(2),16), ZeroExt(lsu_data(3),16), ZeroExt(lsu_data(4),16), ZeroExt(lsu_data(5),16), ZeroExt(lsu_data(6),16) ),
    "b011".U -> Cat5( ZeroExt(lsu_data(3),16), ZeroExt(lsu_data(4),16), ZeroExt(lsu_data(5),16), ZeroExt(lsu_data(6),16), ZeroExt(lsu_data(7),16) ),
    "b100".U -> Cat5( ZeroExt(data_stage1(4),16), ZeroExt(data_stage1(5),16), ZeroExt(data_stage1(6),16), ZeroExt(data_stage1(7),16), ZeroExt(lsu_data(0),16) ),
    "b101".U -> Cat5( ZeroExt(data_stage1(5),16), ZeroExt(data_stage1(6),16), ZeroExt(data_stage1(7),16), ZeroExt(lsu_data(0),16),    ZeroExt(lsu_data(1),16) ),
    "b110".U -> Cat5( ZeroExt(data_stage1(6),16), ZeroExt(data_stage1(7),16), ZeroExt(lsu_data(0),16),    ZeroExt(lsu_data(1),16),    ZeroExt(lsu_data(2),16) ),
    "b111".U -> Cat5( ZeroExt(data_stage1(7),16), ZeroExt(lsu_data(0),16),    ZeroExt(lsu_data(1),16),    ZeroExt(lsu_data(2),16),    ZeroExt(lsu_data(3),16) )
  ))

  val loadv_data   = LookupTreeDefault(intNumVec, 0.U((16*5).W), List(
    "b10000".U -> int16_load_data,
    "b01000".U -> int8_load_data,
    "b00100".U -> int4_load_data,
    "b00010".U -> int2_load_data,
    "b00001".U -> int1_load_data
  ))

  // data to vrf
  val loadv_data_elem = Wire(Vec(5, UInt(16.W)))
  for (i <- 0 until 5) {
    loadv_data_elem(i) := Mux(io.length_k(2,0) >= (i+1).U, loadv_data(16*i+15, 16*i), 0.U(16.W))
  }
  val loadv_data_main   = loadv_data_elem.reduce{ (a, b) => Cat(b, a) }
  val loadv_data_kernel = loadv_data_elem.map{ a => a(7, 0) }.reduce{ (a, b) => Cat(b, a) }

  val loadv_valid = Mux(stage2_valid, (state === s_stage2) && lsu_valid || (state === s_stage1) && lsu_valid && lsu_ex, lsu_valid)

  io.loadv.valid := (isLoadD || isLoadP) && vwidth_valid && k_valid || isLoadW
  io.loadv.src1 := Mux(isLoadD || isLoadP, loadv_src1, src1)
  io.loadv.src2 := Mux(isLoadD || isLoadP, loadv_src2, io.imm)
  io.loadv.func := Mux(isLoadD || isLoadP, loadv_func, LSUOpType.ld)
  val lv_valid = Mux(isLoadW, lsu_valid, Mux((isLoadD || isLoadP) && vwidth_valid && k_valid, loadv_valid, true.B))

  //Debug(valid, "lv_valid: %d, data00: %d, data01: %d, data02: %d, data03: %d, data04: %d, loaddata: %x\n", lv_valid, loadv_data_elem(0), loadv_data_elem(1), loadv_data_elem(2), loadv_data_elem(3), loadv_data_elem(4), lsu_data)

  vreg_mdu.io.vaddr       := io.vec_addr
  vreg_mdu.io.vtag        := io.vtag
  vreg_mdu.io.vop         := func(2,0)
  vreg_mdu.io.k           := io.length_k
  vreg_mdu.io.vwen        := lv_valid && ((isLoadD || isLoadP) && vwidth_valid && k_valid || isLoadW) && !lsu_ex
  vreg_mdu.io.load_data   := loadv_data_main
  vreg_mdu.io.load_kernel := loadv_data_kernel
  vreg_mdu.io.load_vwidth := io.loadv.load_data
  
  // CONV
  val conv_mdu = Module(new CNNConv(length=5))
  conv_mdu.io.conv_valid         := isConv
  conv_mdu.io.k                  := io.length_k
  conv_mdu.io.data_main          := vreg_mdu.io.data_main
  conv_mdu.io.data_kernel        := vreg_mdu.io.data_kernel
  conv_mdu.io.data_main_vwidth   := vreg_mdu.io.data_main_vwidth
  conv_mdu.io.data_kernel_vwidth := vreg_mdu.io.data_kernel_vwidth
  val conv_res = conv_mdu.io.conv_res
  val conv_valid = conv_mdu.io.conv_ok

  // POOL
  val pool_mdu = Module(new CNNPool(length=5))
  pool_mdu.io.pool_valid  := isPool
  pool_mdu.io.k           := io.length_k
  pool_mdu.io.agm         := io.algorithm
  pool_mdu.io.data_main   := vreg_mdu.io.data_main
  pool_mdu.io.data_vwidth := vreg_mdu.io.data_main_vwidth
  val pool_res = pool_mdu.io.pool_res
  val pool_valid = pool_mdu.io.pool_ok
  
  // ACT
  val act_mdu = Module(new CNNAct)
  act_mdu.io.act_valid   := isAct
  act_mdu.io.data_main   := src1
  act_mdu.io.data_zero   := src2
  act_mdu.io.data_vwidth := vreg_mdu.io.act_vwidth
  val act_res = act_mdu.io.act_res
  val act_valid = act_mdu.io.act_ok

  io.in.ready := io.out.ready
  io.out.valid := valid && Mux(isAct, act_valid, Mux(isPool, pool_valid, Mux(isConv, conv_valid, lv_valid)))
  io.out.bits := Mux(isAct, act_res, Mux(isPool, pool_res, conv_res))

  Debug(valid, "out_valid: %d, conv_ok: %d, pool_ok: %d, act_ok: %d\n", io.out.valid, conv_valid, pool_valid, act_valid)
  //Debug(valid, "out_ready: %d, conv_res: %d, pool_res: %d, act_res: %d\n", io.in.ready, conv_res, pool_res, act_res)
  //Debug(valid, "k: %d, main_vwidth: %d, kernel_vwidth: %d\n", io.length_k, vreg_mdu.io.data_main_vwidth, vreg_mdu.io.data_kernel_vwidth)
}