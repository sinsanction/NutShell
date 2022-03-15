package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import top.Settings

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
  val vwidth_valid = (vtype === 0.U) && (select_vwidth(7,4) =/= 0.U) || (vtype === 1.U) && (select_vwidth(3,0) =/= 0.U)

  // state reg
  val s_stage1 :: s_stage2 :: Nil = Enum(2)
  val state = RegInit(s_stage1)
  val data_stage1 = RegInit(0.U(XLEN.W))

  // Int16
  val int16_src1 = Cat(src1(XLEN-1, 3), "b000".U(3.W))
  val int16_src2_stage1 = 0.U(XLEN.W)
  val int16_src2_stage2 = 8.U(XLEN.W)
  val int16_src2 = Mux(state === s_stage2, int16_src2_stage2, int16_src2_stage1)
  val int16_func = LSUOpType.ld
  val int16_stage2_valid = (src1(2,1) +& io.length_k(2,0)) >= 5.U(4.W)
  val int16_load_data = LookupTree(src1(2,1), List(
    "b00".U -> Cat(lsu_data(15,0), data_stage1),
    "b01".U -> Cat(lsu_data(31,0), data_stage1(63,16)),
    "b10".U -> Cat(lsu_data(47,0), data_stage1(63,32)),
    "b11".U -> Cat(lsu_data, data_stage1(63,48))
  ))
  // Int8
  val int8_src1 = Cat(src1(XLEN-1, 3), "b000".U(3.W))
  val int8_src2_stage1 = Mux(src1(2)===1.U, 4.U(XLEN.W), 0.U(XLEN.W))
  val int8_src2_stage2 = 8.U(XLEN.W)
  val int8_src2 = Mux(state === s_stage2, int8_src2_stage2, int8_src2_stage1)
  val int8_func = Mux(state === s_stage2, LSUOpType.lwu, Mux(src1(2)===1.U, LSUOpType.lwu, LSUOpType.ld))
  val int8_stage2_valid = (src1(2,0) +& io.length_k(2,0)) >= 9.U(4.W)
  val int8_load_data = LookupTree(src1(2,0), List(
    "b000".U -> Cat5( ZeroExt(lsu_data( 7, 0),16), ZeroExt(lsu_data(15, 8),16), ZeroExt(lsu_data(23,16),16), ZeroExt(lsu_data(31,24),16), ZeroExt(lsu_data(39,32),16) ),
    "b001".U -> Cat5( ZeroExt(lsu_data(15, 8),16), ZeroExt(lsu_data(23,16),16), ZeroExt(lsu_data(31,24),16), ZeroExt(lsu_data(39,32),16), ZeroExt(lsu_data(47,40),16) ),
    "b010".U -> Cat5( ZeroExt(lsu_data(23,16),16), ZeroExt(lsu_data(31,24),16), ZeroExt(lsu_data(39,32),16), ZeroExt(lsu_data(47,40),16), ZeroExt(lsu_data(55,48),16) ),
    "b011".U -> Cat5( ZeroExt(lsu_data(31,24),16), ZeroExt(lsu_data(39,32),16), ZeroExt(lsu_data(47,40),16), ZeroExt(lsu_data(55,48),16), ZeroExt(lsu_data(63,56),16) ),
    "b100".U -> Cat5( ZeroExt(data_stage1( 7, 0),16), ZeroExt(data_stage1(15, 8),16), ZeroExt(data_stage1(23,16),16), ZeroExt(data_stage1(31,24),16), ZeroExt(lsu_data( 7, 0),16) ),
    "b101".U -> Cat5( ZeroExt(data_stage1(15, 8),16), ZeroExt(data_stage1(23,16),16), ZeroExt(data_stage1(31,24),16), ZeroExt(lsu_data( 7, 0),16),    ZeroExt(lsu_data(15, 8),16) ),
    "b110".U -> Cat5( ZeroExt(data_stage1(23,16),16), ZeroExt(data_stage1(31,24),16), ZeroExt(lsu_data( 7, 0),16),    ZeroExt(lsu_data(15, 8),16),    ZeroExt(lsu_data(23,16),16) ),
    "b111".U -> Cat5( ZeroExt(data_stage1(31,24),16), ZeroExt(lsu_data( 7, 0),16),    ZeroExt(lsu_data(15, 8),16),    ZeroExt(lsu_data(23,16),16),    ZeroExt(lsu_data(31,24),16) )
  ))
  // Int4
  val int4_src1 = Cat("b0".U(1.W), Cat(src1(XLEN-1, 3), "b00".U(2.W)))
  val int4_src2_stage1 = Mux(src1(2)===1.U, 2.U(XLEN.W), 0.U(XLEN.W))
  val int4_src2_stage2 = 4.U(XLEN.W)
  val int4_src2 = Mux(state === s_stage2, int4_src2_stage2, int4_src2_stage1)
  val int4_func = Mux(state === s_stage2, LSUOpType.lhu, Mux(src1(2)===1.U, LSUOpType.lhu, LSUOpType.lwu))
  val int4_stage2_valid = (src1(2,0) +& io.length_k(2,0)) >= 9.U(4.W)
  val int4_load_data = LookupTree(src1(2,0), List(
    "b000".U -> Cat5( ZeroExt(lsu_data( 3, 0),16), ZeroExt(lsu_data( 7, 4),16), ZeroExt(lsu_data(11, 8),16), ZeroExt(lsu_data(15,12),16), ZeroExt(lsu_data(19,16),16) ),
    "b001".U -> Cat5( ZeroExt(lsu_data( 7, 4),16), ZeroExt(lsu_data(11, 8),16), ZeroExt(lsu_data(15,12),16), ZeroExt(lsu_data(19,16),16), ZeroExt(lsu_data(23,20),16) ),
    "b010".U -> Cat5( ZeroExt(lsu_data(11, 8),16), ZeroExt(lsu_data(15,12),16), ZeroExt(lsu_data(19,16),16), ZeroExt(lsu_data(23,20),16), ZeroExt(lsu_data(27,24),16) ),
    "b011".U -> Cat5( ZeroExt(lsu_data(15,12),16), ZeroExt(lsu_data(19,16),16), ZeroExt(lsu_data(23,20),16), ZeroExt(lsu_data(27,24),16), ZeroExt(lsu_data(31,28),16) ),
    "b100".U -> Cat5( ZeroExt(data_stage1( 3, 0),16), ZeroExt(data_stage1( 7, 4),16), ZeroExt(data_stage1(11, 8),16), ZeroExt(data_stage1(15,12),16), ZeroExt(lsu_data( 3, 0),16) ),
    "b101".U -> Cat5( ZeroExt(data_stage1( 7, 4),16), ZeroExt(data_stage1(11, 8),16), ZeroExt(data_stage1(15,12),16), ZeroExt(lsu_data( 3, 0),16),    ZeroExt(lsu_data( 7, 4),16) ),
    "b110".U -> Cat5( ZeroExt(data_stage1(11, 8),16), ZeroExt(data_stage1(15,12),16), ZeroExt(lsu_data( 3, 0),16),    ZeroExt(lsu_data( 7, 4),16),    ZeroExt(lsu_data(11, 8),16) ),
    "b111".U -> Cat5( ZeroExt(data_stage1(15,12),16), ZeroExt(lsu_data( 3, 0),16),    ZeroExt(lsu_data( 7, 4),16),    ZeroExt(lsu_data(11, 8),16),    ZeroExt(lsu_data(15,12),16) )
  ))
  // Int2
  val int2_src1 = Cat("b00".U(2.W), Cat(src1(XLEN-1, 3), "b0".U(1.W)))
  val int2_src2_stage1 = Mux(src1(2)===1.U, 1.U(XLEN.W), 0.U(XLEN.W))
  val int2_src2_stage2 = 2.U(XLEN.W)
  val int2_src2 = Mux(state === s_stage2, int2_src2_stage2, int2_src2_stage1)
  val int2_func = Mux(state === s_stage2, LSUOpType.lbu, Mux(src1(2)===1.U, LSUOpType.lbu, LSUOpType.lhu))
  val int2_stage2_valid = (src1(2,0) +& io.length_k(2,0)) >= 9.U(4.W)
  val int2_load_data = LookupTree(src1(2,0), List(
    "b000".U -> Cat5( ZeroExt(lsu_data( 1, 0),16), ZeroExt(lsu_data( 3, 2),16), ZeroExt(lsu_data( 5, 4),16), ZeroExt(lsu_data( 7, 6),16), ZeroExt(lsu_data( 9, 8),16) ),
    "b001".U -> Cat5( ZeroExt(lsu_data( 3, 2),16), ZeroExt(lsu_data( 5, 4),16), ZeroExt(lsu_data( 7, 6),16), ZeroExt(lsu_data( 9, 8),16), ZeroExt(lsu_data(11,10),16) ),
    "b010".U -> Cat5( ZeroExt(lsu_data( 5, 4),16), ZeroExt(lsu_data( 7, 6),16), ZeroExt(lsu_data( 9, 8),16), ZeroExt(lsu_data(11,10),16), ZeroExt(lsu_data(13,12),16) ),
    "b011".U -> Cat5( ZeroExt(lsu_data( 7, 6),16), ZeroExt(lsu_data( 9, 8),16), ZeroExt(lsu_data(11,10),16), ZeroExt(lsu_data(13,12),16), ZeroExt(lsu_data(15,14),16) ),
    "b100".U -> Cat5( ZeroExt(data_stage1( 1, 0),16), ZeroExt(data_stage1( 3, 2),16), ZeroExt(data_stage1( 5, 4),16), ZeroExt(data_stage1( 7, 6),16), ZeroExt(lsu_data( 1, 0),16) ),
    "b101".U -> Cat5( ZeroExt(data_stage1( 3, 2),16), ZeroExt(data_stage1( 5, 4),16), ZeroExt(data_stage1( 7, 6),16), ZeroExt(lsu_data( 1, 0),16),    ZeroExt(lsu_data( 3, 2),16) ),
    "b110".U -> Cat5( ZeroExt(data_stage1( 5, 4),16), ZeroExt(data_stage1( 7, 6),16), ZeroExt(lsu_data( 1, 0),16),    ZeroExt(lsu_data( 3, 2),16),    ZeroExt(lsu_data( 5, 4),16) ),
    "b111".U -> Cat5( ZeroExt(data_stage1( 7, 6),16), ZeroExt(lsu_data( 1, 0),16),    ZeroExt(lsu_data( 3, 2),16),    ZeroExt(lsu_data( 5, 4),16),    ZeroExt(lsu_data( 7, 6),16) )
  ))
  // Int1
  val int1_src1 = Cat("b000".U(3.W), src1(XLEN-1, 3))
  val int1_src2_stage1 = 0.U(XLEN.W)
  val int1_src2_stage2 = 1.U(XLEN.W)
  val int1_src2 = Mux(state === s_stage2, int1_src2_stage2, int1_src2_stage1)
  val int1_func = LSUOpType.lbu
  val int1_stage2_valid = (src1(2,0) +& io.length_k(2,0)) >= 9.U(4.W)
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

  // ALL
  val loadv_src1 = WireInit(0.U(XLEN.W))
  val loadv_src2 = WireInit(0.U(XLEN.W))
  val loadv_func = WireInit(0.U(7.W))
  val loadv_data = WireInit(0.U((16*5).W))
  val stage2_valid = WireInit(false.B)

  when (isInt16) {
    loadv_src1 := int16_src1
    loadv_src2 := int16_src2
    loadv_func := int16_func
    loadv_data := int16_load_data
    stage2_valid := int16_stage2_valid

  }.elsewhen (isInt8) {
    loadv_src1 := int8_src1
    loadv_src2 := int8_src2
    loadv_func := int8_func
    loadv_data := int8_load_data
    stage2_valid := int8_stage2_valid

  }.elsewhen (isInt4) {
    loadv_src1 := int4_src1
    loadv_src2 := int4_src2
    loadv_func := int4_func
    loadv_data := int4_load_data
    stage2_valid := int4_stage2_valid

  }.elsewhen (isInt2) {
    loadv_src1 := int2_src1
    loadv_src2 := int2_src2
    loadv_func := int2_func
    loadv_data := int2_load_data
    stage2_valid := int2_stage2_valid

  }.elsewhen (isInt1) {
    loadv_src1 := int1_src1
    loadv_src2 := int1_src2
    loadv_func := int1_func
    loadv_data := int1_load_data
    stage2_valid := int1_stage2_valid

  }

  // state reg
  switch (state) {
    is (s_stage1) {
      when ( (isLoadD || isLoadP) && vwidth_valid && stage2_valid && lsu_valid && !lsu_ex ) { state := s_stage2 }
    }
    is (s_stage2) {
      when ( lsu_valid ) { state := s_stage1 }
    }
  }
  when ( (state === s_stage1) && (isLoadD || isLoadP) && vwidth_valid && stage2_valid && lsu_valid && !lsu_ex ) { data_stage1 := lsu_data }

  // data to vrf
  val loadv_data_1 = loadv_data(15, 0)
  val loadv_data_2 = Mux(io.length_k(2,0) >= 2.U, loadv_data(31, 16), 0.U(16.W))
  val loadv_data_3 = Mux(io.length_k(2,0) >= 3.U, loadv_data(47, 32), 0.U(16.W))
  val loadv_data_4 = Mux(io.length_k(2,0) >= 4.U, loadv_data(63, 48), 0.U(16.W))
  val loadv_data_5 = Mux(io.length_k(2,0) >= 5.U, loadv_data(79, 64), 0.U(16.W))
  val loadv_data_main   = Cat5(loadv_data_1, loadv_data_2, loadv_data_3, loadv_data_4, loadv_data_5)
  val loadv_data_kernel = Cat5(loadv_data_1(7, 0), loadv_data_2(7, 0), loadv_data_3(7, 0), loadv_data_4(7, 0), loadv_data_5(7, 0))

  val loadv_valid = Mux(stage2_valid, (state === s_stage2) && lsu_valid || (state === s_stage1) && lsu_valid && lsu_ex, lsu_valid)

  io.loadv.valid := (isLoadD || isLoadP) && vwidth_valid || isLoadW
  io.loadv.src1 := Mux(isLoadD || isLoadP, loadv_src1, src1)
  io.loadv.src2 := Mux(isLoadD || isLoadP, loadv_src2, io.imm)
  io.loadv.func := Mux(isLoadD || isLoadP, loadv_func, LSUOpType.ld)
  val lv_valid = Mux(isLoadW, lsu_valid, Mux((isLoadD || isLoadP) && vwidth_valid, loadv_valid, true.B))

  vreg_mdu.io.vaddr       := io.vec_addr
  vreg_mdu.io.vtag        := io.vtag
  vreg_mdu.io.vop         := func(2,0)
  vreg_mdu.io.k           := io.length_k
  vreg_mdu.io.vwen        := lv_valid && ((isLoadD || isLoadP) && vwidth_valid || isLoadW) && !lsu_ex
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
}