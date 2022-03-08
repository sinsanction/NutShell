package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import top.Settings

object CNNOpType {
  def conv  = "b0000000".U
  def pool  = "b0000001".U
  def act   = "b0000010".U

  def loadv_d = "b0001000".U
  def loadv_p = "b0001001".U
  def loadv_w = "b0001100".U

  def isLoadV(func: UInt) = func(3)
}

class CNNtoLSUIO extends NutCoreBundle {
  val valid = Output(Bool())
  val src1 = Output(UInt(XLEN.W))
  val src2 = Output(UInt(XLEN.W))
  val func = Output(FuOpType())

  val load_valid = Input(Bool())
  val load_data = Input(UInt(XLEN.W))
}

class CNNIO extends FunctionUnitIO {
  val vtag = Input(UInt(3.W))
  val length_k = Input(UInt(4.W))
  val algorithm = Input(UInt(1.W))
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
  
}