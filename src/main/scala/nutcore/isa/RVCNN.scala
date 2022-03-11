/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package nutcore

import chisel3._
import chisel3.util._

object RV64CNNInstr extends HasInstrType {
  def CONV   = BitPat("b00000000_????_00000_100_?????_0001011")
  def POOL   = BitPat("b00000000_????_000??_110_?????_0001011")
  def ACT    = BitPat("b0000000_?????_?????_111_?????_0001011")

  def LOADV_D      = BitPat("b00000_???_????_?????_010_?????_0001011")
  def LOADV_P      = BitPat("b00000_???_????_?????_011_?????_0001011")
  def LOADV_W      = BitPat("b?????_???_????_?????_000_00000_0001011")

  val table = Array(
    CONV          -> List(InstrU, FuType.cnn, CNNOpType.conv),
    POOL          -> List(InstrU, FuType.cnn, CNNOpType.pool),
    ACT           -> List(InstrR, FuType.cnn, CNNOpType.act ),

    LOADV_D       -> List(InstrI, FuType.cnn, CNNOpType.loadv_d),
    LOADV_P       -> List(InstrI, FuType.cnn, CNNOpType.loadv_p),
    LOADV_W       -> List(InstrI, FuType.cnn, CNNOpType.loadv_w)
  )
}

object RVCNNInstr extends HasNutCoreParameter {
  val table = RV64CNNInstr.table
}
