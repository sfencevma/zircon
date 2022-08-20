/*
 * Copyright (c) 2022 Lyn
 * Skew is licensed under Mulan PubL v2.
 * You can use this software according to the terms and conditions of the Mulan PubL v2.
 * You may obtain a copy of Mulan PubL v2 at:
 *         http://license.coscl.org.cn/MulanPubL-2.0
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PubL v2 for more details.
 */
package zircon.common

import chisel3._
import chisel3.util._

trait ScalarOpConstants {
  //  Issue port
  def PORT_SZ    =  7
  def ALU_PORT   =  0
  def BRU_PORT   =  1
  def MUL_PORT   =  2
  def DIV_PORT   =  3
  def LSU_PORT   =  4
  def FPU_PORT   =  5
  def FDIV_PORT  =  6
  def PORT_ALU   = (1 << ALU_PORT).U(PORT_SZ.W)
  def PORT_BRU   = (1 << BRU_PORT).U(PORT_SZ.W)
  def PORT_MUL   = (1 << MUL_PORT).U(PORT_SZ.W)
  def PORT_DIV   = (1 << DIV_PORT).U(PORT_SZ.W)
  def PORT_LSU   = (1 << LSU_PORT).U(PORT_SZ.W)
  def PORT_FPU   = (1 << FPU_PORT).U(PORT_SZ.W)
  def PORT_FDIV  = (1 << FDIV_PORT).U(PORT_SZ.W)

  //  Immediate format
  def IS_SZ  = 3
  def IS_I   = 0.U(IS_SZ.W)  // I-Type  (LD,ALU)
  def IS_S   = 1.U(IS_SZ.W)  // S-Type  (ST)
  def IS_B   = 2.U(IS_SZ.W)  // B-Type (BR)
  def IS_U   = 3.U(IS_SZ.W)  // U-Type  (LUI/AUIPC)
  def IS_J   = 4.U(IS_SZ.W)  // J-Type (J/JAL)
  def IS_X   = BitPat("b???")

  //  Register type
  def RT_SZ      = 3
  def RT_X       = 0.U(RT_SZ.W)
  def RT_FIX     = 1.U(RT_SZ.W)
  def RT_FP      = 2.U(RT_SZ.W)
  def RT_IMM     = 3.U(RT_SZ.W)
  def RT_PC      = 4.U(RT_SZ.W)
  def RT_CSR     = 5.U(RT_SZ.W)

  //  Data Width
  def DW_SZ      = 3
  def DW_X       = 0.U(DW_SZ.W)
  def DW_8       = 1.U(DW_SZ.W)
  def DW_16      = 2.U(DW_SZ.W)
  def DW_32      = 3.U(DW_SZ.W)
  def DW_64      = 4.U(DW_SZ.W)

  //  Micro-op opcodes
  def UOP_SZ      =  7
  def UOP_UND     =  0.U(UOP_SZ.W)
  def UOP_NOP     =  1.U(UOP_SZ.W)
  def UOP_EXCEP   =  2.U(UOP_SZ.W)
  def UOP_LUI     =  3.U(UOP_SZ.W)
  def UOP_AUIPC   =  4.U(UOP_SZ.W)
  def UOP_JAL     =  5.U(UOP_SZ.W)
  def UOP_JALR    =  6.U(UOP_SZ.W)
  def UOP_BEQ     =  7.U(UOP_SZ.W)
  def UOP_BNE     =  8.U(UOP_SZ.W)
  def UOP_BLT     =  9.U(UOP_SZ.W)
  def UOP_BGE     = 10.U(UOP_SZ.W)
  def UOP_ADD     = 12.U(UOP_SZ.W)
  def UOP_SUB     = 13.U(UOP_SZ.W)
  def UOP_SLT     = 14.U(UOP_SZ.W)
  def UOP_XOR     = 16.U(UOP_SZ.W)
  def UOP_OR      = 17.U(UOP_SZ.W)
  def UOP_AND     = 18.U(UOP_SZ.W)
  def UOP_SLL     = 19.U(UOP_SZ.W)
  def UOP_SRL     = 20.U(UOP_SZ.W)
  def UOP_SRA     = 21.U(UOP_SZ.W)
  def UOP_FENCE   = 22.U(UOP_SZ.W)
  def UOP_FENCEI  = 23.U(UOP_SZ.W)
  def UOP_PAUSE   = 24.U(UOP_SZ.W)
  def UOP_ECALL   = 25.U(UOP_SZ.W)
  def UOP_EBREAK  = 26.U(UOP_SZ.W)
  def UOP_SRET    = 27.U(UOP_SZ.W)
  def UOP_URET    = 28.U(UOP_SZ.W)
  def UOP_MRET    = 29.U(UOP_SZ.W)
  def UOP_WFI     = 30.U(UOP_SZ.W)
  def UOP_SFENCE  = 31.U(UOP_SZ.W)
  def UOP_LOAD    = 32.U(UOP_SZ.W)
  def UOP_STORE   = 33.U(UOP_SZ.W)
  def UOP_MUL     = 34.U(UOP_SZ.W)
  def UOP_MULSU   = 35.U(UOP_SZ.W)
  def UOP_MULH    = 36.U(UOP_SZ.W)
  def UOP_MULHSU  = 37.U(UOP_SZ.W)
  def UOP_DIV     = 38.U(UOP_SZ.W)
  def UOP_REM     = 39.U(UOP_SZ.W)
  def UOP_AMO     = 40.U(UOP_SZ.W)
  def UOP_CLASS_S = 41.U(UOP_SZ.W)
  def UOP_MV_S_X  = 42.U(UOP_SZ.W)
  def UOP_MV_X_S  = 43.U(UOP_SZ.W)
  def UOP_CVT_S_X = 44.U(UOP_SZ.W)
  def UOP_CVT_X_S = 45.U(UOP_SZ.W)
  def UOP_EQ_S    = 46.U(UOP_SZ.W)
  def UOP_LT_S    = 47.U(UOP_SZ.W)
  def UOP_LE_S    = 48.U(UOP_SZ.W)
  def UOP_SGNJ_S  = 49.U(UOP_SZ.W)
  def UOP_SGNJN_S = 50.U(UOP_SZ.W)
  def UOP_SGNJX_S = 51.U(UOP_SZ.W)
  def UOP_MAX_S   = 52.U(UOP_SZ.W)
  def UOP_MIN_S   = 53.U(UOP_SZ.W)
  def UOP_ADD_S   = 54.U(UOP_SZ.W)
  def UOP_SUB_S   = 55.U(UOP_SZ.W)
  def UOP_MUL_S   = 56.U(UOP_SZ.W)
  def UOP_DIV_S   = 57.U(UOP_SZ.W)
  def UOP_MADD_S  = 58.U(UOP_SZ.W)
  def UOP_MSUB_S  = 59.U(UOP_SZ.W)
  def UOP_NMADD_S = 60.U(UOP_SZ.W)
  def UOP_NMSUB_S = 61.U(UOP_SZ.W)
  def UOP_CLASS_D = 62.U(UOP_SZ.W)
  def UOP_MV_D_X  = 63.U(UOP_SZ.W)
  def UOP_MV_X_D  = 64.U(UOP_SZ.W)
  def UOP_CVT_S_D = 65.U(UOP_SZ.W)
  def UOP_CVT_D_S = 66.U(UOP_SZ.W)
  def UOP_CVT_D_X = 67.U(UOP_SZ.W)
  def UOP_CVT_X_D = 68.U(UOP_SZ.W)
  def UOP_EQ_D    = 69.U(UOP_SZ.W)
  def UOP_LE_D    = 70.U(UOP_SZ.W)
  def UOP_LT_D    = 71.U(UOP_SZ.W)
  def UOP_SGNJ_D  = 72.U(UOP_SZ.W)
  def UOP_SGNJN_D = 73.U(UOP_SZ.W)
  def UOP_SGNJX_D = 74.U(UOP_SZ.W)
  def UOP_MAX_D   = 75.U(UOP_SZ.W)
  def UOP_MIN_D   = 76.U(UOP_SZ.W)
  def UOP_ADD_D   = 77.U(UOP_SZ.W)
  def UOP_SUB_D   = 78.U(UOP_SZ.W)
  def UOP_MUL_D   = 79.U(UOP_SZ.W)
  def UOP_DIV_D   = 80.U(UOP_SZ.W)
  def UOP_MADD_D  = 81.U(UOP_SZ.W)
  def UOP_MSUB_D  = 82.U(UOP_SZ.W)
  def UOP_NMADD_D = 83.U(UOP_SZ.W)
  def UOP_NMSUB_D = 84.U(UOP_SZ.W)
  def UOP_SQRT_S  = 85.U(UOP_SZ.W)
  def UOP_SQRT_D  = 86.U(UOP_SZ.W)
  def UOP_DRET    = 87.U(UOP_SZ.W)
  def UOP_SYNC    = 88.U(UOP_SZ.W)
  def UOP_SET     = 89.U(UOP_SZ.W)
  def UOP_CLR     = 90.U(UOP_SZ.W)
  def UOP_XCHG    = 91.U(UOP_SZ.W)
  def UOP_MIN     = 92.U(UOP_SZ.W)
  def UOP_MAX     = 93.U(UOP_SZ.W)

  def INST_SYNC   = "b00000000000000000000000000001011".U(32.W)
  def CUSTOM_SYNC = BitPat("b00000000000000000000000000001011")

  //  Wakeup
  def WAKEUP_SZ  = 6
  def WAKEUP_UND = 0.U(WAKEUP_SZ.W)

  //  Load Track
  def TRACK_SZ = 3
}

trait MemoryOpConstants {
  //  Memory ops
  def M_SZ      =  4
  def M_UND     =  1.U(M_SZ.W)
  def M_XRD     =  2.U(M_SZ.W)
  def M_XWR     =  3.U(M_SZ.W)
  def M_PWR     =  4.U(M_SZ.W)
  def M_XA_SWAP =  5.U(M_SZ.W)
  def M_XLR     =  6.U(M_SZ.W)
  def M_XSC     =  7.U(M_SZ.W)
  def M_XA_ADD  =  8.U(M_SZ.W)
  def M_XA_XOR  =  9.U(M_SZ.W)
  def M_XA_OR   = 10.U(M_SZ.W)
  def M_XA_AND  = 11.U(M_SZ.W)
  def M_XA_MIN  = 12.U(M_SZ.W)
  def M_XA_MAX  = 13.U(M_SZ.W)
  def M_SFENCE  = 14.U(M_SZ.W)
  def M_FENCE   = 15.U(M_SZ.W)
}