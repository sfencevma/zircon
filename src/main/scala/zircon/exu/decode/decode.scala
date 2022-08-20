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
package zircon.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util.uintToBitPat
import zircon.common._
import zircon.frontend._
import zircon.utils._

abstract trait DecodeConstants extends ScalarOpConstants with MemoryOpConstants {
  val decode_default: List[BitPat] = {
    //    is val inst             data width                                                      rd val
    //    |   micro-code          |       usign                                                   |     rd type
    //    |   |                   |       |     rs1 val                                           |     |       wakeup
    //    |   |                   |       |     |       rs1 type                                  |     |       |           mem cmd
    //    |   |                   |       |     |       |       rs2 val                           |     |       |           |       imm select
    //    |   |         port      |       |     |       |       |       rs2 type                  |     |       |           |       |   is_amo
    //    |   |         |         |       |     |       |       |       |       rs3 val           |     |       |           |       |   |
    //    |   |         |         |       |     |       |       |       |       |       rs3 type  |     |       |           |       |   |
    //    |   |         |         |       |     |       |       |       |       |       |         |     |       |           |       |   |
    List( N,  UOP_UND,  PORT_ALU, DW_X,   N,    N,      RT_X,   N,      RT_X,   N,      RT_X,     N,    RT_X,   1.U, M_UND,  IS_X, N)
  }
  val table: Array[(BitPat, List[BitPat])]
}

object IDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    LUI -> List(Y, UOP_LUI, PORT_ALU, DW_64, N, N, RT_X, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_U, N),
    AUIPC -> List(Y, UOP_ADD, PORT_ALU, DW_64, N, N, RT_PC, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_U, N),

    //  Branch
    JAL -> List(Y, UOP_JAL, PORT_BRU, DW_64, N, N, RT_PC, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_J, N),
    JALR -> List(Y, UOP_JALR, PORT_BRU, DW_64, N, Y, RT_FIX, N, RT_IMM, Y, RT_PC, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    BEQ -> List(Y, UOP_BEQ, PORT_BRU, DW_64, N, Y, RT_FIX, Y, RT_FIX, Y, RT_IMM, N, RT_X, 1.U, M_UND, IS_B, N),
    BNE -> List(Y, UOP_BNE, PORT_BRU, DW_64, N, Y, RT_FIX, Y, RT_FIX, Y, RT_IMM, N, RT_X, 1.U, M_UND, IS_B, N),
    BLT -> List(Y, UOP_BLT, PORT_BRU, DW_64, N, Y, RT_FIX, Y, RT_FIX, Y, RT_IMM, N, RT_X, 1.U, M_UND, IS_B, N),
    BGE -> List(Y, UOP_BGE, PORT_BRU, DW_64, N, Y, RT_FIX, Y, RT_FIX, Y, RT_IMM, N, RT_X, 1.U, M_UND, IS_B, N),
    BLTU -> List(Y, UOP_BLT, PORT_BRU, DW_64, Y, Y, RT_FIX, Y, RT_FIX, Y, RT_IMM, N, RT_X, 1.U, M_UND, IS_B, N),
    BGEU -> List(Y, UOP_BGE, PORT_BRU, DW_64, Y, Y, RT_FIX, Y, RT_FIX, Y, RT_IMM, N, RT_X, 1.U, M_UND, IS_B, N),

    //  Load and Store
    LB -> List(Y, UOP_LOAD, PORT_LSU, DW_8, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 3.U, M_XRD, IS_I, N),
    LH -> List(Y, UOP_LOAD, PORT_LSU, DW_16, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 3.U, M_XRD, IS_I, N),
    LW -> List(Y, UOP_LOAD, PORT_LSU, DW_32, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 3.U, M_XRD, IS_I, N),
    LBU -> List(Y, UOP_LOAD, PORT_LSU, DW_8, Y, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 3.U, M_XRD, IS_I, N),
    LHU -> List(Y, UOP_LOAD, PORT_LSU, DW_16, Y, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 3.U, M_XRD, IS_I, N),
    LWU -> List(Y, UOP_LOAD, PORT_LSU, DW_32, Y, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 3.U, M_XRD, IS_I, N),
    LD -> List(Y, UOP_LOAD, PORT_LSU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 3.U, M_XRD, IS_I, N),
    SB -> List(Y, UOP_STORE, PORT_LSU, DW_8, N, Y, RT_FIX, N, RT_IMM, Y, RT_FIX, Y, RT_FIX, 3.U, M_XWR, IS_S, N),
    SH -> List(Y, UOP_STORE, PORT_LSU, DW_16, N, Y, RT_FIX, N, RT_IMM, Y, RT_FIX, Y, RT_FIX, 1.U, M_XWR, IS_S, N),
    SW -> List(Y, UOP_STORE, PORT_LSU, DW_32, N, Y, RT_FIX, N, RT_IMM, Y, RT_FIX, Y, RT_FIX, 1.U, M_XWR, IS_S, N),
    SD -> List(Y, UOP_STORE, PORT_LSU, DW_64, N, Y, RT_FIX, N, RT_IMM, Y, RT_FIX, Y, RT_FIX, 1.U, M_XWR, IS_S, N),

    //  OP-Imm
    ADDI -> List(Y, UOP_ADD, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SLTI -> List(Y, UOP_SLT, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SLTIU -> List(Y, UOP_SLT, PORT_ALU, DW_64, Y, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    XORI -> List(Y, UOP_XOR, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    ORI -> List(Y, UOP_OR, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    ANDI -> List(Y, UOP_AND, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SLLI -> List(Y, UOP_SLL, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SRLI -> List(Y, UOP_SRL, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SRAI -> List(Y, UOP_SRA, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    ADDIW -> List(Y, UOP_ADD, PORT_ALU, DW_32, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SLLIW -> List(Y, UOP_SLL, PORT_ALU, DW_32, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SRLIW -> List(Y, UOP_SRL, PORT_ALU, DW_32, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SRAIW -> List(Y, UOP_SRA, PORT_ALU, DW_32, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),

    //  OP
    ADD -> List(Y, UOP_ADD, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SUB -> List(Y, UOP_SUB, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SLL -> List(Y, UOP_SLL, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SLT -> List(Y, UOP_SLT, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SLTU -> List(Y, UOP_SLT, PORT_ALU, DW_64, Y, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    XOR -> List(Y, UOP_XOR, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SRL -> List(Y, UOP_SRL, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),
    SRA -> List(Y, UOP_SRA, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    OR -> List(Y, UOP_OR, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    AND -> List(Y, UOP_AND, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    ADDW -> List(Y, UOP_ADD, PORT_ALU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SUBW -> List(Y, UOP_SUB, PORT_ALU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SLLW -> List(Y, UOP_SLL, PORT_ALU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    SRLW -> List(Y, UOP_SRL, PORT_ALU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),
    SRAW -> List(Y, UOP_SRA, PORT_ALU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),

    //  Fence
    FENCE -> List(Y, UOP_FENCE, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),
    FENCE_I -> List(Y, UOP_FENCE, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),

    //  System
    ECALL -> List(Y, UOP_ECALL, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),
    EBREAK -> List(Y, UOP_EBREAK, PORT_ALU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),
    SRET -> List(Y, UOP_SRET, PORT_ALU, DW_64, N, N, RT_X, N, RT_X, N, RT_X, N, RT_X, 1.U, M_UND, IS_X, N),
    MRET -> List(Y, UOP_MRET, PORT_ALU, DW_64, N, N, RT_X, N, RT_X, N, RT_X, N, RT_X, 1.U, M_UND, IS_X, N),
    WFI -> List(Y, UOP_WFI, PORT_ALU, DW_64, N, N, RT_X, N, RT_X, N, RT_X, N, RT_X, 1.U, M_UND, IS_X, N),
    SFENCE_VMA
      -> List(Y, UOP_SFENCE, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, N, RT_X, 1.U, M_UND, IS_X, N),

    CUSTOM_SYNC -> List(Y, UOP_SYNC, PORT_ALU, DW_64, N, N, RT_X, N, RT_X, N, RT_X, N, RT_X, 0.U, M_UND, IS_X, N)
  )
}

object ZicsrDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    CSRRW -> List(Y, UOP_XCHG, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_CSR, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    CSRRS -> List(Y, UOP_SET, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_CSR, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    CSRRC -> List(Y, UOP_CLR, PORT_ALU, DW_64, N, Y, RT_FIX, Y, RT_CSR, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    CSRRWI -> List(Y, UOP_XCHG, PORT_ALU, DW_64, Y, N, RT_IMM, Y, RT_CSR, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    CSRRSI -> List(Y, UOP_SET, PORT_ALU, DW_64, Y, N, RT_IMM, Y, RT_CSR, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
    CSRRCI -> List(Y, UOP_CLR, PORT_ALU, DW_64, Y, N, RT_IMM, Y, RT_CSR, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_I, N),
  )
}


object MDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    MUL -> List(Y, UOP_MUL, PORT_MUL, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),
    MULH -> List(Y, UOP_MULH, PORT_MUL, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),
    MULHSU -> List(Y, UOP_MULHSU, PORT_MUL, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),
    MULHU -> List(Y, UOP_MULH, PORT_MUL, DW_64, Y, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),
    MULW -> List(Y, UOP_MUL, PORT_MUL, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_UND, IS_X, N),
    DIV -> List(Y, UOP_DIV, PORT_DIV, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 65.U, M_UND, IS_X, N),
    DIVU -> List(Y, UOP_DIV, PORT_DIV, DW_64, Y, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 65.U, M_UND, IS_X, N),
    DIVW -> List(Y, UOP_DIV, PORT_DIV, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 65.U, M_UND, IS_X, N),
    DIVUW -> List(Y, UOP_DIV, PORT_DIV, DW_32, Y, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 65.U, M_UND, IS_X, N),
    REM -> List(Y, UOP_REM, PORT_DIV, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 65.U, M_UND, IS_X, N),
    REMU -> List(Y, UOP_REM, PORT_DIV, DW_64, Y, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 65.U, M_UND, IS_X, N),
    REMW -> List(Y, UOP_REM, PORT_DIV, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 65.U, M_UND, IS_X, N),
    REMUW -> List(Y, UOP_REM, PORT_DIV, DW_32, Y, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 65.U, M_UND, IS_X, N),
  )
}

object ADecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    LR_W -> List(Y, UOP_LOAD, PORT_LSU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 3.U, M_XLR, IS_X, Y),
    SC_W -> List(Y, UOP_STORE, PORT_LSU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XSC, IS_X, Y),
    LR_D -> List(Y, UOP_LOAD, PORT_LSU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 3.U, M_XLR, IS_X, Y),
    SC_D -> List(Y, UOP_STORE, PORT_LSU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XSC, IS_X, Y),
    AMOSWAP_W -> List(Y, UOP_XCHG, PORT_LSU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_SWAP, IS_X, Y),
    AMOADD_W -> List(Y, UOP_ADD, PORT_LSU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_ADD, IS_X, Y),
    AMOXOR_W -> List(Y, UOP_XOR, PORT_LSU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_XOR, IS_X, Y),
    AMOAND_W -> List(Y, UOP_AND, PORT_LSU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_AND, IS_X, Y),
    AMOOR_W -> List(Y, UOP_OR, PORT_LSU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_OR, IS_X, Y),
    AMOMIN_W -> List(Y, UOP_MIN, PORT_LSU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_MIN, IS_X, Y),
    AMOMAX_W -> List(Y, UOP_MAX, PORT_LSU, DW_32, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_MAX, IS_X, Y),
    AMOMINU_W -> List(Y, UOP_MIN, PORT_LSU, DW_32, Y, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_MIN, IS_X, Y),
    AMOMAXU_W -> List(Y, UOP_MAX, PORT_LSU, DW_32, Y, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_MAX, IS_X, Y),
    AMOSWAP_D -> List(Y, UOP_XCHG, PORT_LSU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_SWAP, IS_X, Y),
    AMOADD_D -> List(Y, UOP_ADD, PORT_LSU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_ADD, IS_X, Y),
    AMOXOR_D -> List(Y, UOP_XOR, PORT_LSU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_XOR, IS_X, Y),
    AMOAND_D -> List(Y, UOP_AND, PORT_LSU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_AND, IS_X, Y),
    AMOOR_D -> List(Y, UOP_OR, PORT_LSU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_OR, IS_X, Y),
    AMOMIN_D -> List(Y, UOP_MIN, PORT_LSU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_MIN, IS_X, Y),
    AMOMAX_D -> List(Y, UOP_MAX, PORT_LSU, DW_64, N, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_MAX, IS_X, Y),
    AMOMINU_D -> List(Y, UOP_MIN, PORT_LSU, DW_64, Y, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_MIN, IS_X, Y),
    AMOMAXU_D -> List(Y, UOP_MAX, PORT_LSU, DW_64, Y, Y, RT_FIX, Y, RT_FIX, N, RT_X, Y, RT_FIX, 1.U, M_XA_MAX, IS_X, Y),
  )
}


object FDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(

    FLW -> List(Y, UOP_LOAD, PORT_LSU, DW_32, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FP, 3.U, M_XRD, IS_I, N),
    FSW -> List(Y, UOP_STORE, PORT_LSU, DW_32, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FP, 1.U, M_XWR, IS_S, N),
    FLD -> List(Y, UOP_LOAD, PORT_LSU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FP, 3.U, M_XRD, IS_I, N),
    FSD -> List(Y, UOP_STORE, PORT_LSU, DW_64, N, Y, RT_FIX, N, RT_IMM, N, RT_X, Y, RT_FP, 1.U, M_XWR, IS_S, N),

    FCLASS_S -> List(Y, UOP_CLASS_S, PORT_FPU, DW_32, N, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),
    FCLASS_D -> List(Y, UOP_CLASS_D, PORT_FPU, DW_64, N, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),

    FMV_W_X -> List(Y, UOP_MV_S_X, PORT_FPU, DW_32, N, Y, RT_FIX, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FMV_D_X -> List(Y, UOP_MV_D_X, PORT_FPU, DW_64, N, Y, RT_FIX, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FMV_X_W -> List(Y, UOP_MV_X_S, PORT_FPU, DW_32, N, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),
    FMV_X_D -> List(Y, UOP_MV_X_D, PORT_FPU, DW_64, N, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),

    FSGNJ_S -> List(Y, UOP_SGNJ_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FSGNJ_D -> List(Y, UOP_SGNJ_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FSGNJN_S -> List(Y, UOP_SGNJN_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FSGNJN_D -> List(Y, UOP_SGNJN_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FSGNJX_S -> List(Y, UOP_SGNJX_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FSGNJX_D -> List(Y, UOP_SGNJX_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),

    FCVT_S_D -> List(Y, UOP_CVT_S_D, PORT_FPU, DW_32, N, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FCVT_D_S -> List(Y, UOP_CVT_D_S, PORT_FPU, DW_64, N, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FCVT_W_S -> List(Y, UOP_CVT_X_S, PORT_FPU, DW_32, N, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),
    FCVT_W_D -> List(Y, UOP_CVT_X_D, PORT_FPU, DW_32, N, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),
    FCVT_S_W -> List(Y, UOP_CVT_S_X, PORT_FPU, DW_32, N, Y, RT_FIX, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FCVT_D_W -> List(Y, UOP_CVT_D_X, PORT_FPU, DW_64, N, Y, RT_FIX, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FCVT_WU_S -> List(Y, UOP_CVT_X_S, PORT_FPU, DW_32, Y, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),
    FCVT_WU_D -> List(Y, UOP_CVT_X_D, PORT_FPU, DW_32, Y, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),
    FCVT_S_WU -> List(Y, UOP_CVT_S_X, PORT_FPU, DW_32, Y, Y, RT_FIX, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FCVT_D_WU -> List(Y, UOP_CVT_D_X, PORT_FPU, DW_64, Y, Y, RT_FIX, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FCVT_L_S -> List(Y, UOP_CVT_X_S, PORT_FPU, DW_64, N, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),
    FCVT_L_D -> List(Y, UOP_CVT_X_D, PORT_FPU, DW_64, N, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),
    FCVT_LU_S -> List(Y, UOP_CVT_X_S, PORT_FPU, DW_64, Y, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),
    FCVT_LU_D -> List(Y, UOP_CVT_X_D, PORT_FPU, DW_64, Y, Y, RT_FP, N, RT_X, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_I, N),
    FCVT_S_L -> List(Y, UOP_CVT_S_X, PORT_FPU, DW_32, N, Y, RT_FIX, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FCVT_D_L -> List(Y, UOP_CVT_D_X, PORT_FPU, DW_64, N, Y, RT_FIX, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FCVT_S_LU -> List(Y, UOP_CVT_D_X, PORT_FPU, DW_32, Y, Y, RT_FIX, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),
    FCVT_D_LU -> List(Y, UOP_CVT_D_X, PORT_FPU, DW_64, Y, Y, RT_FIX, N, RT_X, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_I, N),

    FMADD_S -> List(Y, UOP_MADD_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, Y, RT_FP, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FMSUB_S -> List(Y, UOP_MSUB_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, Y, RT_FP, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FNMSUB_S -> List(Y, UOP_NMSUB_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, Y, RT_FP, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FNMADD_S -> List(Y, UOP_NMADD_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, Y, RT_FP, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FMADD_D -> List(Y, UOP_MADD_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, Y, RT_FP, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FMSUB_D -> List(Y, UOP_MSUB_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, Y, RT_FP, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FNMSUB_D -> List(Y, UOP_NMSUB_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, Y, RT_FP, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FNMADD_D -> List(Y, UOP_NMADD_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, Y, RT_FP, Y, RT_FP, 0.U, M_UND, IS_X, N),

    FADD_S -> List(Y, UOP_ADD, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FSUB_S -> List(Y, UOP_SUB, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FMUL_S -> List(Y, UOP_MUL, PORT_FDIV, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FDIV_S -> List(Y, UOP_DIV, PORT_FDIV, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FSQRT_S -> List(Y, UOP_SQRT_S, PORT_FDIV, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FADD_D -> List(Y, UOP_ADD_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FSUB_D -> List(Y, UOP_SUB_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FMUL_D -> List(Y, UOP_MUL_D, PORT_FDIV, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FDIV_D -> List(Y, UOP_DIV_D, PORT_FDIV, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FSQRT_D -> List(Y, UOP_SQRT_D, PORT_FDIV, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),

    FMIN_S -> List(Y, UOP_MIN_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FMAX_S -> List(Y, UOP_MAX_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FMIN_D -> List(Y, UOP_MIN_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),
    FMAX_D -> List(Y, UOP_MAX_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FP, 0.U, M_UND, IS_X, N),

    FEQ_S -> List(Y, UOP_EQ_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_X, N),
    FLT_S -> List(Y, UOP_LT_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_X, N),
    FLE_S -> List(Y, UOP_LE_S, PORT_FPU, DW_32, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_X, N),

    FEQ_D -> List(Y, UOP_EQ_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_X, N),
    FLT_D -> List(Y, UOP_LT_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_X, N),
    FLE_D -> List(Y, UOP_LE_D, PORT_FPU, DW_64, N, Y, RT_FP, Y, RT_FP, N, RT_X, Y, RT_FIX, 0.U, M_UND, IS_X, N),
  )
}


class CtrlSigs extends Bundle with ScalarOpConstants with MemoryOpConstants {
  val legal       = Bool()
  val uopc        = UInt(UOP_SZ.W)
  val port        = UInt(PORT_SZ.W)
  val dw          = UInt(DW_SZ.W)
  val usign       = Bool()

  //  Register information
  val lrs1_vld    = Bool()
  val lrs1_type   = UInt(RT_SZ.W)
  val lrs2_vld    = Bool()
  val lrs2_type   = UInt(RT_SZ.W)
  val lrs3_vld    = Bool()
  val lrs3_type   = UInt(RT_SZ.W)
  val ldst_vld    = Bool()
  val ldst_type   = UInt(RT_SZ.W)
  val wakeup      = UInt(WAKEUP_SZ.W)
  val mem_cmd     = UInt(M_SZ.W)
  val imm_sel     = UInt(IS_SZ.W)

  val is_amo      = Bool()

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, IDecode.decode_default, table)
    val sigs = Seq(
      legal, uopc, port, dw, usign, lrs1_vld, lrs1_type,
      lrs2_vld, lrs2_type, lrs3_vld, lrs3_type, ldst_vld, ldst_type, wakeup, mem_cmd, imm_sel, is_amo
    )
    sigs zip decoder map { case (s, d) => s := d }
    this
  }
}

class RVCExpander(x: UInt, xLen: Int) {
  def rs1p = Cat(1.U(2.W), x(9,7))
  def rs2p = Cat(1.U(2.W), x(4,2))
  def rs2 = x(6,2)
  def rd = x(11,7)
  def addi4spnImm = Cat(x(10,7), x(12,11), x(5), x(6), 0.U(2.W))
  def lwImm = Cat(x(5), x(12,10), x(6), 0.U(2.W))
  def ldImm = Cat(x(6,5), x(12,10), 0.U(3.W))
  def lwspImm = Cat(x(3,2), x(12), x(6,4), 0.U(2.W))
  def ldspImm = Cat(x(4,2), x(12), x(6,5), 0.U(3.W))
  def swspImm = Cat(x(8,7), x(12,9), 0.U(2.W))
  def sdspImm = Cat(x(9,7), x(12,10), 0.U(3.W))
  def luiImm = Cat(Fill(15, x(12)), x(6,2), 0.U(12.W))
  def addi16spImm = Cat(Fill(3, x(12)), x(4,3), x(5), x(2), x(6), 0.U(4.W))
  def addiImm = Cat(Fill(7, x(12)), x(6,2))
  def jImm = Cat(Fill(10, x(12)), x(8), x(10,9), x(6), x(7), x(2), x(11), x(5,3), 0.U(1.W))
  def bImm = Cat(Fill(5, x(12)), x(6,5), x(2), x(11,10), x(4,3), 0.U(1.W))
  def shamt = Cat(x(12), x(6,2))
  def x0 = 0.U(5.W)
  def ra = 1.U(5.W)
  def sp = 2.U(5.W)

  def q0 = {
    def addi4spn = {
      val opc = Mux(x(12,5).orR, 0x13.U(7.W), 0x1F.U(7.W))
      Cat(addi4spnImm, sp, 0.U(3.W), rs2p, opc)
    }
    def ld = Cat(ldImm, rs1p, 3.U(3.W), rs2p, 0x03.U(7.W))
    def lw = Cat(lwImm, rs1p, 2.U(3.W), rs2p, 0x03.U(7.W))
    def fld = Cat(ldImm, rs1p, 3.U(3.W), rs2p, 0x07.U(7.W))
    def flw = {
      if (xLen == 32) Cat(lwImm, rs1p, 2.U(3.W), rs2p, 0x07.U(7.W))
      else ld
    }
    def unimp = Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x3F.U(7.W))
    def sd = Cat(ldImm >> 5, rs2p, rs1p, 3.U(3.W), ldImm(4,0), 0x23.U(7.W))
    def sw = Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x23.U(7.W))
    def fsd = Cat(ldImm >> 5, rs2p, rs1p, 3.U(3.W), ldImm(4,0), 0x27.U(7.W))
    def fsw = {
      if (xLen == 32) Cat(lwImm >> 5, rs2p, rs1p, 2.U(3.W), lwImm(4,0), 0x27.U(7.W))
      else sd
    }
    Seq(addi4spn, fld, lw, flw, unimp, fsd, sw, fsw)
  }

  def q1 = {
    def addi = Cat(addiImm, rd, 0.U(3.W), rd, 0x13.U(7.W))
    def addiw = {
      val opc = Mux(rd.orR, 0x1B.U(7.W), 0x1F.U(7.W))
      Cat(addiImm, rd, 0.U(3.W), rd, opc)
    }
    def jal = {
      if (xLen == 32) Cat(jImm(20), jImm(10,1), jImm(11), jImm(19,12), ra, 0x6F.U(7.W))
      else addiw
    }
    def li = Cat(addiImm, x0, 0.U(3.W), rd, 0x13.U(7.W))
    def addi16sp = {
      val opc = Mux(addiImm.orR, 0x13.U(7.W), 0x1F.U(7.W))
      Cat(addi16spImm, rd, 0.U(3.W), rd, opc)
    }
    def lui = {
      val opc = Mux(addiImm.orR, 0x37.U(7.W), 0x3F.U(7.W))
      val me = Cat(luiImm(31,12), rd, opc)
      Mux(rd === x0 || rd === sp, addi16sp, me)
    }
    def j = Cat(jImm(20), jImm(10,1), jImm(11), jImm(19,12), x0, 0x6F.U(7.W))
    def beqz = Cat(bImm(12), bImm(10,5), x0, rs1p, 0.U(3.W), bImm(4,1), bImm(11), 0x63.U(7.W))
    def bnez = Cat(bImm(12), bImm(10,5), x0, rs1p, 1.U(3.W), bImm(4,1), bImm(11), 0x63.U(7.W))
    def arith = {
      def srli = Cat(shamt, rs1p, 5.U(3.W), rs1p, 0x13.U(7.W))
      def srai = srli | (1 << 30).U
      def andi = Cat(addiImm, rs1p, 7.U(3.W), rs1p, 0x13.U(7.W))
      def rtype = {
        val funct = VecInit(Seq(0.U, 4.U, 6.U, 7.U, 0.U, 0.U, 2.U, 3.U))(Cat(x(12), x(6,5)))
        val sub = Mux(x(6,5) === 0.U, (1 << 30).U, 0.U)
        val opc = Mux(x(12), 0x3B.U(7.W), 0x33.U(7.W))
        Cat(rs2p, rs1p, funct, rs1p, opc) | sub
      }
      VecInit(Seq(srli, srai, andi, rtype))(x(11, 10))
    }
    Seq(addi, jal, li, lui, arith, j, beqz, bnez)
  }

  def q2 = {
    val load_opc = Mux(rd.orR, 0x03.U(7.W), 0x1F.U(7.W))
    def slli = Cat(shamt, rd, 1.U(3.W), rd, 0x13.U(7.W))
    def ldsp = Cat(ldspImm, sp, 3.U(3.W), rd, load_opc)
    def lwsp = Cat(lwspImm, sp, 2.U(3.W), rd, load_opc)
    def fldsp = Cat(ldspImm, sp, 3.U(3.W), rd, 0x07.U(7.W))
    def flwsp = {
      if (xLen == 32) Cat(lwspImm, sp, 2.U(3.W), rd, 0x07.U(7.W))
      else ldsp
    }
    def sdsp = Cat(sdspImm >> 5, rs2, sp, 3.U(3.W), sdspImm(4,0), 0x23.U(7.W))
    def swsp = Cat(swspImm >> 5, rs2, sp, 2.U(3.W), swspImm(4,0), 0x23.U(7.W))
    def fsdsp = Cat(sdspImm >> 5, rs2, sp, 3.U(3.W), sdspImm(4,0), 0x27.U(7.W))
    def fswsp = {
      if (xLen == 32) Cat(swspImm >> 5, rs2, sp, 2.U(3.W), swspImm(4,0), 0x27.U(7.W))
      else sdsp
    }
    def jalr = {
      val mv = Cat(rs2, x0, 0.U(3.W), rd, 0x33.U(7.W))
      val add = Cat(rs2, rd, 0.U(3.W), rd, 0x33.U(7.W))
      val jr = Cat(rs2, rd, 0.U(3.W), x0, 0x67.U(7.W))
      val reserved = Cat(jr >> 7, 0x1F.U(7.W))
      val jr_reserved = Mux(rd.orR, jr, reserved)
      val jr_mv = Mux(rs2.orR, mv, jr_reserved)
      val jalr = Cat(rs2, rd, 0.U(3.W), ra, 0x67.U(7.W))
      val ebreak = Cat(jr >> 7, 0x73.U(7.W)) | (1 << 20).U
      val jalr_ebreak = Mux(rd.orR, jalr, ebreak)
      val jalr_add = Mux(rs2.orR, add, jalr_ebreak)
      Mux(x(12), jalr_add, jr_mv)
    }
    Seq(slli, fldsp, lwsp, flwsp, jalr, fsdsp, swsp, fswsp)
  }

  def decode = {
    val s = VecInit(q0 ++ q1 ++ q2)
    s(Cat(x(1,0), x(15,13)))
  }
}

class DecodeResp(implicit p: Parameters) extends BaseZirconBundle {
  val valid     = Bool()
  val uop       = new MicroOp
  val taken     = Bool()
  val tg_addr   = UInt(vaddrBits.W)
}

class Decode(plWidth: Int)(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new Bundle() {
    val kill = Input(Bool())
    val stall = Input(Bool())
    val reqs = Flipped(Valid(Vec(plWidth, new FetchBufferResp)))
    val resps = Valid(Vec(plWidth, new DecodeResp))
  })

  //  Decode Logic
  val decode_table = IDecode.table ++ ZicsrDecode.table ++ MDecode.table ++
    ADecode.table ++ FDecode.table
  val expander_insts = io.reqs.bits.map(i => new RVCExpander(i.inst, xLen).decode)
  val insts = io.reqs.bits zip expander_insts map { case (i, ext_i) =>
    Mux(i.len, i.inst, ext_i)
  }

  val ctrl_sigs = insts.map(i => {
    Wire(new CtrlSigs).decode(i, decode_table)
  })

  def genImm(inst: UInt, isel: UInt) = {
    def LONGEST_IMM_SZ: Int = 20

    val di24_20 = Mux(isel === IS_B || isel === IS_S, inst(11, 7), inst(24, 20))
    val imm_packed = Cat(inst(31, 25), di24_20, inst(19, 12))
    val sign = imm_packed(LONGEST_IMM_SZ-1).asSInt
    val i30_20 = Mux(isel === IS_U, imm_packed(18,8).asSInt, sign)
    val i19_12 = Mux(isel === IS_U || isel === IS_J, imm_packed(7,0).asSInt, sign)
    val i11    = Mux(isel === IS_U, 0.S,
      Mux(isel === IS_J || isel === IS_B, imm_packed(8).asSInt, sign))
    val i10_5  = Mux(isel === IS_U, 0.S, imm_packed(18,14).asSInt)
    val i4_1   = Mux(isel === IS_U, 0.S, imm_packed(13,9).asSInt)
    val i0     = Mux(isel === IS_S || isel === IS_I, imm_packed(8).asSInt, 0.S)
    Cat(sign, i30_20, i19_12, i11, i10_5, i4_1, i0)
  }

  //  Response
  val resp_valid = Reg(Bool())
  val resp = Reg(Vec(plWidth, new DecodeResp))

  when (io.kill) {
    resp_valid := false.B
  } .elsewhen (io.stall) {
    resp_valid := resp_valid
  } .otherwise {
    resp_valid := io.reqs.valid
  }

  val RD_MSB  = 11
  val RD_LSB  = 7
  val RS1_MSB = 19
  val RS1_LSB = 15
  val RS2_MSB = 24
  val RS2_LSB = 20
  val RS3_MSB = 31
  val RS3_LSB = 27

  for (w <- 0 until plWidth) {
    when(!io.stall) {
      resp(w).valid         := io.reqs.bits(w).valid
      resp(w).uop.uopc      := ctrl_sigs(w).uopc
      resp(w).uop.len       := io.reqs.bits(w).len
      resp(w).uop.port      := ctrl_sigs(w).port
      resp(w).uop.dw        := ctrl_sigs(w).dw
      resp(w).uop.usign     := ctrl_sigs(w).usign
      resp(w).uop.is_amo    := ctrl_sigs(w).is_amo

      resp(w).uop.lrs1_vld  := ctrl_sigs(w).lrs1_vld
      resp(w).uop.lrs1_type := ctrl_sigs(w).lrs1_type
      resp(w).uop.lrs1_lreg := insts(w)(RS1_MSB, RS1_LSB)
      resp(w).uop.lrs2_vld  := ctrl_sigs(w).lrs2_vld
      resp(w).uop.lrs2_type := ctrl_sigs(w).lrs2_type
      resp(w).uop.lrs2_lreg := insts(w)(RS2_MSB, RS2_LSB)
      resp(w).uop.lrs3_vld  := ctrl_sigs(w).lrs3_vld
      resp(w).uop.lrs3_type := ctrl_sigs(w).lrs3_type
      resp(w).uop.lrs3_lreg := insts(w)(RS3_MSB, RS3_LSB)
      resp(w).uop.ldst_vld  := ctrl_sigs(w).ldst_vld
      resp(w).uop.ldst_type := ctrl_sigs(w).ldst_type
      resp(w).uop.ldst_lreg := insts(w)(RD_MSB, RD_LSB)
      resp(w).uop.wakeup    := ctrl_sigs(w).wakeup
      resp(w).uop.mem_cmd   := ctrl_sigs(w).mem_cmd
      resp(w).uop.imm       := genImm(insts(w), ctrl_sigs(w).imm_sel)
      resp(w).uop.cause     := Mux(io.reqs.bits(w).cause.andR, io.reqs.bits(w).cause,
                                Mux(!ctrl_sigs(w).legal, Causes.illegal_instruction.U, 0.U))

      //  Load-Store information
      resp(w).uop.is_ld   := ctrl_sigs(w).uopc === UOP_LOAD
      resp(w).uop.is_st   := ctrl_sigs(w).uopc === UOP_STORE
      resp(w).uop.addr    := io.reqs.bits(w).addr

      //  Predictor information
      val is_jal  = ctrl_sigs(w).uopc === UOP_JAL
      val is_jalr = ctrl_sigs(w).uopc === UOP_JALR
      val rs1 = insts(w)(RS1_MSB, RS1_LSB)
      val rd  = insts(w)(RD_MSB, RD_LSB)
      val rd_is_x1_x5   = isOneOf(rd, Seq(1.U, 5.U))
      val rs1_is_x1_x5  = isOneOf(rs1, Seq(1.U, 5.U))
      val rd_eq_rs1     = rd === rs1

      resp(w).uop.is_br   := isOneOf(ctrl_sigs(w).uopc, Seq(UOP_BEQ, UOP_BNE, UOP_BGE, UOP_BLT))
      resp(w).uop.is_jmp  := (is_jal && !rd_is_x1_x5) ||
                              (is_jalr && !rd_is_x1_x5 && !rs1_is_x1_x5)
      resp(w).uop.is_call := (is_jal && rd_is_x1_x5) ||
                              (is_jalr && rd_is_x1_x5)
      resp(w).uop.is_ret  := is_jalr && ((!rd_is_x1_x5 && rs1_is_x1_x5) ||
                              (rd_is_x1_x5 && rs1_is_x1_x5 && rd_eq_rs1))
      resp(w).uop.taken   := io.reqs.bits(w).taken
      resp(w).uop.tg_addr := io.reqs.bits(w).tg_addr

      //  Floating-point information
      resp(w).uop.rm      := insts(w)(14,12)
      resp(w).uop.dyn_rm  := insts(w)(14,12) === 7.U

      //  Dont Care
      resp(w).uop.rob_id  := DontCare
      resp(w).uop.ld_id   := DontCare
      resp(w).uop.st_id   := DontCare
      resp(w).uop.rsv_id  := DontCare
      resp(w).uop.lrs1    := DontCare
      resp(w).uop.lrs2    := DontCare
      resp(w).uop.lrs3    := DontCare

    }
  }

  io.resps.valid := resp_valid
  io.resps.bits := resp
  //  End
}