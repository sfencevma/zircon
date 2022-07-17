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
import freechips.rocketchip.config.Parameters


class MicroOp(implicit p: Parameters) extends BaseZirconBundle
  with ScalarOpConstants
  with MemoryOpConstants
{
  //  Common information
  val uopc        = UInt(UOP_SZ.W)
  val len         = Bool()
  val dw          = UInt(DW_SZ.W)
  val port        = UInt(PORT_SZ.W)
  val usign       = Bool()

  //
  val is_amo      = Bool()
  val is_csr      = Bool()

  //  Register information
  val lrs1_vld    = Bool()
  val lrs1_type   = UInt(RT_SZ.W)
  val lrs1_lreg   = UInt(lregSz.W)
  val lrs1        = UInt(xLen.W)
  val lrs2_vld    = Bool()
  val lrs2_type   = UInt(RT_SZ.W)
  val lrs2_lreg   = UInt(lregSz.W)
  val lrs2        = UInt(xLen.W)
  val lrs3_vld    = Bool()
  val lrs3_type   = UInt(RT_SZ.W)
  val lrs3_lreg   = UInt(lregSz.W)
  val lrs3        = UInt(xLen.W)
  val ldst_vld    = Bool()
  val ldst_type   = UInt(RT_SZ.W)
  val ldst_lreg   = UInt(lregSz.W)
  val imm         = UInt(immBits.W)

  //  Floating point information
  val dyn_rm      = Bool()
  val rm          = UInt(3.W)

  //  Issue information
  val rob_id      = UInt(robIdBits.W)
  val is_ld       = Bool()
  val ld_id       = UInt(ldqIdBits.W)
  val is_st       = Bool()
  val st_id       = UInt(stqIdBits.W)
  val wakeup      = UInt(WAKEUP_SZ.W)
  val mem_cmd     = UInt(M_SZ.W)

  //  Misc information
  val addr        = UInt(vaddrBits.W)
  val cause       = UInt(eLen.W)
}