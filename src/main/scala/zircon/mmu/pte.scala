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
package zircon.mmu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._

class PTE(implicit p: Parameters) extends Bundle {
  val n         = Bool()
  val pbmt      = UInt(2.W)
  val zero      = UInt(7.W)
  val ppn       = UInt(44.W)
  val rsw       = UInt(2.W)
  val d         = Bool()
  val a         = Bool()
  val g         = Bool()
  val u         = Bool()
  val x         = Bool()
  val w         = Bool()
  val r         = Bool()
  val v         = Bool()

  def table = v && !r && !w && !x
  def leaf  = v && (r || (x && !w)) && a
  def sr    = leaf && r
  def sw    = leaf && w && d
  def sx    = leaf && x
  def ur    = sr && u
  def uw    = sw && u
  def ux    = sx && u
}
