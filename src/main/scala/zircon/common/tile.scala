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
import freechips.rocketchip.config._
import zircon.ifu.icache._
import zircon.ifu._
import zircon.exu.lsu._
import zircon.exu.lsu.dcache.DCacheParams

trait TileParameters {
  val core: ZirconCoreParams
  val icache: ICacheParams
  val dcache: DCacheParams
  val hartId: Int
}

case class ZirconTileParams (
                              core: ZirconCoreParams,
                              icache: ICacheParams,
                              dcache: DCacheParams,
                              hartId: Int
                            )

case object TileKey extends Field[ZirconTileParams]
case object XLEN extends Field[Int]
case object ILEN extends Field[Int]
case object PgLevels extends Field[Int]
case object ASIdBits extends Field[Int]

trait HasTileParameters {
  implicit val p: Parameters

  def tileParams: ZirconTileParams = p(TileKey)
  def usingVM: Boolean = tileParams.core.useVM
  def usingUser: Boolean = tileParams.core.useUser

  def xLen: Int = p(XLEN)
  def eLen: Int = 7
  val fLen: Int = 64
  def xBytes: Int = xLen / 8
  def iLen: Int = p(ILEN)
  def pgIdxBits: Int = 12
  def pgLevelBits: Int = 10 - log2Ceil(xLen / 32)
  def pgLevels: Int = p(PgLevels)
  def lgPgLevels: Int = log2Ceil(pgLevels)
  def minPgLevels: Int = {
    val res = xLen match {
      case 32 => 2
      case 64 => 3
    }
    res
  }
  def asIdBits: Int = p(ASIdBits)
  def hartId: Int = tileParams.hartId

  def paddrBits: Int = 56
  def vaddrBits: Int = 48
  def vpnBits: Int = vaddrBits - pgIdxBits
  def ppnBits: Int = paddrBits - pgIdxBits
}