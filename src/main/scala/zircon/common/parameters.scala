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

trait ZirconCoreParameters {
  val useVM: Boolean
  val useUser: Boolean
  val useDebug: Boolean
  val useAtomics: Boolean
  val useCompressed: Boolean
  val useVector: Boolean = false
  val useRVE: Boolean
  val fetchWidth: Int
  val decodeWidth: Int
  val retireWidth: Int
  val instBits: Int
  val nPMPs: Int
  val nPerfCounters: Int
  val nMSHRs: Int
  val haveFSDirty: Boolean
  val misaWritable: Boolean
  val mtvecWritable: Boolean

  def instBytes: Int = instBits / 8
  def fetchBytes: Int = fetchWidth * instBytes
}

case class ZirconCoreParams (
                              useAtomics: Boolean = true,
                              useDebug: Boolean = false,
                              useUser: Boolean = true,
                              useSupervisor: Boolean = true,
                              useHypervisor: Boolean = false,
                              useVM: Boolean = true,
                              useRVE: Boolean = false,
                              useCompressed: Boolean = true,
                              fetchWidth: Int = 4,
                              decodeWidth: Int = 4,
                              retireWidth: Int = 4,
                              numRobEntries: Int = 128,
                              numLdqEntries: Int = 32,
                              numStqEntries: Int = 32,
                              numRsvEntries: Int = 64,
                              numFetchBufferEntries: Int = 32,
                              nPMPs: Int = 0,
                              nPerfCounters: Int = 16,
                              nMSHRs: Int = 16
                            ) extends ZirconCoreParameters {
  val instBits: Int = 32
  val haveFSDirty: Boolean = true
  val misaWritable: Boolean = false
  val mtvecWritable: Boolean = true
}

trait HasZirconCoreParameters extends HasTileParameters {
  val zirconParams: ZirconCoreParams = tileParams.core.asInstanceOf[ZirconCoreParams]

  val fetchWidth: Int = zirconParams.fetchWidth
  val decodeWidth: Int = zirconParams.decodeWidth
  val retireWidth: Int = zirconParams.retireWidth

  val numRobEntries: Int = zirconParams.numRobEntries
  val numLdqEntries: Int = zirconParams.numLdqEntries
  val numStqEntries: Int = zirconParams.numStqEntries
  val numRsvEntries: Int = zirconParams.numRsvEntries
  val numFetchBufferEntries: Int = zirconParams.numFetchBufferEntries
  val robIdBits: Int = log2Ceil(numRobEntries) + 1
  val ldqIdBits: Int = log2Ceil(numLdqEntries) + 1
  val stqIdBits: Int = log2Ceil(numStqEntries) + 1
  val rsvIdBits: Int = log2Ceil(numRsvEntries)

  val instBits: Int = zirconParams.instBits
  val immBits: Int = 32

  //  Register
  val numLRegs: Int = 32
  val lregSz: Int = log2Ceil(numLRegs)
  val csrAddrBits: Int = 12

  //  Dcache & ICache Parameters
  val icacheParams: ICacheParams = tileParams.icache
  val dcacheParams: DCacheParams = tileParams.dcache
  //
  val nPMPs: Int = zirconParams.nPMPs
  val nPerfCounters: Int = zirconParams.nPerfCounters
  val nMSHRs: Int = zirconParams.nMSHRs
}