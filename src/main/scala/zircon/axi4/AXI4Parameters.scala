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
package zircon.axi4

import chisel3._
import chisel3.internal.sourceinfo._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

trait HasAXI4Parameters {
  def addrBits: Int
  def dataBits: Int
  def idBits:   Int
  def userBits: Int

  val lenBits   = 8
  val sizeBits  = 3
  val burstBits = 2
  val lockBits  = 1
  val cacheBits = 4
  val protBits  = 3
  val qosBits   = 4
  val respBits  = 2

  def SIZE_1B   = 0.U(sizeBits.W)
  def SIZE_2B   = 1.U(sizeBits.W)
  def SIZE_4B   = 2.U(sizeBits.W)
  def SIZE_8B   = 3.U(sizeBits.W)
  def SIZE_16B  = 4.U(sizeBits.W)
  def SIZE_32B  = 5.U(sizeBits.W)
  def SIZE_64B  = 6.U(sizeBits.W)
  def SIZE_128B = 7.U(sizeBits.W)

  def CACHE_RALLOCATE  = 8.U(cacheBits.W)
  def CACHE_WALLOCATE  = 4.U(cacheBits.W)
  def CACHE_MODIFIABLE = 2.U(cacheBits.W)
  def CACHE_BUFFERABLE = 1.U(cacheBits.W)
  def CACHE_NONBUFFERABLE = 0.U(cacheBits.W)

  def PROT_PRIVILEDGED = 1.U(protBits.W)
  def PROT_INSECURE    = 2.U(protBits.W)
  def PROT_INSTRUCTION = 4.U(protBits.W)

  def BURST_FIXED = 0.U(burstBits.W)
  def BURST_INCR  = 1.U(burstBits.W)
  def BURST_WRAP  = 2.U(burstBits.W)

  def RESP_OKAY   = 0.U(respBits.W)
  def RESP_EXOKAY = 1.U(respBits.W)
  def RESP_SLVERR = 2.U(respBits.W)
  def RESP_DECERR = 3.U(respBits.W)
}

case class AXI4BundleParams (
                              addrBits: Int,
                              dataBits: Int,
                              idBits: Int,
                              userBits: Int
                            ) extends HasAXI4Parameters

//  Define the parameters for the nodes themselves
case class AXI4MasterParams (
                              name: String,
                              addrBits: Int,
                              dataBits: Int,
                              idBits: Int,
                              userBits: Int,
                              nBeats: Int,
                              nodePath: Seq[BaseNode] = Seq()
                            )
case class AXI4SlaveParams(
                            name: String,
                            addrBits: Int,
                            dataBits: Int,
                            idBits: Int,
                            userBits: Int,
                            nBeats: Int,
                            nodePath: Seq[BaseNode] = Seq()
                          )

//  Define the parameters for the ports
case class AXI4MasterPortParams (
                                  masters: Seq[AXI4MasterParams]
                                ) {
  val addrBits: Int = masters.map(_.addrBits).max
  val dataBits: Int = masters.map(_.dataBits).max
  val idBits: Int   = masters.map(_.idBits).max
  val userBits: Int = masters.map(_.userBits).max
  val nBeats: Int   = masters.map(_.nBeats).max
}

case class AXI4SlavePortParams (
                                 slaves: Seq[AXI4SlaveParams]
                               ) {
  val addrBits: Int = slaves.map(_.addrBits).max
  val dataBits: Int = slaves.map(_.dataBits).max
  val idBits: Int   = slaves.map(_.idBits).max
  val userBits: Int = slaves.map(_.userBits).max
  val nBeats: Int   = slaves.map(_.nBeats).max
}

//
object AXI4BundleParams {
  def apply(master: AXI4MasterPortParams, slave: AXI4SlavePortParams) = {
    new AXI4BundleParams(
      addrBits = slave.addrBits,
      dataBits = slave.dataBits,
      idBits   = slave.idBits,
      userBits = slave.userBits
    )
  }
}

//  Define the parameters for the an edge (source-sink port pair)
case class AXI4EdgeParams (
                            master: AXI4MasterPortParams,
                            slave: AXI4SlavePortParams,
                            p: Parameters,
                            sourceInfo: SourceInfo
                          ) {
  val bundle = AXI4BundleParams(master, slave)
}