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
import chisel3.util._

abstract class AXI4BundleA(params: AXI4BundleParams) extends Bundle {
  val id    = UInt(params.idBits.W)
  val addr  = UInt(params.addrBits.W)
  val len   = UInt(params.lenBits.W)
  val size  = UInt(params.sizeBits.W)
  val burst = UInt(params.burstBits.W)
  val lock  = UInt(params.lockBits.W)
  val cache = UInt(params.cacheBits.W)
  val prot  = UInt(params.protBits.W)
  val qos   = UInt(params.qosBits.W)
  val user  = UInt(params.userBits.W)
}

//  Write Channel
class AXI4BundleAW(params: AXI4BundleParams) extends AXI4BundleA(params)
class AXI4BundleW(params: AXI4BundleParams) extends Bundle {
  val data = UInt(params.dataBits.W)
  val strb = UInt((params.dataBits/8).W)
  val last = Bool()
}
class AXI4BundleB(params: AXI4BundleParams) extends Bundle {
  val id    = UInt(params.idBits.W)
  val resp  = UInt(params.respBits.W)
  val user  = UInt(params.userBits.W)
}


//  Read Channel
class AXI4BundleAR(params: AXI4BundleParams) extends AXI4BundleA(params)
class AXI4BundleR(params: AXI4BundleParams) extends Bundle {
  val id    = UInt(params.idBits.W)
  val data  = UInt(params.dataBits.W)
  val resp  = UInt(params.respBits.W)
  val user  = UInt(params.userBits.W)
  val last  = Bool()
}

class AXI4Bundle(val params: AXI4BundleParams) extends Bundle {
  val aw  = Decoupled(new AXI4BundleAW(params))
  val w   = Decoupled(new AXI4BundleW(params))
  val b   = Flipped(Decoupled(new AXI4BundleB(params)))
  val ar  = Decoupled(new AXI4BundleAR(params))
  val r   = Flipped(Decoupled(new AXI4BundleR(params)))
}

object AXI4Bundle {
  def apply(params: AXI4BundleParams) = new AXI4Bundle(params)
}