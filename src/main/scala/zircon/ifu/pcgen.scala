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
package zircon.ifu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._

class PCGenRediret(implicit p: Parameters) extends BaseZirconBundle {
  val addr    = UInt(vaddrBits.W)
}

class PCGenResp(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class PCGen(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val stall = Input(Bool())
    val flush = Flipped(Valid(new PCGenRediret))
    val nlp_rdt = Flipped(Valid(new PCGenRediret))
    val bpd_rdt = Flipped(Valid(new PCGenRediret))
    val resp = Valid(new PCGenResp)
  })

  val core_pc = RegInit(UInt(vaddrBits.W), 0.U(vaddrBits.W))
  val next_pc = Mux(io.flush.valid, io.flush.bits.addr,
      Mux(io.bpd_rdt.valid, io.bpd_rdt.bits.addr,
        Mux(io.nlp_rdt.valid, io.nlp_rdt.bits.addr, core_pc + 16.U)))

  val gen_ena = !io.stall || (io.flush.valid || io.nlp_rdt.valid)

  when (gen_ena) {
    core_pc := next_pc
  }
  io.resp.valid := RegEnable(true.B, gen_ena) // RegNext(!io.stall)
  io.resp.bits.addr := core_pc
}