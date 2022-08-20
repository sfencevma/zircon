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
package zircon.frontend

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.utils._

import scala.math._

class BIMReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class BIMResp(implicit p: Parameters) extends BaseZirconBundle {
  val cnt = UInt(2.W)
}

class BIMUpdate(implicit p: Parameters) extends BaseZirconBundle {
  val addr      = UInt(vaddrBits.W)
  val taken     = Bool()
  val old_cnt   = UInt(2.W)
}

class BIM(nSets: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val req = Flipped(Valid(new BIMReq))
    val resp = Output(new BIMResp)
    val upd = Flipped(Valid(new BIMUpdate))
  })

  def compute_idx(addr: UInt, len: Int) = {
    val nChunks = (vaddrBits + len - 1) / len
    val idx_chunks = (0 until nChunks) map {
      i => addr(min((i+1)*len, vaddrBits)-1, i*len)
    }
    idx_chunks.reduce(_^_)
  }

  val bim = SyncReadMem(nSets, UInt(2.W))

  //  Update
  val upd_idx = compute_idx(io.upd.bits.addr, log2Ceil(nSets))
  val upd_data = Mux(io.upd.bits.taken, counterInc(io.upd.bits.old_cnt), counterDec(io.upd.bits.old_cnt))

  when (io.upd.valid) {
    bim.write(upd_idx, upd_data)
  }

  //  Require
  val req_idx = compute_idx(io.req.bits.addr, log2Ceil(nSets))
  val bypass_data = RegNext(upd_data)
  val do_bypass = RegNext(io.upd.valid && (req_idx === upd_idx))
  io.resp.cnt := Mux(do_bypass, bypass_data, bim.read(req_idx, io.req.valid))

  //  End
}