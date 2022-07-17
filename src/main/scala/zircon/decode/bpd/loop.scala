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
package zircon.decode.bpd

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._

case class LoopParams(
                       ageBits: Int = 3,
                       confBits: Int = 3,
                       cntBits: Int = 10,
                       nSets: Int = 128,
                     )

class LoopReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class LoopResp(implicit p: Parameters) extends BaseZirconBundle {
  val taken = Bool()
  val use_loop = Bool()
}

class LoopUpdate(implicit p: Parameters) extends BaseZirconBundle {
  val taken = Bool()
  val loop_taken = Bool()
  val addr = UInt(vaddrBits.W)
}

class LoopPredictor (params: LoopParams)(implicit p: Parameters) extends BaseZirconModule {

  import params._

  def lgSets: Int = log2Ceil(nSets)
  def tagBits: Int = vaddrBits - lgSets - 1
  def AGE_MAX: Int = (1 << ageBits) - 1
  def CONF_MAX: Int = (1 << confBits) - 1
  def CNT_MAX: Int = (1 << cntBits) - 1
  def entrySz: Int = tagBits + ageBits + confBits + 2 * cntBits

  class LoopEntry extends Bundle {
    val tag   = UInt(tagBits.W)
    val age   = UInt(ageBits.W)
    val conf  = UInt(confBits.W)
    val c_cnt = UInt(cntBits.W)
    val p_cnt = UInt(cntBits.W)
  }

  val io = IO(new Bundle() {
    val req = Flipped(Valid(new LoopReq))
    val resp = Output(new LoopResp)
    val upd = Flipped(Valid(new LoopUpdate))
  })


  def compute_idx_and_tag(addr: UInt) = {
    val idx = addr(lgSets, 1)
    val tag = addr(vaddrBits-1, 1+lgSets)
    (idx, tag)
  }

  val data_array = Mem(nSets, UInt(entrySz.W))

  //  Update Prediction
  val (upd_idx, upd_tag) = compute_idx_and_tag(io.upd.bits.addr)
  val upd_info = WireInit(io.upd.bits)

  val entry = data_array(upd_idx).asTypeOf(new LoopEntry)
  val wentry = WireInit(entry)
  val tag_hit = entry.tag === upd_tag

  when (!tag_hit && entry.age > 0.U) {
    wentry.age := entry.age - 1.U
  } .otherwise {
    when (entry.age === 0.U) {
      wentry.tag  := upd_tag
      wentry.age  := AGE_MAX.U
      wentry.c_cnt := CNT_MAX.U
      wentry.conf := 0.U
    } .otherwise {
      when (upd_info.loop_taken === upd_info.taken) {
        when (entry.c_cnt =/= entry.p_cnt) {
          wentry.c_cnt := entry.c_cnt + 1.U
        } .otherwise {
          wentry.c_cnt := 0.U
          when (entry.conf < CONF_MAX.U) {
            wentry.conf := entry.conf + 1.U
          }
        }
      } .otherwise {
        when (entry.age === AGE_MAX.U) {
          wentry.p_cnt := entry.c_cnt
          wentry.c_cnt := 0.U
          wentry.conf := 1.U
        } .otherwise {
          wentry := 0.U.asTypeOf(new LoopEntry)
        }
      }
    }
  }

  when (io.upd.valid) {
    data_array(upd_idx) := wentry.asUInt
  }

  //  Get Prediction
  val (req_idx, req_tag) = compute_idx_and_tag(io.req.bits.addr)

  val do_bypass = RegNext(req_idx === upd_idx && (req_tag === upd_tag) && io.upd.valid) // reqs.map(req => (req._1 === upd_idx) && (req._2 === upd_tag) && io.upd.valid)
  val pred_entry = Mux(do_bypass, wentry, data_array(req_idx).asTypeOf(new LoopEntry))

  val loop_resps = Wire(new LoopResp)
  loop_resps := 0.U.asTypeOf(new LoopResp)

  when (req_tag === pred_entry.tag) {
    loop_resps.taken := pred_entry.c_cnt < pred_entry.p_cnt
    loop_resps.use_loop := pred_entry.conf === CONF_MAX.U
  }
  io.resp := RegEnable(loop_resps, io.req.valid)
}