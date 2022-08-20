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
  val taken     = Bool()
  val use_loop  = Bool()
}

class LoopUpdate(implicit p: Parameters) extends BaseZirconBundle {
  val addr        = UInt(vaddrBits.W)
  val taken       = Bool()
  val loop_taken  = Bool()
}

class LoopPredictor (params: LoopParams)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val req = Flipped(Valid(new LoopReq))
    val resp = Output(new LoopResp)
    val upd = Flipped(Valid(new LoopUpdate))
  })

  def lgSets: Int = log2Ceil(params.nSets)
  def tagBits: Int = vaddrBits - lgSets - 1
  def AGE_MAX: Int = (1 << params.ageBits) - 1
  def CONF_MAX: Int = (1 << params.confBits) - 1
  def CNT_MAX: Int = (1 << params.cntBits) - 1
  def entryBits: Int = tagBits + params.ageBits + params.confBits + 2 * params.cntBits
  def compute_idx_and_tag(addr: UInt) = {
    val idx = addr(lgSets, 1)
    val tag = addr(vaddrBits-1, 1+lgSets)
    (idx, tag)
  }

  class LoopEntry extends Bundle {
    val tag   = UInt(tagBits.W)
    val age   = UInt(params.ageBits.W)
    val conf  = UInt(params.confBits.W)
    val c_cnt = UInt(params.cntBits.W)
    val p_cnt = UInt(params.cntBits.W)
  }

  //  *****************************************
  //  +-------+-------+--------+-------+-------+
  //  | Tag   |  Age  | conf   | c_cnt | p_cnt |
  //  +-------+-------+--------+-------+-------+
  //  |               .........                |
  //  |               .........                |
  //  |               .........                |
  //  +-------+-------+--------+-------+-------+
  //  | Tag   |  Age  | conf   | c_cnt | p_cnt |
  //  +-------+-------+--------+-------+-------+
  //  |tagBits|ageBits|confBits|cntBits|cntBits|
  //  +-------+-------+--------+-------+-------+
  //
  val data_array = SyncReadMem(params.nSets, UInt(entryBits.W))

  //  Update Prediction
  //  *****************************************
  //  Step 1: Read data
  val do_req = io.req.valid
  val (req_idx, req_tag) = compute_idx_and_tag(io.req.bits.addr)
  val pred_entry = data_array.read(req_idx, do_req).asTypeOf(new LoopEntry)
  val resp = Wire(new LoopResp)

  //  Set Default
  resp := 0.U.asTypeOf(new LoopResp)

  val tag_hit = req_tag === pred_entry.tag
  when (tag_hit) {
    resp.taken := pred_entry.c_cnt < pred_entry.p_cnt
    resp.use_loop := pred_entry.conf === CONF_MAX.U
  }
  io.resp := resp

  //  *****************************************
  //  Step 2: Update logic
  val do_upd = io.upd.valid
  val (upd_idx, upd_tag) = compute_idx_and_tag(io.upd.bits.addr)
  val wentry = WireInit(pred_entry)

  when (!tag_hit && pred_entry.age > 0.U) {
    wentry.age := pred_entry.age - 1.U
  } .otherwise {
    when (pred_entry.age === 0.U) {
      wentry.tag  := upd_tag
      wentry.age  := AGE_MAX.U
      wentry.c_cnt := CNT_MAX.U
      wentry.conf := 0.U
    } .otherwise {
      when (resp.taken === io.upd.bits.taken) {
        when (pred_entry.c_cnt =/= pred_entry.p_cnt) {
          wentry.c_cnt := pred_entry.c_cnt + 1.U
        } .otherwise {
          wentry.c_cnt := 0.U
          when (pred_entry.conf < CONF_MAX.U) {
            wentry.conf := pred_entry.conf + 1.U
          }
        }
      } .otherwise {
        when (pred_entry.age === AGE_MAX.U) {
          wentry.p_cnt := pred_entry.c_cnt
          wentry.c_cnt := 0.U
          wentry.conf := 1.U
        } .otherwise {
          wentry := 0.U.asTypeOf(new LoopEntry)
        }
      }
    }
  }

  when (do_upd) {
    data_array(upd_idx) := wentry.asUInt
  }
  //  End
}