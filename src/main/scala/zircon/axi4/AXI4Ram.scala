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
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

class AXI4Ram(params: AXI4SlaveParams)(implicit p: Parameters) extends BaseAXI4SlaveModule(params) {
  override lazy val module = new AXI4SlaveModuleImp(this) {

    def memBytes = in.params.dataBits/8
    def offBits = log2Up(memBytes)
    def offMask = (1 << offBits)-1
    def index(addr: UInt) = ((addr & offMask.U) >> offBits).asUInt
    def ramRows = (1 << in.params.addrBits) >> memBytes

    val ram = SyncReadMem(ramRows, Vec(memBytes, UInt(8.W)))

    b_bits.id := aw_bits_reg.id
    b_bits.resp := in.params.RESP_OKAY
    r_bits.id := ar_bits_reg.id
    r_bits.data := Cat(ram.read(index(read_addr), mem_rd_en).reverse)
    r_bits.resp := in.params.RESP_OKAY
    in.r.bits.data := r_bits.data

    val wdata = Seq.tabulate(memBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    when (mem_wr_en) {
      ram.write(index(aw_bits_reg.addr), VecInit(wdata), in.w.bits.strb.asBools)
    }
  }
}