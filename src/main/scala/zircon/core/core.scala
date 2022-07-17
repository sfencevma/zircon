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
package zircon.core

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import zircon.common._
import zircon.ifu._
import zircon.decode._
import zircon.decode.rename._
import zircon.axi4._

//
//  Zircon has the following stages.
//  if0   - Generate fetch address and nlp redirect.
//  if1   - Load instructions from ICache.
//  if2   - Load instructions from ICache.
//  if3   - Pre-decode and bpd redirect stage 0.
//  if4   - Enqueue to fetch buffer and bpd redirect stage 1.
//  dec0  - Decode.
//  dec1  - Rename.
//  dec2  - RegFile read.
//  iss0  - Issue insts to rsv.
//  iss1  - Select and wakeup.
//  iss2  - Issue.
//  exec  - Execute.
//  wb    - Write back.
//  ret   - Retire.
//

class ZirconCoreIO(implicit p: Parameters) extends BaseZirconBundle {

}

class ZirconCore(params: Seq[AXI4MasterParams])(implicit p: Parameters) extends BaseAXI4MasterModule(params, new ZirconCoreIO()) with HasZirconCoreParameters {
  override lazy val module = new AXI4MasterModuleImp[ZirconCoreIO](this) {
    //  Stage if0 - if2
    val ifu = LazyModule(new IFUStage(Seq(params(0)))).module
    val ifu_io = ifu.io.extend_io.get

    //  Stage if3
    val scan = Module(new ScanStage(decodeWidth))
    scan.io.req := ifu_io.resp
    ifu_io.stall := scan.io.forward_stall

    //  Stage if4
    val fetch_buffer = Module(new FetchBuffer(decodeWidth, numFetchBufferEntries))

    //  Stage dec0
    val decode = Module(new Decode(decodeWidth))

    //  Stage dec1
    val rename = Module(new RenameStage(decodeWidth))

    //  Stage dec2

  }
}