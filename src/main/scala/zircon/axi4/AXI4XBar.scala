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
import freechips.rocketchip.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

class AXI4XBar(implicit p: Parameters) extends LazyModule {
  // val node = AXI4NexusNode()
  lazy val module = new AXI4XBarImp(this)
}

class AXI4XBarImp(outer: AXI4XBar)(implicit p: Parameters) extends LazyModuleImp(outer) {

}