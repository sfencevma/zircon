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

import chisel3.internal.sourceinfo._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

object AXI4Imp extends SimpleNodeImp[AXI4MasterPortParams, AXI4SlavePortParams, AXI4EdgeParams, AXI4Bundle] {
  //  Collect downstream and upstream parameters into an edge
  def edge(pd: AXI4MasterPortParams, pu: AXI4SlavePortParams, p: Parameters, sourceInfo: SourceInfo) = AXI4EdgeParams(pd, pu, p, sourceInfo)

  //  Generate hardware bundle
  def bundle(e: AXI4EdgeParams) = AXI4Bundle(e.bundle)

  //  Tell this node that it has an additional outgoing connection
  override def mixO(pd: AXI4MasterPortParams, node: OutwardNode[AXI4MasterPortParams, AXI4SlavePortParams, AXI4Bundle]) =
    pd.copy(masters = pd.masters.map { c => c.copy(nodePath = node +: c.nodePath) })

  //  Tell this node that it has an additional incoming connection
  override def mixI(pu: AXI4SlavePortParams, node: InwardNode[AXI4MasterPortParams, AXI4SlavePortParams, AXI4Bundle]) =
    pu.copy(slaves = pu.slaves.map { m => m.copy(nodePath = node +: m.nodePath) })

  override def render(e: AXI4EdgeParams): RenderedEdge = RenderedEdge(colour = "#00ccff")
}