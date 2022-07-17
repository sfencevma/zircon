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
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._

//  Define master node
case class AXI4MasterNode(portParams: Seq[AXI4MasterPortParams])(implicit valName: ValName) extends SourceNode(AXI4Imp)(portParams)

//  Define slave node
case class AXI4SlaveNode(portParams: Seq[AXI4SlavePortParams])(implicit valName: ValName) extends SinkNode(AXI4Imp)(portParams)

//  Define nexus node, which will be used in CrossBar
case class AXI4NexusNode(
                          masterFn: Seq[AXI4MasterPortParams] => AXI4MasterPortParams,
                          slaveFn: Seq[AXI4SlavePortParams] => AXI4SlavePortParams
                        )(implicit valName: ValName) extends NexusNode(AXI4Imp)(masterFn, slaveFn)

