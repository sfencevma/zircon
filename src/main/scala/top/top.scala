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
package top

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.amba.axi4.{AXI4Buffer, AXI4IdIndexer, AXI4IdentityNode}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils}
import zircon.frontend._
import zircon.exu._
import zircon.common._
import zircon.axi4._
import zircon.mmu._
import config._

class ZirconTop(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val hart_id = Input(UInt(64.W))
    val reset_vector = Input(UInt(vaddrBits.W))

    val wdogcmp = Vec(PLICConstants.nWdogs, Flipped(new GatewaysIO))
    val rtccmp  = Vec(PLICConstants.nRtcs, Flipped(new GatewaysIO))
    val uart    = Vec(PLICConstants.nUarts, Flipped(new GatewaysIO))
    val qspi    = Vec(PLICConstants.nQspis, Flipped(new GatewaysIO))
    val gpio    = Vec(PLICConstants.nGpios, Flipped(new GatewaysIO))
    val pwm     = Vec(PLICConstants.nPwms, Flipped(new GatewaysIO))

    val mem_node= AXI4MasterBundle(memBusParams)
  })

  val core = Module(new ZirconCore())
  //  Zircon Core
  core.io.hart_id := io.hart_id
  core.io.reset_vector := io.reset_vector

  //  RTC
  val rtcClockDiv = 100
  val rtcTickCycle = rtcClockDiv / 2
  val rtcCounter = RegInit(0.U(log2Ceil(rtcTickCycle + 1).W))
  rtcCounter := Mux(rtcCounter === (rtcTickCycle - 1).U, 0.U, rtcCounter + 1.U)
  val rtcClock = RegInit(false.B)
  when(rtcCounter === 0.U) {
    rtcClock := ~rtcClock
  }

  //  Zircon Devices
  val devices = Module(new AXI4Devices())
  devices.io.wdogcmp  <> io.wdogcmp
  devices.io.rtccmp   <> io.rtccmp
  devices.io.uart     <> io.uart
  devices.io.qspi     <> io.qspi
  devices.io.gpio     <> io.gpio
  devices.io.pwm      <> io.pwm
  devices.io.rtc_tick := rtcClock
  devices.io.node     <> core.io.mmap_node

  //
  val interrupts = Wire(new Interrupts)
  interrupts.msip := devices.io.sft_int
  interrupts.mtip := devices.io.timer_int
  interrupts.meip := devices.io.harts(0) //  Only one hart
  interrupts.debug := false.B // 

  core.io.interrupts := interrupts

  //
  io.mem_node <> core.io.mem_node
  //  End
}

object TopMain extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    val zirconParams: Parameters = ZirconTestUnit.getZirconParameters("ZirconConfig")
    implicit val p: Parameters = zirconParams.alterPartial {
      case TileKey => zirconParams(TileKey)
    }
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new ZirconTop))
    )
    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
      writeOutputFile("./build", s"Zircon.${extension}", contents())
    }
  }
}