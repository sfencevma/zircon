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

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, _}
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import difftest._
import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils}
import zircon.axi4._
import zircon.common._
import zircon.utils._
import huancun.utils.ChiselDB

class SimTop(implicit p: Parameters) extends BaseZirconModule {
    val io = IO(new Bundle() {
      val logCtrl = new LogCtrlIO
      val perfInfo = new PerfInfoIO
      val uart = new UARTIO

      val wdogcmp = Vec(PLICConstants.nWdogs, Flipped(new GatewaysIO))
      val rtccmp  = Vec(PLICConstants.nRtcs, Flipped(new GatewaysIO))
      val uart_int= Vec(PLICConstants.nUarts, Flipped(new GatewaysIO))
      val qspi    = Vec(PLICConstants.nQspis, Flipped(new GatewaysIO))
      val gpio    = Vec(PLICConstants.nGpios, Flipped(new GatewaysIO))
      val pwm     = Vec(PLICConstants.nPwms, Flipped(new GatewaysIO))
      val reset_vector = Input(UInt(vaddrBits.W))
    })

    //  Tile
    val core = Module(new ZirconTop())
    core.io.hart_id := 0.U
    core.io.reset_vector := io.reset_vector

    //  Mem
    val mem_params = AXI4SlaveParams(
      name = "memory",
      addrBits = paddrBits,
      dataBits = (dcacheParams.blockBits max icacheParams.blockBytes) / 4,
      idBits = 4,
      userBits = 4,
      nBeats = 4
    )
    val simAXI4Mem = Module(new AXI4Ram(memByte = 1024 * 1024 * 1024, useBlackBox = false))
    simAXI4Mem.io.node <> core.io.mem_node

    //  Devices
    core.io.wdogcmp  <> io.wdogcmp
    core.io.rtccmp   <> io.rtccmp
    core.io.uart     <> io.uart_int
    core.io.qspi     <> io.qspi
    core.io.gpio     <> io.gpio
    core.io.pwm      <> io.pwm

    //
    val log_begin, log_end, log_level = WireInit(0.U(4.W))
    log_begin := io.logCtrl.log_begin
    log_end := io.logCtrl.log_end
    log_level := io.logCtrl.log_level

    assert(log_begin <= log_end)
    //
    BoringUtils.addSource((GTimer() >= log_begin) && (GTimer() < log_end), "DISPLAY_ENABLE")

    val dummyWire = WireInit(false.B)
    BoringUtils.addSink(dummyWire, "DISPLAY_ENABLE")

    //
    io.uart.in.valid  := DontCare
    io.uart.out.valid := DontCare
    io.uart.out.ch    := DontCare

    //  End
}

object TopMain extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    val zirconParams: Parameters = ZirconTestUnit.getZirconParameters("ZirconConfig")
    implicit val p: Parameters = zirconParams.alterPartial {
      case TileKey => zirconParams(TileKey)
    }
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new SimTop()))
    )
    ChiselDB.addToElaborationArtefacts
    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
      val prefix = extension match {
        case "h" | "cpp" => "chisel_db"
        case _ => "Zircon"
      }
      writeOutputFile("./build", s"$prefix.${extension}", contents())
    }

  }
}