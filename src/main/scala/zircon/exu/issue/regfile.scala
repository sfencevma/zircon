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
package zircon.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import difftest._

class RegFileReadPortIO(addrWidth: Int, dataWidth: Int)(implicit p: Parameters) extends BaseZirconBundle {
  val addr = Input(UInt(addrWidth.W))
  val data = Output(UInt(dataWidth.W))
}


class RegFileWritePortIO(addrWidth: Int, dataWidth: Int)(implicit p: Parameters) extends BaseZirconBundle {
  val addr = Input(UInt(addrWidth.W))
  val data = Input(UInt(dataWidth.W))
}


class RegFile(
               numRegisters: Int,
               numReadPorts: Int,
               numWritePorts: Int,
               dataWidth: Int,
               float: Boolean
             )(implicit p: Parameters) extends BaseZirconModule {
  val addrWidth: Int = log2Ceil(numRegisters)
  val io = IO(new Bundle() {
    val hart_id = Input(UInt(64.W))
    val read_ports = Vec(numReadPorts, new RegFileReadPortIO(addrWidth, dataWidth))
    val write_ports = Flipped(Vec(numWritePorts, Valid(new RegFileWritePortIO(addrWidth, dataWidth))))
  })

  val regfiles = Reg(Vec(numRegisters, UInt(dataWidth.W)))

  //  Write
  val write_reqs = io.write_ports
  for (req <- write_reqs) {
    if (float) {
      when (req.valid) {
        regfiles(req.bits.addr) := req.bits.data
      }
    } else {
      when (req.valid) {
        when (req.bits.addr === 0.U) {
          regfiles(req.bits.addr) := 0.U
        } .otherwise {
          regfiles(req.bits.addr) := req.bits.data
        }
      }
    }
  }

  //  Read
  val read_datas = io.read_ports.map(req => regfiles(req.addr))
  val write_lreg = io.write_ports.map(_.bits.addr)

  for (n <- 0 until numReadPorts) {
    //  With Bypass
    io.read_ports(n).data := (0 until numWritePorts).foldLeft(read_datas(n)) ((m, k) =>
      Mux(io.write_ports(k).valid && (write_lreg(k) === io.read_ports(n).addr), io.write_ports(k).bits.data, m))

    // io.read_ports(n).data := ((0 until numWritePorts).map(i => write_lreg(i)) zip write_reqs.map(_.bits.data))
    //   .scanLeft(read_datas(n)) { case (old_data, (lreg, new_data)) => Mux(lreg === io.read_ports(n).addr, new_data, old_data) }
  }

  //  Difftest
  if (env.EnableDifftest && !float) {
    val difftest = Module(new DifftestArchIntRegState)
    difftest.io.clock := clock
    difftest.io.coreid := io.hart_id
    difftest.io.gpr := regfiles
  }
}