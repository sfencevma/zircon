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
package config

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.frontend._
import zircon.exu._

//*****************************
//  Core configs
class WithNMegaZircon(n: Int = 1) extends Config(
  new Config((site, here, up) => {
    case TileKey =>
      ZirconTileParams (
        core = ZirconCoreParams(),
        icache = ICacheParams(),
        dcache = DCacheParams(),
        hartId = 0
      );
    case XLEN => 64;
    case ASIdBits => 16;
    case PgLevels => 4;
    case ILEN => 32;
    case DebugOptionsKey => DebugOptions();
  })
)