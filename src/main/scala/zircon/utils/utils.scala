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
package zircon.utils

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

/**
 * N-wide one-hot priority encoder.
 */
object selectFirst {
  def apply(in: UInt, n: Int): Vec[UInt] = {
    val sels = Wire(Vec(n, UInt(in.getWidth.W)))
    var mask = in

    for (i <- 0 until n) {
      sels(i) := PriorityEncoderOH(mask)
      mask = mask & (~sels(i))
    }
    sels
  }
  def apply(in: UInt): UInt = apply(in, 1)(0)
}

/**
 * Check whether the element is in the sequence.
 */
object isOneOf {
  def apply(x: UInt, sel: Seq[UInt]): Bool = {
    sel.map(_ === x).reduce(_|_)
  }
}

/**
 * Check the age of the cycle pointer.
 */
object checkOld {
  def apply(x: UInt, y: UInt): Bool = {
    assert(x.getWidth == y.getWidth)
    val n = x.getWidth
    Mux(x(n - 1) ^ y(n - 1), x(n - 2, 0) >= y(n - 2, 0), x(n - 2, 0) < y(n - 2, 0))
  }
}

/**
 * Get real index of a cycle pointer.
 */
object hashIdx {
  def apply(x: UInt) = {
    val n = x.getWidth
    assert(n >= 2)
    x(n - 2, 0)
  }
}

/**
 * Bits extension
 */
object bitsExtend {
  def apply(x: UInt, n: Int, s: Int, b: Bool): UInt = {
    assert(n >= x.getWidth)
    assert(s <= x.getWidth && s >= 0 && n >= s + 1)
    Cat(Fill(n-s-1, b.asUInt), x(s, 0)).asUInt
  }
  def apply(x: SInt, n: Int, s: Int, b: Bool): SInt = {
    assert(n >= x.getWidth)
    assert(s <= x.getWidth && s >= 0 && n >= s + 1)
    Cat(Fill(n-s-1, b.asUInt), x(s, 0)).asSInt
  }
}

/**
 * Signed extension.
 */
object signedExtend {
  //  UInt
  def apply(x: UInt, n: Int, s: Int): UInt = bitsExtend(x, n, s, x(s))
  def apply(x: UInt, n: Int): UInt = apply(x, n, x.getWidth - 1)

  //  SInt
  def apply(x: SInt, n: Int, s: Int): SInt = bitsExtend(x, n, s, x(s))
  def apply(x: SInt, n: Int): SInt = apply(x, n, x.getWidth - 1)
}

/**
 * Zero extension.
 */
object zeroExtend {
  //  UInt
  def apply(x: UInt, n: Int, s: Int): UInt = bitsExtend(x, n, s, false.B)
  def apply(x: UInt, n: Int): UInt = apply(x, n, x.getWidth - 1)

  //  SInt
  def apply(x: SInt, n: Int, s: Int): SInt = bitsExtend(x, n, s, false.B)
  def apply(x: SInt, n: Int): SInt = apply(x, n, x.getWidth - 1)
}

/**
 * Saturating counter increasing.
 */
object counterInc {
  def apply(x: UInt): UInt = {
    val n = x.getWidth
    Mux(x =/= ((1 << n) - 1).U, x + 1.U, x)
  }
}

/**
 * Saturating counter decreasing.
 */
object counterDec {
  def apply(x: UInt): UInt = {
    val n = x.getWidth
    Mux(x =/= 0.U, x - 1.U, x)
  }
}

/**
 * Mask data.
 */
object MaskData {
  def apply(oldData: UInt, newData: UInt, fullmask: UInt) = {
    (newData & fullmask) | (oldData & ~fullmask)
  }
}

/**
 * Round Robin Algorithm
 */
object RoundRobin {
  def apply(nPorts: Int, requests: Vec[Bool], ack: Bool) = {
    if (nPorts == 1) {
      1.U(1.W)
    } else {
      val reqs = Reverse(Cat(requests))
      val mask = RegInit(UInt(nPorts.W), ~0.U(nPorts.W)) 
      val filter = Cat(reqs & ~mask, reqs)
      val unready = (rightOR(filter, nPorts * 2, nPorts) >> 1) | (mask << nPorts)
      val readys = ~((unready >> nPorts) & unready(nPorts-1,0))
      when (ack && reqs.orR) {
        mask := leftOR(readys & reqs, nPorts)
      }
      readys(nPorts-1,0)
    }
  }
}

/**
 * Unknown
 */
object GTimer {
  def apply() = {
    val c = RegInit(0.U(64.W))
    c := c + 1.U
    c
  }
}


/**
 * Hold unless
 */
object HoldUnless {
  def apply[T <: Data](x: T, en: Bool): T = Mux(en, x, RegEnable(x, 0.U.asTypeOf(x), en))
}

