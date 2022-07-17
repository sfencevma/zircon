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
package zircon.util

import chisel3._
import chisel3.util._

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
  def apply(x: UInt, y: UInt) = {
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
 * Signed extension.
 */
object signedExtend {
  def apply(x: UInt, n: Int) = {
    assert(n >= x.getWidth)
    Cat(Fill(n - x.getWidth, 1.U), x).asUInt
  }
  def apply(x: SInt, n: Int) = {
    assert(n >= x.getWidth)
    Cat(Fill(n-x.getWidth, 1.U), x).asSInt
  }
}

/**
 * Zero extension.
 */
object zeroExtend {
  def apply(x: UInt, n: Int) = {
    assert(n >= x.getWidth)
    Cat(Fill(n-x.getWidth, 0.U), x)
  }
}

/**
 * Saturating counter increasing.
 */
object counterInc {
  def apply(x: UInt) = {
    val n = x.getWidth
    Mux(x =/= ((1 << n) - 1).U, x + 1.U, x)
  }
}

/**
 * Saturating counter decreasing.
 */
object counterDec {
  def apply(x: UInt) = {
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