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

case class ReadFn(fn: (Bool, Bool) => (Bool, Bool, UInt))
object ReadFn {
  def apply(x: (Bool, Bool) => (Bool, Bool, UInt)) = new ReadFn(x)
  def apply(x: Bool => (Bool, UInt)) =
    new ReadFn({case (_, oready) =>
      val (ovalid, data) = x(oready)
      (true.B, ovalid, data)
    })
  def apply(x: UInt): ReadFn = ReadFn(ready => (true.B, x))
  def apply(x: Unit): ReadFn = ReadFn(0.U)
}


case class WriteFn(fn: (Bool, Bool, UInt) => (Bool, Bool))
object WriteFn {
  def apply(x: (Bool, Bool, UInt) => (Bool, Bool)) = new WriteFn(x)
  def apply(x: (Bool, UInt) => Bool) =
    new WriteFn({ case (_, oready, data) =>
      (true.B, x(oready, data))
    })
  def apply(x: UInt)  : WriteFn = WriteFn((valid, data) => { when (valid) { x := data }; true.B})
  def apply(x: Unit)  : WriteFn = WriteFn((valid, data) => { true.B })
}


case class RegMapField(width: Int, readFn: ReadFn, writeFn: WriteFn) {
  require(width >= 0)
}

object RegMapField {
  type Map = Seq[(Int, Seq[RegMapField])]
  def apply(n: Int)                           : RegMapField = new RegMapField(n, ReadFn(()), WriteFn(()))
  def apply(n: Int, r: ReadFn, w: WriteFn)    : RegMapField = new RegMapField(n, r, w)
  def apply(n: Int, rw: UInt)                 : RegMapField = new RegMapField(n, ReadFn(rw), WriteFn(rw))
  def r(n: Int, r: ReadFn)                    : RegMapField = new RegMapField(n, r, WriteFn(()))
  def r(n: Int, r: UInt)                      : RegMapField = new RegMapField(n, ReadFn(r), WriteFn(()))
  def w(n: Int, w: WriteFn)                   : RegMapField = new RegMapField(n, ReadFn(()), w)
  def w(n: Int, w: UInt)                      : RegMapField = new RegMapField(n, ReadFn(()), WriteFn(w))
}