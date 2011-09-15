/*
  Memory.scala - memory access
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.hardware

/**
 * Unified memory interface to access basic memory units.
 */
trait Memory {
  def get(pos: Int): Byte
  def getInt(pos: Int): Int
  def getShort(pos: Int): Short
}

object MemoryAccess {
  def cstringAt(mem: Memory, strPtr: Int): String = {
    val buffer = new StringBuilder
    if (strPtr != 0) {
      var pos = strPtr
      var c = mem.get(pos)
      while (c != 0) {
        buffer.append(c.toChar)
        pos += 1
        c = mem.get(pos)
      }
    }
    buffer.toString
  }
}
