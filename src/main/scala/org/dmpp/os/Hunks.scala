/*
  Hunks.scala - hunk access functionality
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.os

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import java.nio._
import org.dmpp.hardware._

object HunkTypes {
  val Unit    = 0x3e7
  val Name    = 0x3e8
  val Code    = 0x3e9
  val Data    = 0x3ea
  val Bss     = 0x3eb
  val Reloc32 = 0x3ec
  val Reloc16 = 0x3ed
  val Reloc8  = 0x3ee
  val End     = 0x3f2
  val Header  = 0x3f3
}

// **********************************************************************
// **** Hunk block types
// **********************************************************************
trait Block

class CodeBlock(data: ByteBuffer, offset: Int, sizeInBytes: Int)
extends Block with Memory {
  println("Code block at offset: " + offset)
  def get(pos: Int): Byte       = data.get(offset + pos)
  def getInt(pos: Int): Int     = data.getInt(offset + pos)
  def getShort(pos: Int): Short = data.getShort(offset + pos)
}

class HeaderBlock(libraries: List[String], hunkTableSize: Int,
                  firstSlotNum: Int, lastSlotNum: Int,
                  hunkSizes: Array[Int]) extends Block {
}

class Reloc32Block(offsets: Map[Int, List[Int]]) extends Block {
  override def toString = offsets.toString
}

object EndBlock extends Block

// **********************************************************************
// **** Hunk reader
// **********************************************************************

class HunkFile(data: ByteBuffer) {
  import HunkTypes._
  if (data.getInt != Header) throw new IllegalArgumentException("not a hunk")
  private var _blocks: List[Block] = Nil
  readBlocks

  def blocks: List[Block] = _blocks

  private def readBlocks {
    var currentBlock: Block = readHeader
    while (currentBlock != EndBlock) {
      _blocks ::= currentBlock
      currentBlock = readBlock
    }
  }

  private def readBlock: Block = {
    //printf("POSITION %d ($%04x)\n", data.position, data.position)
    val id = data.getInt
    id match {
      case Code => readCodeBlock
      case Reloc32 => readReloc32Block
      case End => EndBlock
      case _ =>
        throw new UnsupportedOperationException("Unknown block id: %04x\n".format(id))
    }
  }

  private def readReloc32Block: Block = {
    val result = new HashMap[Int, List[Int]]
    var numOffsets = data.getInt
    println("Reloc32Block, # offsets = " + numOffsets)
    while (numOffsets > 0) {
      val hunkNumber = data.getInt
      var hunkOffsets: List[Int] = Nil
      for (i <- 0 until numOffsets) {
        hunkOffsets ::= data.getInt(data.position + i * 4)
      }
      result(hunkNumber) = hunkOffsets.reverse
      printf("hunk #%d, offsets = %s [%d]\n", hunkNumber, result(hunkNumber).toString,
             result(hunkNumber).length)
      // next reloc
      data.position(data.position + numOffsets * 4)
      numOffsets = data.getInt
      printf("# OFFSETS ARE NOW NEXT: %d pos = %d\n", numOffsets, data.position)
    }
    
    new Reloc32Block(result)
  }

  private def readCodeBlock: Block = {
    val numLongWords = data.getInt
    val dataStart = data.position
    val codeBlock = new CodeBlock(data, dataStart, numLongWords * 4)
    data.position(dataStart + numLongWords * 4)
    printf("CodeBlock, # long words = %d, data start = %d [$%04x]\n",
           numLongWords, dataStart, dataStart)
    codeBlock
  }

  private def readHeader = {
    val libraryNames = readLibraryList
    val hunkTableSize = data.getInt
    val firstSlotNum = data.getInt
    val lastSlotNum = data.getInt
    val numHunkSizes = lastSlotNum - firstSlotNum + 1
    var hunkSizes = Array[Int](numHunkSizes)
    for (i <- 0 until numHunkSizes) hunkSizes(i) = data.getInt
    printf("Hunk header: libnames: %s tabsize: %d slot1: %d slotlast: %d, # hunk sizes: %d, sizes = %s\n",
           libraryNames.toString, hunkTableSize, firstSlotNum, lastSlotNum, numHunkSizes,
           hunkSizes)
    hunkSizes.foreach(size => printf("hunk size: %d\n", size))
    printf("Data position now: %d\n", data.position)
    new HeaderBlock(libraryNames, hunkTableSize, firstSlotNum,
                    lastSlotNum, hunkSizes)
  }

  private def readLibraryList = {
    var result: List[String] = Nil
    var nameLength = data.getInt
    while (nameLength != 0) {
      val buffer = new StringBuilder
      var strpos = data.position
      var c = data.get(strpos)
      while (c != 0) {
        buffer.append(c.toChar)
        strpos += 1
        c = data.get(strpos)
      }
      result ::= buffer.toString
      data.position(data.position + (4 * nameLength) + 4)
      nameLength = data.getInt
    }
    result.reverse
  }
}
