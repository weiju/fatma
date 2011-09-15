/*
  KeymapWriter.scala - keymap writing functionality
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.os.devices

import java.io._
import java.nio._
import scala.collection.mutable.ArrayBuffer

import org.dmpp.os._

/**
 * A service class to serialize keymap information
 */
object KeymapWriter {
  // Header data
  val HunkHeaderSize         = 24
  val HunkHeaderNumLibraries = 0  // number of libraries to write, always 0
  val HunkHeaderTableSize    = 1
  val HunkHeaderFirstSlot    = 0
  val HunkHeaderLastSlot     = 0

  // Code block
  val CodeBlockHeaderSize    = 8
  val CodeBlockStart         = HunkHeaderSize
  val CodeBlockDataStart     = CodeBlockStart + CodeBlockHeaderSize
  val SizeListNode           = 14
  val SizeKeyMapStruct       = 32
  val SizeKeyMapNode         = SizeListNode + SizeKeyMapStruct
  val SizeTypesTable         = KeyMapConstants.NumLoKeys
  val SizeCapsableTable      = 8
  val SizeRepeatableTable    = 8

  val Reloc32BlockHeaderSize = 4
  val EndBlockSize = 4
}

class KeymapWriter(keymapObject: KeyMapObject) {
  import KeyMapConstants._
  import KeymapWriter._

  var out: ByteBuffer = null

  // The KeyMapNode pointers offsets are always the same
  val reloc32Offsets = new ArrayBuffer[Int]
  reloc32Offsets.append(10, 14, 18, 22, 26, 30, 34, 38, 42)

  def write(file: File) {
    println("Writing keymap: " + keymapObject.name + " to file: " + file)
    val fos = new FileOutputStream(file)
    fos.write(toByteArray)
    fos.close
  }

  /**
   * Arrange to write the following Hunk blocks:
   * 1. Header
   * 2. Code
   * 3. Reloc32
   * 4. End
   */
  def toByteArray = {
    println("Keymap to byte array: " + keymapObject.name)
    var codeBlockSize = (CodeBlockHeaderSize +
                         SizeKeyMapNode +
                         SizeTypesTable * 2 +
                         SizeCapsableTable * 2 +
                         SizeRepeatableTable * 2 +
                         sizeLoKeymap +
                         sizeHiKeymap +
                         + (keymapObject.name.length + 1))

    printf("SIZE LO KEYMAP = %d\n", sizeLoKeymap)
    printf("SIZE HI KEYMAP = %d\n", sizeHiKeymap)

    // pad code block size so reloc32 starts at an even address
    // TODO: In fact, the code block data size needs to be a multiple
    // of 4 (because the size is stored in long words)
    codeBlockSize = adjustToDividableBy4(codeBlockSize)
    val codeBlockDataSize = codeBlockSize - CodeBlockHeaderSize

    val numPointers = numPointerOffsets

    // 8 for numOffsets + hunk number, 4 for the finishing 0 as block end
    // indicator
    val reloc32BlockSize = Reloc32BlockHeaderSize + 8 + numPointers * 4 + 4
    var numBytes = (HunkHeaderSize + codeBlockSize + reloc32BlockSize +
                    EndBlockSize)
    printf("code block size = %d # total bytes = %d\n", codeBlockSize, numBytes)
    val bytes = new Array[Byte](numBytes)
    out = ByteBuffer.wrap(bytes)

    writeHunkHeader(codeBlockDataSize / 4)
    writeCodeBlock(codeBlockDataSize)

    val reloc32BlockOffset = HunkHeaderSize + codeBlockSize
    writeReloc32Block(reloc32BlockOffset)
    out.putInt(numBytes - 4, HunkTypes.End)
    bytes
  }

  private def adjustToEvenSize(size: Int) = {
    if (size % 2 == 0) size else size + 1
  }

  private def adjustToDividableBy4(size: Int) = {
    if (size % 4 == 0) size else size + (4 - (size % 4))
  }

  private def sizeLoKeymap = {
    var size = 4 * NumLoKeys // base table size
    for (keyCode <- 0 until NumLoKeys) {
      if (isDefined(keyCode) && keymapObject.isString(keyCode)) {
        size += sizeStringData(keyCode)
      } else if (isDefined(keyCode) && keymapObject.isDead(keyCode)) {
        size += sizeDeadData(keyCode)
      }
    }
    size
  }
  private def sizeHiKeymap = {
    var size = 4 * NumHiKeys // base table size
    for (keyCode <- NumLoKeys until (NumLoKeys + NumHiKeys)) {
      if (isDefined(keyCode) && keymapObject.isString(keyCode)) {
        size += sizeStringData(keyCode)
      } else if (isDefined(keyCode) && keymapObject.isDead(keyCode)) {
        size += sizeDeadData(keyCode)
      }
    }
    size
  }

  /**
   * String data is layed out as follows:
   * 1. the table of 16-bit descriptors
   * 2. the string area, padded with a zero if odd-sized
   */
  private def sizeStringData(keyCode: Int) = {
    // size of string descriptor area is (num strings * 2 bytes)
    val stringValues = keymapObject.stringValuesFor(keyCode)
    var size = stringValues.length * 2
    stringValues.foreach(str => size += str.length)
    adjustToEvenSize(size)
  }

  /**
   * Dead data is layed out as follows:
   * 1. the table of 16-bit descriptors
   * 2. a translation table of
   *    |num double dead types + 1|x|num single dead types + 1| bytes
   * Pad with zero if odd-sized
   */
  private def sizeDeadData(keyCode: Int) = {
    val deadDataDescriptors = keymapObject.deadDataDescriptorsFor(keyCode)
    var size = deadDataDescriptors.length * 2
    deadDataDescriptors.foreach(desc => desc match {
      case DeadDataDeadable(values) => size += values.length
      case _ => ;
    })
    adjustToEvenSize(size)
  }

  /**
   * Determines the number of relocatable offsets in the keymap object.
   * Besides the fixed pointers of the KeyMapNode structure, this includes
   * the pointers to string and dead data
   */
  private def numPointerOffsets = {    
    // KeyMapNode pointers at 10, 14, 18, 22, 26, 30, 34, 38, 42 are always the same
    var result = 9
    for (keyCode <- 0 until NumKeys) {
      if (isDefined(keyCode) &&
          (keymapObject.isString(keyCode) || keymapObject.isDead(keyCode))) {
        result += 1
      }
    }
    result
  }

  private def writeHunkHeader(sizeCodeBlock: Int) {
    out.putInt(0,  HunkTypes.Header)
    out.putInt(4,  HunkHeaderNumLibraries)
    out.putInt(8,  HunkHeaderTableSize)
    out.putInt(12, HunkHeaderFirstSlot)
    out.putInt(16, HunkHeaderLastSlot)
    out.putInt(20, sizeCodeBlock)
  }

  private def writeCodeBlock(dataSizeInBytes: Int) {
    printf("writeCodeBlock(), expected size: %d bytes\n", dataSizeInBytes)
    // Code block header
    out.putInt(CodeBlockStart, HunkTypes.Code)
    out.putInt(CodeBlockStart + 4, dataSizeInBytes / 4)

    // KeyMapNode
    out.putInt(CodeBlockDataStart,     0) // lnSucc
    out.putInt(CodeBlockDataStart + 4, 0) // lnPred
    out.put(CodeBlockDataStart + 8,    0) // lnType
    out.put(CodeBlockDataStart + 9,    0) // lnPri

    // write the keymap name to the end of the code block
    val nameOffset =
      CodeBlockDataStart + dataSizeInBytes - (keymapObject.name.length + 1)
    out.putInt(CodeBlockDataStart + 10,
               nameOffset - CodeBlockDataStart) // lnName
    for (i <- 0 until keymapObject.name.length) {
      out.put(nameOffset + i, keymapObject.name(i).asInstanceOf[Byte])
    }
    out.put(nameOffset + keymapObject.name.length, 0)

    // Write key mapping tables
    var nextTableOffset = SizeKeyMapNode // start right after the KeyMapNode struct
    nextTableOffset = writeTables(CodeBlockDataStart + 14, nextTableOffset,
                                  sizeLoKeymap, 0)
    nextTableOffset = writeTables(CodeBlockDataStart + 30, nextTableOffset,
                                  sizeHiKeymap, 64)
  }

  private def writeTables(pointerBase: Int, nextTable: Int, keymapSize: Int,
                          startKey: Int) = {
    var nextTableOffset = nextTable
    val capsablePred = (keyCode: Int) => keymapObject.isCapsable(keyCode)
    val repeatablePred = (keyCode: Int) => keymapObject.isRepeatable(keyCode)

    out.putInt(pointerBase, nextTableOffset) // km_hiKeyMapTypes
    writeTypesTable(CodeBlockDataStart + nextTableOffset, startKey)
    nextTableOffset += SizeTypesTable

    out.putInt(pointerBase + 4, nextTableOffset) // km_hiKeyMap
    writeKeymapTable(CodeBlockDataStart + nextTableOffset, startKey)
    nextTableOffset += keymapSize

    out.putInt(pointerBase + 8, nextTableOffset) // km_hiCapsable
    writeBitFlagTable(capsablePred,
                      CodeBlockDataStart + nextTableOffset, startKey)
    nextTableOffset += SizeCapsableTable

    out.putInt(pointerBase + 12, nextTableOffset) // km_hiRepeatable
    writeBitFlagTable(repeatablePred,
                      CodeBlockDataStart + nextTableOffset, startKey, true)
    nextTableOffset += SizeRepeatableTable
    nextTableOffset
  }

  private def writeBitFlagTable(pred: (Int) => Boolean,
                                tableStart: Int, startCode: Int,
                                repeatable: Boolean = false) {
    // write byte-by-byte
    for (bytenum <- 0 until 8) {
      val fromCode = startCode + 8 * bytenum
      val untilCode = startCode + 8 * (bytenum + 1)
      var value = 0
      for (keyCode <- fromCode until untilCode) {
        val bitindex = keyCode - fromCode
        if (pred(keyCode)) value |= (1 << bitindex)
      }
      out.put(tableStart + bytenum, value.asInstanceOf[Byte])
    }
  }
  private def writeTypesTable(tableStart: Int, startCode: Int) {
    val endCode = startCode + NumLoKeys
    for (keyCode <- startCode until endCode) {
      val index = keyCode - startCode
      out.put(tableStart + index,
              keymapObject.typeTableValueFor(keyCode))
    }
  }

  private def writeKeymapTable(tableStart: Int, startCode: Int) {

    // auxiliary table starts right after the base data table
    var auxTableOffset = tableStart + NumLoKeys * 4
    val endCode = startCode + NumLoKeys
    for (keyCode <- startCode until endCode) {
      val index = keyCode - startCode
      // write base table data
      if (isDefined(keyCode) && keymapObject.isString(keyCode)) {
        auxTableOffset = writeStringData(keyCode, tableStart, index,
                                         auxTableOffset)
      } else if (isDefined(keyCode) && keymapObject.isDead(keyCode)) {
        auxTableOffset = writeDeadData(keyCode, tableStart, index,
                                       auxTableOffset)
      } else {
        out.putInt(tableStart + index * 4,
                   keymapObject.mapIntValueFor(keyCode))
      }
    }
  }

  private def writeStringData(keyCode: Int, tableStart: Int, index: Int,
                              auxTableOffset: Int) = {
    val stringValues = keymapObject.stringValuesFor(keyCode)
    // add the string data address to the base table
    // and the entry address to reloc32Offsets
    val baseEntryAddress = tableStart + index * 4
    out.putInt(baseEntryAddress, auxTableOffset - CodeBlockDataStart)
    reloc32Offsets.append(baseEntryAddress - CodeBlockDataStart)

    //  points to the area behind the descriptors
    var auxTableEnd = auxTableOffset + stringValues.length * 2

    for (stringIndex <- 0 until stringValues.length) {
      val stringValue = stringValues(stringIndex)
      val descriptorStart = auxTableOffset + 2 * stringIndex
      out.put(descriptorStart, stringValue.length.asInstanceOf[Byte])
      out.put(descriptorStart + 1,
              (auxTableEnd - auxTableOffset).asInstanceOf[Byte])

      // now place the string at auxTableEnd, without a 0 byte
      for (charPos <- 0 until stringValue.length) {
        out.put(auxTableEnd + charPos,
                stringValue(charPos).asInstanceOf[Byte])
      }
      auxTableEnd += stringValue.length
    }
    // advance the aux table offset
    // pad the end of the aux table if necessary
    val oldAuxTableOffset = auxTableOffset
    val newAuxTableOffset = adjustToEvenSize(auxTableEnd)
    newAuxTableOffset
  }

  private def writeDeadData(keyCode: Int, tableStart: Int, index: Int,
                            descriptorTable: Int) = {
    val numColumns = keymapObject.numDeadDataTableColumns
    val descriptors = keymapObject.deadDataDescriptorsFor(keyCode)

    // add the dead data address to the base table
    // and the entry address to reloc32Offsets
    val baseEntryAddress = tableStart + index * 4
    out.putInt(baseEntryAddress, descriptorTable - CodeBlockDataStart)
    reloc32Offsets.append(baseEntryAddress - CodeBlockDataStart)

    //  points to the area behind the descriptors
    var descriptorTableEnd = descriptorTable + descriptors.length * 2
    for (descriptorIndex <- 0 until descriptors.length) {
      val descriptor = descriptors(descriptorIndex)
      val descriptorStart = descriptorTable + 2 * descriptorIndex
      descriptor match {
        case DeadDataNone(value) =>
          out.put(descriptorStart, 0)
          out.put(descriptorStart + 1, value.asInstanceOf[Byte])
        case DeadDataDeadSingle(index) =>
          out.put(descriptorStart, DpfDead.asInstanceOf[Byte])
          out.put(descriptorStart + 1, index.asInstanceOf[Byte])
        case DeadDataDeadDouble(index) =>
          out.put(descriptorStart, DpfDead.asInstanceOf[Byte])
          val indexValue = keymapObject.numDeadDataTableColumns << 4 + index
          out.put(descriptorStart + 1, indexValue.asInstanceOf[Byte])
        case DeadDataDeadable(values) =>
          out.put(descriptorStart, DpfMod.asInstanceOf[Byte])
          out.put(descriptorStart + 1,
                  (descriptorTableEnd - descriptorTable).asInstanceOf[Byte])                  
          // write out the values at the end of the table
          for (valueIndex <- 0 until values.length) {
            out.put(descriptorTableEnd + valueIndex,
                    values(valueIndex).asInstanceOf[Byte])
          }
          descriptorTableEnd += values.length
        case _ => ;
      }
    }
    // advance the aux table offset
    // pad the end of the aux table if necessary
    adjustToEvenSize(descriptorTableEnd)
  }

  private def writeReloc32Block(offset: Int) {
    out.putInt(offset, HunkTypes.Reloc32)
    out.putInt(offset + 4, reloc32Offsets.length)
    out.putInt(offset + 8, 0) // hunk number is always 0
    val offsetDataStart = offset + 12
    for (index <- 0 until reloc32Offsets.length) {
      out.putInt(offsetDataStart + index * 4, reloc32Offsets(index))
    }
    // terminating 0
    out.putInt(offsetDataStart + reloc32Offsets.length * 4, 0)
  }
}
