/*
  KeymapReader.scala - keymap reading functionality
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.os.devices

import org.dmpp.hardware._
import org.dmpp.os._
import scala.collection.mutable.HashSet
import java.io._
import java.nio._

object KeymapReader {
  import KeyMapConstants._

  def read(data: Memory) = {
    // The code block stores a KeyMapNode structure
    // and its associated data
    val listNode = new Node(data.getInt(0), data.getInt(4),
                            data.get(8), data.get(9), data.getInt(10))
    // read the list name
    val keymapName = MemoryAccess.cstringAt(data, listNode.lnName)
    val keymap = new KeyMap(data.getInt(14), data.getInt(18), data.getInt(22),
                            data.getInt(26),
                            data.getInt(30), data.getInt(34), data.getInt(38),
                            data.getInt(42))
    println("Keymap: " + keymap)
    val keymapObject = new KeyMapObjectBuilder(keymapName, data, keymap).build
    keymapObject
  }
}

class KeyMapObjectBuilder(name: String, data: Memory, keymap: KeyMap) {
  import KeyMapConstants._

  def build = {
    val flags = readFlags
    val mapValues = readMapValues
    val stringValues = readStringValues(flags, mapValues)
    val deadDataDescriptors =
      readDeadDataDescriptors(flags, mapValues)
    new KeyMapObject(name, flags, mapValues, stringValues,
                     deadDataDescriptors)
  }

  private def numTableEntriesFor(numModifiers: Int) = {
    var result = 1
    for (i <- 0 until numModifiers) result *= 2
    result
  }

  private def readStringValues(flags: Array[KeycodeFlags],
                               mapValues: Array[Int]) = {
    val result = new Array[List[String]](TableSize)
    for (keyCode <- 0 until TableSize) {
      if (flags(keyCode).isString && isDefined(keyCode)) {
        var stringValues: List[String] = Nil
        val numModifiers = flags(keyCode).numModifiers
        val descriptorTable = mapValues(keyCode)
        val numTableEntries = numTableEntriesFor(numModifiers)

        var descriptorOffset = descriptorTable
        for (i <- 0 until numTableEntries) {
          val length = data.get(descriptorOffset)
          val stringOffset = data.get(descriptorOffset + 1)
          val buffer = new StringBuilder
          for (j <- 0 until length) {
            val c = data.get(descriptorTable + stringOffset + j)
            buffer.append(c.toChar)
          }
          stringValues ::= buffer.toString
          descriptorOffset += 2
        }
        result(keyCode) = stringValues.reverse
      }
    }
    result
  }

  private def readDeadDataDescriptors(flags: Array[KeycodeFlags],
                                      mapValues: Array[Int]) = {
    def getMaxDeadIndexes = {
      var maxSingleIndex = 0
      val doubleIndexTypes = new HashSet[Int]()

      for (keyCode <- 0 until TableSize) {
        if (flags(keyCode).isDead && isDefined(keyCode)) {

          val numModifiers = flags(keyCode).numModifiers
          val descriptorTable = mapValues(keyCode)
          var descriptorOffset = descriptorTable
          val numTableEntries = numTableEntriesFor(numModifiers)

          for (i <- 0 until numTableEntries) {
            (data.get(descriptorOffset) & 0xff) match {
              case DpfDead =>
                val descriptorValue = data.get(descriptorOffset + 1)
                val singleIndex = descriptorValue & 0x0f
                val doubleIndex = (descriptorValue >>> 4) & 0x0f
                if (singleIndex > maxSingleIndex) maxSingleIndex = singleIndex
                if (doubleIndex > 0) {
                  doubleIndexTypes.add(descriptorValue)
                }
              case _ => ;
            }
            descriptorOffset += 2
          }
        }
      }
      // return the double dead size + 1, because we also need the "alone" type
      (maxSingleIndex, doubleIndexTypes.size)
    }

    val (numSingleDeadTypes, numDoubleDeadTypes) = getMaxDeadIndexes
    val deadCharTableSize = (numSingleDeadTypes + 1) * (numDoubleDeadTypes + 1)
    printf("# SINGLE DEAD TYPES = %d # DOUBLE DEAD TYPES = %d TRANSTABLE SIZE = %d\n",
           numSingleDeadTypes, numDoubleDeadTypes, deadCharTableSize)

    val deadData = new Array[List[DeadDataDescriptor]](TableSize)
    for (keyCode <- 0 until TableSize) {
      if (flags(keyCode).isDead && isDefined(keyCode)) {
        var deadDataDescriptors: List[DeadDataDescriptor] = Nil
        var numModifiers = flags(keyCode).numModifiers
        val descriptorTable = mapValues(keyCode)
        val numTableEntries = numTableEntriesFor(numModifiers)
        var descriptorOffset = descriptorTable
        for (i <- 0 until numTableEntries) {
          val descriptorValue = data.get(descriptorOffset + 1) & 0xff
          val deadData = (data.get(descriptorOffset) & 0xff) match {
            case DpfMod =>
              //val translationTableAddress = descriptorOffset + descriptorValue
              val translationTableAddress = descriptorTable + descriptorValue
              val translationValues = new Array[Int](deadCharTableSize)
              for (i <- 0 until deadCharTableSize) {
                translationValues(i) = data.get(translationTableAddress + i) & 0xff
              }
              DeadDataDeadable(translationValues)
            case DpfDead =>
              val singleIndex = descriptorValue & 0x0f
              val doubleValue = (descriptorValue >>> 4) & 0x0f
              if (doubleValue != 0) DeadDataDeadDouble(singleIndex)
              else DeadDataDeadSingle(singleIndex)
            case _ => DeadDataNone(descriptorValue)
          }
          deadDataDescriptors ::= deadData
          descriptorOffset += 2
        }
        deadData(keyCode) = deadDataDescriptors.reverse
      }
    }
    deadData
  }


  private def readMapValues: Array[Int] = {
    val mapValues = new Array[Int](TableSize)
    for (keyCode <- 0 until TableSize) {
      mapValues(keyCode) = keymapValue(keyCode)
    }
    mapValues
  }
  private def keymapValue(keyCode: Int) = {
    val tableOffset = keymapTableOffset(keyCode)
    val index = keyIndex(keyCode)
    data.getInt(tableOffset + 4 * index) & 0xffffffff
  }

  private def readFlags = {
    val flags = new Array[KeycodeFlags](TableSize)
    for (keyCode <- 0 until TableSize) {
      flags(keyCode) = KeycodeFlags(
        isFlagSet(keyCode, KcfNop),
        isFlagSet(keyCode, KcfString),
        isFlagSet(keyCode, KcfDead),
        isFlagSet(keyCode, KcfDownUp),
        isFlagSet(keyCode, KcfShift),
        isFlagSet(keyCode, KcfAlt),
        isFlagSet(keyCode, KcfControl),
        isCapsable(keyCode),
        isRepeatable(keyCode))
    }
    flags
  }
  private def isFlagSet(keyCode: Int, flag: Int) = {
    (data.get(typeTableOffset(keyCode) + keyIndex(keyCode)) &
     flag) == flag
  }
  private def isCapsable(keyCode: Int) = {
    isBitSet(capsableTableOffset(keyCode), keyCode)
  }
  private def isRepeatable(keyCode: Int) = {
    isBitSet(repeatableTableOffset(keyCode), keyCode)
  }
  private def isBitSet(tableOffset: Int, keyCode: Int) = {
    val index = keyIndex(keyCode)
    val bytenum = index / 8
    val bitindex = index % 8
    val byteval: Int = data.get(tableOffset + bytenum)
    val mask: Int = 1 << bitindex
    (byteval & mask) == mask
  }
  private def keyIndex(keyCode: Int) = {
    if (isLoKey(keyCode)) keyCode - LoKeyRangeStart
    else keyCode - HiKeyRangeStart
  }

  private def typeTableOffset(keyCode: Int) =
    if (isLoKey(keyCode)) keymap.loKeyMapTypes else keymap.hiKeyMapTypes

  private def keymapTableOffset(keyCode: Int) =
    if (isLoKey(keyCode)) keymap.loKeyMap else keymap.hiKeyMap

  private def capsableTableOffset(keyCode: Int) =
    if (isLoKey(keyCode)) keymap.loCapsable else keymap.hiCapsable

  private def repeatableTableOffset(keyCode: Int) =
    if (isLoKey(keyCode)) keymap.loRepeatable else keymap.hiRepeatable

  private def isLoKey(keyCode: Int) =
    keyCode >= LoKeyRangeStart && keyCode <= LoKeyRangeEnd
}

