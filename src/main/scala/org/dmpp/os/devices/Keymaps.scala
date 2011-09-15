/*
  Keymaps.scala - keymap access functionality
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.os.devices

import scala.collection.mutable.{ArrayBuffer, Map}

// ****** Definitions *******

object KeyMapConstants {
  val LoKeyRangeStart = 0x00
  val LoKeyRangeEnd   = 0x3f
  val HiKeyRangeStart = 0x40
  val HiKeyRangeEnd   = 0x7f

  val NumLoKeys       = 64
  val NumHiKeys       = 64
  val NumKeys         = 128
  val TableSize       = NumKeys

  /* Type constants */
  val KcNoqual      = 0
  val KcVanilla     = 7 // Vanilla = Shift+Alt+Ctrl, not plain !

  val KcbShift      = 0
  val KcbAlt        = 1
  val KcbControl    = 2
  val KcbDownUp     = 4
  val KcbDead       = 5
  val KcbString     = 6  
  val KcbNop        = 7

  // Control flags, used in xKeyMapTypes
  val KcfShift      = 1 // mapping is different for shifted/non-shifted
  val KcfAlt        = 2 // mapping is different for alt/non-alt
  val KcfControl    = 4 // mapping is different for ctrl/non-ctrl
  val KcfDownUp     = 8
  val KcfDead       = 0x20 // dead-able
  val KcfString     = 0x40 // maps to string
  val KcfNop        = 0x80 // maps to nothing

  val DpbMod        = 0
  val DpbDead       = 3

  val DpfMod        = 0x01
  val DpfDead       = 0x08
  val Dp2dIndexMask = 0x0f
  val Dp2dFacShift  = 4

  def isUndefined(keyCode: Int): Boolean = {
    List(0x0e, 0x1c, 0x2c, 0x3b, 0x47, 0x48, 0x49, 0x4b).contains(keyCode) ||
    keyCode >= 0x6b
  }
  def isDefined(keyCode: Int) = !isUndefined(keyCode)
}

sealed trait DeadType
case object DeadNone extends DeadType {
  override def toString = "-"
}
case object DeadSingle extends DeadType {
  override def toString = "Dead (Single)"
}
case object DeadDouble extends DeadType {
  override def toString = "Dead (Double)"
}
case object Deadable extends DeadType {
  override def toString = "Deadable"
}

sealed trait DeadDataDescriptor {
  def deadType: DeadType
}
case class DeadDataNone(value: Int) extends DeadDataDescriptor {
  def deadType = DeadNone
}
case class DeadDataDeadSingle(translationIndex: Int)
extends DeadDataDescriptor {
  def deadType = DeadSingle
}
case class DeadDataDeadDouble(translationIndex: Int)
extends DeadDataDescriptor {
  def deadType = DeadDouble
}

case class DeadDataDeadable(translationValues: Array[Int]) extends DeadDataDescriptor {
  def deadType = Deadable
}

/**
 * Scala representation of KeyMap struct.
 */
class KeyMap(var loKeyMapTypes: Int, var loKeyMap: Int, var loCapsable: Int,
             var loRepeatable: Int,
             var hiKeyMapTypes: Int, var hiKeyMap: Int, var hiCapsable: Int,
             var hiRepeatable: Int) {
  override def toString = {
    ("KeyMap Struct:\n" +
     "loKeyMapTypes = %d [$%04x], loKeyMap = %d [$%04x], " +
     "loCapsable = %d [$%04x], loRepeatable = %d [$%04x]\n" +
     "hiKeyMapTypes = $%04x, hiKeyMap = $%04x, hiCapsable = %04x, " +
     "hiRepeatable = %04x\n").format(loKeyMapTypes, loKeyMapTypes, loKeyMap,
                                     loKeyMap, loCapsable, loCapsable,
                                     loRepeatable, loRepeatable,
                                     hiKeyMapTypes, hiKeyMap, hiCapsable,
                                     hiRepeatable)
  }
}

/**
 * Flags of a keycode. Immutable to make handling simpler.
 */
case class KeycodeFlags(isNop: Boolean, isString: Boolean, isDead: Boolean,
                        isDownUp: Boolean, isShift: Boolean, isAlt: Boolean,
                        isCtrl: Boolean, isCapsable: Boolean,
                        isRepeatable: Boolean) {
  def numModifiers = {
    var sum = 0
    if (isShift) sum += 1
    if (isAlt) sum += 1
    if (isCtrl) sum += 1
    sum
  }

  def isVanilla = isShift && isAlt && isCtrl

  def typeTableValue: Byte = {
    import KeyMapConstants._
    var result = 0
    if (isNop)    result |= KcfNop
    if (isString) result |= KcfString
    if (isDead)   result |= KcfDead
    if (isShift)  result |= KcfShift
    if (isAlt)    result |= KcfAlt
    if (isCtrl)   result |= KcfControl
    if (isDownUp) result |= KcfDownUp
    result.asInstanceOf[Byte]
  }

  /**
   * The preferred way to clone a KeycodeFlags object.
   * Based on the values of the current object, selected
   * values can be modified
   */
  def withFlags(isNop: Boolean = this.isNop,
                isString: Boolean = this.isString,
                isDead: Boolean = this.isDead,
                isDownUp: Boolean = this.isDownUp,
                isShift: Boolean = this.isShift,
                isAlt: Boolean = this.isAlt,
                isCtrl: Boolean = this.isCtrl,
                isCapsable: Boolean = this.isCapsable,
                isRepeatable: Boolean = this.isRepeatable) = {
    KeycodeFlags(isNop, isString, isDead, isDownUp, isShift, isAlt, isCtrl,
                 isCapsable, isRepeatable)
  }
}


class KeyMapObject(var name: String,
                   flags: Array[KeycodeFlags], mapValues: Array[Int],
                   stringValues: Array[List[String]],
                   deadDataDescriptors: Array[List[DeadDataDescriptor]]) {

  import KeyMapConstants._

  // tables containing all possible modifier combinations for each
  // key code
  private val stringValueTable = Array.ofDim[String](NumKeys, 8)
  private val deadDataTable = Array.ofDim[DeadDataDescriptor](NumKeys, 8)

  fillStringValueTable
  fillDeadDataTable

  def numDeadDataTableRows = maxDoubleIndex + 1
  def numDeadDataTableColumns = maxSingleIndex + 1
  def deadDataTableSize = numDeadDataTableRows * numDeadDataTableColumns

  private def maxSingleIndex: Int = {
    var maxIndex = 0
    for (keyCode <- 0 until NumKeys) {
      if (isDefined(keyCode) && isDead(keyCode)) {
        val modifierIndexes = modifierIndexesFor(keyCode)
        modifierIndexes.foreach { index =>
          deadDataTable(keyCode)(index) match {
            case DeadDataDeadSingle(index) =>
              if (index > maxIndex) maxIndex = index
            case _ => ;
          }
        }
      }
    }
    maxIndex
  }

  private def maxDoubleIndex: Int = {
    var maxIndex = 0
    for (keyCode <- 0 until NumKeys) {
      if (isDefined(keyCode) && isDead(keyCode)) {
        val modifierIndexes = modifierIndexesFor(keyCode)
        modifierIndexes.foreach { index =>
          deadDataTable(keyCode)(index) match {
            case DeadDataDeadDouble(index) =>
              if (index > maxIndex) maxIndex = index
            case _ => ;
          }
        }
      }
    }
    maxIndex
  }

  private def fillStringValueTable {
    for (keyCode <- 0 until NumKeys) {
      if (isDefined(keyCode) && isString(keyCode)) {
        val indexes = modifierIndexesFor(keyCode)
        val stringVals = stringValues(keyCode)
        if (stringVals.length != indexes.length) {
          throw new IllegalStateException("NOO, # INDEXES != # STRINGVALS !")
        }
        for (i <- 0 until stringVals.length) {
          stringValueTable(keyCode)(indexes(i)) = stringVals(i)
        }
      }
      // fill the table's empty slots with ""
      for (i <- 0 until stringValueTable(keyCode).length) {
        if (stringValueTable(keyCode)(i) == null) {
          stringValueTable(keyCode)(i) = ""
        }
      }
    }
  }
  private def fillDeadDataTable {
    for (keyCode <- 0 until NumKeys) {
      if (isDefined(keyCode) && isDead(keyCode)) {
        val indexes = modifierIndexesFor(keyCode)
        val deadData = deadDataDescriptors(keyCode)
        if (deadData.length != indexes.length) {
          throw new IllegalStateException("NOO, # INDEXES != # DEAD DATA !")
        }
        //println(("dead keycode: $%02x, indexes = ".format(keyCode)) + indexes)
        for (i <- 0 until deadData.length) {
          deadDataTable(keyCode)(indexes(i)) = deadData(i)
        }
      }
      // fill the table's empty slots with DeadDescriptorNone
      for (i <- 0 until deadDataTable(keyCode).length) {
        if (deadDataTable(keyCode)(i) == null) {
          deadDataTable(keyCode)(i) = DeadDataNone(0x20)
        }
      }
    }
  }


  // 0 = none, 1 = shift, 2 = alt 3 = alt+shift
  // 4 = ctrl, 5 = ctrl+shift 6 = ctrl+alt, 7=ctrl+alt+shift
  private def modifierIndexesFor(keyCode: Int) = {
    var result = new ArrayBuffer[Int]
    result.append(0)
    if (isShift(keyCode))                    result.append(1)
    if (isAlt(keyCode))                      result.append(2)
    if (isShift(keyCode) && isAlt(keyCode))  result.append(3)
    if (isCtrl(keyCode))                     result.append(4)
    if (isShift(keyCode) && isCtrl(keyCode)) result.append(5)
    if (isAlt(keyCode) && isCtrl(keyCode))   result.append(6)
    if (isShift(keyCode) && isAlt(keyCode) && isCtrl(keyCode))
      result.append(7)
    result
  }

  def flagsFor(keyCode: Int) = flags(keyCode)
  def setFlagsFor(keyCode: Int, newFlags: KeycodeFlags) {
    flags(keyCode) = newFlags
  }

  def mapValuesFor(keyCode: Int): Array[Int] = {
    val value = mapValues(keyCode)
    val result = new Array[Int](4)
    result(0) = value & 0xff
    result(1) = value >>> 8 & 0xff
    result(2) = value >>> 16 & 0xff
    result(3) = value >>> 24 & 0xff
    result
  }
  def setMapValue(keyCode: Int, index: Int, value: Int) {
    val currentMapValue = mapValues(keyCode)
    val setMask = value << 8 * index
    val clearMask = if (index == 0) 0xffffff00
                    else if (index == 1) 0xffff00ff
                    else if (index == 2) 0xff00ffff
                    else 0x00ffffff
    mapValues(keyCode) = (currentMapValue & clearMask) | setMask
  }
  def mapIntValueFor(keyCode: Int): Int = mapValues(keyCode)
  def stringValuesFor(keyCode: Int) = {
    var result = new ArrayBuffer[String]
    for (index <- modifierIndexesFor(keyCode)) {
      result.append(stringValueTable(keyCode)(index))
    }
    result
  }
  def setStringValueFor(keyCode: Int, index: Int, value: String) {
    stringValueTable(keyCode)(modifierIndexesFor(keyCode)(index)) = value
  }
  def deadDataDescriptorsFor(keyCode: Int) = {
    var result = new ArrayBuffer[DeadDataDescriptor]
    for (index <- modifierIndexesFor(keyCode)) {
      result.append(deadDataTable(keyCode)(index))
    }
    result
  }
  def setDeadDataDescriptor(keyCode: Int, index: Int,
                            newValue: DeadDataDescriptor) {
    deadDataTable(keyCode)(modifierIndexesFor(keyCode)(index)) = newValue
  }

  def allDeadDataDescriptorsFor(keyCode: Int) = {
    val result = new Array[DeadDataDescriptor](8)
    for (i <- 0 until 8) result(i) = deadDataTable(keyCode)(i)
    result
  }
  def setAllDeadDataDescriptors(descriptors: Map[Int, Array[DeadDataDescriptor]]) {
    descriptors.keys.foreach { keyCode => deadDataTable(keyCode) = descriptors(keyCode) }
  }
  def resizeTranslationTables {
    val newSize = deadDataTableSize
    for (keyCode <- 0 until NumKeys) {
      for (index <- 0 until 8) {
        val descriptor = deadDataTable(keyCode)(index)
        descriptor match {
          case DeadDataDeadable(translationValues) =>
            if (newSize < translationValues.length) {
              val newValues = new Array[Int](newSize)
              for (i <- 0 until newSize) newValues(i) = translationValues(i)
              deadDataTable(keyCode)(index) = DeadDataDeadable(newValues)
            } else if (newSize > translationValues.length) {
              val newValues = new Array[Int](newSize)
              for (i <- 0 until translationValues.length) newValues(i) = translationValues(i)
              for (i <- translationValues.length until newSize) newValues(i) = '?'.asInstanceOf[Int]
              deadDataTable(keyCode)(index) = DeadDataDeadable(newValues)
            }
          case _ => ;
        }
      }
    }
  }

  def isString(keyCode: Int) = flagsFor(keyCode).isString
  def isDead(keyCode: Int) = flagsFor(keyCode).isDead
  def isNop(keyCode: Int) = flagsFor(keyCode).isNop
  def isShift(keyCode: Int) = flagsFor(keyCode).isShift
  def isAlt(keyCode: Int) = flagsFor(keyCode).isAlt
  def isCtrl(keyCode: Int) = flagsFor(keyCode).isCtrl
  def isCapsable(keyCode: Int) = flagsFor(keyCode).isCapsable
  def isRepeatable(keyCode: Int) = flagsFor(keyCode).isRepeatable
  def isDownUp(keyCode: Int) = flagsFor(keyCode).isDownUp
  def typeTableValueFor(keyCode: Int) = flagsFor(keyCode).typeTableValue

  // setters
  def setIsShift(keyCode: Int, flag: Boolean) {
    flags(keyCode) = flags(keyCode).withFlags(isShift = flag)
  }
  def setIsAlt(keyCode: Int, flag: Boolean) {
    flags(keyCode) = flags(keyCode).withFlags(isAlt = flag)
  }
  def setIsCtrl(keyCode: Int, flag: Boolean) {
    flags(keyCode) = flags(keyCode).withFlags(isCtrl = flag)
  }
  def setIsCapsable(keyCode: Int, flag: Boolean) {
    flags(keyCode) = flags(keyCode).withFlags(isCapsable = flag)
  }
  def setIsRepeatable(keyCode: Int, flag: Boolean) {
    flags(keyCode) = flags(keyCode).withFlags(isRepeatable = flag)
  }
  def setIsDownUp(keyCode: Int, flag: Boolean) {
    flags(keyCode) = flags(keyCode).withFlags(isDownUp = flag)
  }

  // mutual exclusive flags: note that setting this will have an influence
  // on the other flags in the group (nop|string|dead)
  def setIsNop(keyCode: Int) {
    flags(keyCode) = flags(keyCode).withFlags(isNop = true, isString = false,
                                              isDead = false)
  }
  def setIsString(keyCode: Int) {
    flags(keyCode) = flags(keyCode).withFlags(isNop = false, isString = true,
                                              isDead = false)
  }
  def setIsDead(keyCode: Int) {
    flags(keyCode) = flags(keyCode).withFlags(isNop = false, isString = false,
                                              isDead = true)
  }
  def setIsRegular(keyCode: Int) {
    flags(keyCode) = flags(keyCode).withFlags(isNop = false, isString = false,
                                              isDead = false)
  }

  def unshiftedValue(keyCode: Int): String = {
    if (isString(keyCode)) {
      stringValuesFor(keyCode)(0)
    } else if (isDead(keyCode)) {
      val deadDataDescriptor = deadDataDescriptorsFor(keyCode)(0)
      deadDataDescriptor match {
        case DeadDataNone(value) =>
          "" + value.asInstanceOf[Char]
        case DeadDataDeadable(values) =>
          "" + values(0).asInstanceOf[Char]
        case _ => ""
      }
    } else if (isNop(keyCode)) {
      ""
    } else {
      "" + mapValuesFor(keyCode)(0).asInstanceOf[Char]
    }
  }
}
