/*
  MapValuesPanel.scala
  Created on 2011-09-02
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.fatma

import javax.swing._
import javax.swing.table._
import java.awt.{FlowLayout, Dimension, Color}
import org.dmpp.os.devices._

/**
 * Mapping of regular keymap characters. It is impossible to map all
 * combinations with three modifiers tqo 4 slots, so in the "vanilla"
 * setting (all three modifiers are allowed), when Ctrl is pressed,
 * any other modifiers are ignored.
 * The modifiers always have the priority:
 * 1. Shift
 * 2. Alt
 * 3. Ctrl
 * So,
 * - if one modifier is allowed, only two slots are filled
 * - if two modifiers are allowed, 4 slots are filled, according to
 *   priority and the last slot is for the combination
 * - if three modifiers are allowed, this is equal to Shift+Alt,
 *   except if Ctrl is pressed, bit 5 and 6 is cleared from the
 *   unmodified slot
 */
class MapValuesPanel(editor: KeyMapEditor) extends JPanel(new FlowLayout(FlowLayout.LEFT)) {
  import CharConverter._

  val values = Array[String]("", "", "", "")
  val viewValues = Array[String]("", "", "", "")

  private def keymapObject = editor.keymapObject
  private def selectedKey = editor.selectedKey
  private def  mapValues = keymapObject.mapValuesFor(selectedKey)
  private def flags = keymapObject.flagsFor(selectedKey)

  object MapValuesTableModel extends AbstractTableModel {
    val ColumnTitles = Array("Modifier", "Value", "View")
    def getRowCount = {
      flags.numModifiers match {
        case 0 => 1
        case 1 => 2
        case _ => 4
      }
    }
    def getColumnCount = 3
    def rowTitles: Array[String] = {
      if (flags.numModifiers == 0) Array("-")
      else if (flags.numModifiers == 1 && flags.isShift) Array("-", "Shift")
      else if (flags.numModifiers == 1 && flags.isAlt) Array("-", "Alt")
      else if (flags.numModifiers == 1 && flags.isCtrl) Array("-", "Ctrl")
      else if (flags.numModifiers == 2 &&
               flags.isShift && flags.isAlt) Array("-", "Shift", "Alt", "Shift+Alt")
      else if (flags.numModifiers == 2 &&
               flags.isShift && flags.isCtrl) Array("-", "Shift", "Ctrl", "Shift+Ctrl")
      else if (flags.numModifiers == 2 &&
               flags.isAlt && flags.isCtrl) Array("-", "Alt", "Ctrl", "Alt+Ctrl")
      else if (flags.numModifiers == 3) Array("*", "Shift", "Alt", "Shift+Alt")
      else Array()
    }
    override def getColumnName(column: Int) = ColumnTitles(column)
    def getValueAt(row: Int, column: Int) = {
      if (column == 0) rowTitles(row)
      else if (column == 1) values(row)
      else if (column == 2) viewValues(row)
      else "???"
    }
    override def isCellEditable(row: Int, column: Int) = column == 1
    override def setValueAt(value: Object, row: Int, column: Int) {
      try {
        val newValue = Integer.parseInt(value.toString, 16)
        if (newValue >= 0 && newValue <= 255) {
          val command = new KeyboardEditorCommand {
            val originalKey = selectedKey
            val originalRow = row
            val originalValue = keymapObject.mapValuesFor(originalKey)(row)
            def execute {
              keymapObject.setMapValue(selectedKey, row, newValue)
            }
            def undo {
              keymapObject.setMapValue(originalKey, row, originalValue)
            }
          }
          editor.executeCommand(command)
        }
      } catch {
        case e: Throwable => e.printStackTrace
      }
    }
  }
  val table = new JTable(MapValuesTableModel) {
    showVerticalLines = true
    showHorizontalLines = true
    gridColor = Color.LIGHT_GRAY
  }
  val scrollPane = new JScrollPane(table)
  scrollPane.setPreferredSize(new Dimension(200, 100))
  setBorder(BorderFactory.createTitledBorder(
    BorderFactory.createEtchedBorder, "Map Values"))
  add(scrollPane)

  def update {
    for (i <- 0 until 4) {
      values(i) = "%02x".format(mapValues(i))
      viewValues(i) = convertChar(mapValues(i).asInstanceOf[Char])
    }
    MapValuesTableModel.fireTableDataChanged
  }
}
