/*
  StringTablePanel.scala
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.fatma

import javax.swing._
import javax.swing.table._
import java.awt.{FlowLayout, Color, Dimension}
import org.dmpp.os.devices._

// Escape sequences:
// One character: 0x9b
// Two characters: 0x1b 0x5b = Esc+[ = \e [
class StringTablePanel(editor: KeyMapEditor) extends JPanel(new FlowLayout(FlowLayout.LEFT)) {
  import CharConverter._
  import ModifierTitles._

  private def keymapObject = editor.keymapObject
  private def selectedKey = editor.selectedKey
  private def flags = keymapObject.flagsFor(selectedKey)
  private def stringTable = keymapObject.stringValuesFor(selectedKey)

  object StringTableModel extends AbstractTableModel {
    val ColumnTitles = Array("Modifiers", "Bytes", "String")
    def getRowCount = stringTable.length
    def getColumnCount = ColumnTitles.length
    override def getColumnName(column: Int) = ColumnTitles(column)
    def getValueAt(row: Int, column: Int) = {
      try {
        if (column == 0) {
          if (flags != null) titlesFor(flags)(row) else "?" 
        } else if (column == 1) {
          if (stringTable(row) == null) "" else charsToHex(stringTable(row))
        } else if (column == 2) {
          if (stringTable(row) == null) "" else convertString(stringTable(row))
        } else "???"
      } catch {
        case e: Throwable =>
          e.printStackTrace
          "<ERROR>"
      }
    }
    override def isCellEditable(row: Int, column: Int) = column == 1
    override def setValueAt(value: Object, row: Int, col: Int) {
      val command = new KeyboardEditorCommand {
        val originalValue = stringTable(row)
        val originalKey = selectedKey
        val originalIndex = row
        def execute {
          val strvalue = value.toString
          val buffer = new StringBuilder
          var index = 0
          while (index < strvalue.length) {
            val hexstr = "" + strvalue(index) + strvalue(index + 1)
            val hexnum = Integer.parseInt(hexstr, 16)
            buffer.append(hexnum.asInstanceOf[Char])
            index += 2
          }
          keymapObject.setStringValueFor(originalKey, originalIndex, buffer.toString)
        }
        def undo {
          keymapObject.setStringValueFor(originalKey, originalIndex, originalValue)
        }
      }
      editor.executeCommand(command)
    }
  }
  val table = new JTable(StringTableModel) {
    showVerticalLines = true
    showHorizontalLines = true
    gridColor = Color.LIGHT_GRAY
  }
  val scrollPane = new JScrollPane(table)
  scrollPane.setPreferredSize(new Dimension(400, 160))
  setBorder(BorderFactory.createTitledBorder(
    BorderFactory.createEtchedBorder, "Strings"))
  add(scrollPane)

  def update {
    StringTableModel.fireTableDataChanged
  }
}
