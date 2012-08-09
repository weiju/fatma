/*
  DeadDataPanel.scala
  Created on 2011-08-21
  This file is part of amiga-tools. Please see README and LICENSE for
  more information and licensing details.
*/
package org.dmpp.fatma

import javax.swing._
import javax.swing.event._
import javax.swing.table._
import java.awt.{FlowLayout, Dimension, Color}
import java.awt.event._
import scala.collection.mutable.HashMap
import org.dmpp.os.devices._

class DeadDataPanel(editor: KeyMapEditor) extends JPanel(new FlowLayout(FlowLayout.LEFT)) {
  import CharConverter._
  import ModifierTitles._
  import KeyMapConstants._
  var translationValues: Array[Int] = null

  private def keymapObject = editor.keymapObject
  private def selectedKey = editor.selectedKey
  private def flags = keymapObject.flagsFor(selectedKey)
  private def deadDataDescriptors = keymapObject.deadDataDescriptorsFor(selectedKey)
  private def numDeadDataRows = keymapObject.numDeadDataTableRows
  private def numDeadDataColumns = keymapObject.numDeadDataTableColumns

  object DeadDescriptorTableModel extends AbstractTableModel {
    val ColumnTitles = Array("Modifiers", "Dead Type", "Values", "View")
    def getRowCount = deadDataDescriptors.length
    def getColumnCount = ColumnTitles.length
    override def getColumnName(column: Int) = ColumnTitles(column)
    override def isCellEditable(row: Int, column: Int) = {
      column == 1 || column == 2 && deadDataDescriptors(row).deadType != Deadable
    }
    def getValueAt(row: Int, column: Int) = {
      try {
        if (column == 0) {
          if (flags != null) titlesFor(flags)(row) else "?"
        } else if (column == 1) {
          if (deadDataDescriptors(row) == null) "" else deadDataDescriptors(row).deadType
        } else if (column == 2) {
          if (deadDataDescriptors(row) == null) ""
          else {
            deadDataDescriptors(row) match {
              case DeadDataNone(value) => "%02x".format(value)
              case DeadDataDeadSingle(index) => "%d".format(index)
              case DeadDataDeadDouble(index) => "%d".format(index)
              case _ => "N/A"
            }
          }
        } else if (column == 3) {
          if (deadDataDescriptors(row) == null) ""
          else {
            deadDataDescriptors(row) match {
              case DeadDataNone(value) => convertChar(value.asInstanceOf[Char])
              case DeadDataDeadable(_) => "(see table)"
              case DeadDataDeadSingle(_) => "(index)"
              case DeadDataDeadDouble(_) => "(index)"
              case _ => "N/A"
            }
          }
        } else "???"
      } catch {
        case e: Throwable => e.printStackTrace
        "<ERROR>"
      }
    }
    override def setValueAt(value: Object, row: Int, col: Int) {
      // saving original state in the closure
      val originalKey = selectedKey
      val originalDescriptor = deadDataDescriptors(row)
      val originalIndex = row
      val originalNumRows = keymapObject.numDeadDataTableRows
      val originalNumColumns = keymapObject.numDeadDataTableColumns

      if (col == 1 && value != deadDataDescriptors(row).deadType) {
        // After we changed the descriptor type, we need to check
        // the max indexes in the translation table and resize if
        // necessary
        // An undo has to restore all deadable descriptors
        val command = new KeyboardEditorCommand {
          val originalDeadableData = new HashMap[Int, Array[DeadDataDescriptor]]
          for (keyCode <- 0 until NumKeys) {
            if (isDefined(keyCode) && keymapObject.isDead(keyCode)) {
              val descriptors = keymapObject.allDeadDataDescriptorsFor(keyCode)
              originalDeadableData(keyCode) = descriptors
            }
          }

          def execute {
            val newDescriptor = value match {
              case DeadNone => DeadDataNone('?'.asInstanceOf[Int])
              case DeadSingle => DeadDataDeadSingle(1)
              case DeadDouble => DeadDataDeadDouble(1)
              case Deadable =>
                val translationValues = new Array[Int](keymapObject.deadDataTableSize)
                for (i <- 0 until translationValues.length)
                  translationValues(i) = '?'.asInstanceOf[Int]
                DeadDataDeadable(translationValues)
              case _ =>
                throw new UnsupportedOperationException("unsupported type: " + value)
            }
            keymapObject.setDeadDataDescriptor(originalKey, originalIndex,
                                               newDescriptor)
            keymapObject.resizeTranslationTables
          }
          def undo {
            try {
              keymapObject.setAllDeadDataDescriptors(originalDeadableData)
            } catch {
              case e: Throwable => e.printStackTrace
            }
          }
        }
        try {
          editor.executeCommand(command)
        } catch {
          case e: Throwable => e.printStackTrace
        }
      } else if (col == 2) {
        val command = new KeyboardEditorCommand {
          def execute {
            val newValue = Integer.parseInt(value.toString, 16)
            val newDescriptor = originalDescriptor match {
              case DeadDataNone(_) => DeadDataNone(newValue)
              case DeadDataDeadSingle(_) => DeadDataDeadSingle(newValue)
              case DeadDataDeadDouble(_) => DeadDataDeadDouble(newValue)
              case _ => null
            }
            keymapObject.setDeadDataDescriptor(originalKey, originalIndex,
                                               newDescriptor)
            keymapObject.resizeTranslationTables
          }
          def undo {
            keymapObject.setDeadDataDescriptor(originalKey, originalIndex,
                                               originalDescriptor)
            keymapObject.resizeTranslationTables
          }
        }
        try {
          editor.executeCommand(command)
        } catch {
          case e: Throwable => e.printStackTrace
        }
      }
    }
  }

  object DeadTranslationTableModel extends AbstractTableModel {
    val ColumnTitles = Array("Values", "View")
    def getRowCount = if (translationValues != null) numDeadDataRows else 0
    def getColumnCount = 2
    override def getColumnName(column: Int) = ColumnTitles(column)
    def getValueAt(row: Int, column: Int) = {
      try  {
        if (translationValues == null) "-"
        else {
          val startIndex = row * numDeadDataColumns
          val buffer = new StringBuilder
          for (i <- 0 until numDeadDataColumns) {
            val value = translationValues(startIndex + i)
            column match {
              case 0 => buffer.append("%02x".format(value))
              case _ => buffer.append(convertChar(value.asInstanceOf[Char]))
            }
          }
          buffer.toString
        }
      } catch {
        case e: Throwable => 
          e.printStackTrace
          throw e
      }
    }
    override def isCellEditable(row: Int, column: Int) = column == 0
    override def setValueAt(value: Object, row: Int, col: Int) {
      val originalKey = selectedKey
      val originalIndex = descriptorTable.getSelectedRow
      val originalDescriptor = deadDataDescriptors(row).asInstanceOf[DeadDataDeadable]

      val command = new KeyboardEditorCommand {
        def execute {
          val strvalue = value.toString
          val newValues = new Array[Int](keymapObject.deadDataTableSize)
          for (i <- 0 until newValues.length)
            newValues(i) = originalDescriptor.translationValues(i)

          var strIndex = 0
          var resultIndex = row * keymapObject.numDeadDataTableColumns
          while (strIndex < strvalue.length && resultIndex < newValues.length) {
            val hexstr = "" + strvalue(strIndex) + strvalue(strIndex + 1)
            val hexnum = Integer.parseInt(hexstr, 16)
            newValues(resultIndex) = hexnum
            strIndex += 2
            resultIndex += 1
          }
          printf("Set value: %s key: $%02x, index: %d\n", value, originalKey, originalIndex)
          keymapObject.setDeadDataDescriptor(originalKey, originalIndex,
                                             DeadDataDeadable(newValues))
          // A trick to make the values visible immediately
          translationValues = newValues
          fireTableCellUpdated(row, col)
        }
        def undo {
            keymapObject.setDeadDataDescriptor(originalKey, originalIndex,
                                               originalDescriptor)
        }
      }
      try {
        editor.executeCommandNoUpdateUI(command)
        editor.updateKeyboardView
      } catch {
        case e: Throwable => e.printStackTrace
      }
    }
  }

  val deadTypeCombobox = new JComboBox(Array[Object](DeadNone, DeadSingle,
                                                     DeadDouble, Deadable))
  val descriptorTable = new JTable(DeadDescriptorTableModel) {
    showVerticalLines = true
    showHorizontalLines = true
    gridColor = Color.LIGHT_GRAY
    setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    getColumnModel.getColumn(1).setCellEditor(
      new DefaultCellEditor(deadTypeCombobox))
  }
  val scrollPane = new JScrollPane(descriptorTable)
  scrollPane.setPreferredSize(new Dimension(400, 160))
  setBorder(BorderFactory.createTitledBorder(
    BorderFactory.createEtchedBorder, "Dead Data"))
  add(scrollPane)
  descriptorTable.getSelectionModel.addListSelectionListener(
    new ListSelectionListener {
      def valueChanged(e: ListSelectionEvent) {
        if (!e.getValueIsAdjusting &&
            descriptorTable.getSelectedRow >= 0) {
          val index = descriptorTable.getSelectedRow
          val descriptor = deadDataDescriptors(index)
          translationValues = descriptor match {
            case DeadDataDeadable(values) => values
            case _ => null
          }
          DeadTranslationTableModel.fireTableDataChanged
        }
      }
    })

  val translationTable = new JTable(DeadTranslationTableModel) {
    showVerticalLines = true
    showHorizontalLines = true
    gridColor = Color.LIGHT_GRAY
    setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  }
  val scrollPane2 = new JScrollPane(translationTable)
  scrollPane2.setPreferredSize(new Dimension(300, 160))
  add(scrollPane2)

  def update {
    translationValues = null
    DeadDescriptorTableModel.fireTableDataChanged
    DeadTranslationTableModel.fireTableDataChanged
  }
}
